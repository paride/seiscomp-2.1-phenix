/***************************************************************************** 
 * arclink.cc
 *
 * ArcLink server
 *
 * (c) 2004 Andres Heinloo, GFZ Potsdam
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/
 *****************************************************************************/

#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <set>
#include <cstring>
#include <cstdio>
#include <cstdarg>
#include <csignal>
#include <cerrno>

#include <unistd.h>
#include <syslog.h>
#include <termios.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <libxml/xmlIO.h>
#include <libxml/tree.h>

#if defined(__GNU_LIBRARY__) || defined(__GLIBC__)
#include <getopt.h>
#endif

#ifdef TCPWRAP
extern "C" {
#include "tcpd.h"
}
#endif

#include "confbase.h"
#include "conf_ini.h"
#include "confattr.h"
#include "cppstreams.h"
#include "utils.h"
#include "diag.h"

#define MYVERSION   "0.1 (2004.245)"

using namespace std;
using namespace CfgParser;
using namespace CPPStreams;
using namespace Utilities;

namespace {

const int         BLOCKSIZE        = 512;
const int         MAXCMDLEN        = 100;
const int         ERR_SIZE         = 100;
const int         REQUEST_FD       = 62;
const int         RESPONSE_FD      = 63;
const char *const SHELL            = "/bin/sh";
const char *const ident_str        = "ArcLink v" MYVERSION;
string            config_file      = "/home/sysop/config/arclink.ini";

#if defined(__GNU_LIBRARY__) || defined(__GLIBC__)
const char *const opterr_message = "Try `%s --help' for more information\n";
const char *const help_message = 
    "Usage: %s [options]\n"
    "\n"
    "-v                            Increase verbosity level\n"
    "    --verbosity=LEVEL         Set verbosity level\n"
    "-D, --daemon                  Daemon mode\n"
    "-f, --config-file=FILE        Alternative configuration file\n"
    "-V, --version                 Show version information\n"
    "-h, --help                    Show this help message\n";
#else
const char *const opterr_message = "Try `%s -h' for more information\n";
const char *const help_message =
    "Usage: %s [options]\n"
    "\n"
    "-v             Increase verbosity level\n"
    "-D             Daemon mode\n"
    "-f FILE        Alternative configuration file\n"
    "-V             Show version information\n"
    "-h             Show this help message\n";
#endif

string daemon_name;
int verbosity = 0;
bool daemon_mode = false, daemon_init = false;
volatile sig_atomic_t terminate_proc = 0;

void int_handler(int sig)
  {
    terminate_proc = 1;
  }

//*****************************************************************************
// Close-on-Exec versions of some UNIX calls
//*****************************************************************************

// We don't want to give open file descriptors to request handlers

inline int xopen(const char *pathname, int flags)
  {
    int fd;
    if((fd = open(pathname, flags)) >= 0)
        fcntl(fd, F_SETFD, FD_CLOEXEC);

    return fd;
  }

inline int xcreat(const char *pathname, mode_t mode)
  {
    int fd;
    if((fd = creat(pathname, mode)) >= 0)
        fcntl(fd, F_SETFD, FD_CLOEXEC);

    return fd;
  }

inline int xsocket(int domain, int type, int protocol)
  {
    int fd;
    if((fd = socket(domain, type, protocol)) >= 0)
        fcntl(fd, F_SETFD, FD_CLOEXEC);

    return fd;
  }

#if defined(__GNU_LIBRARY__) && __GNU_LIBRARY__ < 2
inline int xaccept(int s, struct sockaddr *addr, int *addrlen)
#else
inline int xaccept(int s, struct sockaddr *addr, socklen_t *addrlen)
#endif
  {
    int fd;
    if((fd = accept(s, addr, addrlen)) >= 0)
        fcntl(fd, F_SETFD, FD_CLOEXEC);

    return fd;
  }

//*****************************************************************************
// XML Helpers
//*****************************************************************************

void xml_error(void *ctx, const char *fmt, ...)
  { 
    va_list ap;
    va_start(ap, fmt);
    
    char errbuf[ERR_SIZE];
    vsnprintf(errbuf, ERR_SIZE, fmt, ap);

    va_end(ap);
    
    const char* p = errbuf;
    while(*p)
      {
        int len;
        if((len = strcspn(p, "\n")) > 0)
            logs(LOG_ERR) << string(p, len);

        if(p[len] == '\n')
          {
            logs(LOG_ERR) << endl;
            ++p;
          }

        p += len;  
      }
  }

xmlNodePtr xml_new_child(xmlNodePtr parent, const char *name)
  {
    xmlNodePtr retval;
    if((retval = xmlNewChild(parent, NULL, (const xmlChar *) name, NULL)) == NULL)  
        throw bad_alloc();

    return retval;
  } 

void xml_new_prop(xmlNodePtr node, const char *name, const char *value)
  { 
    if(xmlNewProp(node, (const xmlChar *) name, (const xmlChar *) value) == NULL)
        throw bad_alloc();
  }

//*****************************************************************************
// Exceptions
//*****************************************************************************

class ArclinkError: public GenericException
  {
  public:
    ArclinkError(const string &message):
      GenericException("Arclink", message) {}
  };

class ArclinkLibraryError: public ArclinkError
  {
  public:
    ArclinkLibraryError(const string &message):
      ArclinkError(message + " (" + strerror(errno) + ")") {}
  };

class ArclinkCannotOpenFile: public ArclinkLibraryError
  {
  public:
    ArclinkCannotOpenFile(const string &name):
      ArclinkLibraryError("cannot open file '" + name + "'") {}
  };

class ArclinkRequestError: public ArclinkError
  {
  public:
    ArclinkRequestError(const string &message):
      ArclinkError(message) {}
  };

//*****************************************************************************
// LogFunc
//*****************************************************************************

class LogFunc
  {
  public:
    enum { msglen = 200 };
    
    void operator()(int priority, const string &msg)
      {
        if(daemon_init)
          {
            syslog(priority, "%s", msg.c_str());
          }
        else
          {
            int verb = 2;
            
            switch(priority)
              {
              case LOG_EMERG:
              case LOG_ALERT:
              case LOG_CRIT:
              case LOG_ERR:
                verb = -1; break;

              case LOG_WARNING:
              case LOG_NOTICE:
                verb = 0; break;

              case LOG_INFO:
                verb = 1; break;

              case LOG_DEBUG:
                verb = 2;
              }

            if(verbosity < verb)
                return;

            time_t t = time(NULL);
            char *p = asctime(localtime(&t));
            string::size_type n = msg.length();
            while(n > 0 && (msg[n - 1] == '\n' || msg[n - 1] == '\r')) --n;
            string msgout = string(p, strlen(p) - 1) + " - " + daemon_name +
              ": " + string(msg, 0, n) + '\n';
            write(STDOUT_FILENO, msgout.c_str(), msgout.length());
          }
      }
  };

//*****************************************************************************
// MessageBuffer
//*****************************************************************************

class MessageBuffer
  {
  friend class MessageStore;
  friend class rc_ptr<MessageBuffer>;
  private:
    char *dataptr;
    int used_bytes;

    ~MessageBuffer()
      {
        free(dataptr);
      }

  public:
    const int capacity;

    MessageBuffer(int capacity_init): used_bytes(0), capacity(capacity_init)
      {
        if((dataptr = (char *) malloc(capacity)) == NULL) throw bad_alloc();
      }

    char *data() const
      {
        return dataptr;
      }

    int size() const
      {
        return used_bytes;
      }
  };

//*****************************************************************************
// MessageStore
//*****************************************************************************

class MessageStore
  {
  private:
    list<rc_ptr<MessageBuffer> > &buffer_list;
    const int bufsize;

  public:
    MessageStore(list<rc_ptr<MessageBuffer> > &buffer_list_init,
      int bufsize_init):
      buffer_list(buffer_list_init), bufsize(bufsize_init) {}

    rc_ptr<MessageBuffer> get_buffer();
    void queue_buffer(rc_ptr<MessageBuffer> buf, int size);
  };

rc_ptr<MessageBuffer> MessageStore::get_buffer()
  {
    return new MessageBuffer(bufsize);
  }

void MessageStore::queue_buffer(rc_ptr<MessageBuffer> buf, int size)
  {
    buf->used_bytes = size;
    buffer_list.push_back(buf);
  }

//*****************************************************************************
// MessageOutput
//*****************************************************************************

class MessageOutput
  {
  private:
    rc_ptr<MessageStore> msgs;
    rc_ptr<MessageBuffer> buf;
    int pos;

  public:
    enum { msglen = 100 };

    MessageOutput(list<rc_ptr<MessageBuffer> > &buffer_list, int bufsize):
      msgs(new MessageStore(buffer_list, bufsize)), buf(NULL), pos(0) {}

    ~MessageOutput();
    
    void operator()(int priority, const string &msg);
  };

MessageOutput::~MessageOutput()
  {
    if(buf != NULL) msgs->queue_buffer(buf, pos);
  }
    
void MessageOutput::operator()(int priority, const string &msg)
  {
    const char* p = msg.c_str();
    int len = msg.length();
    int nwritten = 0;

    while(nwritten < len)
      {
        if(buf == NULL)
          {
            buf = msgs->get_buffer();
            pos = 0;
          }

        int size = min(buf->capacity - pos, len - nwritten);
        memcpy(buf->data() + pos, p + nwritten, size);
        nwritten += size;
        pos += size;

        internal_check(pos <= buf->capacity);
        
        if(pos == buf->capacity)
          {
            msgs->queue_buffer(buf, pos);
            buf = NULL;
          }
      }
  }

//*****************************************************************************
// XMLOutput
//*****************************************************************************

class XMLOutput
  {
  private:
    rc_ptr<MessageStore> msgs;
    rc_ptr<MessageBuffer> buf;
    int pos;

    void iowrite(const char *buf, int len);
    void ioclose();

    static int iowrite_proxy(void *ctx, const char *buf, int len)
      {
        static_cast<XMLOutput *>(ctx)->iowrite(buf, len);
        return len;
      }

    static int ioclose_proxy(void *ctx)
      {
        static_cast<XMLOutput *>(ctx)->ioclose();
        return 0;
      }

  public:
    XMLOutput(list<rc_ptr<MessageBuffer> > &buffer_list, int bufsize):
      msgs(new MessageStore(buffer_list, bufsize)), buf(NULL), pos(0) {}
    ~XMLOutput();
     void write_doc(xmlDocPtr doc);
  };

void XMLOutput::iowrite(const char *p, int len)
  {
    int nwritten = 0;

    while(nwritten < len)
      {
        if(buf == NULL)
          {
            buf = msgs->get_buffer();
            pos = 0;
          }

        int size = min(buf->capacity - pos, len - nwritten);
        memcpy(buf->data() + pos, p + nwritten, size);
        nwritten += size;
        pos += size;

        internal_check(pos <= buf->capacity);
        
        if(pos == buf->capacity)
          {
            msgs->queue_buffer(buf, pos);
            buf = NULL;
          }
      }
  }

void XMLOutput::ioclose()
  {
    if(buf != NULL)
      {
        msgs->queue_buffer(buf, pos);
        buf = NULL;
      }
  }

XMLOutput::~XMLOutput()
  {
    ioclose();
  }
    
void XMLOutput::write_doc(xmlDocPtr doc)
  {
    xmlOutputBufferPtr obuf;
    if((obuf = xmlOutputBufferCreateIO(iowrite_proxy, ioclose_proxy, this, NULL)) == NULL)
        throw bad_alloc();

//    xmlSaveFileTo(obuf, doc, NULL);
//  pretty-print for easier debugging
    xmlSaveFormatFileTo(obuf, doc, NULL, 1);
  }

//*****************************************************************************
// StatusInterface
//*****************************************************************************

enum RequestStatus
  { 
    STATUS_UNSET,
    STATUS_PROC,
    STATUS_SYNERR,
    STATUS_CANCEL,
    STATUS_OK,
    STATUS_WARN,
    STATUS_RETRY,
    STATUS_NODATA
  };

class StatusInterface
  {
  protected:
    RequestStatus _status;
    string _message;
    int _size;

  public:
    StatusInterface(): _status(STATUS_UNSET), _size(0) {}

    void set_status(RequestStatus s)
      {
        _status = s;
      }
      
    enum RequestStatus status() const
      {
        return _status;
      }
    
    void set_message(const string &msg)
      {
        _message = msg;
      }

    string message() const
      {
        return _message;
      }
    
    void set_size(int s)
      {
        _size = s;
      }
    
    int size() const
      {
        return _size;
      }
    
    bool status_unset() const
      {
        return (_status == STATUS_UNSET);
      }
    
    const char *status_string() const
      {
        switch(_status)
          {
          case STATUS_PROC:   return "PROCESSING";
          case STATUS_SYNERR: return "SYNERR";
          case STATUS_CANCEL: return "CANCELLED";
          case STATUS_OK:     return "OK";
          case STATUS_WARN:   return "WARNING";
          case STATUS_RETRY:  return "RETRY";
          case STATUS_NODATA: return "NODATA";
          case STATUS_UNSET:  return "UNSET";
          }

        return "UNKNOWN";
      }
  };

//*****************************************************************************
// RequestLine
//*****************************************************************************

class RequestLine: public StatusInterface
  {
  private:
    const string &ref;

  public:
    RequestLine(const string &ref_init): ref(ref_init) {}

    void cancel();
    void getxml(xmlNodePtr parent) const;
  };

void RequestLine::cancel()
  {
    if(_status != STATUS_PROC && _status != STATUS_OK &&
      _status != STATUS_WARN)
        return;

    set_status(STATUS_CANCEL);
    set_size(0);
  }

void RequestLine::getxml(xmlNodePtr parent) const
  {
    xmlNodePtr child;
    child = xml_new_child(parent, "line");
    xml_new_prop(child, "content", ref.c_str());
    xml_new_prop(child, "status", status_string());
    xml_new_prop(child, "size", to_string(_size).c_str());
    xml_new_prop(child, "message", _message.c_str());
  }
  
//*****************************************************************************
// RequestVolume
//*****************************************************************************

class RequestVolume: public StatusInterface
  {
  private:
    list<rc_ptr<RequestLine> > req_lines;

  public:
    const string id;

    RequestVolume(const string &id_init): id(id_init) {}
    
    void add_line(rc_ptr<RequestLine> ln)
      {
        req_lines.push_back(ln);
      }

    bool downloadable()
      {
        return (_status == STATUS_OK || _status == STATUS_WARN);
      }

    void cancel();
    void getxml(xmlNodePtr parent) const;
  };

void RequestVolume::cancel()
  {
    if(_status != STATUS_PROC && _status != STATUS_OK &&
      _status != STATUS_WARN)
        return;
    
    set_status(STATUS_CANCEL);
    set_size(0);
        
    list<rc_ptr<RequestLine> >::iterator p;
    for(p = req_lines.begin(); p != req_lines.end(); ++p)
        (*p)->cancel();
  }

void RequestVolume::getxml(xmlNodePtr parent) const
  {
    xmlNodePtr child;
    child = xml_new_child(parent, "volume");
    xml_new_prop(child, "id", id.c_str());
    xml_new_prop(child, "status", status_string());
    xml_new_prop(child, "size", to_string(_size).c_str());
    xml_new_prop(child, "message", _message.c_str());

    list<rc_ptr<RequestLine> >::const_iterator p;
    for(p = req_lines.begin(); p != req_lines.end(); ++p)
        (*p)->getxml(child);
  }

//*****************************************************************************
// Request
//*****************************************************************************

typedef int RequestID;
const int RID_ALL = -1;

enum RequestType { REQ_WAVEFORM, REQ_INVENTORY, REQ_RESPONSE };

class Request
  {
  private:
    const string location;
    const int max_lines;
    const string user;
    const string institution;
    list<string> content;
    vector<rc_ptr<RequestLine> > req_lines;
    map<string, rc_ptr<RequestVolume> > req_volumes;
    bool im_ready;

    const char *type_string() const;
    
  public:
    const RequestID id;
    const RequestType type;
    Request(RequestID id_init, RequestType type_init,
      const string &user_init, const string &pass,
      const string &institution_init, const string &location_init,
      int max_lines_init);

    int size() const;
    void response_accepted(const vector<string> &cmdvec);
    void response_rejected(const vector<string> &cmdvec, const string &msg);
    void response_STATUS(const vector<string> &cmdvec);
    void response(const vector<string> &s);
    void push_line(const string &str);
    void push_end();
    int open_first(map<string, rc_ptr<RequestVolume> >::iterator &vol_iter,
      size_t pos);
    int open_next(map<string, rc_ptr<RequestVolume> >::iterator &vol_iter);
    int open_vol(const string &volume, size_t pos, int &volsize);
    void getmsg(ostream &mout) const;
    void getxml(xmlNodePtr parent, bool show_user) const;
    void getstate();

    list<string>::const_iterator begin() const
      {
        return content.begin();
      }

    list<string>::const_iterator end() const
      {
        return content.end();
      }

    bool auth(const string &user1) const
      {
        return (user1 == user);
      }

    bool ready() const
      {
        return im_ready;
      }
    
    void reset()
      {

      }
  };

const char *Request::type_string() const
  {
    switch(type)
      {
      case REQ_WAVEFORM:  return "WAVEFORM";
      case REQ_INVENTORY: return "INVENTORY";
      case REQ_RESPONSE:  return "RESPONSE";
      }

    return "UNKNOWN";
  }
    
Request::Request(RequestID id_init, RequestType type_init,
  const string &user_init, const string &pass, const string &institution_init,
  const string &location_init, int max_lines_init):
  location(location_init), max_lines(max_lines_init), user(user_init),
  institution(institution_init), im_ready(false), id(id_init), type(type_init)
  {
    if(pass.length() > 0)
        content.push_back("USER " + user + " " + pass);
    else
        content.push_back("USER " + user);

    if(institution.length() > 0)
        content.push_back("INSTITUTION " + institution);

    content.push_back("REQUEST " + string(type_string()) + " " + to_string(id));
  }

int Request::size() const
  {
    int n = 0;
    map<string, rc_ptr<RequestVolume> >::const_iterator p;
    for(p = req_volumes.begin(); p != req_volumes.end(); ++p)
        n += p->second->size();

    return n;
  }

void Request::response_STATUS(const vector<string> &cmdvec)
  {
    if(cmdvec.size() < 4)
        throw ArclinkRequestError("STATUS requires at least 3 arguments");

    rc_ptr<StatusInterface> sti;
    if(!strcasecmp(cmdvec[1].c_str(), "LINE"))
      {
        char c;
        int ln;

        if(sscanf(cmdvec[2].c_str(), "%d%c", &ln, &c) != 1 ||
          ln >= (int)req_lines.size())
            throw ArclinkRequestError("invalid line number");

        if(!strcasecmp(cmdvec[3].c_str(), "PROCESSING"))
          {
            if(cmdvec.size() != 5)
                throw ArclinkRequestError("incorrect number of arguments");

            rc_ptr<RequestVolume> vol;
            map<string, rc_ptr<RequestVolume> >::iterator p;
            if((p = req_volumes.find(cmdvec[4])) != req_volumes.end())
              {
                vol = p->second;
              }
            else
              {
                vol = new RequestVolume(cmdvec[4]);
                vol->set_status(STATUS_PROC);
                req_volumes.insert(make_pair(cmdvec[4], vol));
              }
            
            req_lines[ln]->set_status(STATUS_PROC);
            vol->add_line(req_lines[ln]);
            return;
          }

        if(!strcasecmp(cmdvec[3].c_str(), "CANCEL"))
          {
            if(cmdvec.size() != 4)
                throw ArclinkRequestError("incorrect number of arguments");
            
            req_lines[ln]->cancel();
            return;
          }

        sti = rc_ptr_cast<StatusInterface>(req_lines[ln]);
      }
    else if(!strcasecmp(cmdvec[1].c_str(), "VOLUME"))
      {
        map<string, rc_ptr<RequestVolume> >::iterator p;
        if((p = req_volumes.find(cmdvec[2])) == req_volumes.end())
            throw ArclinkRequestError("volume not found");

        if(!strcasecmp(cmdvec[3].c_str(), "CANCEL"))
          {
            if(cmdvec.size() != 4)
                throw ArclinkRequestError("incorrect number of arguments");
            
            p->second->cancel();
            return;
          }

        sti = rc_ptr_cast<StatusInterface>(p->second);
      }

    if(!strcasecmp(cmdvec[3].c_str(), "OK"))
      {
        sti->set_status(STATUS_OK);
      }
    else if(!strcasecmp(cmdvec[3].c_str(), "WARN"))
      {
        sti->set_status(STATUS_WARN);
      }
    else if(!strcasecmp(cmdvec[3].c_str(), "RETRY"))
      {
        sti->set_status(STATUS_RETRY);
      }
    else if(!strcasecmp(cmdvec[3].c_str(), "NODATA"))
      {
        sti->set_status(STATUS_NODATA);
      }
    else if(!strcasecmp(cmdvec[3].c_str(), "MESSAGE"))
      {
        if(cmdvec.size() < 5)
            throw ArclinkRequestError("incorrect number of arguments");

        vector<string>::const_iterator p = cmdvec.begin() + 4;
        string msg = *p;

        while((++p) != cmdvec.end())
            msg += (" " + *p);

        sti->set_message(msg);
      }
    else if(!strcasecmp(cmdvec[3].c_str(), "SIZE"))
      {
        if(cmdvec.size() != 5)
            throw ArclinkRequestError("incorrect number of arguments");

        char c;
        int i;
        if(sscanf(cmdvec[4].c_str(), "%d%c", &i, &c) != 1)
            throw ArclinkRequestError("invalid size");

        sti->set_size(i);
      }
    else
      {
        throw ArclinkRequestError("invalid status message");
      }
  }
 
void Request::response(const vector<string> &cmdvec)
  {
    if(!strcasecmp(cmdvec[0].c_str(), "STATUS"))
      {
        response_STATUS(cmdvec);
        return;
      }

    if(!strcasecmp(cmdvec[0].c_str(), "CANCEL"))
      {
        map<string, rc_ptr<RequestVolume> >::iterator p1;
        for(p1 = req_volumes.begin(); p1 != req_volumes.end(); ++p1)
            p1->second->cancel();
            
        // cancel also remaining lines that are not in any volume
        vector<rc_ptr<RequestLine> >::iterator p2;
        for(p2 = req_lines.begin(); p2 != req_lines.end(); ++p2)
            (*p2)->cancel();

        im_ready = true;
        return;
      }
    
    if(!strcasecmp(cmdvec[0].c_str(), "END"))
      {
        if(cmdvec.size() != 1)
            throw ArclinkRequestError("syntax error");

        im_ready = true;
        return;
      }

    throw ArclinkRequestError("syntax error");
  }

void Request::push_line(const string &str)
  {
    if((int)content.size() >= max_lines)
        return;
    
    content.push_back(str);
    req_lines.push_back(new RequestLine(content.back()));
  }

void Request::push_end()
  {
    if((int)content.size() >= max_lines)
        throw ArclinkRequestError("maximum request size exceeded");

    content.push_back("END");
  }

int Request::open_first(map<string, rc_ptr<RequestVolume> >::iterator &vol_iter,
  size_t pos)
  {
    if(!im_ready)
        return -1;
    
    vol_iter = req_volumes.begin();

    while(vol_iter != req_volumes.end() && !vol_iter->second->downloadable())
        ++vol_iter;
    
    if(vol_iter == req_volumes.end())
        throw ArclinkRequestError("no downloadable volumes found");
    
    int fd;
    if((fd = xopen((location + "." + vol_iter->first).c_str(), O_RDONLY)) < 0)
        logs(LOG_ERR) << strerror(errno) << endl;

    // seek...

    ++vol_iter;
    return fd;
  }

int Request::open_next(map<string, rc_ptr<RequestVolume> >::iterator &vol_iter)
  {
    internal_check(im_ready);
    
    while(vol_iter != req_volumes.end() && !vol_iter->second->downloadable())
        ++vol_iter;
    
    if(vol_iter == req_volumes.end())
        return -1;
    
    int fd;
    if((fd = xopen((location + "." + vol_iter->first).c_str(), O_RDONLY)) < 0)
        logs(LOG_ERR) << strerror(errno) << endl;

    ++vol_iter;
    return fd;
  }

int Request::open_vol(const string &volume, size_t pos, int &volsize)
  {
    map<string, rc_ptr<RequestVolume> >::iterator p;
    if((p = req_volumes.find(volume)) == req_volumes.end())
        throw ArclinkRequestError("volume not found");

    if(!p->second->downloadable())
        throw ArclinkRequestError("volume is not downloadable");
    
    int fd;
    if((fd = xopen((location + "." + p->first).c_str(), O_RDONLY)) < 0)
        logs(LOG_ERR) << strerror(errno) << endl;

    // seek...

    volsize = p->second->size();
    return fd;
  }

void Request::getmsg(ostream &mout) const
  {
    list<string>::const_iterator p;
    for(p = content.begin(); p != content.end(); ++p)
      {
        mout << *p << flush;
      }
  }

void Request::getxml(xmlNodePtr parent, bool show_user) const
  {
    xmlNodePtr child;
    child = xml_new_child(parent, "request");
    xml_new_prop(child, "id", to_string(id).c_str());
    xml_new_prop(child, "type", type_string());

    if(show_user)
      {
        xml_new_prop(child, "user", user.c_str());
        xml_new_prop(child, "institution", institution.c_str());
      }
        
    xml_new_prop(child, "size", to_string(size()).c_str());
    xml_new_prop(child, "ready", (im_ready? "true": "false"));

    map<string, rc_ptr<RequestVolume> >::const_iterator p;
    for(p = req_volumes.begin(); p != req_volumes.end(); ++p)
        p->second->getxml(child);

    bool found_unassigned = false;
    vector<rc_ptr<RequestLine> >::const_iterator p1;
    for(p1 = req_lines.begin(); p1 != req_lines.end(); ++p1)
      {
        if((*p1)->status_unset())
          {
            if(!found_unassigned)
              {
                child = xml_new_child(child, "volume");
                xml_new_prop(child, "id", "UNSET");
                xml_new_prop(child, "status", "UNSET");
                xml_new_prop(child, "size", "0");
                xml_new_prop(child, "message", "");
                found_unassigned = true;
              }

            (*p1)->getxml(child);
          }
      }
  }

void Request::getstate()
  {

  }

//*****************************************************************************
// Reqhandler
//*****************************************************************************

class Reqhandler: private CFIFO_Partner
  {
  private:
    CFIFO cfifo;
    const string cmdline;
    pid_t pid;
    int request_fd;
    int response_fd;
    bool output_active;
    bool sigterm_sent;
    bool sigkill_sent;
    bool shutdown_requested;
    Timer start_retry_timer;
    Timer shutdown_timer;
    int send_timeout;
    rc_ptr<Request> req;
    list<string>::const_iterator req_iter;

    void check_proc();
    void kill_proc();
    void term_proc();

    void response_accepted(const vector<string> &cmdvec);
    void response_rejected(const vector<string> &cmdvec,
      const string &msg);
    void cfifo_callback(const string &cmd);
    void check_response();
    
  public:
    Reqhandler(const string &cmdline_init, int send_timeout_init,
      int start_retry, int shutdown_wait):
      cfifo(*this, MAXCMDLEN), cmdline(cmdline_init),
      pid(-1), request_fd(-1), response_fd(-1), output_active(false),
      sigterm_sent(false), sigkill_sent(false), shutdown_requested(false),
      start_retry_timer(start_retry, 0), shutdown_timer(shutdown_wait, 0),
      send_timeout(send_timeout_init), req(NULL) {}

    ~Reqhandler();
        
    void start();
    void push_request();
    bool check();
    void shutdown();
    
    void attach_request(rc_ptr<Request> r)
      {
        req = r;
        req_iter = req->begin();
      }
   
    void detach_request()
      {
        req = NULL;
      }
    
    bool ready()
      {
        return ((req == NULL || req->ready()) && !shutdown_requested);
      }

    bool pending()
      {
        return (output_active && req != NULL && req_iter != req->end());
      }
    
    pair<int, int> filedes()
      {
        return make_pair(response_fd, request_fd);
      }
  };

Reqhandler::~Reqhandler()
  {
    if(request_fd >= 0)
        close(request_fd);

    if(response_fd >= 0)
        close(response_fd);
  }

void Reqhandler::check_proc()
  {
    internal_check(pid != 0);
    
    int status, completed;

    if(pid < 0) return;

    if((completed = waitpid(pid, &status, WNOHANG)) < 0)
      {
        logs(LOG_ERR) << "waitpid: " << strerror(errno) << endl;
        pid = -1;
        if(request_fd >= 0)
          {
            close(request_fd);
            request_fd = -1;
            output_active = false;
          }

        return;
      }

    if(!completed || WIFSTOPPED(status)) return;

    if(request_fd >= 0)
      {
        close(request_fd);
        request_fd = -1;
        output_active = false;
      }

    if(WIFSIGNALED(status))
      {
        logs(LOG_WARNING) << "[" << pid << "] terminated on signal " <<
          WTERMSIG(status) << endl;
      }
    else if(WIFEXITED(status) && WEXITSTATUS(status) != 0)
      {
        logs(LOG_WARNING) << "["<< pid << "] terminated with error status " <<
          WEXITSTATUS(status) << endl;
      }
    else
      {
        logs(LOG_NOTICE) << "["<< pid << "] terminated" << endl;
      }

    pid = -1;
    start_retry_timer.reset();
  }
    
void Reqhandler::kill_proc()
  {
    internal_check(pid > 0);
    
    kill(pid, SIGKILL);
    sigkill_sent = true;
  }

void Reqhandler::term_proc()
  {
    internal_check(pid > 0);
    
    kill(pid, SIGTERM);
    sigterm_sent = true;
    output_active = false;
    shutdown_timer.reset();
  }

void Reqhandler::response_accepted(const vector<string> &cmdvec)
  {
    vector<string>::const_iterator p = cmdvec.begin();
    string full_response;

    for(const char* q = p->c_str(); *q; ++q)
        full_response += toupper(*q);

    while((++p) != cmdvec.end())
        full_response += (string(" ") + *p);

    logs(LOG_INFO) << "["<< pid << "] " << full_response << endl;
  }

void Reqhandler::response_rejected(const vector<string> &cmdvec,
  const string &msg)
  {
    vector<string>::const_iterator p = cmdvec.begin();
    string full_response = *p;

    while((++p) != cmdvec.end())
        full_response += (string(" ") + *p);

    logs(LOG_WARNING) << "["<< pid << "] invalid response: " << full_response << endl;
    logs(LOG_WARNING) << "["<< pid << "] " << msg << endl;
  }

void Reqhandler::cfifo_callback(const string &cmd)
  {
    if(req == NULL)
      {
        logs(LOG_WARNING) << "["<< pid << "] response '" << cmd << "'ignored, "
          "because the request handler is not associated with any request" << endl;

        return;
      }
    
    vector<string> cmdvec;
    const char *p = cmd.c_str();
    int arglen = 0;

    while(p += arglen, p += strspn(p, " "), arglen = strcspn(p, " "))
        cmdvec.push_back(string(p, arglen));

    if(cmdvec.size() == 0)
        return;

    try
      {
        req->response(cmdvec);
        response_accepted(cmdvec);
      }
    catch(ArclinkRequestError &e)
      {
        response_rejected(cmdvec, e.what());
        term_proc();                          // terminate or not ???
      }
  }

void Reqhandler::check_response()
  {
    int r = 0;
    
    try
      {
        if((r = cfifo.check(response_fd)) == 0 && !sigterm_sent && !sigkill_sent)
            logs(LOG_WARNING) << "["<< pid << "] unexpected eof cmd" << endl;
      }
    catch(CFIFO_ReadError &e)
      {
        logs(LOG_WARNING) << "["<< pid << "] read error (" <<
          e.what() << ")" << endl;
      }
    catch(CFIFO_Overflow &e)
      {
        logs(LOG_WARNING) << "["<< pid << "] response too long" << endl;
      }

    if(r == 0)
      {
        close(response_fd);
        response_fd = -1;
      }
  }

void Reqhandler::start()
  {
    internal_check(request_fd < 0);
    internal_check(response_fd < 0);
    
    int request_pipe[2];
    int response_pipe[2];

    N(pipe(request_pipe));
    N(pipe(response_pipe));
    
    N(pid = fork());

    if(pid) 
      {
        close(request_pipe[0]);
        close(response_pipe[1]);
        
        request_fd = request_pipe[1];
        response_fd = response_pipe[0];
        
        N(fcntl(request_fd, F_SETFD, FD_CLOEXEC));
        N(fcntl(response_fd, F_SETFD, FD_CLOEXEC));

        N(fcntl(request_fd, F_SETFL, O_NONBLOCK));
        N(fcntl(response_fd, F_SETFL, O_NONBLOCK));
        
        sigterm_sent = false;
        sigkill_sent = false;
        shutdown_requested = false;
        output_active = true;

        if(req != NULL)
          {
            req->reset();
            req_iter = req->begin();
          }

        return;
      }

    close(request_pipe[1]);
    close(response_pipe[0]);
    
    if(request_pipe[0] != REQUEST_FD)
      {
        N(dup2(request_pipe[0], REQUEST_FD));
        close(request_pipe[0]);
      }

    if(response_pipe[1] != RESPONSE_FD)
      {
        N(dup2(response_pipe[1], RESPONSE_FD));
        close(response_pipe[1]);
      }

    logs(LOG_INFO) << "["<< getpid() << "] starting shell" << endl;
    
    execl(SHELL, SHELL, "-c", cmdline.c_str(), NULL);

    logs(LOG_ERR) << string() + "cannot execute shell '" + SHELL + "' "
      "(" + strerror(errno) + ")" << endl;
    exit(0);
  }

void Reqhandler::push_request()
  {
    if(!output_active || req == NULL || req_iter == req->end())
        return;
    
    internal_check(request_fd >= 0);
    
    string req_str = *req_iter;
    
    int r;
    if((r = writen_tmo(request_fd, (req_str + "\r\n").c_str(),
      req_str.length() + 2, send_timeout)) < 0)
      {
        logs(LOG_WARNING) << "[" << pid << "] write error (" <<
          strerror(errno) << ")" << endl;

        term_proc();
      }
    else if(r == 0)
      {
        logs(LOG_WARNING) << "[" << pid << "] write timeout" << endl;
        term_proc();
      }
    else
      {
        ++req_iter;
      }
  }

bool Reqhandler::check()
  {
    internal_check(pid != 0);

    if(response_fd >= 0)
        check_response();
    
    check_proc();

    if(pid < 0)
      {
        if(shutdown_requested)
          {
            if(sigkill_sent && response_fd >= 0)
              {
                close(response_fd);
                response_fd = -1;
              }

            if(response_fd < 0)
                return true;
          }

        if(!shutdown_requested && start_retry_timer.expired())
          {
            if(response_fd >= 0)
              {
                close(response_fd);
                response_fd = -1;
              }

            start();
          }

        return false;
      }

    if(!sigterm_sent)
      {
        if(response_fd < 0)
          {
            term_proc();
          }

        return false;
      }
      
    if(!sigkill_sent && shutdown_timer.expired())
      {
        logs(LOG_WARNING) << "["<< pid << "] shutdown time expired" << endl;
        kill_proc();
      }

    return false;
  }

void Reqhandler::shutdown()
  {
    if(!sigterm_sent && pid > 0)
        term_proc();
    
    shutdown_requested = true;
  }

//*****************************************************************************
// Connection
//*****************************************************************************

class ConnectionPartner
  {
  public:
    virtual rc_ptr<Request> new_request(RequestType reqtype,
      const string &user, const string &pass, const string &institution) = 0;
    virtual void queue_request(rc_ptr<Request> req) = 0;
    virtual bool purge_request(RequestID rid, const string &user) = 0;
    virtual rc_ptr<Request> find_request(RequestID rid,
      const string &user) const = 0;
    virtual bool get_status(list<rc_ptr<MessageBuffer> > &buflist,
      RequestID rid, const string &user) const = 0;
    virtual ~ConnectionPartner() {};
  };

class Connection: private CFIFO_Partner
  {
  private:
    ConnectionPartner &partner;
    CFIFO cfifo;
    Stream logs;
    const string ident;
    const int clientfd;
    const unsigned int ipaddr;
    const string host;
    const int port;
    string username;
    string password;
    string institution;
    string response_str;
    string errmsg;
    bool errflg;
    bool have_response;
    bool disconnect_requested;
    bool authentification;
    rc_ptr<Request> req;
    list<rc_ptr<MessageBuffer> > msg_bufs;
    bool downloading;
    bool download_vol;
    int download_fd;
    size_t download_pos;
    map<string, rc_ptr<RequestVolume> >::iterator vol_iter;

    void request_accepted(const vector<string> &cmdvec);
    void request_rejected(const vector<string> &cmdvec,
      const string &msg);
    void response(const string &str);
    void request_ok(const vector<string> &cmdvec);
    void request_error(const vector<string> &cmdvec,
      const string &errmsg);
            
    void command_STATUS(const vector<string> &cmdvec);
    void command__STATUS(const vector<string> &cmdvec);
    void command_PURGE(const vector<string> &cmdvec);
    void command_REQUEST(const vector<string> &cmdvec);
    void command_DOWNLOAD(const vector<string> &cmdvec);
    void request(const vector<string> &cmdvec);
    void cfifo_callback(const string &cmd);
    enum DeliveryResult { Ok, Retry, Fail };
    DeliveryResult do_deliver();

    bool auth()
      {
        authentification = true;
        return true;
      }

  public:
    Connection(ConnectionPartner &partner_init, const string &ident_init,
      unsigned int ipaddr_init, const string &host_init, int port_init, int fd,
      const Stream &logs_init):
      partner(partner_init), cfifo(*this, MAXCMDLEN), logs(logs_init),
      ident(ident_init), clientfd(fd), ipaddr(ipaddr_init), host(host_init),
      port(port_init), errmsg("success\r\n"), errflg(false),
      have_response(false), disconnect_requested(false),
      authentification(false), downloading(false), download_fd(-1),
      download_pos(0) {}
      
    ~Connection() { }

    bool deliver();
    bool input();
    void disconnect();

    bool pending()
      {
        return (have_response || !msg_bufs.empty() || downloading);
      }

    int filedes()
      {
        return clientfd;
      }
  };

void Connection::request_accepted(const vector<string> &cmdvec)
  {
    vector<string>::const_iterator p = cmdvec.begin();
    string full_request;

    for(const char* q = p->c_str(); *q; ++q)
        full_request += toupper(*q);

    while((++p) != cmdvec.end())
        full_request += (string(" ") + *p);

    logs(LOG_INFO) << full_request << endl;
    errmsg = "success\r\n";
  }

void Connection::request_rejected(const vector<string> &cmdvec,
  const string &msg)
  {
    vector<string>::const_iterator p = cmdvec.begin();
    string full_request = *p;

    while((++p) != cmdvec.end())
        full_request += (string(" ") + *p);

    logs(LOG_WARNING) << "invalid request: " << full_request << endl;
    logs(LOG_WARNING) << msg << endl;
    errmsg = msg + "\r\n";
  }

void Connection::response(const string &str)
  {
    response_str = str;
    have_response = true;
  }

void Connection::request_ok(const vector<string> &cmdvec)
  {
    request_accepted(cmdvec);
    response("OK\r\n");
  }

void Connection::request_error(const vector<string> &cmdvec,
  const string &msg)
  {
    request_rejected(cmdvec, msg);
    response("ERROR\r\n");
  }

void Connection::cfifo_callback(const string &cmd)
  {
    vector<string> cmdvec;
    const char *p = cmd.c_str();
    int arglen = 0;

    while(p += arglen, p += strspn(p, " "), arglen = strcspn(p, " "))
        cmdvec.push_back(string(p, arglen));

    if(cmdvec.size() > 0)
        request(cmdvec);
  }

void Connection::command_STATUS(const vector<string> &cmdvec)
  {
    if(cmdvec.size() != 2)
      {
        request_error(cmdvec, "STATUS requires 1 argument");
        return;
      }

    if(!strcasecmp(cmdvec[1].c_str(), "ALL"))
      {
        if(partner.get_status(msg_bufs, RID_ALL, username))
          {
            request_accepted(cmdvec);
            return;
          }

        request_error(cmdvec, "unknown error");
        return;
      }

    RequestID rid;
    char c;
    if(sscanf(cmdvec[1].c_str(), "%d%c", &rid, &c) != 1)
      {
        request_error(cmdvec, "invalid request ID");
        return;
      }

    if(partner.get_status(msg_bufs, rid, username))
      {
        request_accepted(cmdvec);
        return;
      }

    request_error(cmdvec, string() + "request " + to_string(rid) + 
      " not found or access denied");
  }

void Connection::command__STATUS(const vector<string> &cmdvec)
  {
    if(cmdvec.size() != 2)
      {
        request_error(cmdvec, "STATUS requires 1 argument");
        return;
      }

    RequestID rid;
    char c;
    if(sscanf(cmdvec[1].c_str(), "%d%c", &rid, &c) != 1)
      {
        request_error(cmdvec, "invalid request ID");
        return;
      }

    rc_ptr<Request> req;
    if((req = partner.find_request(rid, username)) == NULL)
      {
        request_error(cmdvec, string() + "request " + to_string(rid) + 
          " not found or access denied");
        return;
      }

    request_accepted(cmdvec);

    ostream mout(NULL);
    redirect_ostream(mout, MessageOutput(msg_bufs, BLOCKSIZE));
    req->getmsg(mout);
    delete mout.rdbuf();
  }

void Connection::command_PURGE(const vector<string> &cmdvec)
  {
    if(cmdvec.size() != 2)
      {
        request_error(cmdvec, "PURGE requires 1 argument");
        return;
      }

    RequestID rid;
    char c;
    if(sscanf(cmdvec[1].c_str(), "%d%c", &rid, &c) != 1)
      {
        request_error(cmdvec, "invalid request ID");
        return;
      }

    if(!partner.purge_request(rid, username))
      {
        request_error(cmdvec, string() + "request " + to_string(rid) + 
          " not found or access denied");
        return;
      }

    request_ok(cmdvec);
  }
    
void Connection::command_REQUEST(const vector<string> &cmdvec)
  {
    if(cmdvec.size() != 2)
      {
        request_error(cmdvec, "REQUEST requires 1 argument");
        return;
      }

    req = NULL;
    if(!strcasecmp(cmdvec[1].c_str(), "WAVEFORM"))
        req = partner.new_request(REQ_WAVEFORM, username, password,
          institution);
    else if(!strcasecmp(cmdvec[1].c_str(), "INVENTORY"))
        req = partner.new_request(REQ_INVENTORY, username, password,
          institution);
    else if(!strcasecmp(cmdvec[1].c_str(), "RESPONSE"))
        req = partner.new_request(REQ_RESPONSE, username, password,
          institution);

    if(req == NULL)
      {
        request_error(cmdvec, "unsupported request type or maximum number "
          "of requests exceeded");
        return;
      }

    errflg = false;
    request_ok(cmdvec);
  }

void Connection::command_DOWNLOAD(const vector<string> &cmdvec)
  {
    int n;
    char c;
    int dlpos = 0;
    if(cmdvec.size() == 3)
      {
        char c;
        if(sscanf(cmdvec[2].c_str(), "%d%c", &dlpos, &c) != 1)
          {
            request_error(cmdvec, "invalid start position");
            return;
          }
      }
    else if(cmdvec.size() != 2)
      {
        request_error(cmdvec, "DOWNLOAD requires 1 or 2 arguments");
        return;
      }

    int rid;
    n = sscanf(cmdvec[1].c_str(), "%d%c", &rid, &c);

    if(n != 1 && (n != 2 || c != '.'))
      {
        request_error(cmdvec, "invalid request ID");
        return;
      }

    if((req = partner.find_request(rid, username)) == NULL)
      {
        request_error(cmdvec, string() + "request " + to_string(rid) + 
          " not found or access denied");
        return;
      }

    char volume[100];
    n = sscanf(cmdvec[1].c_str(), "%*d.%99s%c", volume, &c);

    try
      {
        if(n == -1)
          {
            if(!req->ready())
              {
                request_error(cmdvec, "request not ready for download");
                return;
              }
            
            if((download_fd = req->open_first(vol_iter, dlpos)) < 0)
              {
                request_error(cmdvec, "cannot open product");
                return;
              }

            download_pos = dlpos;
            download_vol = false;
            downloading = true;
            response(to_string(req->size()) + "\r\n");
          }
        else if(n == 1)
          {
            int size = -1;
            if((download_fd = req->open_vol(volume, dlpos, size)) < 0)
              {
                request_error(cmdvec, "cannot open product");
                return;
              }

            download_pos = dlpos;
            download_vol = true;
            downloading = true;
            response(to_string(size) + "\r\n");
          }
        else
          {
            request_error(cmdvec, "invalid volume id");
            req = NULL;
            return;
          }
      }
    catch(ArclinkRequestError &e)
      {
        req = NULL;
        request_error(cmdvec, e.what());
      }
  }

void Connection::request(const vector<string> &cmdvec)
  {
    msg_bufs.clear();

    if(downloading)
      {
        req = NULL;

        if(download_fd != -1)
            close(download_fd);

        download_fd = -1;
        download_pos = 0;
        downloading = false;
      }
    
    if(!strcasecmp(cmdvec[0].c_str(), "END"))
      {
        if(req == NULL)
          {
            request_error(cmdvec, "END out of context");
          }
        else if(cmdvec.size() != 1)
          {
            request_error(cmdvec, "END requires 0 arguments");
            req = NULL;
          }
        else
          {
            request_accepted(cmdvec);

            try
              {
                req->push_end();

                if(errflg)
                  {
                    logs(LOG_WARNING) << "request contains errors" << endl;
                    errmsg = "request contains errors\r\n";
                    response("ERROR\r\n");
                  }
                else
                  {
                    response(to_string(req->id) + "\r\n");
                    partner.queue_request(req);
                  }
              }
            catch(ArclinkRequestError &e)
              {
                logs(LOG_WARNING) << "request error: " + string(e.what()) << endl;
                errmsg = string(e.what()) + "\r\n";
                response("ERROR\r\n");
              }

            req = NULL;
          }

        errflg = false;
        return;
      }
    
    if(req != NULL)
      {
        try
          {
            vector<string>::const_iterator p = cmdvec.begin();
            string cmdstr = *p;

            while((++p) != cmdvec.end())
                cmdstr += (" " + *p);

            req->push_line(cmdstr);
          }
        catch(ArclinkRequestError &e)
          {
            request_rejected(cmdvec, e.what());
            errflg |= true;
          }

        return;
      }
    
    if(!strcasecmp(cmdvec[0].c_str(), "HELLO"))
      {
        if(cmdvec.size() == 1)
          {
            request_accepted(cmdvec);
            response(ident);
            return;
          }
        
        request_error(cmdvec, "HELLO requires 0 arguments");
        return;
      }
      
    if(!strcasecmp(cmdvec[0].c_str(), "BYE"))
      {
        if(cmdvec.size() == 1)
          {
            request_accepted(cmdvec);
            disconnect_requested = true;
            return;
          }

        request_error(cmdvec, "BYE requires 0 arguments");
        return;
      }

    if(!strcasecmp(cmdvec[0].c_str(), "USER"))
      {
        if(cmdvec.size() == 2)
          {
            username = cmdvec[1];
            if(auth())
              {
                request_ok(cmdvec);
                return;
              }
          }
        else if(cmdvec.size() == 3)
          {
            username = cmdvec[1];
            password = cmdvec[2];
            if(auth())
              {
                request_ok(cmdvec);
                return;
              }
          }

        request_error(cmdvec, "USER requires 1 or 2 arguments");
        return;
      }

    if(!strcasecmp(cmdvec[0].c_str(), "INSTITUTION"))
      {
        if(cmdvec.size() > 1)
          {
            institution = cmdvec[1];
            vector<string>::const_iterator p;
            for(p = cmdvec.begin() + 2; p != cmdvec.end(); ++p)
                institution += (" " + *p);
      
            request_ok(cmdvec);
            return;
          }

        request_error(cmdvec, "INSTITUTION requires at least 1 argument");
        return;
      }

    if(!strcasecmp(cmdvec[0].c_str(), "SHOWERR"))
      {
        if(cmdvec.size() != 1)
          {
            request_error(cmdvec, "SHOWERR requires 1 argument");
            return;
          }

        response(errmsg);
        request_accepted(cmdvec);
        return;
      }

    if(!authentification)
      {
        request_error(cmdvec, "authentification required");
        return;
      }
    
    if(!strcasecmp(cmdvec[0].c_str(), "REQUEST"))
        command_REQUEST(cmdvec);
    else if(!strcasecmp(cmdvec[0].c_str(), "STATUS"))
        command_STATUS(cmdvec);
    else if(!strcasecmp(cmdvec[0].c_str(), "_STATUS")) // secret kludge ;-)
        command__STATUS(cmdvec);
    else if(!strcasecmp(cmdvec[0].c_str(), "DOWNLOAD"))
        command_DOWNLOAD(cmdvec);
    else if(!strcasecmp(cmdvec[0].c_str(), "PURGE"))
        command_PURGE(cmdvec);
    else
        request_error(cmdvec, "unsupported command");
  }

Connection::DeliveryResult Connection::do_deliver()
  {
    if(have_response)
      {
        if(writen(clientfd, response_str.c_str(), response_str.length()) <= 0)
            return Fail;
        
        have_response = false;
      }
    else if(!msg_bufs.empty())
      {
        rc_ptr<MessageBuffer> buf = msg_bufs.front();
        msg_bufs.pop_front();
    
        if(writen(clientfd, buf->data(), buf->size()) <= 0)
            return Fail;

        if(msg_bufs.empty() && writen(clientfd, "END\r\n", 5) <= 0)
            return Fail;
      }
    else if(downloading)
      {
        internal_check(download_fd >= 0);
        internal_check(download_pos >= 0);
        internal_check(req != NULL);
        
        char buf[BLOCKSIZE];
        int bytes_read = read(download_fd, buf, BLOCKSIZE);

        if(bytes_read < 0)
          {
            logs(LOG_ERR) << "read error: " << strerror(errno) << endl;
            if(writen(clientfd, "END\r\n", 5) <= 0)
                return Fail;
          }
        else if(bytes_read == 0)
          {
            close(download_fd);
            download_fd = -1;
            download_pos = 0;
            
            if(download_vol)
              {
                downloading = false;
                req = NULL;
              }
            else
              {
                if((download_fd = req->open_next(vol_iter)) >= 0)
                    return Retry;

                downloading = false;
                req = NULL;
              }

            if(writen(clientfd, "END\r\n", 5) <= 0)
                return Fail;
          }
        else
          {
            if(writen(clientfd, buf, bytes_read) != bytes_read)
                return Fail;

            download_pos += bytes_read;
          }
      }
            
    return Ok;
  }

bool Connection::deliver()
  {
    errno = 0;
    int r;
    while((r = do_deliver()) == Retry);
    if(r == Fail)
      {
        if(errno != 0)
            logs(LOG_NOTICE) << "socket error: " << strerror(errno) << endl;

        if(downloading)
          {
            if(download_fd != -1)
                close(download_fd);

            download_fd = -1;
            download_pos = 0;
            downloading = false;
          }
    
        return true;
      }

    return false;
  }
 
bool Connection::input()
  {
    int r = 0;
    
    try
      {
        if((r = cfifo.check(clientfd)) == 0)
            return true;
      }
    catch(CFIFO_ReadError &e)
      {
        logs(LOG_NOTICE) << "socket error: " << strerror(errno) << endl;
        return true;
      }
    catch(CFIFO_Overflow &e)
      {
        logs(LOG_WARNING) << "command buffer overflow" << endl;
        return true;
      }

    if(disconnect_requested)
        return true;

    return false;
  }

void Connection::disconnect()
  {
    logs(LOG_NOTICE) << "closing connection" << endl;
    
    shutdown(clientfd, SHUT_RDWR);
    close(clientfd);  // maybe not the best to do it here
  }

//*****************************************************************************
// Arclink
//*****************************************************************************

class Arclink: private ConnectionPartner
  {
  private:
    string organization;
    string request_dir;
    string handler_cmd;
    int max_connections;
    int max_requests;
    int max_lines;
    int max_handlers_soft;
    int max_handlers_hard;
    int handler_start_retry;
    int handler_shutdown_wait;
    int handler_timeout;
    int tcp_port;
    int listenfd;
    time_t last_check;

    map<RequestID, rc_ptr<Request> > requests;
    list<rc_ptr<Request> > request_queue;
    list<rc_ptr<Reqhandler> > free_handlers;
    list<rc_ptr<Reqhandler> > busy_handlers;
    list<rc_ptr<Connection> > connections;

    // Callbacks from Connection;
    rc_ptr<Request> new_request(RequestType reqtype, const string &user,
      const string &pass, const string &institution);
    void queue_request(rc_ptr<Request> req);
    bool purge_request(RequestID rid, const string &user);
    rc_ptr<Request> find_request(RequestID rid, const string &user) const;
    bool get_status(list<rc_ptr<MessageBuffer> > &buflist,
      RequestID rid, const string &user) const;
    
    void client_connect();

  public:
    Arclink():
      max_connections(0), max_requests(0), max_lines(100),
      max_handlers_soft(10), max_handlers_hard(100), handler_start_retry(60),
      handler_shutdown_wait(10), handler_timeout(600), listenfd(-1),
      last_check(0) { }

    void config(rc_ptr<CfgAttributeMap> atts, rc_ptr<CfgElementMap> elms);
    void setup();
    void check();
    void shutdown();
    void restore_state(const string &filename);
    void save_state(const string &filename);
  };

rc_ptr<Request> Arclink::new_request(RequestType reqtype, const string &user,
  const string &pass, const string &institution)
  {
    static RequestID tmp = 0;  // simulation
    
    if(max_requests != 0 && (int)request_queue.size() >= max_requests)
        return NULL;
        
    rc_ptr<Request> req = new Request(tmp, reqtype, user, pass, institution,
      request_dir + "/" + to_string(tmp), max_lines);
    
    ++tmp;
    return req;
  }

void Arclink::queue_request(rc_ptr<Request> req)
  {
    requests[req->id] = req;
    request_queue.push_back(req);
  }

bool Arclink::purge_request(RequestID rid, const string &user)
  {
    map<RequestID, rc_ptr<Request> >::iterator p;
    if((p = requests.find(rid)) == requests.end())
        return false;

    if(user != "admin" && !p->second->auth(user))
        return false;

    requests.erase(p);
    return true;
  }

rc_ptr<Request> Arclink::find_request(RequestID rid, const string &user) const
  {
    map<RequestID, rc_ptr<Request> >::const_iterator p;
    if((p = requests.find(rid)) == requests.end())
        return NULL;

    if(user != "admin" && !p->second->auth(user))
        return NULL;

    return p->second;
  }

bool Arclink::get_status(list<rc_ptr<MessageBuffer> > &buflist,
  RequestID rid, const string &user) const
  {
    rc_ptr<Request> req;
    if(rid != RID_ALL && (req = find_request(rid, user)) == NULL)
        return false;
    
    xmlSetGenericErrorFunc(NULL, xml_error);

    xmlDocPtr doc;
    if((doc = xmlNewDoc((const xmlChar *) "1.0")) == NULL)
        throw bad_alloc();

    if((doc->children = xmlNewDocNode(doc, NULL, (const xmlChar *) "arclink", NULL)) == NULL)
        throw bad_alloc();

    if(rid == RID_ALL)
      {
        map<RequestID, rc_ptr<Request> >::const_iterator p;
        for(p = requests.begin(); p != requests.end(); ++p)
          {
            if(p->second->auth(user))
                p->second->getxml(doc->children, false);
            else if(user == "admin")
                p->second->getxml(doc->children, true);
          }
      }
    else
      {
        req->getxml(doc->children, false);
      }
    
    XMLOutput xout(buflist, BLOCKSIZE);

    xout.write_doc(doc);
    xmlFreeDoc(doc);
    xmlSetGenericErrorFunc(NULL, NULL);

    return true;
  }

void Arclink::config(rc_ptr<CfgAttributeMap> atts, rc_ptr<CfgElementMap> elms)
  {
    atts->add_item(StringAttribute("organization", organization));
    atts->add_item(StringAttribute("request_dir", request_dir));
    atts->add_item(IntAttribute("connections", max_connections, 0, IntAttribute::lower_bound));
    atts->add_item(IntAttribute("request_queue", max_requests, 0, IntAttribute::lower_bound));
    atts->add_item(IntAttribute("request_size", max_lines, 0, IntAttribute::lower_bound));
    atts->add_item(IntAttribute("handlers_soft", max_handlers_soft, 0, IntAttribute::lower_bound));
    atts->add_item(IntAttribute("handlers_hard", max_handlers_hard, 0, IntAttribute::lower_bound));
    atts->add_item(StringAttribute("handler_cmd", handler_cmd));
    atts->add_item(IntAttribute("handler_start_retry", handler_start_retry, 0, IntAttribute::lower_bound));
    atts->add_item(IntAttribute("handler_shutdown_wait", handler_shutdown_wait, 0, IntAttribute::lower_bound));
    atts->add_item(IntAttribute("handler_timeout", handler_timeout, 0, IntAttribute::lower_bound));
    atts->add_item(IntAttribute("port", tcp_port, 1, 65535));
  }

void Arclink::setup()
  {
    if(handler_cmd.length() == 0)
      {
        logs(LOG_ERR) << "'handler_cmd' is not specified" << endl;
        exit(1);
      }
    
    if(request_dir.length() == 0)
      {
        logs(LOG_ERR) << "'request_dir' is not defined" << endl;
        exit(1);
      }

    mkdir(request_dir.c_str(), 0755);

    int fd;
    const string test_file = request_dir + "/test";
    if((fd = creat(test_file.c_str(), 0644)) < 0)
      {
        logs(LOG_ERR) << "cannot create files under 'request_dir' (" <<
          request_dir << ")" << endl;
        exit(1);
      }  
    
    close(fd);
    unlink(test_file.c_str());
    
    N(listenfd = xsocket(PF_INET, SOCK_STREAM, 0));

    int optval = 1;
    N(setsockopt(listenfd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval)));

    struct sockaddr_in inet_addr;
    inet_addr.sin_family = AF_INET;
    inet_addr.sin_port = htons(tcp_port);
    inet_addr.sin_addr.s_addr = htonl(INADDR_ANY);

    if(bind(listenfd, (struct sockaddr *) &inet_addr, sizeof(inet_addr)) < 0)
        throw ArclinkLibraryError("bind error");

    N(listen(listenfd, 5));
  }

void Arclink::client_connect()
  {
    int clientfd;
    struct sockaddr_in inet_addr;

#if defined(__GNU_LIBRARY__) && __GNU_LIBRARY__ < 2
    int len;
#else
    socklen_t len;
#endif

    len = sizeof(inet_addr);
    if((clientfd = xaccept(listenfd, (struct sockaddr *) &inet_addr, &len)) < 0)
      {
        logs(LOG_NOTICE) << "socket error: " << strerror(errno) << endl;
        return;
      }
    
    unsigned int ipaddr = ntohl(inet_addr.sin_addr.s_addr);
    int port = ntohs(inet_addr.sin_port);

#ifdef TCPWRAP
    struct request_info req;
    request_init(&req, RQ_DAEMON, daemon_name.c_str(), RQ_FILE, clientfd, 0);
    fromhost(&req);
    
    const char* host = eval_client(&req);
    
    if(!hosts_access(&req))
      {
        logs(LOG_WARNING) << host << ":" << port << " : not allowed" << endl;
        shutdown(clientfd, SHUT_RDWR);
        close(clientfd);
        return;
      }
#else
    int optval = 1;
    N(setsockopt(clientfd, SOL_SOCKET, SO_KEEPALIVE, &optval, sizeof(optval)));
    const char* host = inet_ntoa(inet_addr.sin_addr);
#endif
    
    if(max_connections != 0 && (int)connections.size() >= max_connections)
      {
        logs(LOG_NOTICE) << host << ":" << port << " : maximum number of "
          "connections (" << max_connections << ") exceeded" << endl;
        ::shutdown(clientfd, SHUT_RDWR);
        close(clientfd);
        return;
      }
        
    logs(LOG_NOTICE) << string(host) << ":" << port <<
      " : opening connection" << endl;
    
    rc_ptr<Connection> conn = new Connection(*this, string(ident_str) +
      "\r\n" + organization + "\r\n", ipaddr, host, port, clientfd,
      CPPStreams::logs.stream(string(host) + ":" + to_string(port) + " : "));

    connections.push_back(conn);
  }

void Arclink::check()
  {
    fd_set read_set;
    fd_set write_set;
    FD_ZERO(&write_set);
    FD_ZERO(&read_set);
    int fd_max = -1;

    while((int)request_queue.size() > 0 &&
      (int)busy_handlers.size() < max_handlers_hard)
      {
        rc_ptr<Reqhandler> rqh;
        if(free_handlers.size() > 0)
          {
            rqh = free_handlers.front();
            free_handlers.pop_front();
          }
        else
          {
            rqh = new Reqhandler(handler_cmd, handler_timeout,
              handler_start_retry, handler_shutdown_wait);
            rqh->start();
          }

        busy_handlers.push_back(rqh);

        rc_ptr<Request> req = request_queue.front();
        request_queue.pop_front();

        rqh->attach_request(req);
      }
        
    FD_SET(listenfd, &read_set);
    fd_max = listenfd;
    
    list<rc_ptr<Reqhandler> >::iterator h = busy_handlers.begin();
    while(h != busy_handlers.end())
      {
        if((*h)->ready())
          {
            (*h)->detach_request();
            if((int)free_handlers.size() < max_handlers_soft)
              {
                free_handlers.push_back(*h);
                busy_handlers.erase(h++);
              }
            else
              {
                (*h)->shutdown();
              }
        
            continue;
          }

        pair<int, int> fd = (*h)->filedes();
        
        if(fd.first != -1)
          {
            FD_SET(fd.first, &read_set);

            if(fd.first > fd_max)
                fd_max = fd.first;
          }
        
        if(fd.second != -1 && (*h)->pending())
          {
            FD_SET(fd.second, &write_set);
 
            if(fd.second > fd_max)
                fd_max = fd.second;
          }

        ++h;
      }

    list<rc_ptr<Connection> >::iterator c;
    for(c = connections.begin(); c != connections.end(); ++c)
      {
        int fd = (*c)->filedes();
        
        FD_SET(fd, &read_set); 

        if((*c)->pending())
            FD_SET(fd, &write_set);
            
        if(fd > fd_max)
            fd_max = fd;
      }

    struct timeval tv;
    tv.tv_sec = 1;
    tv.tv_usec = 0;

    if(select(fd_max + 1, &read_set, &write_set, NULL, &tv) < 0)
      {
        if(errno == EINTR) return;
        throw ArclinkLibraryError("select error");
      }

    if(FD_ISSET(listenfd, &read_set))
        client_connect();
    
    time_t curtime = time(NULL);
    
    h = busy_handlers.begin();
    while(h != busy_handlers.end())
      {
        pair<int, int> fd = (*h)->filedes();

        if(fd.second != -1 && FD_ISSET(fd.second, &write_set))
            (*h)->push_request();
            
        if((fd.first != -1 && FD_ISSET(fd.first, &read_set)) ||
          last_check != curtime)
          {
            if((*h)->check())
              {
                busy_handlers.erase(h++);
                continue;
              }
          }

        ++h;
      }

    last_check = curtime;
            
    c = connections.begin();
    while(c != connections.end())
      {
        int fd = (*c)->filedes();

        if(fd != -1)
          {
            if((FD_ISSET(fd, &write_set) && (*c)->deliver()) ||
              (FD_ISSET(fd, &read_set) && (*c)->input()))
              {
                (*c)->disconnect();
                connections.erase(c++);
                continue;
              }
          }

        ++c;
      }
  }

void Arclink::shutdown()
  {
    logs(LOG_INFO) << "shutting down" << endl;

    list<rc_ptr<Reqhandler> >::iterator p = free_handlers.begin();
    while(p != free_handlers.end())
      {
        busy_handlers.push_back(*p);
        free_handlers.erase(p++);
      }
    
    for(p = busy_handlers.begin(); p != busy_handlers.end(); ++p)
        (*p)->shutdown();
    
    while(!busy_handlers.empty())
        check();
  }

void Arclink::restore_state(const string &filename)
  {

  }

void Arclink::save_state(const string &filename)
  {
    map<RequestID, rc_ptr<Request> >::iterator q;
    for(q = requests.begin(); q != requests.end(); ++q)
      {
        q->second->getstate();
      }
  }

Arclink arclink;

//*****************************************************************************
// Locking the lockfile
//*****************************************************************************

bool run_check(const string &lockfile)
  {
    int fd, val;
    char buf[10];
    struct flock lock;
  
    if((fd = open(lockfile.c_str(), O_WRONLY | O_CREAT,
      S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)) < 0)
        throw ArclinkCannotOpenFile(lockfile);

    lock.l_type = F_WRLCK;
    lock.l_start = 0;
    lock.l_whence = SEEK_SET;
    lock.l_len = 0;
  
    if(fcntl(fd, F_SETLK, &lock) < 0)
      {
        if(errno == EACCES || errno == EAGAIN) return 1;
        else throw ArclinkLibraryError("cannot lock file '" + lockfile + "'");
      }
  
    N(ftruncate(fd, 0));
    sprintf(buf, "%d\n", getpid());
    
    errno = 0;
    if(write(fd, buf, strlen(buf)) != (int) strlen(buf))
        throw ArclinkLibraryError("cannot write pid to '" + lockfile + "'");

    N((val = fcntl(fd,F_GETFD,0)));

    val |= FD_CLOEXEC;
    N(fcntl(fd, F_SETFD, val));

    return 0;
  }

//*****************************************************************************
// get_progname()
//*****************************************************************************

string get_progname(char *argv0)
  {
    string::size_type pos;
    string s = argv0;
    if((pos = s.rfind('/')) != string::npos)
        s = string(argv0, pos + 1, string::npos);

    return s;
  }

} // unnamed namespace

namespace CPPStreams {

Stream logs = make_stream(LogFunc());

}

//*****************************************************************************
// Main
//*****************************************************************************

int main(int argc, char **argv)
try
  {
#if defined(__GNU_LIBRARY__) || defined(__GLIBC__)
    struct option ops[] = 
      {
        { "verbosity",      required_argument, NULL, 'X' },
        { "daemon",         no_argument,       NULL, 'D' },
        { "config-file",    required_argument, NULL, 'f' },
        { "version",        no_argument,       NULL, 'V' },
        { "help",           no_argument,       NULL, 'h' },
        { NULL }
      };
#endif

    daemon_name = get_progname(argv[0]);
    
    int c;
#if defined(__GNU_LIBRARY__) || defined(__GLIBC__)
    while((c = getopt_long(argc, argv, "vDf:Vh", ops, NULL)) != EOF)
#else
    while((c = getopt(argc, argv, "vDf:Vh")) != EOF)
#endif
      {
        switch(c)
          {
          case 'v': ++verbosity; break;
          case 'X': verbosity = atoi(optarg); break;
          case 'D': daemon_mode = true; break;
          case 'f': config_file = optarg; break;
          case 'V': cout << ident_str << endl;
                    exit(0);
          case 'H': fprintf(stdout, help_message, get_progname(argv[0]).c_str());
                    exit(0);
          case '?': fprintf(stderr, opterr_message, get_progname(argv[0]).c_str());
                    exit(1);
          }
      }

    if(optind != argc)
      {
        fprintf(stderr, help_message, get_progname(argv[0]).c_str());
        exit(1);
      }

    struct sigaction sa;
    sa.sa_handler = int_handler;
    sa.sa_flags = SA_RESTART;
    N(sigemptyset(&sa.sa_mask));
    N(sigaction(SIGINT, &sa, NULL));
    N(sigaction(SIGTERM, &sa, NULL));
    
    sa.sa_handler = SIG_IGN;
    N(sigaction(SIGHUP, &sa, NULL));
    N(sigaction(SIGPIPE, &sa, NULL));
    
    if(daemon_mode)
      {
        logs(LOG_INFO) << ident_str << " started" << endl;
        logs(LOG_INFO) << "take a look into syslog files for more messages" << endl;
        openlog(daemon_name.c_str(), 0, SYSLOG_FACILITY);
        daemon_init = true;
      }

    redirect_ostream(cout, LogFunc(), LOG_INFO);
    redirect_ostream(cerr, LogFunc(), LOG_ERR);
    redirect_ostream(clog, LogFunc(), LOG_ERR);

    logs(LOG_NOTICE) << ident_str << " started" << endl;
    
    rc_ptr<CfgAttributeMap> atts = new CfgAttributeMap;
    rc_ptr<CfgElementMap> elms = new CfgElementMap;
    arclink.config(atts, elms);
    
    string lockfile;
    atts->add_item(StringAttribute("lockfile", lockfile));

    logs(LOG_INFO) << "loading configuration from file '" << config_file << "'" << endl;
    read_config_ini(config_file, daemon_name, atts, elms);
    
    if(lockfile.length() != 0 && run_check(lockfile))
      {
        logs(LOG_ERR) << "already running" << endl;
        exit(1);
      }
    
    try
      {
        arclink.setup();
        while(!terminate_proc)
            arclink.check();
      }
    catch(exception &e)
      {
        arclink.shutdown();
        throw;
      }

    arclink.shutdown();
    return 0;
  }
catch(exception &e)
  {
    logs(LOG_ERR) << e.what() << endl;
    return 1;
  }
catch(...)
  {
    logs(LOG_ERR) << "unknown exception" << endl;
    return 1;
  }
 
