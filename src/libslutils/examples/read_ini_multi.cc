/***************************************************************************** 
 * read_xml.cc
 *
 * Demonstration of config parser and logging facilities
 *
 * (c) 2003 Andres Heinloo, GFZ Potsdam
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/
 *****************************************************************************/

#include <iostream>

#include <unistd.h>
#include <syslog.h>
#include <sys/time.h>

#include "utils.h"
#include "cppstreams.h"
#include "confbase.h"
#include "conf_ini.h"
#include "confattr.h"

namespace {

using namespace std;
using namespace Utilities;
using namespace CPPStreams;
using namespace CfgParser;

//*****************************************************************************
// LogFunc
//*****************************************************************************

class LogFunc
  {
  public:
    enum { msglen = 200 };
    
    void operator()(int priority, const string &msg)
      {
        time_t t = time(NULL);
        char* p = asctime(localtime(&t));
        string msgout = string(p, strlen(p) - 1) + " - read_ini_multi: " + msg;
        write(STDOUT_FILENO, msgout.c_str(), msgout.length());
      }
  };

//*****************************************************************************
// KeywordElement
//*****************************************************************************

class KeywordElement: public CfgElement
  {
  private:
    string name;
    string aParameter;
    
  public:
    KeywordElement(): CfgElement("aKeyword") {}

    rc_ptr<CfgAttributeMap> start_attributes(ostream &cfglog,
      const string &name);

    void end_attributes(ostream &cfglog);
  };
        
rc_ptr<CfgAttributeMap> KeywordElement::start_attributes(ostream &cfglog,
  const string &name)
  {
    cfglog << "  <- start of scope \"aKeyword\" (" << name << ")" << endl;
    
    KeywordElement::name = name;
    aParameter = "";

    rc_ptr<CfgAttributeMap> atts = new CfgAttributeMap;
    atts->add_item(StringAttribute("aParameter", aParameter));
    return atts;
  }

void KeywordElement::end_attributes(ostream &cfglog)
  {
    cout << "              aParameter = " << aParameter << endl;

    cfglog << "  <- end of scope \"aKeyword\" (" << name << ")" << endl;
  }
 
//*****************************************************************************
// SectionElement
//*****************************************************************************

class SectionElement: public CfgElement
  {
  private:
    string name;
    int anInteger;
    string aString;
    
  public:
    SectionElement(): CfgElement("section") {}

    rc_ptr<CfgAttributeMap> start_attributes(ostream &cfglog,
      const string &name);

    void end_attributes(ostream &cfglog);

    rc_ptr<CfgElementMap> start_children(ostream &cfglog,
      const string &);

    void end_children(ostream &cfglog);
  };
        
rc_ptr<CfgAttributeMap> SectionElement::start_attributes(ostream &cfglog,
  const string &name)
  {
    cfglog << "<- start of section [" << name << "]" << endl;
    
    SectionElement::name = name;
    anInteger = -1;
    aString = "";

    rc_ptr<CfgAttributeMap> atts = new CfgAttributeMap;
    atts->add_item(IntAttribute("anInteger", anInteger, 0,
      IntAttribute::lower_bound));
    atts->add_item(StringAttribute("aString", aString));
    return atts;
  }

void SectionElement::end_attributes(ostream &cfglog)
  {
    cout << "            anInteger = " << anInteger << endl;
    cout << "            aString = " << aString << endl;
  }
 
rc_ptr<CfgElementMap> SectionElement::start_children(ostream &cfglog,
  const string &)
  {
    rc_ptr<CfgElementMap> elms = new CfgElementMap;
    elms->add_item(KeywordElement());
    return elms;
  }

void SectionElement::end_children(ostream &cfglog)
  {
    cfglog << "<- end of section [" << name << "]" << endl;
  }
  
} // unnamed namespace

//*****************************************************************************
// Main
//*****************************************************************************

namespace CPPStreams {

Stream logs = make_stream(LogFunc());

}

int main()
try
  {
    redirect_ostream(cout, LogFunc(), LOG_INFO);
    redirect_ostream(cerr, LogFunc(), LOG_ERR);
    redirect_ostream(clog, LogFunc(), LOG_ERR);

    logs(LOG_INFO) << "read_ini_multi started" << endl;
    read_config_ini("data.ini", SectionElement());
    logs(LOG_INFO) << "read_xml finished" << endl;
    
    return 0;
  }
catch(exception &e)
  {
    logs(LOG_ERR) << e.what() << endl;
    logs(LOG_INFO) << "read_xml finished" << endl;
    return 1;
  }
catch(...)
  {
    logs(LOG_ERR) << "unknown exception" << endl;
    logs(LOG_INFO) << "read_xml finished" << endl;
    return 1;
  }
 
