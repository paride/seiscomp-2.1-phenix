package edu.iris.dmc.LISService;

import java.lang.*;
import java.util.*;
import java.text.*;
import java.io.*;
import java.net.*;
import edu.iris.dmc.LogByDay.*;
/**

* <PRE>
*
* This class reads files in a BUD NE dir structure and multiplexes them 
* together and writes to a socket, optionally accepting tokens to use to match 
* filenames for multiplexing.
*
* Usage: BudNetSocket -b budDir -m matchme -n network -r recordSize -x extension -p port
*
* Defaults: BudNetSocket -b /bud/bud -r 4096 1492 
*
* </PRE>
*
* @author       Sandy Stromme
* @version      1.0

**/

public class BudNetSocket {

	String budDir = null;
	String logDir = null;
	LogByDay lbd;
	String oFileName;
//-AH	OutputStream ofs = null;
	File ofile;
	Vector filenames;
	Date date;
	Vector ifs;
	int recordSize;
	long restart = 900000;
	BudFilter sf = null;

	class WriteException extends Exception {
		public WriteException() { super(); }
		public WriteException(String s) { super(s); }
	}

//-AH-begin
	// Our gateway to SeedLink, defined in java_plugin.cc
	native static int send_packet(byte[] rec);

	void send_packet_wrapper(byte[] rec) throws WriteException {
		int r = send_packet(rec);

		if(r == -1) {
			System.out.println("error sending data to SeedLink");
			throw new WriteException("write error");
		}
		
		if(r == -2)
			this.log_this("could not read Mini-SEED record");
	}
//-AH-end

	// Mulitplexes files with names like budDir/NE/STAT.NE.LO.CHA.YYYY.JJJ
	// and that match "matchme"  (use to specify only mux certain channels,
	// certain stations, etc.)
	// The matchme string can contain multiple patterns separated by ';'
	// but there is no wildcard support. (Sorry, not regexp)

//-AH	BudNetSocket(String budDir, String logDir, String network,int recordSize,
//-AH		String extension, String matchme, Socket socket) {
//-AH-begin
	BudNetSocket(String budDir, String logDir, String network,int recordSize,
		String extension, String matchme) {
//-AH-end

		int i;
		String logFileName;
		this.date = new Date();
		this.recordSize = recordSize;
		this.filenames = new Vector();
		this.ifs = new Vector();
		this.budDir = budDir;
		this.logDir = logDir;
		if (extension != null) {
			logFileName = 
		logDir+"/"+network+"."+extension+".socket.log";
			this.oFileName = logDir+"/"+network+"."+extension;
		} else {
			logFileName = 
		logDir+"/"+network+".socket.log";
			this.oFileName = logDir+"/"+network;
		}

		this.sf = new BudFilter(matchme);

		// prepare mux file
		this.lbd = new LogByDay(logFileName, true, true);
		this.log_this("Started multiplex for " + network + " " + matchme);
		get_this(network);
//-AH		try {
//-AH			this.ofs = socket.getOutputStream();
//-AH		} catch (IOException ioe2) {
//-AH			System.out.println("after get_this, IO Error:" + ioe2.getMessage());
//-AH    			System.exit(-1);
//-AH		}
		
		this.log_this("Done with initialization for " + network);
	}

//-AH	void getNewOfs(Socket socket) {
//-AH		try {
//-AH			if (this.ofs != null) this.ofs.close();
//-AH			this.ofs = socket.getOutputStream();
//-AH		} catch (IOException ioe2) {
//-AH			System.out.println("getNewOfs, IO Error:" + ioe2.getMessage());
//-AH    			System.exit(-1);
//-AH		}
//-AH	}

	void read_write() throws WriteException {

		int i;
		FileInputStream IFS;

		byte[] lrec = new byte[this.recordSize];
		int bytes;

		for (i = 0; i < this.ifs.size(); i++) {
			try {
				IFS = (FileInputStream) this.ifs.elementAt(i);
				while (IFS.available() >= this.recordSize) {
					bytes = IFS.read(lrec);
					if (bytes != this.recordSize) {
						this.log_this("ERROR - read "+bytes+" bytes for "+this.filenames.elementAt(i));
//-AH					}
//-AH-begin
					} else {
						send_packet_wrapper(lrec);
					} 
//-AH-end
//-AH					try {
//-AH						this.ofs.write(lrec);
//-AH						this.ofs.flush();
//-AH					} catch (IOException ioe2) {
//-AH						System.out.println("after write/flush, IO Error:" + ioe2.getMessage());
//-AH						throw new WriteException("write error");
//-AH					}	
				}
			} catch (IOException ioe2) {
				System.out.println("read_write, IO Error:" + ioe2.getMessage());
    				System.exit(-1);
			}

		}	
	}

	void read_write_close(int i) throws WriteException {

		FileInputStream IFS;
		byte[] lrec = new byte[this.recordSize];
		int bytes;

		try {
			IFS = (FileInputStream) this.ifs.elementAt(i);
			while (IFS.available() >= this.recordSize) {
				bytes = IFS.read(lrec);
//-AH-begin
				send_packet_wrapper(lrec);
//-AH-end
//-AH				try {
//-AH					this.ofs.write(lrec);
//-AH					this.ofs.flush();
//-AH				} catch (IOException ioe2) {
//-AH					System.out.println("write-exception, IO Error:" + ioe2.getMessage());
//-AH					throw new WriteException("write error");
//-AH				}	
			}	
			IFS.close();
		} catch (IOException ioe2) {
			System.out.println("read_write_close, IO Error:" + ioe2.getMessage());
    			System.exit(-1);
		}

	}

	void get_this(String network) {

		String [] tmpstanames;
		String [] tmpnames;
		String station;
		File dir;
		FileInputStream IFS;
		File ifile;
		int k = 0;
		int j = 0;
		int i = 0;
		String tmp;

		// make list of files to read from
		dir =  new File(budDir+"/"+network);
		tmpstanames = dir.list();

		for (k = 0; k < tmpstanames.length; k++) {


			station = tmpstanames[k];
			dir =  new File(budDir+"/"+network+"/"+station);

			//if (this.sf == null) {
				tmpnames = dir.list(this.sf);
			//} else {
				tmpnames = dir.list(this.sf);
			//}

			if (tmpnames == null) continue;
	
			// sort - is this really how to do this? 
			for (i = 0; i < tmpnames.length; i++) {
				for (j = i + 1; j < tmpnames.length; j++) { 
					if (tmpnames[i].compareTo(tmpnames[j]) > 0) {
						tmp = tmpnames[i];
						tmpnames[i] = tmpnames[j];
						tmpnames[j] = tmp;
					}
				}
			}
			j = 0;
			// after sorting then fill this.filenames with only latest files
			for (i = 0; i < tmpnames.length; i++) {
				if (i + 1 >= tmpnames.length ||
			(!tmpnames[i].regionMatches(true, 0, tmpnames[i+1], 0, tmpnames[i].length()-9))){
					this.filenames.addElement((Object)budDir+"/"+network+"/"+station+"/"+tmpnames[i]);
				}
			}

		}

		// open input files...
		for (i = 0; i < this.filenames.size(); i++) {
			ifile = new File((String)this.filenames.elementAt(i));
			try {
				this.ifs.addElement((Object) new FileInputStream(ifile));
				IFS = (FileInputStream) this.ifs.elementAt(i);
				IFS.skip(IFS.available());
			} catch (IOException ioe) {
				System.out.println("IO Error:" + ioe.getMessage());
    				System.exit(-1);
			}
			this.log_this("Initialization opening "+(String)this.filenames.elementAt(i)); 
		}

	}	

	void log_this(String msg) {
		if (!this.lbd.timestamp(msg)) { 
			System.out.println("log file error");
    			System.exit(-1);
		}	
	}

	void update_this(String network) throws WriteException {

		String [] tmpnames;
		String [] tmpstanames;
		String station;
		Date compare_date;
		Vector newfilenames = new Vector();
		File dir;
		File ifile;
		int k = 0;
		int j = 0;
		int i = 0;
		String new_tmp = new String();
		String this_tmp = new String();

		// make list of files to read from
		dir =  new File(budDir+"/"+network);
		tmpstanames = dir.list();
		
		for (k = 0; k < tmpstanames.length; k++) {
	
		station = tmpstanames[k];
		
		try {
			// for each station, make new list of files to read from
			dir =  new File (budDir+"/"+network+"/"+station);
			if (this.sf == null) {
				tmpnames = dir.list();
			} else {
				tmpnames = dir.list(this.sf);
			}

			if (tmpnames == null) continue;

			// sort - is this really how to do this? 
			for (i = 0; i < tmpnames.length; i++) {
				for (j = i + 1; j < tmpnames.length; j++) { 
					if (tmpnames[i].compareTo(tmpnames[j]) > 0) {
						new_tmp = tmpnames[i];
						tmpnames[i] = tmpnames[j];
						tmpnames[j] = new_tmp;
					}
				}
			}

			for (i = 0; i < tmpnames.length; i++) {
				if ((i + 1 >= tmpnames.length) ||
			(!tmpnames[i].regionMatches(true,0,tmpnames[i+1],0,tmpnames[i].length()-9))){

			newfilenames.addElement((Object)budDir+"/"+network+"/"+station+"/"+tmpnames[i]);
				}
			}

			// close, replace this.filenames that have a later version, open later version
			for (i = 0; i < this.filenames.size(); i++) {
				// for every old file...
				for (j = 0; j < newfilenames.size(); j++) {
					// try to match a new file...
					this_tmp = (String) this.filenames.elementAt(i);
					new_tmp = (String) newfilenames.elementAt(j);
					if (
					(this_tmp.regionMatches(true, 
					0, new_tmp, 
					0, new_tmp.length()-9))) {
						if (this_tmp.compareTo(new_tmp) < 0) {
						// have a later day file...
							// log it...
			this.log_this("Closing "+(String)this.filenames.elementAt(i)+" and opening "+new_tmp);
							// do it...
							read_write_close(i);
							this.filenames.setElementAt((Object)new_tmp,i);
							ifile = 
				new File((String) this.filenames.elementAt(i));
							this.ifs.setElementAt((Object)new FileInputStream(ifile),i);
							break;
						}
						// same file found...
						break;
					}

				}
			}
		} catch (IOException ioe) {
			System.out.println("IO Error:" + ioe.getMessage());
    			System.exit(-1);
		}

	}

		try {
		// open and add any new stream files
		for (j = 0; j < newfilenames.size(); j++) {
			for (i = 0; i < this.filenames.size(); i++) {
				this_tmp = (String) this.filenames.elementAt(i);
				new_tmp = (String) newfilenames.elementAt(j);
				if (this_tmp.regionMatches(true,0,new_tmp,0,new_tmp.length()-9)){
					break;
				}
			}
			if (i == this.filenames.size()) {
			// no match was found, add this new file...
				System.out.println("new stream found:"+new_tmp);
				ifile = new File(new_tmp);
				this.ifs.addElement((Object) new FileInputStream(ifile));
				this.filenames.addElement((Object) new_tmp);
				this.log_this("Adding "+new_tmp);
			}
		}

		// remove any files that we were looking at but are no longer there...
		for (i = 0; i < this.filenames.size(); i++) {
			for (j = 0; j < newfilenames.size(); j++) {
				this_tmp = (String) this.filenames.elementAt(i);
				new_tmp = (String) newfilenames.elementAt(j);
				if (this_tmp.regionMatches(true,0,new_tmp,0,new_tmp.length())){
					break;
				}
			}
			if (j == newfilenames.size()) {
			// no match was found
				// close and remove obsolete files that are gone
				read_write_close(i);
				this.ifs.removeElementAt(i);
				this.log_this("Removing "+(String) this.filenames.elementAt(i));
				this.filenames.removeElementAt(i);
			}
		}

		} catch (IOException ioe) {
			System.out.println("IO Error:" + ioe.getMessage());
    			System.exit(-1);
		}
		this.log_this("Number of channels is "+this.filenames.size());

	}	
/**
* @author       Sandy Stromme
* @version      1.0
* @param	-r recordSize
* @param	-n network
* @param	-x extension
* @param	-b budDir
* @param	-p port

**/

	public static void main(String args[]) {

		int update_freq = 256;
		int i, count = 0, port = 1492;
		BudNetSocket muxer;
//-AH    		ServerSocket serverSocket = null;
//-AH		Socket socket = null;
//-AH    		InetAddress ia;

		String network = null, extension = null, pattern = null, budDir = null, logDir = null;
		int recordSize = 512;
	
		for (i = 0; i < args.length; i++) {

			if (args[i].compareTo("-n")==0) {
				i++;
				network = args[i];
			} else if (args[i].compareTo("-r")==0) {
				i++;
				recordSize = Integer.parseInt(args[i]);
			} else if (args[i].compareTo("-x")==0) {
				i++;
				extension = args[i];
			} else if (args[i].compareTo("-b")==0) {
				i++;
				budDir = args[i];
			} else if (args[i].compareTo("-m")==0) {
				i++;
				pattern = args[i];
			} else if (args[i].compareTo("-l")==0) {
				i++;
				logDir = args[i];
//-AH			} else if (args[i].compareTo("-p")==0) {
//-AH				i++;
//-AH				port = Integer.parseInt(args[i]);
			}

		}

		if (budDir == null) budDir = "/bud/bud";
		if (logDir == null) logDir = "/bud/muxed";


//-AH		try {
//-AH    			serverSocket = new ServerSocket(port);
//-AH		} catch (IOException e) {
//-AH    			System.out.println("Could not listen on port: "+port);
//-AH    			System.exit(-1);
//-AH		}
//-AH		try {
//-AH    			socket = serverSocket.accept();
//-AH		} catch (IOException e) {
//-AH    			System.out.println("Accept failed: "+port);
//-AH    			System.exit(-1);
//-AH		}

		if (network != null) {
//-AH			muxer = new BudNetSocket(budDir,logDir,network,recordSize,extension,pattern,socket);
//-AH-begin
			muxer = new BudNetSocket(budDir,logDir,network,recordSize,extension,pattern);
//-AH-end
		} else {
//-AH			System.out.println("Usage: BudNetSocket -n network [-b budDir] [-l logDir] [-r recordSize] [-x extension] [-m matchPattern] [-p port]");
//-AH			System.out.println("Defaults: budDir=/bud/bud, logDir=/bud/muxed, recordSize=512, no extension, no matchPatern (so all), port=1492");
//-AH-begin
			System.out.println("Usage: bud_plugin -n network [-b budDir] [-l logDir] [-r recordSize] [-x extension] [-m matchPattern]");
			System.out.println("Defaults: budDir=/bud/bud, logDir=/bud/muxed, recordSize=512, no extension, no matchPatern (so all)");
//-AH-end
			System.out.println("MatchPatern will do a string match on the base filename.");
			System.out.println("Multiple match patterns can be separated buy ';', no wild card or regular expression support.");
			System.out.println("Example '-p \"BHZ;BHN;BHE\".");
			System.out.println("'Extension' will be included as part of the log file name.");
			return;
		}
//-AH                ia = socket.getInetAddress();
//-AH		muxer.log_this("Opened connection to "+ia.getHostName());
		
		while (true) {
			try {
				Thread.sleep(2000);
			} catch (InterruptedException e) {
				System.out.println("Interrupted:" + e.getMessage());
			}
			try {
				// read_write loops over all in files, reads and writes bytes available
				muxer.read_write();
				if (count >= update_freq) {
					muxer.update_this(network);
					count = 0;
				} else {
					count++;
				}		
			} catch (WriteException e) {
				muxer.log_this("Write Exception occurred"); 
				muxer.log_this(e.toString()); 
//-AH				try {
//-AH					socket.close();
//-AH    					socket = serverSocket.accept();
//-AH                			ia = socket.getInetAddress();
//-AH					muxer.log_this("Opened connection to "+ia.getHostName());
//-AH					muxer.getNewOfs(socket);
//-AH				} catch (IOException e1) {
//-AH    					System.out.println("IOException: "+e1.getMessage());
//-AH    					System.exit(-1);
//-AH				}

				// force update since may have had to wait awhile for connection...
				count = update_freq;
			}
		}

	}

}
