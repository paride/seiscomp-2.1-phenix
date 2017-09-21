package edu.iris.dmc.LogByDay;

import java.lang.*;
import java.util.*;
import java.text.*;
import java.io.*;

/**

* @author       Sandy Stromme
* @version      1.0

**/

public class LogByDay {

	String logFileName;
	SimpleDateFormat ldf_ext;
	SimpleDateFormat ldf_timestamp;
	TimeZone gmt;

	public LogByDay() {

		this.logFileName = new String("logFile");
		this.ldf_ext = new SimpleDateFormat(".yyyy.DDD");
		this.ldf_timestamp = new SimpleDateFormat("yyyy,DDD HH:mm:ss z");
		this.gmt = TimeZone.getTimeZone("GMT");
		ldf_ext.setTimeZone(this.gmt);
		ldf_timestamp.setTimeZone(this.gmt);

	}

	public LogByDay(String myName) {

		this.logFileName = new String(myName);
		this.ldf_ext = new SimpleDateFormat(".yyyy.DDD");
		this.ldf_timestamp = new SimpleDateFormat("yyyy,DDD HH:mm:ss z");
		this.gmt = TimeZone.getTimeZone("GMT");
		this.ldf_ext.setTimeZone(this.gmt);
		this.ldf_timestamp.setTimeZone(this.gmt);

	}

	public LogByDay(String myName, boolean gmt) {

		this.logFileName = new String(myName);
		this.ldf_ext = new SimpleDateFormat(".yyyy.DDD");
		this.ldf_timestamp = new SimpleDateFormat("yyyy,DDD HH:mm:ss z");
		this.gmt = TimeZone.getTimeZone("GMT");
		if (gmt) {
			this.ldf_ext.setTimeZone(this.gmt);
			this.ldf_timestamp.setTimeZone(this.gmt);
		}

	}

	public LogByDay(String myName, boolean gmt_ext, boolean gmt_timestamp) {

		this.logFileName = new String(myName);
		this.ldf_ext = new SimpleDateFormat(".yyyy.DDD");
		this.ldf_timestamp = new SimpleDateFormat("yyyy,DDD HH:mm:ss z");
		this.gmt = TimeZone.getTimeZone("GMT");
		if (gmt_ext) {
			this.ldf_ext.setTimeZone(this.gmt);
		}
		if (gmt_timestamp) {
			this.ldf_timestamp.setTimeZone(this.gmt);
		}

	}

	public boolean write(String logmsg) {
		try{
			FileWriter logwriter = 
				new FileWriter(this.logFileName+this.ldf_ext.format(new Date()), true);	
			logwriter.write(logmsg+"\n");
			logwriter.close();
	 	} catch (IOException ioe) {
			System.out.println("IO Error opening log file " + 
				this.logFileName + ":  "+
				ioe.getMessage());
			return false;
		}
		return true;
	}

	public boolean timestamp(String logmsg) {
		try{
			FileWriter logwriter = 
				new FileWriter(this.logFileName+this.ldf_ext.format(new Date()), true);	
			logwriter.write(this.ldf_timestamp.format(new Date())+" - "+logmsg+"\n");
			logwriter.close();
	 	} catch (IOException ioe) {
			System.out.println("IO Error opening log file " + 
				this.logFileName + ":  "+
				ioe.getMessage());
			return false;
		}
		return true;
	}

	public static void main(String args[]) {

		if (args.length == 0 || args.length > 2)  {
	System.out.println("Usage:  java LogByDay logfilename (-t)");
			System.exit(1);
		}

		String logFileName = new String("log");
		int i;
		boolean timestamp = false;

		for (i = 0; i < args.length; i++) {
			if (args[i].compareTo("-t")==0) {
				timestamp = true;	
			} else {
				logFileName = args[i];
			}
		}

		LogByDay lbd = new LogByDay(logFileName);
		String inLine;

		BufferedReader in
		   = new BufferedReader(new InputStreamReader(System.in));
		try {
			if (!lbd.timestamp(new String("Start logging"))) {
	System.out.println("Could not write to log file "+lbd.logFileName);
					System.exit(1);
			}
			if (timestamp) {
				while ((inLine = in.readLine()) != null) {
				// log stdin
					if (!lbd.timestamp(inLine)) {
	System.out.println("Could not write to log file "+lbd.logFileName);
						System.exit(1);
					}
				} 

			} else {
				while ((inLine = in.readLine()) != null) {
				// timestamp log stdin
					if (!lbd.write(inLine)) {
	System.out.println("Could not write to log file "+lbd.logFileName);
						System.exit(1);
					}
				} 
			}
			if (!lbd.timestamp(new String("End logging"))) {
	System.out.println("Could not write to log file "+lbd.logFileName);
					System.exit(1);
			}
	 	} catch (IOException ioe) {
			System.out.println("IO Error reading input");
		}
		System.exit(0);
	}

}
