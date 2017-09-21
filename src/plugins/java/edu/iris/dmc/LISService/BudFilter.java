package edu.iris.dmc.LISService;

import java.lang.*;
import java.util.*;
import java.text.*;
import java.io.*;
import java.net.*;

/**

* This class verifies that files have names like STA.NE.LO.CHA.YYYY.JJJ,
* and optionally, that they match "matchme".
*
* The matchme string can contain multiple patterns separated by ';'
* There is no wildcard support.  For instance, to match all BH
* channels, use a matchme string of ".BH" -but- note that this will match
* with network BH too.  This class should be made to use
* regular expressions for matching.
*
* @author	Sandy Stromme
* @version	1.0

**/

public class BudFilter implements FilenameFilter {

	String [] pattern;

/**

* @author	Sandy Stromme
* @version	1.0

**/
	public BudFilter() {

		pattern = null;

	}
/**

* @author	Sandy Stromme
* @version	1.0
* @param	matchme - a search of the filename will be done for tokens delimited by ';' in
*		matchme. Since semi-colon is used as a delimiter for match tokens, it cannot be matched.
* @return	none

**/

	public BudFilter(String matchme) {

		int i, count;

		if (matchme == null) {
			pattern = null;
			return;
		}

		StringTokenizer st = 
			new StringTokenizer (matchme.toUpperCase(),";");

		count = st.countTokens();	
		pattern = new String[count];
		i = 0;
		while (i < count) {
			pattern[i++] = new String(st.nextToken());
		}
	}

/**

* @author	Sandy Stromme
* @version	1.0
* @return	True if file has a valid 'sta.ne.loc.chan.jday.year' filename and if it 
*		matches any specified match.   
* 		False if file has an 'invalid' filename or if it does not matches any specified match.

**/
	
	public boolean accept (File d, String name) {

		int i,  count;

		// verify files are like OBN.II..BHZ.2001.045

		// count the dots...
		StringTokenizer st = 
			new StringTokenizer (name,".");
		count = st.countTokens();	
		if (count != 5 && count != 6) 
			return false;

		// right length?
		if (name.length() < 17) return false;
		if (name.length() > 23) return false;

		// last 2 dots in the right place?
		if (name.charAt(name.length()-9) != '.') return false;	
		if (name.charAt(name.length()-4) != '.') return false;	

		// verify year,jday are numeric
		for (i = name.length()-1; i > name.length()-4; i--) {
			if (! Character.isDigit(name.charAt(i))) return false;	
		}
		for (i = name.length()-6; i > name.length()-9; i--) {
			if (! Character.isDigit(name.charAt(i))) return false;	
		}

		if (this.pattern == null) {
			// no pattern to try and match 
			return true;
		}

		for (i = 0; i < this.pattern.length; i++) {
			if (name.indexOf(this.pattern[i]) != -1) 
				return true;
		}
		return false;
	}

}

