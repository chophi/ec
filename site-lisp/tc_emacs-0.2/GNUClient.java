/*
 * GNUClient.java
 * Copyright (C) 2005 Tomasz Malesinski <tmal@mimuw.edu.pl>
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
 *
 */

import java.lang.*;
import java.io.*;

class GNUClientException extends Exception {
  GNUClientException(String message) {
    super(message);
  }
}

public class GNUClient {

  public static String elispEval(String forms) throws GNUClientException {
    String[] args = new String[4];

    args[0] = "gnuclient";
    // args[1] = "-batch";
    args[1] = "-eval";
    args[2] = forms;

    try {
      Process p = Runtime.getRuntime().exec(args);

      try {
	InputStreamReader isr = new InputStreamReader(p.getInputStream());
	BufferedReader br = new BufferedReader(isr);
	String res = "";
	String line;

	while ((line = br.readLine()) != null)
	  res += line + "\n";
      
	boolean end = false;
	while (!end) {
	  try {
	    p.waitFor();
	    if (p.exitValue() != 0)
	      throw new GNUClientException("gnuclient failed");
	    end = true;
	  } catch (InterruptedException e) {
	  }
	}

	return res;
      } finally {
	p.destroy();
      }
      
    } catch (IOException e) {
      throw new GNUClientException("IO error: " + e.getMessage());
    }

  }

  public static String quote(String s) {
    return "\"" + s.replaceAll("(\"|\\\\)", "\\\\$1") + "\"";
  }
}
