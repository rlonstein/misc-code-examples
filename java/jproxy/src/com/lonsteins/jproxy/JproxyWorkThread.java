// $Header: /var/cvs/jproxy/jproxy/src/com/lonsteins/jproxy/JproxyWorkThread.java,v 1.1.1.1 2002/02/06 01:54:41 lonstein Exp $
package com.lonsteins.jproxy;



import java.io.InputStream;
import java.io.OutputStream;
import java.io.IOException;

import java.net.Socket;

import util.HexDumpEncoder;


/**
 *  Title: JproxyWorkThread
 *  Description: Thread to shuffle packets between IO streams with logging
 *  Copyright: Copyright (c) 2002 -- Released under the GPLv2.
 *
 * @author     R. Lonstein
 * @created    January 26, 2002
 * @version    0.2
 *
 * 0.1 - Initial release
 * $Log: JproxyWorkThread.java,v $
 * Revision 1.1.1.1  2002/02/06 01:54:41  lonstein
 * Imported to CVS.
 *
 */
public class JproxyWorkThread extends Thread {

  private InputStream    in;
  private OutputStream   out;
  private String         prefix;
  private boolean        verbose;
  private byte[]         buffer    = new byte[BUFFER_SIZE];
  private HexDumpEncoder hexdumper = new HexDumpEncoder();

  /**
   *  class constant
   */
  public final static int BUFFER_SIZE = 4096;

  /**
   *  Constructor for the JproxyWorkThread object
   *
   * @param  name             String - name for the socket thread
   * @param  sockIn           Socket
   * @param  sockOut          Socket
   * @param  verbose          boolean - debug on/off
   * @exception  IOException
   */
  public JproxyWorkThread(
          String name, Socket sockIn, Socket sockOut, boolean verbose)
            throws IOException {

    in           = sockIn.getInputStream();
    out          = sockOut.getOutputStream();
    prefix       = name;
    this.verbose = verbose;

    setPriority(NORM_PRIORITY - 1); // reduce our priority
  }

  /**
   *  Main processing method for the JproxyWorkThread object
   */
   public void run() {

    if (verbose) {
      System.out.println("Starting work thread " + prefix);
    }

    try {
      int len;

      while ((len = in.read(buffer)) != -1) {
        if (verbose) {
          System.out.println(formatByteBufferASCII(buffer, len));
        }

        out.write(buffer, 0, len);
      }
    }
    catch (IOException ioe) {

     /** ignored... the stream libraries hide the socket so we don't know why
      *  report the message anyway.
      */
      System.out.println(prefix+ioe.getMessage());
    }

    try {

      // good housekeeping seal...
      out.flush();
      out.close();
      in.close();
    }
    catch (IOException ioe) {

      // ignored but reported again... see above.
      System.out.println(prefix+ioe.getMessage());
    }

    System.out.println("Closed sockets. Exiting thread " + prefix);
  }

  /**
   *  Format a byte buffer into traditional debug hex/ascii dump
   *  string using C. McManus's encoder library (differs from that
   *  under sun.misc.* by virtue of being maintained...)
   *
   *
   * @param buffer
   * @param len
   * @return      String
   */
  private synchronized String formatByteBufferASCII(byte[] buffer, int len) {

    if (len < buffer.length) {
      // dup valid part of buffer before encoding
      byte[] buf = new byte[len];

     /**
       *  System provided array copy faster? TBD.
       *
       *      for (int i = 0; i < len; i++) {
       *        buf[i] = buffer[i];
       *      }
      */

      System.arraycopy(buffer,0,buf,0,len);

      return hexdumper.encodeBuffer(buf);
    }
    else {
      // buffer full, no need to dup portion
      return hexdumper.encodeBuffer(buffer);
    }

  }
}
