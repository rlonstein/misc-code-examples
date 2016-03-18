// $Header: /var/cvs/jproxy/jproxy/src/com/lonsteins/jproxy/Jproxy.java,v 1.1.1.1 2002/02/06 01:54:41 lonstein Exp $
package com.lonsteins.jproxy;



import gnu.getopt.Getopt;

import java.util.StringTokenizer;
import java.util.Enumeration;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.io.IOException;


/**
 *  Title: jproxy
 *  Description: Logging port forwarder using modified threading example in
 *               Chris Longo's PlugProxy.
 *  Copyright: Copyright (c) 2002 -- Released under the GPLv2.
 *
 * @author     R. Lonstein
 * @created    January 26, 2002
 * @version    0.2
 *
 *  0.1 - initial release
 *  $Log: Jproxy.java,v $
 *  Revision 1.1.1.1  2002/02/06 01:54:41  lonstein
 *  Imported to CVS.
 *
 */
public class Jproxy {

  private static String  remoteHost;
  private static int     remotePort;
  private static String  localHost;    // for multiple interfaces
  private static int     localPort;
  private static boolean verbose = false;

  /**
   *  Constructor for the Jproxy object
   *
   * @param  args             Description of Parameter
   * @exception  IOException  Description of Exception
   */
  public Jproxy(String[] args) throws IOException {

    parseOpts(args);
    if (verbose) {
      System.out.println("debugging enabled.");
    }
    JproxyListener jl = new JproxyListener(localHost, localPort, remoteHost,
                                           remotePort, verbose);
  }

  /**
   *  Parse commandline options and set vars
   *
   * @param  args String[]
   */
  private void parseOpts(String[] args) {

    Getopt   g = new Getopt(this.getClass().getName().toString(), args,
                            "l:r:v");
    int      c;
    String   arg;
    String[] pair;

    while ((c = g.getopt()) != -1) {
      switch (c) {

        case 'l' :    // localhost:port
          arg = g.getOptarg();

          if (null == arg) {
            usage();
          }

          pair      = parseHostPortArg(arg);
          localHost = pair[0];
          localPort = -1;

          try {
            localPort = Integer.parseInt(pair[1]);
          }
          catch (NumberFormatException ignored) {

            // ignored, Java sucks in regard to string/int conversion
          }
          break;

        //
        case 'r' :    // remotehost:port
          arg = g.getOptarg();

          if (null == arg) {
            usage();
          }

          pair       = parseHostPortArg(arg);
          remoteHost = pair[0];

          try {
            remotePort = Integer.parseInt(pair[1]);
          }
          catch (NumberFormatException ignored) {

            // ignored, still sucks
          }
          break;

        //
        case 'v' :
          verbose = true;
          break;

        //
        case '?' :    // all others
          usage();    // getopt() already printed an error

        //
        default :
          usage();
      }
    }

    // last sanity checks
    if ((localHost == null) || (remoteHost == null)) {
      System.out.println("\nmust supply system name!");
      usage();
    }

    if ((localPort < 0) || (remotePort < 0) || (localPort > 65535)
            || (remotePort > 65535)) {
      System.out.println("\nport must be in range 0..65535!");
      usage();
    }

  }

  /**
   * Method parseHostPortArg
   *
   * Split a string containing host:port or just port into pieces
   * returning them in fixed positions in an array of String.
   * Localhost is assumed when host not found.
   *
   * @param arg
   *
   * @return String[] where String[0]=host and String[1]=port
   *
   */
  private String[] parseHostPortArg(String arg) {

    String   host     = null;
    String   port     = null;
    String[] pair     = new String[2];
    int      delimPos = arg.indexOf(':');

    if (delimPos < 0) {

      // no delim - assume localhost, get port # from arg
      host = "localhost";
      port = arg;
    }
    else {

      // have delim - try to extract host & port
      host = arg.substring(0, delimPos);

      if (delimPos + 1 < arg.length()) {
        port = arg.substring(delimPos + 1);
      }
      else {

        // nothing following delim, abort
        usage();
      }
    }

    pair[0] = host;
    pair[1] = port;

    return pair;
  }

  /**
   * Method usage
   *
   * Spout usage description then die
   *
   */
  private static void usage() {

    System.out.println(
      "\nusage: com.lonsteins.jproxy.Jproxy -l [localhost:]<port> -r [remote:]<port> -v");
    System.out.println(" -v enables verbose mode with hex/ascii dump logging.");
    System.out.println(
      " The optional localhost is useful when multiple interfaces/ips are present.");
    System.out
      .println(" If remote is not specified, will assume localhost.");
    System.out.println();
    Runtime.getRuntime().exit(1);    // kill VM and die
  }

  /**
   *  The main program for the Jproxy class
   *
   * @param  args             The command line arguments
   * @exception  IOException
   */
  public static void main(String[] args) throws IOException {

    if (args.length < 4) {

      // bail, no args
      usage();
    }

    new Jproxy(args);
  }
}
