// $Header$
package com.lonsteins.jproxy;



import java.io.IOException;

import java.net.Socket;
import java.net.ServerSocket;
import java.net.InetAddress;
import java.net.UnknownHostException;


/**
 *  Title: JproxyListener
 *  Description: Local socket listener, spawns threads for each connection.
 *  Copyright: Copyright (c) 2002 -- Released under the GPLv2.
 *
 * @author     R. Lonstein
 * @created    January 26, 2002
 * @version    0.2
 *
 *  0.1 - initial release
 *  $Log$
 */
public class JproxyListener implements Runnable {

  private boolean noStopRequested = true;
  private Thread  sockThread;
  private int     SOMAXCONN = 10;              // max # of pending connections, probably safe
  private int     SOCK_TIMEOUT_MILLIS = 60000; // drop socket after 60sec. blocked

  private String       localHost;
  private int          localPort;
  private String       remoteHost;
  private int          remotePort;
  private boolean      verbose;
  private ServerSocket listener;

  /**
   *  Constructor JproxyListener
   *
   * @param  localHost
   * @param  localPort
   * @param  remoteHost
   * @param  remotePort
   * @param  verbose
   */
  public JproxyListener(String localHost, int localPort, String remoteHost,
                        int remotePort, boolean verbose) {

    this.localHost  = localHost;
    this.localPort  = localPort;
    this.remoteHost = remoteHost;
    this.remotePort = remotePort;
    this.verbose    = verbose;

    try {
      listener = new ServerSocket(localPort, SOMAXCONN,
                                  InetAddress.getByName(localHost));
    }
    catch (UnknownHostException iae) {

      // couldn't resolve hostname
      iae.printStackTrace();

      noStopRequested = false;
    }
    catch (IOException ioe) {

      // probably couldn't bind local server socket
      ioe.printStackTrace();

      noStopRequested = false;
    }

    sockThread = new Thread(this);

    // run at lower priority (sort of reverse of Unix priority ranking)
    sockThread.setPriority(Thread.NORM_PRIORITY - 1);

    sockThread.start();
  }

  /**
   *  Method run
   */
  public void run() {

    while (noStopRequested) {
      try {

        // block waiting for a connection
        if (verbose) {
          System.out.println("Listening on " + localHost + ":" + localPort
                             + ".");
        }

        Socket sockLocal = listener.accept();

        if (verbose) {
          System.out.println("Connection from "
                             + sockLocal.getInetAddress().toString() + ".");
        }

        // make new connection to remote host
        if (verbose) {
          System.out.println("Connecting to " + remoteHost + ":" + remotePort
                             + ".");
        }

        Socket sockRemote = new Socket(remoteHost, remotePort);
        sockRemote.setSoTimeout(SOCK_TIMEOUT_MILLIS);

        if (verbose) {
          System.out.println("Connected to "+remoteHost+".");
        }

        // create work thread connecting local to remote
        new JproxyWorkThread(localHost + ":" + localPort, sockLocal,
                             sockRemote, verbose).start();
        if (verbose) {
          System.out.println("work thread local --> remote running.");
        }

        // create work thread connecting remote to local
        new JproxyWorkThread(remoteHost + ":" + remotePort,
                             sockRemote, sockLocal, verbose).start();
        if (verbose) {
          System.out.println("work thread local <-- remote running.");
        }
      }
      catch (IOException ioe) {
        ioe.printStackTrace();

        noStopRequested = false;
      }
    }
  }
}
