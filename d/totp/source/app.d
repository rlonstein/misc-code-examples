import hotp;
import base32;

import std.bitmanip : nativeToBigEndian;
import std.conv;
import std.datetime.systime : Clock;
import std.stdio;

void usage() {
  stderr.writeln("Time-based One-Time Password generator (rfc-6238)");
  stderr.writeln("Usage: totp <KEY> [interval]");
}

int main(string[] args) {
  int rc = 0;
  string keystr;
  ubyte[] key;
  int interval = 30;

  if (args.length < 2) {
    usage();
    rc = 1;
    return rc;
  }

  keystr = args[1];
  try {
    key = Base32.decode(keystr);
  }
  catch (base32.Base32Exception e) {
    stderr.writeln("ERROR: Failed to decode key!");
    rc = 1;
    return rc;
  }

  if (args.length == 4) {
    try {
      interval = to!int(args[2]);
    }
    catch (ConvException e) {
      stderr.writeln("ERROR: Interval not an integer");
      rc = 1;
      return rc;
    }
  }

  long time = Clock.currTime().toUnixTime();
  long counter = time/interval;
  ubyte[8] c = nativeToBigEndian(counter);
  stdout.writefln("Time = %d, Counter = %d, Bytes = %(%02X %)", time, counter, c);
  uint totp = hotp_sha1(key, c);

  stdout.writefln("%d", totp);

  return rc;
}

