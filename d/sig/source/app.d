/**
 * sig -- select a random quotation from a signature file where
 *        each stanza is separated by a single percentage sign on
 *        a line by itself.
 *
 * Copyright (c) 2018, 2019 Ross Lonstein <rlonstein@pobox.com>
 *
 * Permission to use, copy, modify, and/or distribute this software
 * for any purpose with or without fee is hereby granted, provided
 * that the above copyright notice and this permission notice appear
 * in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
 * WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
 * AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
 * OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

import std.array : join;
import std.conv : to;
import std.exception : ErrnoException;
import std.file : exists;
import std.format : format;
import std.path;
import std.random : Random, uniform, unpredictableSeed;
import std.range : enumerate;
import std.stdio : File, stderr, stdin, writeln;
import std.string : indexOf;
import clid;

const string default_sig_file = "~/.sigs";

int main() {
  struct CLIConfig {
    @Parameter("filename", 'f')
    @Description(format("filename of signature collection (default '%s')", default_sig_file)
                 , "path")
    string filename = default_sig_file;
  }

  auto config = parseArguments!CLIConfig();
  string filename = config.filename;
  if (filename[0] == '~') {
    filename = expandTilde(filename);
  }
  if (!isValidPath(filename)) {
    stderr.writefln("Invalid path: %s", filename);
    return -1;
  }
  if (!exists(filename)) {
    stderr.writefln("File doesn't exist: %s", filename);
    return -1;
  }

  File fh;
  try {
    fh = File(filename, "r");    
  }
  catch (ErrnoException err) {
    stderr.writefln("Error! %s", err.msg);
    return -1;
  }
  
  auto gen = Random(unpredictableSeed);
  string[] quote, acc;
  int count = 0;
  float rand;

  foreach (line; fh.byLine()) {
    if (indexOf(line, '%') != 0) {
      acc ~= to!string(line);
    } else {
      count++;
      rand = uniform(0.0f, count, gen);
      if (rand < 1) {
        quote = acc.dup;
      }
      acc = null;
    }
  }
  string sig = join(quote, "\n");
  writeln(sig);
  return 0;
}
