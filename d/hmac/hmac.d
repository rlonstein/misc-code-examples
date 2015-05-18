/**
 * HMAC, see https://tools.ietf.org/html/rfc2104, implemented as a D-Language template
 *
 * Copyright (c) 2015, Ross Lonstein <rlonstein@pobox.com>
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

ubyte[] HMAC(T, alias blocksize = 64)(in ubyte[] key, in ubyte[] msg) {
  import std.digest.digest;

  assert(isDigest!T);

  ubyte[] k;
  ubyte[blocksize] opad = 0x5c;
  ubyte[blocksize] ipad = 0x36;

  if (key.length > blocksize) {
    // hash long keys
    k = digest!T(key);
  }
  else {
    k = key.dup;
  }

  // right pad short keys with zero by extending array
  if (k.length < blocksize) {
    k.length = blocksize;
  }

  // Update pads with key
  for(ushort i; i<blocksize; i++) {
    opad[i] = opad[i] ^ k[i];
    ipad[i] = ipad[i] ^ k[i];
  }

  ubyte[digestLength!T] h = digest!T(opad, digest!T(ipad, msg));
  return h.dup;
}


unittest {
  import std.digest.sha;
  import std.digest.md;

  auto h = HMAC!SHA1(null, null);
  assert(toHexString(h) == "FBDB1D1B18AA6C08324B7D64B71FB76370690E1D");

  h = HMAC!(MD5, 64)(null, null);
  assert(toHexString(h) == "74E6F7298A9C2D168935F58C001BAD88");
  
  h = HMAC!SHA1(cast(ubyte[])"key", cast(ubyte[])"The quick brown fox jumps over the lazy dog");
  assert(toHexString(h) == "DE7C9B85B8B78AA6BC8A7A36F70A90701C9DB4D9");

  h = HMAC!MD5(cast(ubyte[])"key", cast(ubyte[])"The quick brown fox jumps over the lazy dog");
  assert(toHexString(h) == "80070713463E7749B90C2DC24911E275");  
}
