/*
 * See https://www.ietf.org/rfc/rfc4226.txt
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

uint hotp_sha1(in ubyte[] key, in ubyte[] msg, in ubyte len=6) {
  import hmac;
  import std.digest.sha;
  auto h = HMAC!SHA1(key, msg);
  return dtrunc(h,len);
}

unittest {
  // rfc4226, Appendix D test values
  auto key = cast(ubyte[])"12345678901234567890";
  auto test_vector = [755224, 287082, 359152, 969429, 338314,
                      254676, 287922, 162583, 399871, 520489];
  for (ubyte i; i < 10; i++) {
    assert(hotp_sha1(key, [0x0,0x0,0x0,0x0,0x0,0x0,0x0,i], 6) == test_vector[i]);
  }
}


uint dtrunc(in ubyte[] hmac, in ubyte len) {
  ushort offset = hmac[$-1] & 0xf;
  uint n = (hmac[offset] & 0x7f) << 24
    | (hmac[offset+1] & 0xff) << 16
    | (hmac[offset+2] & 0xff) << 8
    | (hmac[offset+3] & 0xff);
  n = n % (10 ^^ len);
  return n;
}

unittest {
  // test case from RFC4226, Sec. 5.4
  ubyte[20] h = [0x1f,0x86,0x98,0x69,0x0e,0x02,0xca,0x16,0x61,0x85,0x50,0xef,0x7f,0x19,0xda,0x8e,0x94,0x5b,0x55,0x5a];
  uint n = dtrunc(h, 6);
  assert(n == 872921);
}

