// Copyright (c) 2012, Miron Brezuleanu
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
using System;
using System.IO;
using System.Security.Cryptography;
using System.Linq;
using Shovel.Exceptions;

namespace Shovel.Serialization
{
    public static class Utils
    {
        internal static byte Endianess ()
        {
            return (byte)(BitConverter.IsLittleEndian ? 1 : 0);
        }

        internal static void WriteBytes (Stream s, byte[] bytes)
        {
            s.Write (bytes, 0, bytes.Length);
        }

        private static byte[] sixteenZeroes = new byte[16];

        internal static MemoryStream SerializeWithMd5CheckSum (Action<Stream> body)
        {
            var ms = new MemoryStream ();
            WriteBytes (ms, sixteenZeroes);
            ms.WriteByte (Endianess ());
            WriteBytes (ms, BitConverter.GetBytes ((int)Shovel.Api.Version));
            body (ms);
            using (var md5 = MD5.Create()) {
                ms.Seek (0, SeekOrigin.Begin);
                var md5Bytes = md5.ComputeHash (ms);
                ms.Seek (0, SeekOrigin.Begin);
                WriteBytes (ms, md5Bytes);
            }
            return ms;
        }

        internal static object DeserializeWithMd5CheckSum (MemoryStream ms, Func<Stream, object> body)
        {
            // Check MD5 checksum.
            ms.Seek (0, SeekOrigin.Begin);
            byte[] expectedMd5 = new byte[16];
            ms.Read (expectedMd5, 0, expectedMd5.Length);
            ms.Seek (0, SeekOrigin.Begin);
            WriteBytes (ms, sixteenZeroes);
            using (var md5 = MD5.Create()) {
                ms.Seek (0, SeekOrigin.Begin);
                var actualMd5 = md5.ComputeHash (ms);
                if (!expectedMd5.SequenceEqual (actualMd5)) {
                    throw new BrokenDataException ();
                }
            }   
            ms.Seek (0, SeekOrigin.Begin);
            WriteBytes (ms, expectedMd5);
            // Check endianess.
            if (ms.ReadByte () != Utils.Endianess ()) {
                throw new EndianessMismatchException ();
            }
            // Check version.
            if (ReadInt (ms) > Shovel.Api.Version) {
                throw new VersionNotSupportedException ();
            }
            return body (ms);
        }

        // FIXME: these allocate a lot of byte[] objects.
        // Should find a way to avoid this (have the caller pass the byte[]?).
        internal static int ReadInt (Stream ms)
        {
            var bytes = new byte[4];
            ms.Read (bytes, 0, 4);
            return BitConverter.ToInt32 (bytes, 0);
        }

        internal static long ReadLong (Stream ms)
        {
            var bytes = new byte[8];
            ms.Read (bytes, 0, 8);
            return BitConverter.ToInt64 (bytes, 0);
        }
    }
}

