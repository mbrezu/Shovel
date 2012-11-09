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
using System.Collections.Generic;
using System.Text;
using System.Security.Cryptography;
using System.IO;

namespace Shovel
{
	public static class Utils
	{
		internal static string ComputeSourcesMd5 (List<SourceFile> sources)
		{
			using (MemoryStream ms = new MemoryStream()) {
				foreach (var sourceFile in sources) {
					var bytes = Encoding.UTF8.GetBytes (sourceFile.Content);
					ms.Write (bytes, 0, bytes.Length);
				}
				ms.Seek(0, SeekOrigin.Begin);
				byte[] hash;
				using (MD5 md5Hash = MD5.Create()) {
					hash = md5Hash.ComputeHash(ms);
				}
				var sb = new StringBuilder();
				foreach (var b in hash) {
					sb.AppendFormat("{0:X2}", b);
				}
				return sb.ToString();
			}
		}

		internal static List<string> ExtractRelevantSource (
            string[] sourceLines, Position startPos, Position endPos, string linePrefix = "")
		{
			var fileName = startPos.FileName;
			var startLine = startPos.Line;
			var endLine = endPos.Line;
			var addEllipsis = endLine > startLine;
			var firstLine = sourceLines [startLine - 1];

			List<string> result = new List<string> ();

			StringBuilder sb1 = new StringBuilder ();
			sb1.AppendFormat ("{0}file '{1}' line {2}: {3}", linePrefix, fileName, startLine, firstLine);
			if (addEllipsis) {
				sb1.Append (" [...content snipped...]");
			}
			result.Add (sb1.ToString ());

			var underline = Utils.Underline (
                Math.Max (startPos.Column, Utils.FirstNonBlank (firstLine)),
                Math.Min (firstLine.Length, addEllipsis ? firstLine.Length : endPos.Column)
			);
			var underlinedLine = String.Format (
                "{0}file '{1}' line {2}: {3}", linePrefix, fileName, startLine, underline);
			result.Add (underlinedLine);

			return result;
		}

		internal static string Underline (int start, int end)
		{
			StringBuilder sb = new StringBuilder ();
			for (var i = 0; i < start - 1; i++) {
				sb.Append (' ');
			}
			for (var i = 0; i <= end - start; i++) {
				sb.Append ('^');
			}
			return sb.ToString ();
		}

		internal static int FirstNonBlank (string line)
		{
			int result = 1;
			while (true) {
				if (result > line.Length) {
					return 1;
				}
				if (line [result - 1] != ' ' && line [result - 1] != '\t') {
					return result;
				}
				result ++;
			}
		}

		internal static void Panic ()
		{
			throw new InvalidOperationException ("Shovel internal WTF.");
		}
	}
}

