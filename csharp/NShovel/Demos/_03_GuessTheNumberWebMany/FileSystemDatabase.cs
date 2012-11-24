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
using System.Collections.Generic;

namespace _03_GuessTheNumberWebMany
{
    public class FileSystemDatabase
    {
        string directory;

        public FileSystemDatabase (string directory)
        {
            this.directory = directory;
            if (!Directory.Exists (this.directory)) {
                Directory.CreateDirectory (this.directory);
            }
        }

        public Dictionary<string, byte[]> Read (int key)
        {
            lock (this.GetType()) {
                var objDir = Path.Combine (this.directory, key.ToString ());
                if (!Directory.Exists (objDir)) {
                    return null;
                }
                var result = new Dictionary<string, byte[]> ();
                foreach (var fileName in Directory.EnumerateFiles(objDir)) {
                    var fieldName = Path.GetFileName (fileName);
                    result [fieldName] = File.ReadAllBytes (fileName);
                }
                return result;
            }
        }

        public void Write (int key, Dictionary<string, byte[]> obj)
        {
            lock (this.GetType()) {
                var objDir = Path.Combine (this.directory, key.ToString ());
                if (Directory.Exists (objDir)) {
                    Directory.Delete (objDir, true);
                }
                Directory.CreateDirectory (objDir);
                foreach (var field in obj.Keys) {
                    var fileName = Path.Combine (objDir, field);
                    File.WriteAllBytes (fileName, obj [field]);
                }
            }
        }

        public int GetFreshId ()
        {
            lock (this.GetType ()) {
                var fileName = Path.Combine (this.directory, "LastKey");
                int newKey;
                if (!File.Exists (fileName)) {
                    newKey = 1;
                } else {
                    var str = File.ReadAllText (fileName);
                    newKey = int.Parse (str) + 1;
                }
                File.WriteAllText (fileName, newKey.ToString ());
                return newKey;
            }
        }
    }
}

