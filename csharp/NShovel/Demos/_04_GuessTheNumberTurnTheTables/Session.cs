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

namespace _04_GuessTheNumberTurnTheTables
{
    public class Session
    {
        public byte[] ShovelVmState { get; set; }

        public byte[] ShovelVmBytecode { get; set; }

        public string ShovelVmSources { get; set; }

        public StringBuilder PageContent { get; set; }

        public enum ReadStates
        {
            None,
            ReadInteger,
            ReadChar,
            ReadLine
        }
        ;

        public int Id { get; set; }

        public ReadStates ReadState { get; set; }

        public Session ()
        {
            this.PageContent = new StringBuilder ();
        }

        readonly static string ShovelVmStateName = "ShovelVmState";
        readonly static string ShovelVmBytecodeName = "ShovelVmBytecode";
        readonly static string ShovelVmSourcesName = "ShovelVmSources";
        readonly static string PageContentName = "PageContent";
        readonly static string ReadStateName = "ReadState";

        public static Session Load (FileSystemDatabase fsd, int key)
        {
            var toLoad = fsd.Read (key);
            if (toLoad == null) {
                return null;
            }
            var result = new Session ();
            result.Id = key;
            if (toLoad.ContainsKey (ShovelVmStateName)) {
                result.ShovelVmState = toLoad [ShovelVmStateName];
            }
            if (toLoad.ContainsKey (ShovelVmBytecodeName)) {
                result.ShovelVmBytecode = toLoad [ShovelVmBytecodeName];
            }
            if (toLoad.ContainsKey (ShovelVmSourcesName)) {
                result.ShovelVmSources = Encoding.UTF8.GetString (toLoad [ShovelVmSourcesName]);
            }
            if (toLoad.ContainsKey (PageContentName)) {
                result.PageContent.Append (Encoding.UTF8.GetString (toLoad [PageContentName]));
            }
            if (toLoad.ContainsKey (ReadStateName)) {
                result.ReadState = (ReadStates)BitConverter.ToInt32 (toLoad [ReadStateName], 0);
            }
            return result;
        }

        public void Save (FileSystemDatabase fsd)
        {
            var toSave = new Dictionary<string, byte[]> ();
            toSave [ShovelVmStateName] = this.ShovelVmState;
            toSave [ShovelVmBytecodeName] = this.ShovelVmBytecode;
            toSave [PageContentName] = Encoding.UTF8.GetBytes (this.PageContent.ToString ());
            toSave [ReadStateName] = BitConverter.GetBytes ((int)this.ReadState);
            toSave [ShovelVmSourcesName] = Encoding.UTF8.GetBytes (this.ShovelVmSources);           
            fsd.Write (this.Id, toSave);
        }
    }
}

