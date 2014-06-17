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

namespace Shovel.Compiler.Types
{
    public class Token
    {
        public enum Types
        {
            Number,
            Identifier,
            Keyword,
            LiteralString,
            FileName,
            UserDefinedPrimitive,
            Punctuation
        }
        public Types Type { get; set; }

        public int StartPos { get; set; }

        public int EndPos { get; set; }

        public string Content { get; set; }
        
        public bool IsRequiredPrimitive { get; set; }
        
        public bool IsRelational { get; set; }

        public bool IsAdderOp { get; set; }

        public bool IsMultiplierOp { get; set; }

        public bool IsLogicalOrOp { get; set; }

        public bool IsLogicalAndOp { get; set; }

        public bool IsPostfixOp { get;set; }

        public override string ToString ()
        {
            return string.Format (
                "[Token: Type={0}, StartPos={1}, EndPos={2}, Content='{3}']", 
                Type, StartPos, EndPos, Content);
        }

    }
}

