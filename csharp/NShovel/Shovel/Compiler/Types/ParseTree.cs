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
using System.Linq;

namespace Shovel.Compiler.Types
{
    public class ParseTree
    {
        public enum Labels
        {
            FileName,
            Var,
            Assignment,
            Prim0,
            Name,
            NamedBlock,
            Call,
            BlockReturn,
            Void,
            Bool,
            String,
            Number,
            List,
            UserDefinedPrimitive,
            Context,
            If,
            Fn,
            Begin,
            Placeholder
        }

        public Labels Label { get; set; }

        public int StartPos { get; set; }

        public int EndPos { get; set; }

        public IEnumerable<ParseTree> Children { get; set; }

        public string Content { get; set; }

        public override string ToString ()
        {
            var sb = new StringBuilder ();
            this.RenderToStringBuilder (sb, 0);
            return sb.ToString ();
        }

        public void RenderToStringBuilder (StringBuilder sb, int indentation)
        {
            var content = String.IsNullOrEmpty(this.Content) ? "" : String.Format (" '{0}'", this.Content);
            sb.AppendFormat ("{4}{0} ({1} -- {2}){3}\n", 
                             this.Label, 
                             this.StartPos, this.EndPos,
                             content,			                 
                             new String (' ', indentation));
            if (this.Children != null) {
                foreach (var child in this.Children) {
                    child.RenderToStringBuilder (sb, indentation + 2);
                }
            }
        }

        public void RunOnChildren(Func<ParseTree, ParseTree> fn)
        {
            if (Children != null)
            {
                if (Children is ParseTree[])
                {
                    var arChildren = (ParseTree[])Children;
                    for (int i = 0; i < arChildren.Length; i++)
                    {
                        arChildren[i] = fn(arChildren[i]);
                    }
                }
                else if (Children is List<ParseTree>)
                {
                    var liChildren = (List<ParseTree>)Children;
                    for (int i = 0; i < liChildren.Count; i++)
                    {
                        liChildren[i] = fn(liChildren[i]);
                    }
                }
                else
                {
                    Children = Children.Select(fn).ToArray<ParseTree>();
                }
            }
        }
    }
}

