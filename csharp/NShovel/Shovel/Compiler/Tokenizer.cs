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

namespace Shovel.Compiler
{
    public class Tokenizer
    {
        public Tokenizer (SourceFile source)
        {
            this.source = source;
        }

        List<Token> tokens;
        SourceFile source;
        int pos;

        public List<Token> Tokens {
            get {
                if (this.tokens == null) {
                    this.tokens = this.Tokenize ();
                }
                return this.tokens;
            }
        }

        List<Token> Tokenize ()
        {
            List<Token> result = new List<Token> ();
            result.Add (new Token () {
                Type = Token.Types.FileName, 
                Content = this.source.FileName }
            );
            this.pos = 0;
            while (!this.Finished()) {
                this.EatWhiteSpace ();
                var ch = this.CurrentChar ();
                if (Char.IsLetter (ch) || ch == '_' || ch == '@') {
                    result.Add (this.TokenizeIdentifier ());
                } else if (Char.IsDigit (ch)) {
                    result.Add (this.TokenizeNumber ());
                } else if (ch == '"' || ch == '\'') {
                    result.Add (this.TokenizeStringLiteral (ch));
                } else if (ch == '/' && this.LookAhead () == '/') {
                    this.TokenizeComment ();
                } else {
                    result.Add (this.TokenizePunctuation ());
                }
            }
            return result;
        }
        
        char? LookAhead ()
        {
            var laPos = this.pos + 1;
            if (laPos < this.source.Content.Length) {
                return this.source.Content [laPos];
            } else {
                return null;
            }
        }
        
        Token MakePunctuationToken (int length)
        {
            int startPos = this.pos;
            for (int i = 0; i < length; i++) {
                this.NextChar ();
            }
            return new Token () {
                Type = Token.Types.Punctuation,
                StartPos = startPos,
                EndPos = this.pos - 1,
                Content = this.source.Content.Substring (startPos, length)
            };
        }
        
        Token TokenizePunctuation ()
        {
            var ch = this.CurrentChar ();
            if (ch == '.' 
                || ch == '(' || ch == ')' 
                || ch == '[' || ch == ']'
                || ch == '{' || ch == '}'
                || ch == ',') {
                return this.MakePunctuationToken (1);
            } else if (ch == '=') {
                var la = this.LookAhead ();
                var isRelational = la == '=';
                Token result;
                if (isRelational) {
                    result = this.MakePunctuationToken (2);
                } else {
                    result = this.MakePunctuationToken (1);
                }
                result.IsRelational = isRelational;
                return result;
            } else if (ch == '+' || ch == '-') {
                var result = this.MakePunctuationToken (1);
                result.IsAdderOp = true;
                return result;
            } else if (ch == '<' || ch == '>') {
                var la = this.LookAhead ();
                var isLongRelational = la == '=';
                var isMultiplier = ch == '<' && la == '<' || ch == '>' && la == '>';
                var isRelational = isLongRelational || !isMultiplier;
                Token result;
                if (isLongRelational || isMultiplier) {
                    result = this.MakePunctuationToken (2);
                } else {
                    result = this.MakePunctuationToken (1);
                }
                result.IsRelational = isRelational;
                result.IsMultiplierOp = isMultiplier;
                return result;
            } else if (ch == '*' || ch == '/' || ch == '%' || ch == '^') {
                var result = this.MakePunctuationToken (1);
                result.IsMultiplierOp = true;
                return result;
            } else if (ch == '|') {
                var la = this.LookAhead ();
                var isLogical = la == '|';
                Token result;
                if (isLogical) {
                    result = this.MakePunctuationToken (2);
                } else {
                    result = this.MakePunctuationToken (1);
                }
                if (isLogical) {
                    result.IsLogicalOrOp = true;
                } else {
                    result.IsAdderOp = true;
                }
                return result;
            } else if (ch == '&') {
                var la = this.LookAhead ();
                var isLogical = la == '&';
                Token result;
                if (isLogical) {
                    result = this.MakePunctuationToken (2);
                } else {
                    result = this.MakePunctuationToken (1);
                }
                if (isLogical) {
                    result.IsLogicalAndOp = true;
                } else {
                    result.IsMultiplierOp = true;
                }
                return result;
            } else if (ch == '!') {
                var la = this.LookAhead ();
                var isRelational = la == '=';
                Token result;
                if (isRelational) {
                    result = this.MakePunctuationToken (2);
                } else {
                    result = this.MakePunctuationToken (1);
                }
                result.IsRelational = isRelational;
                return result;
            } else {
                var pos = Position.CalculatePosition (this.source, this.pos);
                var message = String.Format ("Unexpected character '{0}'.", ch);
                var lines = Utils.ExtractRelevantSource (
                    this.source.Content.Split ('\n'), pos, pos);
                var errorMessage = String.Format (
                    "{0}\n{1}\n{2}", message, lines [0], lines [1]);
                throw new ShovelException () {
                    Message = errorMessage,
                    FileName = pos.FileName,
                    Line = pos.Line,
                    Column = pos.Column
                };
            }
        }
        
        void TokenizeComment ()
        {
            this.NextChar ();
            this.NextChar ();
            this.TokenizePred (Token.Types.FileName, ch => ch != '\n');
            this.NextChar ();
        }
        
        Token TokenizeStringLiteral (char quote)
        {
            bool escaped = false;
            int quoteCounter = 0;
            var result = TokenizePred (Token.Types.LiteralString, (ch) => {
                var goOn = quoteCounter < 2;
                if (ch == quote && !escaped) {
                    quoteCounter ++;
                }
                escaped = ch == '\\';
                return goOn;
            }
            );  
            if (quoteCounter < 2) {
                throw new ShovelException () {
                    Message = "Expected an end quote, but reached the end of file.",
                    FileName = this.source.FileName,
                    AtEof = true
                };
            }
            return result;
        }
        
        Token TokenizeNumber ()
        {
            bool afterDecimalDot = false;
            return TokenizePred (Token.Types.Number, (ch) => {
                if (afterDecimalDot) {
                    return Char.IsDigit (ch);
                } else {                    
                    if (ch == '.') {
                        afterDecimalDot = true;
                        return true;
                    } else {
                        return Char.IsDigit (ch);
                    }
                }                   
            }
            );
        }
        
        Token TokenizeIdentifier ()
        {
            var result = TokenizePred (Token.Types.Identifier, (ch) => 
                 Char.IsLetterOrDigit (ch) || ch == '@' || ch == '_'
            );
            if (result.Content [0] == '@') {
                result.Type = Token.Types.UserDefinedPrimitive;
            } else if (Tokenizer.keywords.Contains (result.Content)) {
                result.Type = Token.Types.Keyword;
            } else if (Tokenizer.requiredPrimitives.Contains (result.Content)) {
                result.IsRequiredPrimitive = true;
            }
            return result;
        }
        
        static HashSet<String> keywords = new HashSet<String> (
            new string[] { "var", "if", "else", "fn", "return", 
            "true", "false", "null", "block", "context" }
            );
        static HashSet<String> requiredPrimitives = new HashSet<String> (
            new string[] { "pow",
          "array", "arrayN", "length", "slice", "push", "pop",
          "lower", "upper",
          "hash", "keys", "hasKey",
          "utcSecondsSinceUnixEpoch", "decodeTime", "encodeTime",
          "isString", "isHash", "isBool", "isArray", "isNumber", "isCallable",
          "string", "stringRepresentation",
          "parseInt", "parseFloat", "floor",
            "panic" }
            );
        
        Token TokenizePred (Token.Types type, Func<char, bool> pred)
        {
            int startPos = this.pos;
            while (!this.Finished() && pred(this.CurrentChar())) {
                this.NextChar ();
            }
            return new Token () {
                StartPos = startPos,
                EndPos = this.pos - 1,
                Type = type,
                Content = this.source.Content.Substring(startPos, this.pos - startPos)
            };
        }

        bool Finished ()
        {
            return this.pos >= this.source.Content.Length;
        }

        char CurrentChar ()
        {
            return this.source.Content [this.pos];
        }

        void NextChar ()
        {
            this.pos ++;
        }

        void EatWhiteSpace ()
        {
            while (!this.Finished () && Char.IsWhiteSpace(this.CurrentChar ())) {
                this.NextChar ();
            }
        }

    }
}

