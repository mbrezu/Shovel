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
using Shovel.Exceptions;
using Shovel.Compiler.Types;

namespace Shovel.Compiler
{
    public class Tokenizer
    {
        public Tokenizer(SourceFile source, int initialPos = 0, int limit = -1)
        {
            this.source = source;
            this.initialPos = initialPos;
            this.limit = limit;
        }

        List<Token> tokens;
        SourceFile source;
        int pos;
        int initialPos;
        int limit;
        public bool StringInterpolation { get; set; }
        int bracketCounter = 0;

        public List<Token> Tokens
        {
            get
            {
                if (this.tokens == null)
                {
                    this.tokens = this.Tokenize();
                }
                return this.tokens;
            }
        }

        List<Token> Tokenize()
        {
            List<Token> result = new List<Token>();
            result.Add(new Token()
            {
                Type = Token.Types.FileName,
                Content = this.source.FileName
            });
            this.pos = initialPos;
            this.EatWhiteSpace();
            while (!this.Finished())
            {
                var ch = this.CurrentChar();
                if (Char.IsLetter(ch) || ch == '_' || ch == '@')
                {
                    result.Add(this.TokenizeIdentifier());
                }
                else if (Char.IsDigit(ch))
                {
                    result.Add(this.TokenizeNumber());
                }
                else if (ch == '"' || ch == '\'')
                {
                    result.Add(this.TokenizeStringLiteral(ch));
                }
                else if (ch == '/' && this.LookAhead() == '/')
                {
                    this.TokenizeComment();
                }
                else if (ch == '/' && this.LookAhead() == '*')
                {
                    this.TokenizeMultilineComment();
                }
                else
                {
                    var punct = this.TokenizePunctuation();
                    if (punct != null) { 
                        result.Add(punct);
                    }
                    else
                    {
                        break;
                    }
                }
                this.EatWhiteSpace();
            }
            return result;
        }

        char? LookAhead(int howFar = 1)
        {
            var laPos = this.pos + howFar;
            if (laPos < this.source.Content.Length)
            {
                return this.source.Content[laPos];
            }
            else
            {
                return null;
            }
        }

        Token MakePunctuationToken(int length)
        {
            int startPos = this.pos;
            for (int i = 0; i < length; i++)
            {
                this.NextChar();
            }
            return new Token()
            {
                Type = Token.Types.Punctuation,
                StartPos = startPos,
                EndPos = this.pos - 1,
                Content = this.source.Content.Substring(startPos, length)
            };
        }

        Token TokenizePunctuation()
        {
            var ch = this.CurrentChar();
            if (ch == '.')
            {
                var la = this.LookAhead();
                if (la != '.')
                {
                    return this.MakePunctuationToken(1);
                }
                else
                {
                    var la2 = this.LookAhead(2);
                    if (la2 == '.')
                    {
                        return this.MakePunctuationToken(3);
                    }
                    else
                    {
                        RaiseTokenizerError("Unknown token '..'. Did you mean '...'?");
                        throw new Exception(); // Just to keep the compiler happy.
                    }
                }
            }
            else if (ch == '(' || ch == ')'
              || ch == '[' || ch == ']'
              || ch == '{' || ch == '}'
              || ch == ',')
            {
                if (ch == '{')
                {
                    bracketCounter ++;
                }
                else if (ch == '}')
                {
                    bracketCounter --;
                    if (StringInterpolation && bracketCounter < 0)
                    {
                        return null;
                    }
                }
                return this.MakePunctuationToken(1);
            }
            else if (ch == '=')
            {
                var la = this.LookAhead();
                var isRelational = la == '=';
                Token result;
                if (isRelational)
                {
                    result = this.MakePunctuationToken(2);
                }
                else
                {
                    result = this.MakePunctuationToken(1);
                }
                result.IsRelational = isRelational;
                return result;
            }
            else if (ch == '+')
            {
                var result = this.MakePunctuationToken(1);
                result.IsAdderOp = true;
                return result;
            }
            else if (ch == '-')
            {
                var la = this.LookAhead();
                if (la == '>')
                {
                    var result = this.MakePunctuationToken(2);
                    result.IsPostfixOp = true;
                    return result;
                }
                else
                {
                    var result = this.MakePunctuationToken(1);
                    result.IsAdderOp = true;
                    return result;
                }
            }
            else if (ch == '<' || ch == '>')
            {
                var la = this.LookAhead();
                var isLongRelational = la == '=';
                var isMultiplier = ch == '<' && la == '<' || ch == '>' && la == '>';
                var isRelational = isLongRelational || !isMultiplier;
                Token result;
                if (isLongRelational || isMultiplier)
                {
                    result = this.MakePunctuationToken(2);
                }
                else
                {
                    result = this.MakePunctuationToken(1);
                }
                result.IsRelational = isRelational;
                result.IsMultiplierOp = isMultiplier;
                return result;
            }
            else if (ch == '*' || ch == '/' || ch == '%' || ch == '^')
            {
                var result = this.MakePunctuationToken(1);
                result.IsMultiplierOp = true;
                return result;
            }
            else if (ch == '|')
            {
                var la = this.LookAhead();
                var isLogical = la == '|';
                Token result;
                if (isLogical)
                {
                    result = this.MakePunctuationToken(2);
                }
                else
                {
                    result = this.MakePunctuationToken(1);
                }
                if (isLogical)
                {
                    result.IsLogicalOrOp = true;
                }
                else
                {
                    result.IsAdderOp = true;
                }
                return result;
            }
            else if (ch == '&')
            {
                var la = this.LookAhead();
                var isLogical = la == '&';
                Token result;
                if (isLogical)
                {
                    result = this.MakePunctuationToken(2);
                }
                else
                {
                    result = this.MakePunctuationToken(1);
                }
                if (isLogical)
                {
                    result.IsLogicalAndOp = true;
                }
                else
                {
                    result.IsMultiplierOp = true;
                }
                return result;
            }
            else if (ch == '!')
            {
                var la = this.LookAhead();
                var isRelational = la == '=';
                Token result;
                if (isRelational)
                {
                    result = this.MakePunctuationToken(2);
                }
                else
                {
                    result = this.MakePunctuationToken(1);
                }
                result.IsRelational = isRelational;
                return result;
            }
            else if (ch == '$')
            {
                return this.MakePunctuationToken(1);
            }
            else if (ch == ':')
            {
                return this.MakePunctuationToken(1);
            }
            else
            {
                RaiseTokenizerError(String.Format("Unexpected character '{0}'.", ch));
                throw new Exception(); // Just to keep the compiler happy.
            }
        }

        private void RaiseTokenizerError(string message)
        {
            var pos = Position.CalculatePosition(this.source, this.pos);
            var lines = Utils.ExtractRelevantSource(
                this.source.Content.Split('\n'), pos, pos);
            var errorMessage = String.Format(
                "{0}\n{1}\n{2}", message, lines[0], lines[1]);
            throw new ShovelException()
            {
                ShovelMessage = errorMessage,
                FileName = pos.FileName,
                Line = pos.Line,
                Column = pos.Column
            };
        }

        void TokenizeMultilineComment()
        {
            this.NextChar();
            this.NextChar();
            while (!Finished())
            {
                if (this.CurrentChar() == '*' && this.LookAhead() == '/')
                {
                    this.NextChar();
                    this.NextChar();
                    return;
                }
                else
                {
                    this.NextChar();
                }
            }
            throw new ShovelException()
            {
                ShovelMessage = "Reached the end of the file while parsing a multiline comment.",
                FileName = this.source.FileName,
                AtEof = true
            };
        }

        void TokenizeComment()
        {
            this.NextChar();
            this.NextChar();
            this.TokenizePred(Token.Types.FileName, ch => ch != '\n');
            this.NextChar();
        }

        Token TokenizeStringLiteral(char quote)
        {
            bool escaped = false;
            int quoteCounter = 0;
            var result = TokenizePred(Token.Types.LiteralString, (ch) =>
            {
                var goOn = quoteCounter < 2;
                if (ch == quote && !escaped)
                {
                    quoteCounter++;
                }
                escaped = ch == '\\';
                return goOn;
            }
            );
            if (quoteCounter < 2)
            {
                throw new ShovelException()
                {
                    ShovelMessage = "Expected an end quote, but reached the end of file.",
                    FileName = this.source.FileName,
                    AtEof = true
                };
            }
            return result;
        }

        Token TokenizeNumber()
        {
            bool afterDecimalDot = false;
            return TokenizePred(Token.Types.Number, (ch) =>
            {
                if (afterDecimalDot)
                {
                    return Char.IsDigit(ch);
                }
                else
                {
                    if (ch == '.')
                    {
                        afterDecimalDot = true;
                        return true;
                    }
                    else
                    {
                        return Char.IsDigit(ch);
                    }
                }
            }
            );
        }

        Token TokenizeIdentifier()
        {
            var result = TokenizePred(Token.Types.Identifier, (ch) =>
                 Char.IsLetterOrDigit(ch) || ch == '@' || ch == '_'
            );
            if (result.Content[0] == '@')
            {
                result.Type = Token.Types.UserDefinedPrimitive;
            }
            else if (Tokenizer.keywords.Contains(result.Content))
            {
                result.Type = Token.Types.Keyword;
            }
            else if (Tokenizer.requiredPrimitives.Contains(result.Content))
            {
                result.IsRequiredPrimitive = true;
            }
            return result;
        }

        static HashSet<String> keywords = new HashSet<String>(
            new string[] { "var", "if", "else", "fn", "return", 
            "true", "false", "null", "block", "context" }
            );
        static HashSet<String> requiredPrimitives = new HashSet<String>(
            new string[] { "pow",
          "array", "arrayN", "length", "slice", "push", "pop",
          "lower", "upper",
          "hash", "keys", "hasKey",
          "utcSecondsSinceUnixEpoch", "decodeTime", "encodeTime",
          "isString", "isHash", "isBool", "isArray", "isNumber", "isInteger", "isCallable",
          "string", "stringRepresentation",
          "parseInt", "parseFloat", "floor",
          "panic", "delete",
          "defstruct", "make", "hashToStruct", "structToHash", "isStruct", "isStructInstance", "apply",
          "setHandlers", "format" });

        Token TokenizePred(Token.Types type, Func<char, bool> pred)
        {
            int startPos = this.pos;
            while (!this.Finished() && pred(this.CurrentChar()))
            {
                this.NextChar();
            }
            return new Token()
            {
                StartPos = startPos,
                EndPos = this.pos - 1,
                Type = type,
                Content = this.source.Content.Substring(startPos, this.pos - startPos)
            };
        }

        bool Finished()
        {
            return this.pos >= this.source.Content.Length || (limit != -1 && this.pos >= limit);
        }

        char CurrentChar()
        {
            return this.source.Content[this.pos];
        }

        void NextChar()
        {
            this.pos++;
        }

        void EatWhiteSpace()
        {
            while (!this.Finished() && Char.IsWhiteSpace(this.CurrentChar()))
            {
                this.NextChar();
            }
        }

    }
}

