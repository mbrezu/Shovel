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
using System.Linq;

namespace Shovel.Compiler
{
	public class Parser
	{
		List<Token> tokens;
		List<SourceFile> sources;
		int tokenIndex;
		string fileName;
		Token lastToken;

		public Parser (List<Token> tokens, List<SourceFile> sources)
		{
			this.tokens = tokens;
			this.sources = sources;
		}

		List<ParseTree> parseTrees;

		public List<ParseTree> ParseTrees {
			get {
				if (this.parseTrees == null) {
					this.parseTrees = this.Parse ();
				}
				return this.parseTrees;
			}
		}

		List<ParseTree> Parse ()
		{
			var result = new List<ParseTree> ();
			this.fileName = this.tokens [0].Content;
			result.Add (new ParseTree () {
                Label = ParseTree.Labels.FileName,
                StartPos = 0,
                EndPos = 0,
                Content = this.fileName
            }
			);
			this.tokenIndex = 1;
			while (!this.Finished()) {
				result.Add (this.ParseStatement ());
			}
			return result;
		}

		bool Finished ()
		{
			return this.tokenIndex == this.tokens.Count;
		}

		ParseTree ParseBlockReturn ()
		{
			return this.WithNewParseTree (ParseTree.Labels.BlockReturn, pt => {
				this.ConsumeToken (Token.Types.Keyword, "return");
				pt.Children = new ParseTree[] { 
					this.ParseExpression (),
					this.ParseExpression ()
				};
			}
			);
		}

		ParseTree ParseStatement ()
		{
			if (this.TokenIs (Token.Types.Keyword, "var")) {
				return this.ParseVarDeclaration ();
			} else if (this.TokenIs (Token.Types.Keyword, "return")) {
				return this.ParseBlockReturn ();
			} else { 
				var expr = this.ParseExpression ();
				if (this.TokenIs (Token.Types.Punctuation, "=")) {
					return this.ParseAssignment (expr);
				} else {
					return expr;
				}
			}
		}

		ParseTree ParseAssignment (ParseTree expr)
		{
			return this.WithAnchoredParseTree (expr.StartPos, ParseTree.Labels.Assignment, pt => {
				pt.Children = new ParseTree[] { 
                    expr, 
                    this.TokenAsParseTree (ParseTree.Labels.Prim0), 
                    this.ParseExpression () };
			}
			);
		}

		ParseTree TokenAsParseTree (ParseTree.Labels prim0)
		{
			return this.WithNewParseTree (prim0, pt => {
				pt.Content = this.CurrentToken ().Content;
				this.NextToken ();
			}
			);
		}

		bool TokenIs (Token.Types type, string content = null)
		{
			if (!Finished ()) {
				var token = this.CurrentToken ();
				return token.Type == type && (content == null || token.Content == content);
			} else {
				return false;
			}
		}

		ParseTree ParseVarDeclaration ()
		{
			return WithNewParseTree (ParseTree.Labels.Var, pt => {
				this.ConsumeToken (Token.Types.Keyword, "var");
				var lhs = this.ParseName (false);
				this.ConsumeToken (Token.Types.Punctuation, "=");
				var rhs = this.ParseExpression ();
				pt.Children = new ParseTree[] { lhs, rhs};
			}
			);
		}

		ParseTree ParseName (bool canBeRequiredPrimitive = true)
		{
			this.RequireTokenType (Token.Types.Identifier);
			var token = this.CurrentToken ();
			if (token.IsRequiredPrimitive) {
				if (canBeRequiredPrimitive) {
					return this.TokenAsParseTree (ParseTree.Labels.Prim0);
				} else {
					this.RaiseError (String.Format ("Name '{0}' is reserved for a primitive.", token.Content));
				}
			}
			return this.TokenAsParseTree (ParseTree.Labels.Name);
		}

		ParseTree ParseExpression ()
		{
			if (this.TokenIs (Token.Types.Keyword, "fn")) {
				return this.ParseLambda ();
			} else if (this.TokenIs (Token.Types.Keyword, "if")) {
				return this.ParseIf ();
			} else if (this.TokenIs (Token.Types.Keyword, "block")) {
				return this.WithNewParseTree (ParseTree.Labels.NamedBlock, pt => {
					this.ConsumeToken(Token.Types.Keyword, "block");
					pt.Children = new ParseTree[] {
                        this.ParseExpression (),
                        this.ParseStatement ()
                    };
				}
				);
			} else {
				return this.LeftAssoc (
					this.ParseOrTerm, 
					token => token.IsLogicalOrOp,
					this.PostProcessLogicalOr);
			}
		}

		static ParseTree trueParseTree;

		static ParseTree GetTrueParseTree ()
		{
			if (Parser.trueParseTree == null) {
				Parser.trueParseTree = new ParseTree () {
					Label = ParseTree.Labels.Bool,
					Content = "true",
					StartPos = -1,
					EndPos = -1
				};
			}
			return Parser.trueParseTree;
		}

		static ParseTree falseParseTree;

		static ParseTree GetFalseParseTree ()
		{
			if (Parser.falseParseTree == null) {
				Parser.falseParseTree = new ParseTree () {
					Label = ParseTree.Labels.Bool,
					Content = "false",
					StartPos = -1,
					EndPos = -1
				};
			}
			return Parser.falseParseTree;
		}

		static ParseTree nullParseTree;
		static ParseTree GetNullParseTree ()
		{
			if (Parser.nullParseTree == null) {
				Parser.nullParseTree = new ParseTree() {
					Label = ParseTree.Labels.Void,
					Content = "null",
					StartPos = -1,
					EndPos = -1
				};
			}
			return Parser.nullParseTree;
		}		

		ParseTree MaybeRewriteAsIfExpression (
			ParseTree parseTree, 
			string opName, 
			Func<ParseTree, ParseTree, IEnumerable<ParseTree>> ifChildren)
		{
			if (this.IsRequiredPrimitiveCall (parseTree, opName)) {
				var operands = parseTree.Children.ToArray ();
				var op = operands [0];
				var t1 = operands [1];
				var t2 = operands [2];
				return new ParseTree () {
					Label = ParseTree.Labels.If,
					StartPos = op.StartPos,
					EndPos = op.EndPos,
					Children = ifChildren(t1, t2)
				};
			} else {
				return parseTree;
			}
		}

		bool IsRequiredPrimitiveCall (ParseTree parseTree, string opName)
		{
			if (parseTree.Label == ParseTree.Labels.Call) {
				var op = parseTree.Children.First ();
				if (op.Label == ParseTree.Labels.Prim0 && op.Content == opName) {
					return true;
				}
			}
			return false;
		}

		ParseTree PostProcessLogicalOr (ParseTree arg)
		{
			return this.MaybeRewriteAsIfExpression (
				arg, "||", 
				(t1, t2) => new ParseTree[] { t1, Parser.GetTrueParseTree (), t2 });
		}

		ParseTree PostProcessLogicalAnd (ParseTree arg)
		{
			return this.MaybeRewriteAsIfExpression (
				arg, "&&", 
				(t1, t2) => new ParseTree[] { t1, t2, Parser.GetFalseParseTree () });
		}

		ParseTree ParseOrTerm ()
		{
			return LeftAssoc (
				this.ParseAndTerm, 
				token => token.IsLogicalAndOp,
				this.PostProcessLogicalAnd);
		}

		ParseTree ParseAndTerm ()
		{
			return LeftAssoc (this.ParseRelationalTerm, token => token.IsRelational); 
		}

		ParseTree ParseRelationalTerm ()
		{
			return LeftAssoc (this.ParseAdditionTerm, token => token.IsAdderOp);
		}

		ParseTree ParseAdditionTerm ()
		{
			return LeftAssoc (this.ParseMultiplicationTerm, token => token.IsMultiplierOp);
		}

		ParseTree ParseMultiplicationTerm ()
		{
			if (this.TokenIs (Token.Types.Punctuation, "-")) {
				return this.ParseUnaryMinus ();
			} else if (this.TokenIs (Token.Types.Punctuation, "!")) {
				return this.ParseLogicalNot ();
			} else {
				return this.ParseAtomish ();
			}
		}

		ParseTree ParseAtomish ()
		{
			if (this.TokenIs (Token.Types.Number)) {
				return this.ParseNumber ();
			} else if (this.TokenIs (Token.Types.LiteralString)) {
				return this.ParseLiteralString ();
			} else if (this.TokenIs (Token.Types.Keyword, "true")
				|| this.TokenIs (Token.Types.Keyword, "false")) {
				return this.ParseBool ();
			} else if (this.TokenIs (Token.Types.Keyword, "null")) {
				return this.ParseVoid ();
			} else {
				return this.ParseIdentifierOrCallOrRef ();
			}
		}

		static Tuple<Token.Types, String> OpenParenthesis = Tuple.Create (Token.Types.Punctuation, "(");
		static Tuple<Token.Types, String> Comma = Tuple.Create (Token.Types.Punctuation, ",");
		static Tuple<Token.Types, String> CloseParenthesis = Tuple.Create (Token.Types.Punctuation, ")");

		// Gref is short for 'generic reference'.
		ParseTree ParseIdentifierOrCallOrRef (ParseTree forcedStart = null)
		{
			var start = forcedStart;
			if (start == null) {
				start = this.ParseParenthesizedOrName ();
			}
			if (this.TokenIs (Token.Types.Punctuation, ".")) {
				var first = this.WithAnchoredParseTree (start.StartPos, ParseTree.Labels.Call, pt => {
					this.RequireTokenExactly (Token.Types.Punctuation, ".");
					pt.Children = new ParseTree[] {
						this.MakePrim0ParseTree ("svm_gref_dot"),
						start,
						this.ParseNameAsString ()
					};
				}
				);
				return this.ParseIdentifierOrCallOrRef (first);
			} else if (this.TokenIs (Token.Types.Punctuation, "(")) {
				var first = this.WithAnchoredParseTree (start.StartPos, ParseTree.Labels.Call, pt => {
					var children = new List<ParseTree> ();
					children.Add (start);
					children.AddRange (this.ParseList (
						this.ParseExpression, 
						Parser.OpenParenthesis, Parser.Comma, Parser.CloseParenthesis)
					);
					pt.Children = children;
				}
				);
				return this.ParseIdentifierOrCallOrRef (first);
			} else if (this.TokenIs (Token.Types.Punctuation, "[")) {
				ParseTree grefParseTree = null;
				int grefStartPos = 0;
				int grefEndPos = 0;
				var result = this.WithAnchoredParseTree (start.StartPos, ParseTree.Labels.Call, pt => {
					grefStartPos = this.CurrentToken ().StartPos;
					this.RequireTokenExactly (Token.Types.Punctuation, "[");
					grefParseTree = this.MakePrim0ParseTree ("svm_gref");
					pt.Children = new ParseTree[] {
						grefParseTree,
						start,
						this.ParseExpression ()
					};
					this.ConsumeToken (Token.Types.Punctuation, "]");
					grefEndPos = this.LastToken ().EndPos;
				}
				);
				grefParseTree.StartPos = grefStartPos;
				grefParseTree.EndPos = grefEndPos;
				return result;
			} else {
				return start;
			}
		}

		IEnumerable<ParseTree> ParseList (
			Func<ParseTree> itemParser, 
			Tuple<Token.Types, String> openParen, 
			Tuple<Token.Types, String> sep, 
			Tuple<Token.Types, String> closeParen)
		{
			this.ConsumeToken (openParen.Item1, openParen.Item2);
			var result = new List<ParseTree> ();
			while (true) {
				if (!this.TokenIs (closeParen.Item1, closeParen.Item2)) {
					result.Add (itemParser ());
				}
				this.RequireToken (sep, closeParen);
				if (this.TokenIs (closeParen.Item1, closeParen.Item2)) {
					break;
				}
				this.ConsumeToken (sep.Item1, sep.Item2);
			}
			this.ConsumeToken (closeParen.Item1, closeParen.Item2);
			return result;
		}

		void RequireToken (params Tuple<Token.Types, string>[] alternatives)
		{
			foreach (var alt in alternatives) {
				if (this.TokenIs (alt.Item1, alt.Item2)) {
					return;
				}
			}
			this.RequireTokenError (
				alternatives.Select (alt => (Token.Types?)alt.Item1),
				alternatives.Select (alt => alt.Item2),
				this.CurrentToken ());
		}

		ParseTree ParseNameAsString ()
		{
			this.RequireTokenType (Token.Types.Identifier);
			var result = this.TokenAsParseTree (ParseTree.Labels.String);
			result.Content = String.Format ("\"{0}\"", result.Content);
			return result;
		}

		ParseTree ParseParenthesizedOrName ()
		{
			if (this.TokenIs (Token.Types.Identifier)) {
				return this.ParseName ();
			} else if (this.TokenIs (Token.Types.Keyword, "context")) {
				return this.ParseContext ();
			} else if (this.TokenIs (Token.Types.Punctuation, "(")) {
				this.ConsumeToken (Token.Types.Punctuation, "(");
				var result = this.ParseExpression ();
				this.ConsumeToken (Token.Types.Punctuation, ")");
				return result;
			} else if (this.TokenIs (Token.Types.Punctuation, "{")) {
				return this.ParseBlock ();
			} else if (this.TokenIs (Token.Types.UserDefinedPrimitive)) {
				return this.ParsePrimitive ();
			}  
			string message;
			if (!this.Finished ()) {
				message = String.Format ("Unexpected token '{0}'.", this.CurrentToken ().Content);
			} else {
				message = "Unexpected end of file.";
			}
			this.RaiseError (message);
			return null; // This is never reached, RaiseError throws.
		}

		ParseTree ParsePrimitive ()
		{
			this.RequireTokenType (Token.Types.Identifier);
			return this.TokenAsParseTree (ParseTree.Labels.UserDefinedPrimitive);
		}

		ParseTree ParseBlock ()
		{
			return this.WithNewParseTree (ParseTree.Labels.Begin, pt => {
				this.ConsumeToken (Token.Types.Punctuation, "{");
				var statements = new List<ParseTree> ();
				while (!this.Finished() && !this.TokenIs (Token.Types.Punctuation, "}")) {
					statements.Add (this.ParseStatement ());
				}
				this.ConsumeToken (Token.Types.Punctuation, "}");
				pt.Children = statements;
			}
			);
		}

		ParseTree ParseContext ()
		{
			this.RequireTokenExactly (Token.Types.Keyword, "context");
			return this.TokenAsParseTree (ParseTree.Labels.Context);
		}

		ParseTree ParseVoid ()
		{
			return this.TokenAsParseTree (ParseTree.Labels.Void);
		}

		ParseTree ParseBool ()
		{
			return this.TokenAsParseTree (ParseTree.Labels.Bool);
		}

		ParseTree ParseLiteralString ()
		{
			return this.TokenAsParseTree (ParseTree.Labels.String);
		}

		ParseTree ParseNumber ()
		{
			return this.TokenAsParseTree (ParseTree.Labels.Number);
		}

		ParseTree ParseLogicalNot ()
		{
			return this.WithNewParseTree (ParseTree.Labels.Call, pt => {
				this.RequireTokenExactly (Token.Types.Punctuation, "!");
				pt.Children = new ParseTree[] {
                    this.MakePrim0ParseTree ("!"),
                    this.ParseMultiplicationTerm ()
                };
			}
			);
		}

		ParseTree ParseUnaryMinus ()
		{
			return this.WithNewParseTree (ParseTree.Labels.Call, pt => {
				this.RequireTokenExactly (Token.Types.Punctuation, "-");
				pt.Children = new ParseTree[] {
                    this.MakePrim0ParseTree ("unary_minus"),
                    this.ParseMultiplicationTerm ()
                };
			}
			);
		}

		ParseTree LeftAssoc (
            Func<ParseTree> subParser, 
            Func<Token, bool> predicate, 
            Func<ParseTree, ParseTree> postProcessor = null)
		{
			var lhs = subParser ();
			while (this.CurrentToken() != null && predicate(this.CurrentToken())) {
				lhs = this.WithAnchoredParseTree (lhs.StartPos, ParseTree.Labels.Call, pt => {
					pt.Children = new ParseTree[] {
                        this.MakePrim0ParseTree (this.CurrentToken ().Content),
                        lhs,
                        subParser ()
                    };
				}
				);
				if (postProcessor != null) {
					lhs = postProcessor (lhs);
				}
			}
			return lhs;
		}

		ParseTree MakePrim0ParseTree (string primitiveName)
		{
			return this.WithNewParseTree (ParseTree.Labels.Prim0, pt => {
				pt.Content = primitiveName;
				this.NextToken ();
			}
			);
		}

		ParseTree ParseIf ()
		{
			return this.WithNewParseTree (ParseTree.Labels.If, pt => {
				this.ConsumeToken (Token.Types.Keyword, "if");
				var pred = this.ParseExpression ();
				var then = this.ParseExpression ();
				if (this.TokenIs (Token.Types.Keyword, "else")) {
					this.NextToken ();
					pt.Children = new ParseTree[] {
						pred, then, this.ParseStatement ()
					};
				} else {
					pt.Children = new ParseTree[] { pred, then, Parser.GetNullParseTree() };
				}
			}
			);
		}

		ParseTree ParseLambda ()
		{
			return this.WithNewParseTree (ParseTree.Labels.Fn, pt => {
				this.ConsumeToken(Token.Types.Keyword, "fn");
				var args = this.ParseLambdaArgs ();
				var body = this.ParseStatement ();
				pt.Children = new ParseTree[] { args, body };
			}
			);
		}

		ParseTree ParseLambdaArgs ()
		{
			return this.WithNewParseTree (ParseTree.Labels.List, pt => {
				if (this.TokenIs (Token.Types.Punctuation, "(")) {
					pt.Children = this.ParseList (
						() => this.ParseName (false),
						Parser.OpenParenthesis,
						Parser.Comma,
						Parser.CloseParenthesis);
				} else {
					pt.Children = new ParseTree[] { this.ParseName (false) };
				}
			}
			);
		}

		ParseTree WithNewParseTree (ParseTree.Labels label, Action<ParseTree> action)
		{
			ParseTree result = new ParseTree ();
			result.StartPos = this.CurrentToken ().StartPos;
			result.Label = label;
			action (result);
			result.EndPos = this.LastToken ().EndPos;
			return result;
		}

		ParseTree WithAnchoredParseTree (int startPos, ParseTree.Labels label, Action<ParseTree> action)
		{
			ParseTree result = new ParseTree ();
			result.StartPos = startPos;
			result.Label = label;
			action (result);
			result.EndPos = this.LastToken ().EndPos;
			return result;
		}

		void ConsumeToken (Token.Types type, string content)
		{
			this.RequireTokenExactly (type, content);
			this.NextToken ();
		}

		void NextToken ()
		{
			this.lastToken = this.CurrentToken ();
			this.tokenIndex++;
		}

		Token LastToken ()
		{
			return this.lastToken;
		}

		void RequireTokenExactly (Token.Types type, string content)
		{
			if (!this.TokenIs (type, content)) {
				this.RequireTokenError (
                    new Token.Types?[] { type }, 
                    new string[] { content },
                    this.CurrentToken ());
			}
		}

		void RequireTokenType (Token.Types type)
		{
			if (!this.TokenIs (type)) {
				this.RequireTokenError (
                    new Token.Types?[] { type }, 
                    new string[] { },
                    this.CurrentToken ());
			}
		}

		void RequireTokenError (
            IEnumerable<Token.Types?> types, IEnumerable<string> contents, Token actualToken)
		{
			var possibleTypes = types.Where (type => type != null);
			var possibleContents = contents.Where (content => content != null);
			string expectation = "";
			if (possibleContents.Count () == 0) {
				if (possibleTypes.Count () == 0) {
					Utils.Panic ();
				} else if (possibleTypes.Count () == 1) {
					expectation = String.Format ("Expected a {0}", possibleTypes.First ().ToString ().ToLower ());
				} else {
					expectation = String.Format (
                        "Expected one of {0}",
                        String.Join (", ", possibleTypes.Select (type => type.ToString ().ToLower ())));
				}
			} else if (possibleContents.Count () == 1) {
				expectation = String.Format ("Expected '{0}'", possibleContents.First ());
			} else {
				expectation = String.Format (
                    "Expected one of {0}",
                    String.Join (", ", possibleContents.Select (content => "'" + content + "'")));
			}
			string actualInput;
			if (actualToken != null) {
				actualInput = String.Format ("but got '{0}'", actualToken.Content);
			} else {
				actualInput = "but reached the end of input";
			}
			var message = String.Format ("{0}, {1}.", expectation, actualInput);
			this.RaiseError (message);
		}

		Token CurrentToken ()
		{
			if (!this.Finished ()) {
				return this.tokens [this.tokenIndex];
			} else {
				return null;
			}
		}

		void RaiseError (string message)
		{
			var token = this.CurrentToken ();
			Position pos = token != null ? this.GetPosition (token.StartPos) : null;
			string fileName = pos != null ? pos.FileName : null;
			bool atEof = token == null;
			string errorMessage;
			if (pos != null) {
				var startPos = pos;
				var endPos = this.GetPosition (token.EndPos);
				var lines = Utils.ExtractRelevantSource (GetSourceFile ().Content.Split ('\n'), startPos, endPos);
				errorMessage = String.Format ("{0}\n{1}\n{2}", message, lines [0], lines [1]);
			} else {
				errorMessage = message;
			}
			var error = new ShovelException ();
			error.Message = errorMessage;
			if (pos != null) {
				error.Line = pos.Line;
				error.Column = pos.Column;
			} else {
				error.AtEof = atEof;
			}
			error.FileName = fileName;
			throw error;
		}

		SourceFile GetSourceFile ()
		{
			var ourSource = this.sources.Where (source => source.FileName == this.fileName);
			if (ourSource.Count () != 1) {
				Utils.Panic ();
			}
			return ourSource.First ();
		}

		Position GetPosition (int pos)
		{
			return Position.CalculatePosition (GetSourceFile (), pos);
		}

	}
}

