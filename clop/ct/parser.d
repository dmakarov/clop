/++
This module was automatically generated from the following grammar:

#  The MIT License (MIT)
#  =====================
#
#  Copyright (c) 2015 Dmitri Makarov <dmakarov@alumni.stanford.edu>
#
#  Permission is hereby granted, free of charge, to any person obtaining a copy
#  of this software and associated documentation files (the "Software"), to deal
#  in the Software without restriction, including without limitation the rights
#  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#  copies of the Software, and to permit persons to whom the Software is
#  furnished to do so, subject to the following conditions:
#
#  The above copyright notice and this permission notice shall be included in all
#  copies or substantial portions of the Software.
#
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#  SOFTWARE.

CLOP:

 TranslationUnit        <- ExternalDeclarations? KernelBlock
 KernelBlock            <- (SyncPattern :Spacing)? RangeDecl :Spacing CompoundStatement (:Spacing Transformations)?
 ExternalDeclarations   <- ExternalDeclaration (:Spacing ExternalDeclaration)*
 ExternalDeclaration    <  FunctionDefinition / Declaration
 FunctionDefinition     <  TypeSpecifier Declarator CompoundStatement
 SyncPattern            <  Identifier
 RangeDecl              <  "NDRange" '(' RangeList ')'
 RangeList              <  RangeSpec (',' RangeSpec)*
 RangeSpec              <  Identifier ':' ConditionalExpr ".." ConditionalExpr
 Transformations        <  "apply" '(' TransList ')'
 TransList              <  TransSpec (',' TransSpec)*
 TransSpec              <  Identifier ('(' ')' / '(' ArgumentExprList ')')

# CLOP_Decl:
 Declaration            <  DeclarationSpecifiers InitDeclaratorList? ';'
 Declarator             <  (Identifier / '(' Declarator ')')
                           ('[' ']' / '(' ')' / '[' ConditionalExpr ']' / '(' ParameterList ')' / '(' IdentifierList ')')*
 StorageClassSpecifier  <- "local"
 TypeSpecifier          <- "void" / "bool" / "char" / "uchar" /
                           "short" / "ushort" / "int" / "uint" / "long" / "ulong" /
                           "float" / "double" / "half" / "size_t" / StructSpecifier
 DeclarationSpecifiers  <  (StorageClassSpecifier / TypeSpecifier) DeclarationSpecifiers?
 StructSpecifier        <  "struct" (Identifier ('{' StructDeclarationList '}')? / '{' StructDeclarationList '}')
 StructDeclarationList  <  StructDeclaration (:Spacing StructDeclaration)*
 StructDeclaration      <  TypeSpecifier StructDeclaratorList ';'
 StructDeclaratorList   <  StructDeclarator (',' StructDeclarator)*
 StructDeclarator       <  (Declarator ConditionalExpr? / ConditionalExpr)
 InitDeclaratorList     <  InitDeclarator (',' InitDeclarator)*
 InitDeclarator         <  Declarator ('=' Initializer)?
 InitializerList        <  Initializer (',' Initializer)*
 Initializer            <  AssignmentExpr / '{' InitializerList ','? '}'
 ParameterList          <  ParameterDeclaration (',' ParameterDeclaration)*
 ParameterDeclaration   <  TypeSpecifier Declarator
 TypeName               <  TypeSpecifier ('[' ']' / '[' ConditionalExpr ']') ('[' ']' / '[' ConditionalExpr ']')*

# CLOP_Stmt:
 StatementList          <- Statement (:Spacing Statement)*
 Statement              <  CompoundStatement / ExpressionStatement / IfStatement / IterationStatement / ReturnStatement / Declaration
 CompoundStatement      <  '{' StatementList? '}'
 ExpressionStatement    <  Expression ';'
 IfStatement            <  "if" '(' Expression ')' Statement ("else" Statement)?
 IterationStatement     <  WhileStatement / ForStatement
 WhileStatement         <  "while" '(' Expression ')' Statement
 ForStatement           <  "for" '(' Expression? ';' Expression? ';' Expression? ')' Statement
 ReturnStatement        <  "return" Expression? ';'

# CLOP_Expr:
 PrimaryExpr            <  Identifier ('!' StringLiteral)? / FloatLiteral / IntegerLiteral / '(' Expression ')'
 PostfixExpr            <  PrimaryExpr ( '[' Expression ']'
                                       / '(' ')'
                                       / '(' ArgumentExprList ')'
                                       / '.' Identifier
                                       / "++"
                                       / "--" )*
 ArgumentExprList       <  AssignmentExpr (',' AssignmentExpr)*
 UnaryExpr              <  PostfixExpr
                         / IncrementExpr
                         / DecrementExpr
                         / UnaryOperator CastExpr
 UnaryOperator          <- [-+~!]
 IncrementExpr          < "++" UnaryExpr
 DecrementExpr          < "--" UnaryExpr
 CastExpr               <  UnaryExpr / '(' TypeName ')' CastExpr
 MultiplicativeExpr     <  CastExpr (MultiplicativeOperator MultiplicativeExpr)*
 MultiplicativeOperator <- [*%/]
 AdditiveExpr           <  MultiplicativeExpr (AdditiveOperator AdditiveExpr)*
 AdditiveOperator       <- [-+]
 ShiftExpr              <  AdditiveExpr (ShiftOperator ShiftExpr)*
 ShiftOperator          <- "<<" / ">>"
 RelationalExpr         <  ShiftExpr (RelationalOperator RelationalExpr)*
 RelationalOperator     <- "<=" / ">=" / "<" / ">"
 EqualityExpr           <  RelationalExpr (EqualityOperator EqualityExpr)*
 EqualityOperator       <- "==" / "!="
 ANDExpr                <  EqualityExpr ('&' ANDExpr)*
 ExclusiveORExpr        <  ANDExpr ('^' ExclusiveORExpr)*
 InclusiveORExpr        <  ExclusiveORExpr ('|' InclusiveORExpr)*
 LogicalANDExpr         <  InclusiveORExpr ("&&" LogicalANDExpr)*
 LogicalORExpr          <  LogicalANDExpr ("||" LogicalORExpr)*
 ConditionalExpr        <  LogicalORExpr ('?' Expression ':' ConditionalExpr)?
 AssignmentExpr         <  UnaryExpr AssignmentOperator AssignmentExpr / ConditionalExpr
 AssignmentOperator     <- "=" / "*=" / "/=" / "%=" / "+=" / "-=" / "<<=" / ">>=" / "&=" / "^=" / "|="
 Expression             <  AssignmentExpr (',' AssignmentExpr)*

# CLOP_Term:
 IdentifierList         <  Identifier (',' Identifier)*
 Identifier             <~ !Keyword [a-zA-Z_] [a-zA-Z0-9_]*
 Keyword                <- "NDRange" / "apply"
                         / "auto" / "break" / "case" / "char" / "const" / "continue"
                         / "default" / "double" / "do" / "else" / "enum" / "extern"
                         / "float" / "for" / "goto" / "if" / "inline" / "int" / "long"
                         / "register" / "restrict" / "return" / "short" / "signed"
                         / "sizeof" / "static" / "struct" / "switch" / "typedef"
                         / "union" / "unsigned" / "void" / "volatile" / "while"
 Spacing                <~ (space / endOfLine / Comment)*
 Comment                <~ "//" (!endOfLine .)* endOfLine
 IntegerLiteral         <~ Sign? Integer IntegerSuffix?
 Integer                <~ digit+
 IntegerSuffix          <- "Lu" / "LU" / "uL" / "UL" / "L" / "u" / "U"
 FloatLiteral           <~ Sign? Integer "." Integer? (("e" / "E") Sign? Integer)?
 StringLiteral          <~ doublequote (DQChar)* doublequote
 DQChar                 <- EscapeSequence
                         / !doublequote .
 EscapeSequence         <~ backslash ( quote
                                     / doublequote
                                     / backslash
                                     / [abfnrtv]
                                     )
 Sign                   <- "-" / "+"

# Local Variables:
# mode: bison
# End:


+/
module clop.ct.parser;

public import pegged.peg;
import std.algorithm: startsWith;
import std.functional: toDelegate;

struct GenericCLOP(TParseTree)
{
    import pegged.dynamic.grammar;
    struct CLOP
    {
    enum name = "CLOP";
    static ParseTree delegate(ParseTree)[string] before;
    static ParseTree delegate(ParseTree)[string] after;
    static ParseTree delegate(ParseTree)[string] rules;

    static this()
    {
        rules["TranslationUnit"] = toDelegate(&TranslationUnit);
        rules["KernelBlock"] = toDelegate(&KernelBlock);
        rules["ExternalDeclarations"] = toDelegate(&ExternalDeclarations);
        rules["ExternalDeclaration"] = toDelegate(&ExternalDeclaration);
        rules["FunctionDefinition"] = toDelegate(&FunctionDefinition);
        rules["SyncPattern"] = toDelegate(&SyncPattern);
        rules["RangeDecl"] = toDelegate(&RangeDecl);
        rules["RangeList"] = toDelegate(&RangeList);
        rules["RangeSpec"] = toDelegate(&RangeSpec);
        rules["Transformations"] = toDelegate(&Transformations);
        rules["TransList"] = toDelegate(&TransList);
        rules["TransSpec"] = toDelegate(&TransSpec);
        rules["Declaration"] = toDelegate(&Declaration);
        rules["Declarator"] = toDelegate(&Declarator);
        rules["StorageClassSpecifier"] = toDelegate(&StorageClassSpecifier);
        rules["TypeSpecifier"] = toDelegate(&TypeSpecifier);
        rules["DeclarationSpecifiers"] = toDelegate(&DeclarationSpecifiers);
        rules["StructSpecifier"] = toDelegate(&StructSpecifier);
        rules["StructDeclarationList"] = toDelegate(&StructDeclarationList);
        rules["StructDeclaration"] = toDelegate(&StructDeclaration);
        rules["StructDeclaratorList"] = toDelegate(&StructDeclaratorList);
        rules["StructDeclarator"] = toDelegate(&StructDeclarator);
        rules["InitDeclaratorList"] = toDelegate(&InitDeclaratorList);
        rules["InitDeclarator"] = toDelegate(&InitDeclarator);
        rules["InitializerList"] = toDelegate(&InitializerList);
        rules["Initializer"] = toDelegate(&Initializer);
        rules["ParameterList"] = toDelegate(&ParameterList);
        rules["ParameterDeclaration"] = toDelegate(&ParameterDeclaration);
        rules["TypeName"] = toDelegate(&TypeName);
        rules["StatementList"] = toDelegate(&StatementList);
        rules["Statement"] = toDelegate(&Statement);
        rules["CompoundStatement"] = toDelegate(&CompoundStatement);
        rules["ExpressionStatement"] = toDelegate(&ExpressionStatement);
        rules["IfStatement"] = toDelegate(&IfStatement);
        rules["IterationStatement"] = toDelegate(&IterationStatement);
        rules["WhileStatement"] = toDelegate(&WhileStatement);
        rules["ForStatement"] = toDelegate(&ForStatement);
        rules["ReturnStatement"] = toDelegate(&ReturnStatement);
        rules["PrimaryExpr"] = toDelegate(&PrimaryExpr);
        rules["PostfixExpr"] = toDelegate(&PostfixExpr);
        rules["ArgumentExprList"] = toDelegate(&ArgumentExprList);
        rules["UnaryExpr"] = toDelegate(&UnaryExpr);
        rules["UnaryOperator"] = toDelegate(&UnaryOperator);
        rules["IncrementExpr"] = toDelegate(&IncrementExpr);
        rules["DecrementExpr"] = toDelegate(&DecrementExpr);
        rules["CastExpr"] = toDelegate(&CastExpr);
        rules["MultiplicativeExpr"] = toDelegate(&MultiplicativeExpr);
        rules["MultiplicativeOperator"] = toDelegate(&MultiplicativeOperator);
        rules["AdditiveExpr"] = toDelegate(&AdditiveExpr);
        rules["AdditiveOperator"] = toDelegate(&AdditiveOperator);
        rules["ShiftExpr"] = toDelegate(&ShiftExpr);
        rules["ShiftOperator"] = toDelegate(&ShiftOperator);
        rules["RelationalExpr"] = toDelegate(&RelationalExpr);
        rules["RelationalOperator"] = toDelegate(&RelationalOperator);
        rules["EqualityExpr"] = toDelegate(&EqualityExpr);
        rules["EqualityOperator"] = toDelegate(&EqualityOperator);
        rules["ANDExpr"] = toDelegate(&ANDExpr);
        rules["ExclusiveORExpr"] = toDelegate(&ExclusiveORExpr);
        rules["InclusiveORExpr"] = toDelegate(&InclusiveORExpr);
        rules["LogicalANDExpr"] = toDelegate(&LogicalANDExpr);
        rules["LogicalORExpr"] = toDelegate(&LogicalORExpr);
        rules["ConditionalExpr"] = toDelegate(&ConditionalExpr);
        rules["AssignmentExpr"] = toDelegate(&AssignmentExpr);
        rules["AssignmentOperator"] = toDelegate(&AssignmentOperator);
        rules["Expression"] = toDelegate(&Expression);
        rules["IdentifierList"] = toDelegate(&IdentifierList);
        rules["Identifier"] = toDelegate(&Identifier);
        rules["Keyword"] = toDelegate(&Keyword);
        rules["Spacing"] = toDelegate(&Spacing);
   }

    template hooked(alias r, string name)
    {
        static ParseTree hooked(ParseTree p)
        {
            ParseTree result;

            if (name in before)
            {
                result = before[name](p);
                if (result.successful)
                    return result;
            }

            result = r(p);
            if (result.successful || name !in after)
                return result;

            result = after[name](p);
            return result;
        }

        static ParseTree hooked(string input)
        {
            return hooked!(r, name)(ParseTree("",false,[],input));
        }
    }

    static void addRuleBefore(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar name
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(ruleName,rule; dg.rules)
            if (ruleName != "Spacing") // Keep the local Spacing rule, do not overwrite it
                rules[ruleName] = rule;
        before[parentRule] = rules[dg.startingRule];
    }

    static void addRuleAfter(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar named
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(name,rule; dg.rules)
        {
            if (name != "Spacing")
                rules[name] = rule;
        }
        after[parentRule] = rules[dg.startingRule];
    }

    static bool isRule(string s)
    {
        return s.startsWith("CLOP.");
    }
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    mixin decimateTree;
    static TParseTree TranslationUnit(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(ExternalDeclarations), KernelBlock), "CLOP.TranslationUnit")(p);
        }
        else
        {
            if(auto m = tuple(`TranslationUnit`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(ExternalDeclarations), KernelBlock), "CLOP.TranslationUnit"), "TranslationUnit")(p);
                memo[tuple(`TranslationUnit`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TranslationUnit(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(ExternalDeclarations), KernelBlock), "CLOP.TranslationUnit")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(ExternalDeclarations), KernelBlock), "CLOP.TranslationUnit"), "TranslationUnit")(TParseTree("", false,[], s));
        }
    }
    static string TranslationUnit(GetName g)
    {
        return "CLOP.TranslationUnit";
    }

    static TParseTree KernelBlock(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.and!(SyncPattern, pegged.peg.discard!(Spacing))), RangeDecl, pegged.peg.discard!(Spacing), CompoundStatement, pegged.peg.option!(pegged.peg.and!(pegged.peg.discard!(Spacing), Transformations))), "CLOP.KernelBlock")(p);
        }
        else
        {
            if(auto m = tuple(`KernelBlock`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.and!(SyncPattern, pegged.peg.discard!(Spacing))), RangeDecl, pegged.peg.discard!(Spacing), CompoundStatement, pegged.peg.option!(pegged.peg.and!(pegged.peg.discard!(Spacing), Transformations))), "CLOP.KernelBlock"), "KernelBlock")(p);
                memo[tuple(`KernelBlock`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree KernelBlock(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.and!(SyncPattern, pegged.peg.discard!(Spacing))), RangeDecl, pegged.peg.discard!(Spacing), CompoundStatement, pegged.peg.option!(pegged.peg.and!(pegged.peg.discard!(Spacing), Transformations))), "CLOP.KernelBlock")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.and!(SyncPattern, pegged.peg.discard!(Spacing))), RangeDecl, pegged.peg.discard!(Spacing), CompoundStatement, pegged.peg.option!(pegged.peg.and!(pegged.peg.discard!(Spacing), Transformations))), "CLOP.KernelBlock"), "KernelBlock")(TParseTree("", false,[], s));
        }
    }
    static string KernelBlock(GetName g)
    {
        return "CLOP.KernelBlock";
    }

    static TParseTree ExternalDeclarations(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(ExternalDeclaration, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(Spacing), ExternalDeclaration))), "CLOP.ExternalDeclarations")(p);
        }
        else
        {
            if(auto m = tuple(`ExternalDeclarations`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(ExternalDeclaration, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(Spacing), ExternalDeclaration))), "CLOP.ExternalDeclarations"), "ExternalDeclarations")(p);
                memo[tuple(`ExternalDeclarations`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExternalDeclarations(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(ExternalDeclaration, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(Spacing), ExternalDeclaration))), "CLOP.ExternalDeclarations")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(ExternalDeclaration, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(Spacing), ExternalDeclaration))), "CLOP.ExternalDeclarations"), "ExternalDeclarations")(TParseTree("", false,[], s));
        }
    }
    static string ExternalDeclarations(GetName g)
    {
        return "CLOP.ExternalDeclarations";
    }

    static TParseTree ExternalDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, FunctionDefinition, Spacing), pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), "CLOP.ExternalDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`ExternalDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, FunctionDefinition, Spacing), pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), "CLOP.ExternalDeclaration"), "ExternalDeclaration")(p);
                memo[tuple(`ExternalDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExternalDeclaration(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, FunctionDefinition, Spacing), pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), "CLOP.ExternalDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, FunctionDefinition, Spacing), pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), "CLOP.ExternalDeclaration"), "ExternalDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string ExternalDeclaration(GetName g)
    {
        return "CLOP.ExternalDeclaration";
    }

    static TParseTree FunctionDefinition(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, CompoundStatement, Spacing)), "CLOP.FunctionDefinition")(p);
        }
        else
        {
            if(auto m = tuple(`FunctionDefinition`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, CompoundStatement, Spacing)), "CLOP.FunctionDefinition"), "FunctionDefinition")(p);
                memo[tuple(`FunctionDefinition`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionDefinition(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, CompoundStatement, Spacing)), "CLOP.FunctionDefinition")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, CompoundStatement, Spacing)), "CLOP.FunctionDefinition"), "FunctionDefinition")(TParseTree("", false,[], s));
        }
    }
    static string FunctionDefinition(GetName g)
    {
        return "CLOP.FunctionDefinition";
    }

    static TParseTree SyncPattern(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "CLOP.SyncPattern")(p);
        }
        else
        {
            if(auto m = tuple(`SyncPattern`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "CLOP.SyncPattern"), "SyncPattern")(p);
                memo[tuple(`SyncPattern`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SyncPattern(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "CLOP.SyncPattern")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "CLOP.SyncPattern"), "SyncPattern")(TParseTree("", false,[], s));
        }
    }
    static string SyncPattern(GetName g)
    {
        return "CLOP.SyncPattern";
    }

    static TParseTree RangeDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("NDRange"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, RangeList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "CLOP.RangeDecl")(p);
        }
        else
        {
            if(auto m = tuple(`RangeDecl`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("NDRange"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, RangeList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "CLOP.RangeDecl"), "RangeDecl")(p);
                memo[tuple(`RangeDecl`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RangeDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("NDRange"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, RangeList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "CLOP.RangeDecl")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("NDRange"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, RangeList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "CLOP.RangeDecl"), "RangeDecl")(TParseTree("", false,[], s));
        }
    }
    static string RangeDecl(GetName g)
    {
        return "CLOP.RangeDecl";
    }

    static TParseTree RangeList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RangeSpec, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, RangeSpec, Spacing)), Spacing))), "CLOP.RangeList")(p);
        }
        else
        {
            if(auto m = tuple(`RangeList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RangeSpec, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, RangeSpec, Spacing)), Spacing))), "CLOP.RangeList"), "RangeList")(p);
                memo[tuple(`RangeList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RangeList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RangeSpec, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, RangeSpec, Spacing)), Spacing))), "CLOP.RangeList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RangeSpec, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, RangeSpec, Spacing)), Spacing))), "CLOP.RangeList"), "RangeList")(TParseTree("", false,[], s));
        }
    }
    static string RangeList(GetName g)
    {
        return "CLOP.RangeList";
    }

    static TParseTree RangeSpec(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing)), "CLOP.RangeSpec")(p);
        }
        else
        {
            if(auto m = tuple(`RangeSpec`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing)), "CLOP.RangeSpec"), "RangeSpec")(p);
                memo[tuple(`RangeSpec`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RangeSpec(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing)), "CLOP.RangeSpec")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing)), "CLOP.RangeSpec"), "RangeSpec")(TParseTree("", false,[], s));
        }
    }
    static string RangeSpec(GetName g)
    {
        return "CLOP.RangeSpec";
    }

    static TParseTree Transformations(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("apply"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TransList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "CLOP.Transformations")(p);
        }
        else
        {
            if(auto m = tuple(`Transformations`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("apply"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TransList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "CLOP.Transformations"), "Transformations")(p);
                memo[tuple(`Transformations`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Transformations(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("apply"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TransList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "CLOP.Transformations")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("apply"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TransList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "CLOP.Transformations"), "Transformations")(TParseTree("", false,[], s));
        }
    }
    static string Transformations(GetName g)
    {
        return "CLOP.Transformations";
    }

    static TParseTree TransList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TransSpec, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TransSpec, Spacing)), Spacing))), "CLOP.TransList")(p);
        }
        else
        {
            if(auto m = tuple(`TransList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TransSpec, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TransSpec, Spacing)), Spacing))), "CLOP.TransList"), "TransList")(p);
                memo[tuple(`TransList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TransList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TransSpec, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TransSpec, Spacing)), Spacing))), "CLOP.TransList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TransSpec, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TransSpec, Spacing)), Spacing))), "CLOP.TransList"), "TransList")(TParseTree("", false,[], s));
        }
    }
    static string TransList(GetName g)
    {
        return "CLOP.TransList";
    }

    static TParseTree TransSpec(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentExprList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), "CLOP.TransSpec")(p);
        }
        else
        {
            if(auto m = tuple(`TransSpec`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentExprList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), "CLOP.TransSpec"), "TransSpec")(p);
                memo[tuple(`TransSpec`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TransSpec(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentExprList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), "CLOP.TransSpec")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentExprList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), "CLOP.TransSpec"), "TransSpec")(TParseTree("", false,[], s));
        }
    }
    static string TransSpec(GetName g)
    {
        return "CLOP.TransSpec";
    }

    static TParseTree Declaration(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclarationSpecifiers, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InitDeclaratorList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.Declaration")(p);
        }
        else
        {
            if(auto m = tuple(`Declaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclarationSpecifiers, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InitDeclaratorList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.Declaration"), "Declaration")(p);
                memo[tuple(`Declaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Declaration(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclarationSpecifiers, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InitDeclaratorList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.Declaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclarationSpecifiers, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InitDeclaratorList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.Declaration"), "Declaration")(TParseTree("", false,[], s));
        }
    }
    static string Declaration(GetName g)
    {
        return "CLOP.Declaration";
    }

    static TParseTree Declarator(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing))), "CLOP.Declarator")(p);
        }
        else
        {
            if(auto m = tuple(`Declarator`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing))), "CLOP.Declarator"), "Declarator")(p);
                memo[tuple(`Declarator`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Declarator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing))), "CLOP.Declarator")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing))), "CLOP.Declarator"), "Declarator")(TParseTree("", false,[], s));
        }
    }
    static string Declarator(GetName g)
    {
        return "CLOP.Declarator";
    }

    static TParseTree StorageClassSpecifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("local"), "CLOP.StorageClassSpecifier")(p);
        }
        else
        {
            if(auto m = tuple(`StorageClassSpecifier`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!("local"), "CLOP.StorageClassSpecifier"), "StorageClassSpecifier")(p);
                memo[tuple(`StorageClassSpecifier`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StorageClassSpecifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("local"), "CLOP.StorageClassSpecifier")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("local"), "CLOP.StorageClassSpecifier"), "StorageClassSpecifier")(TParseTree("", false,[], s));
        }
    }
    static string StorageClassSpecifier(GetName g)
    {
        return "CLOP.StorageClassSpecifier";
    }

    static TParseTree TypeSpecifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("void"), pegged.peg.literal!("bool"), pegged.peg.literal!("char"), pegged.peg.literal!("uchar"), pegged.peg.literal!("short"), pegged.peg.literal!("ushort"), pegged.peg.literal!("int"), pegged.peg.literal!("uint"), pegged.peg.literal!("long"), pegged.peg.literal!("ulong"), pegged.peg.literal!("float"), pegged.peg.literal!("double"), pegged.peg.literal!("half"), pegged.peg.literal!("size_t"), StructSpecifier), "CLOP.TypeSpecifier")(p);
        }
        else
        {
            if(auto m = tuple(`TypeSpecifier`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("void"), pegged.peg.literal!("bool"), pegged.peg.literal!("char"), pegged.peg.literal!("uchar"), pegged.peg.literal!("short"), pegged.peg.literal!("ushort"), pegged.peg.literal!("int"), pegged.peg.literal!("uint"), pegged.peg.literal!("long"), pegged.peg.literal!("ulong"), pegged.peg.literal!("float"), pegged.peg.literal!("double"), pegged.peg.literal!("half"), pegged.peg.literal!("size_t"), StructSpecifier), "CLOP.TypeSpecifier"), "TypeSpecifier")(p);
                memo[tuple(`TypeSpecifier`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TypeSpecifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("void"), pegged.peg.literal!("bool"), pegged.peg.literal!("char"), pegged.peg.literal!("uchar"), pegged.peg.literal!("short"), pegged.peg.literal!("ushort"), pegged.peg.literal!("int"), pegged.peg.literal!("uint"), pegged.peg.literal!("long"), pegged.peg.literal!("ulong"), pegged.peg.literal!("float"), pegged.peg.literal!("double"), pegged.peg.literal!("half"), pegged.peg.literal!("size_t"), StructSpecifier), "CLOP.TypeSpecifier")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("void"), pegged.peg.literal!("bool"), pegged.peg.literal!("char"), pegged.peg.literal!("uchar"), pegged.peg.literal!("short"), pegged.peg.literal!("ushort"), pegged.peg.literal!("int"), pegged.peg.literal!("uint"), pegged.peg.literal!("long"), pegged.peg.literal!("ulong"), pegged.peg.literal!("float"), pegged.peg.literal!("double"), pegged.peg.literal!("half"), pegged.peg.literal!("size_t"), StructSpecifier), "CLOP.TypeSpecifier"), "TypeSpecifier")(TParseTree("", false,[], s));
        }
    }
    static string TypeSpecifier(GetName g)
    {
        return "CLOP.TypeSpecifier";
    }

    static TParseTree DeclarationSpecifiers(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, StorageClassSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclarationSpecifiers, Spacing))), "CLOP.DeclarationSpecifiers")(p);
        }
        else
        {
            if(auto m = tuple(`DeclarationSpecifiers`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, StorageClassSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclarationSpecifiers, Spacing))), "CLOP.DeclarationSpecifiers"), "DeclarationSpecifiers")(p);
                memo[tuple(`DeclarationSpecifiers`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DeclarationSpecifiers(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, StorageClassSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclarationSpecifiers, Spacing))), "CLOP.DeclarationSpecifiers")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, StorageClassSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclarationSpecifiers, Spacing))), "CLOP.DeclarationSpecifiers"), "DeclarationSpecifiers")(TParseTree("", false,[], s));
        }
    }
    static string DeclarationSpecifiers(GetName g)
    {
        return "CLOP.DeclarationSpecifiers";
    }

    static TParseTree StructSpecifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StructDeclarationList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StructDeclarationList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), Spacing)), "CLOP.StructSpecifier")(p);
        }
        else
        {
            if(auto m = tuple(`StructSpecifier`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StructDeclarationList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StructDeclarationList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), Spacing)), "CLOP.StructSpecifier"), "StructSpecifier")(p);
                memo[tuple(`StructSpecifier`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructSpecifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StructDeclarationList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StructDeclarationList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), Spacing)), "CLOP.StructSpecifier")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StructDeclarationList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StructDeclarationList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), Spacing)), "CLOP.StructSpecifier"), "StructSpecifier")(TParseTree("", false,[], s));
        }
    }
    static string StructSpecifier(GetName g)
    {
        return "CLOP.StructSpecifier";
    }

    static TParseTree StructDeclarationList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructDeclaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, StructDeclaration, Spacing)), Spacing))), "CLOP.StructDeclarationList")(p);
        }
        else
        {
            if(auto m = tuple(`StructDeclarationList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructDeclaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, StructDeclaration, Spacing)), Spacing))), "CLOP.StructDeclarationList"), "StructDeclarationList")(p);
                memo[tuple(`StructDeclarationList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructDeclarationList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructDeclaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, StructDeclaration, Spacing)), Spacing))), "CLOP.StructDeclarationList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructDeclaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, StructDeclaration, Spacing)), Spacing))), "CLOP.StructDeclarationList"), "StructDeclarationList")(TParseTree("", false,[], s));
        }
    }
    static string StructDeclarationList(GetName g)
    {
        return "CLOP.StructDeclarationList";
    }

    static TParseTree StructDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, StructDeclaratorList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.StructDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`StructDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, StructDeclaratorList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.StructDeclaration"), "StructDeclaration")(p);
                memo[tuple(`StructDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructDeclaration(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, StructDeclaratorList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.StructDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, StructDeclaratorList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.StructDeclaration"), "StructDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string StructDeclaration(GetName g)
    {
        return "CLOP.StructDeclaration";
    }

    static TParseTree StructDeclaratorList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructDeclarator, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, StructDeclarator, Spacing)), Spacing))), "CLOP.StructDeclaratorList")(p);
        }
        else
        {
            if(auto m = tuple(`StructDeclaratorList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructDeclarator, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, StructDeclarator, Spacing)), Spacing))), "CLOP.StructDeclaratorList"), "StructDeclaratorList")(p);
                memo[tuple(`StructDeclaratorList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructDeclaratorList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructDeclarator, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, StructDeclarator, Spacing)), Spacing))), "CLOP.StructDeclaratorList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructDeclarator, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, StructDeclarator, Spacing)), Spacing))), "CLOP.StructDeclaratorList"), "StructDeclaratorList")(TParseTree("", false,[], s));
        }
    }
    static string StructDeclaratorList(GetName g)
    {
        return "CLOP.StructDeclaratorList";
    }

    static TParseTree StructDeclarator(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing))), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing)), Spacing), "CLOP.StructDeclarator")(p);
        }
        else
        {
            if(auto m = tuple(`StructDeclarator`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing))), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing)), Spacing), "CLOP.StructDeclarator"), "StructDeclarator")(p);
                memo[tuple(`StructDeclarator`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructDeclarator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing))), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing)), Spacing), "CLOP.StructDeclarator")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing))), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing)), Spacing), "CLOP.StructDeclarator"), "StructDeclarator")(TParseTree("", false,[], s));
        }
    }
    static string StructDeclarator(GetName g)
    {
        return "CLOP.StructDeclarator";
    }

    static TParseTree InitDeclaratorList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InitDeclarator, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, InitDeclarator, Spacing)), Spacing))), "CLOP.InitDeclaratorList")(p);
        }
        else
        {
            if(auto m = tuple(`InitDeclaratorList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InitDeclarator, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, InitDeclarator, Spacing)), Spacing))), "CLOP.InitDeclaratorList"), "InitDeclaratorList")(p);
                memo[tuple(`InitDeclaratorList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree InitDeclaratorList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InitDeclarator, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, InitDeclarator, Spacing)), Spacing))), "CLOP.InitDeclaratorList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InitDeclarator, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, InitDeclarator, Spacing)), Spacing))), "CLOP.InitDeclaratorList"), "InitDeclaratorList")(TParseTree("", false,[], s));
        }
    }
    static string InitDeclaratorList(GetName g)
    {
        return "CLOP.InitDeclaratorList";
    }

    static TParseTree InitDeclarator(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing)), Spacing))), "CLOP.InitDeclarator")(p);
        }
        else
        {
            if(auto m = tuple(`InitDeclarator`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing)), Spacing))), "CLOP.InitDeclarator"), "InitDeclarator")(p);
                memo[tuple(`InitDeclarator`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree InitDeclarator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing)), Spacing))), "CLOP.InitDeclarator")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing)), Spacing))), "CLOP.InitDeclarator"), "InitDeclarator")(TParseTree("", false,[], s));
        }
    }
    static string InitDeclarator(GetName g)
    {
        return "CLOP.InitDeclarator";
    }

    static TParseTree InitializerList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Initializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing)), Spacing))), "CLOP.InitializerList")(p);
        }
        else
        {
            if(auto m = tuple(`InitializerList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Initializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing)), Spacing))), "CLOP.InitializerList"), "InitializerList")(p);
                memo[tuple(`InitializerList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree InitializerList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Initializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing)), Spacing))), "CLOP.InitializerList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Initializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing)), Spacing))), "CLOP.InitializerList"), "InitializerList")(TParseTree("", false,[], s));
        }
    }
    static string InitializerList(GetName g)
    {
        return "CLOP.InitializerList";
    }

    static TParseTree Initializer(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, InitializerList, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "CLOP.Initializer")(p);
        }
        else
        {
            if(auto m = tuple(`Initializer`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, InitializerList, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "CLOP.Initializer"), "Initializer")(p);
                memo[tuple(`Initializer`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Initializer(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, InitializerList, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "CLOP.Initializer")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, InitializerList, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "CLOP.Initializer"), "Initializer")(TParseTree("", false,[], s));
        }
    }
    static string Initializer(GetName g)
    {
        return "CLOP.Initializer";
    }

    static TParseTree ParameterList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ParameterDeclaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ParameterDeclaration, Spacing)), Spacing))), "CLOP.ParameterList")(p);
        }
        else
        {
            if(auto m = tuple(`ParameterList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ParameterDeclaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ParameterDeclaration, Spacing)), Spacing))), "CLOP.ParameterList"), "ParameterList")(p);
                memo[tuple(`ParameterList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ParameterList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ParameterDeclaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ParameterDeclaration, Spacing)), Spacing))), "CLOP.ParameterList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ParameterDeclaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ParameterDeclaration, Spacing)), Spacing))), "CLOP.ParameterList"), "ParameterList")(TParseTree("", false,[], s));
        }
    }
    static string ParameterList(GetName g)
    {
        return "CLOP.ParameterList";
    }

    static TParseTree ParameterDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), "CLOP.ParameterDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`ParameterDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), "CLOP.ParameterDeclaration"), "ParameterDeclaration")(p);
                memo[tuple(`ParameterDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ParameterDeclaration(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), "CLOP.ParameterDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), "CLOP.ParameterDeclaration"), "ParameterDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string ParameterDeclaration(GetName g)
    {
        return "CLOP.ParameterDeclaration";
    }

    static TParseTree TypeName(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), Spacing))), "CLOP.TypeName")(p);
        }
        else
        {
            if(auto m = tuple(`TypeName`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), Spacing))), "CLOP.TypeName"), "TypeName")(p);
                memo[tuple(`TypeName`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TypeName(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), Spacing))), "CLOP.TypeName")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), Spacing))), "CLOP.TypeName"), "TypeName")(TParseTree("", false,[], s));
        }
    }
    static string TypeName(GetName g)
    {
        return "CLOP.TypeName";
    }

    static TParseTree StatementList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Statement, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(Spacing), Statement))), "CLOP.StatementList")(p);
        }
        else
        {
            if(auto m = tuple(`StatementList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Statement, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(Spacing), Statement))), "CLOP.StatementList"), "StatementList")(p);
                memo[tuple(`StatementList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StatementList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Statement, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(Spacing), Statement))), "CLOP.StatementList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Statement, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(Spacing), Statement))), "CLOP.StatementList"), "StatementList")(TParseTree("", false,[], s));
        }
    }
    static string StatementList(GetName g)
    {
        return "CLOP.StatementList";
    }

    static TParseTree Statement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CompoundStatement, Spacing), pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing), pegged.peg.wrapAround!(Spacing, IfStatement, Spacing), pegged.peg.wrapAround!(Spacing, IterationStatement, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing), pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), "CLOP.Statement")(p);
        }
        else
        {
            if(auto m = tuple(`Statement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CompoundStatement, Spacing), pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing), pegged.peg.wrapAround!(Spacing, IfStatement, Spacing), pegged.peg.wrapAround!(Spacing, IterationStatement, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing), pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), "CLOP.Statement"), "Statement")(p);
                memo[tuple(`Statement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Statement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CompoundStatement, Spacing), pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing), pegged.peg.wrapAround!(Spacing, IfStatement, Spacing), pegged.peg.wrapAround!(Spacing, IterationStatement, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing), pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), "CLOP.Statement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CompoundStatement, Spacing), pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing), pegged.peg.wrapAround!(Spacing, IfStatement, Spacing), pegged.peg.wrapAround!(Spacing, IterationStatement, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing), pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), "CLOP.Statement"), "Statement")(TParseTree("", false,[], s));
        }
    }
    static string Statement(GetName g)
    {
        return "CLOP.Statement";
    }

    static TParseTree CompoundStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StatementList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "CLOP.CompoundStatement")(p);
        }
        else
        {
            if(auto m = tuple(`CompoundStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StatementList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "CLOP.CompoundStatement"), "CompoundStatement")(p);
                memo[tuple(`CompoundStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CompoundStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StatementList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "CLOP.CompoundStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StatementList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "CLOP.CompoundStatement"), "CompoundStatement")(TParseTree("", false,[], s));
        }
    }
    static string CompoundStatement(GetName g)
    {
        return "CLOP.CompoundStatement";
    }

    static TParseTree ExpressionStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.ExpressionStatement")(p);
        }
        else
        {
            if(auto m = tuple(`ExpressionStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.ExpressionStatement"), "ExpressionStatement")(p);
                memo[tuple(`ExpressionStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExpressionStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.ExpressionStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.ExpressionStatement"), "ExpressionStatement")(TParseTree("", false,[], s));
        }
    }
    static string ExpressionStatement(GetName g)
    {
        return "CLOP.ExpressionStatement";
    }

    static TParseTree IfStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), Spacing))), "CLOP.IfStatement")(p);
        }
        else
        {
            if(auto m = tuple(`IfStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), Spacing))), "CLOP.IfStatement"), "IfStatement")(p);
                memo[tuple(`IfStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IfStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), Spacing))), "CLOP.IfStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), Spacing))), "CLOP.IfStatement"), "IfStatement")(TParseTree("", false,[], s));
        }
    }
    static string IfStatement(GetName g)
    {
        return "CLOP.IfStatement";
    }

    static TParseTree IterationStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, WhileStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForStatement, Spacing)), "CLOP.IterationStatement")(p);
        }
        else
        {
            if(auto m = tuple(`IterationStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, WhileStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForStatement, Spacing)), "CLOP.IterationStatement"), "IterationStatement")(p);
                memo[tuple(`IterationStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IterationStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, WhileStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForStatement, Spacing)), "CLOP.IterationStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, WhileStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForStatement, Spacing)), "CLOP.IterationStatement"), "IterationStatement")(TParseTree("", false,[], s));
        }
    }
    static string IterationStatement(GetName g)
    {
        return "CLOP.IterationStatement";
    }

    static TParseTree WhileStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "CLOP.WhileStatement")(p);
        }
        else
        {
            if(auto m = tuple(`WhileStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "CLOP.WhileStatement"), "WhileStatement")(p);
                memo[tuple(`WhileStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree WhileStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "CLOP.WhileStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "CLOP.WhileStatement"), "WhileStatement")(TParseTree("", false,[], s));
        }
    }
    static string WhileStatement(GetName g)
    {
        return "CLOP.WhileStatement";
    }

    static TParseTree ForStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "CLOP.ForStatement")(p);
        }
        else
        {
            if(auto m = tuple(`ForStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "CLOP.ForStatement"), "ForStatement")(p);
                memo[tuple(`ForStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ForStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "CLOP.ForStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "CLOP.ForStatement"), "ForStatement")(TParseTree("", false,[], s));
        }
    }
    static string ForStatement(GetName g)
    {
        return "CLOP.ForStatement";
    }

    static TParseTree ReturnStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.ReturnStatement")(p);
        }
        else
        {
            if(auto m = tuple(`ReturnStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.ReturnStatement"), "ReturnStatement")(p);
                memo[tuple(`ReturnStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReturnStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.ReturnStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.ReturnStatement"), "ReturnStatement")(TParseTree("", false,[], s));
        }
    }
    static string ReturnStatement(GetName g)
    {
        return "CLOP.ReturnStatement";
    }

    static TParseTree PrimaryExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing)), Spacing))), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "CLOP.PrimaryExpr")(p);
        }
        else
        {
            if(auto m = tuple(`PrimaryExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing)), Spacing))), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "CLOP.PrimaryExpr"), "PrimaryExpr")(p);
                memo[tuple(`PrimaryExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PrimaryExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing)), Spacing))), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "CLOP.PrimaryExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing)), Spacing))), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "CLOP.PrimaryExpr"), "PrimaryExpr")(TParseTree("", false,[], s));
        }
    }
    static string PrimaryExpr(GetName g)
    {
        return "CLOP.PrimaryExpr";
    }

    static TParseTree PostfixExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimaryExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentExprList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing)), Spacing))), "CLOP.PostfixExpr")(p);
        }
        else
        {
            if(auto m = tuple(`PostfixExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimaryExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentExprList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing)), Spacing))), "CLOP.PostfixExpr"), "PostfixExpr")(p);
                memo[tuple(`PostfixExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PostfixExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimaryExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentExprList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing)), Spacing))), "CLOP.PostfixExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimaryExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentExprList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing)), Spacing))), "CLOP.PostfixExpr"), "PostfixExpr")(TParseTree("", false,[], s));
        }
    }
    static string PostfixExpr(GetName g)
    {
        return "CLOP.PostfixExpr";
    }

    static TParseTree ArgumentExprList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing)), Spacing))), "CLOP.ArgumentExprList")(p);
        }
        else
        {
            if(auto m = tuple(`ArgumentExprList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing)), Spacing))), "CLOP.ArgumentExprList"), "ArgumentExprList")(p);
                memo[tuple(`ArgumentExprList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArgumentExprList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing)), Spacing))), "CLOP.ArgumentExprList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing)), Spacing))), "CLOP.ArgumentExprList"), "ArgumentExprList")(TParseTree("", false,[], s));
        }
    }
    static string ArgumentExprList(GetName g)
    {
        return "CLOP.ArgumentExprList";
    }

    static TParseTree UnaryExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, PostfixExpr, Spacing), pegged.peg.wrapAround!(Spacing, IncrementExpr, Spacing), pegged.peg.wrapAround!(Spacing, DecrementExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryOperator, Spacing), pegged.peg.wrapAround!(Spacing, CastExpr, Spacing))), "CLOP.UnaryExpr")(p);
        }
        else
        {
            if(auto m = tuple(`UnaryExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, PostfixExpr, Spacing), pegged.peg.wrapAround!(Spacing, IncrementExpr, Spacing), pegged.peg.wrapAround!(Spacing, DecrementExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryOperator, Spacing), pegged.peg.wrapAround!(Spacing, CastExpr, Spacing))), "CLOP.UnaryExpr"), "UnaryExpr")(p);
                memo[tuple(`UnaryExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UnaryExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, PostfixExpr, Spacing), pegged.peg.wrapAround!(Spacing, IncrementExpr, Spacing), pegged.peg.wrapAround!(Spacing, DecrementExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryOperator, Spacing), pegged.peg.wrapAround!(Spacing, CastExpr, Spacing))), "CLOP.UnaryExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, PostfixExpr, Spacing), pegged.peg.wrapAround!(Spacing, IncrementExpr, Spacing), pegged.peg.wrapAround!(Spacing, DecrementExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryOperator, Spacing), pegged.peg.wrapAround!(Spacing, CastExpr, Spacing))), "CLOP.UnaryExpr"), "UnaryExpr")(TParseTree("", false,[], s));
        }
    }
    static string UnaryExpr(GetName g)
    {
        return "CLOP.UnaryExpr";
    }

    static TParseTree UnaryOperator(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+"), pegged.peg.literal!("~"), pegged.peg.literal!("!")), "CLOP.UnaryOperator")(p);
        }
        else
        {
            if(auto m = tuple(`UnaryOperator`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+"), pegged.peg.literal!("~"), pegged.peg.literal!("!")), "CLOP.UnaryOperator"), "UnaryOperator")(p);
                memo[tuple(`UnaryOperator`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UnaryOperator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+"), pegged.peg.literal!("~"), pegged.peg.literal!("!")), "CLOP.UnaryOperator")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+"), pegged.peg.literal!("~"), pegged.peg.literal!("!")), "CLOP.UnaryOperator"), "UnaryOperator")(TParseTree("", false,[], s));
        }
    }
    static string UnaryOperator(GetName g)
    {
        return "CLOP.UnaryOperator";
    }

    static TParseTree IncrementExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "CLOP.IncrementExpr")(p);
        }
        else
        {
            if(auto m = tuple(`IncrementExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "CLOP.IncrementExpr"), "IncrementExpr")(p);
                memo[tuple(`IncrementExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IncrementExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "CLOP.IncrementExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "CLOP.IncrementExpr"), "IncrementExpr")(TParseTree("", false,[], s));
        }
    }
    static string IncrementExpr(GetName g)
    {
        return "CLOP.IncrementExpr";
    }

    static TParseTree DecrementExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "CLOP.DecrementExpr")(p);
        }
        else
        {
            if(auto m = tuple(`DecrementExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "CLOP.DecrementExpr"), "DecrementExpr")(p);
                memo[tuple(`DecrementExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DecrementExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "CLOP.DecrementExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "CLOP.DecrementExpr"), "DecrementExpr")(TParseTree("", false,[], s));
        }
    }
    static string DecrementExpr(GetName g)
    {
        return "CLOP.DecrementExpr";
    }

    static TParseTree CastExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TypeName, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, CastExpr, Spacing))), "CLOP.CastExpr")(p);
        }
        else
        {
            if(auto m = tuple(`CastExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TypeName, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, CastExpr, Spacing))), "CLOP.CastExpr"), "CastExpr")(p);
                memo[tuple(`CastExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CastExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TypeName, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, CastExpr, Spacing))), "CLOP.CastExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TypeName, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, CastExpr, Spacing))), "CLOP.CastExpr"), "CastExpr")(TParseTree("", false,[], s));
        }
    }
    static string CastExpr(GetName g)
    {
        return "CLOP.CastExpr";
    }

    static TParseTree MultiplicativeExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CastExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MultiplicativeOperator, Spacing), pegged.peg.wrapAround!(Spacing, MultiplicativeExpr, Spacing)), Spacing))), "CLOP.MultiplicativeExpr")(p);
        }
        else
        {
            if(auto m = tuple(`MultiplicativeExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CastExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MultiplicativeOperator, Spacing), pegged.peg.wrapAround!(Spacing, MultiplicativeExpr, Spacing)), Spacing))), "CLOP.MultiplicativeExpr"), "MultiplicativeExpr")(p);
                memo[tuple(`MultiplicativeExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MultiplicativeExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CastExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MultiplicativeOperator, Spacing), pegged.peg.wrapAround!(Spacing, MultiplicativeExpr, Spacing)), Spacing))), "CLOP.MultiplicativeExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CastExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MultiplicativeOperator, Spacing), pegged.peg.wrapAround!(Spacing, MultiplicativeExpr, Spacing)), Spacing))), "CLOP.MultiplicativeExpr"), "MultiplicativeExpr")(TParseTree("", false,[], s));
        }
    }
    static string MultiplicativeExpr(GetName g)
    {
        return "CLOP.MultiplicativeExpr";
    }

    static TParseTree MultiplicativeOperator(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("*"), pegged.peg.literal!("%"), pegged.peg.literal!("/")), "CLOP.MultiplicativeOperator")(p);
        }
        else
        {
            if(auto m = tuple(`MultiplicativeOperator`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("*"), pegged.peg.literal!("%"), pegged.peg.literal!("/")), "CLOP.MultiplicativeOperator"), "MultiplicativeOperator")(p);
                memo[tuple(`MultiplicativeOperator`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MultiplicativeOperator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("*"), pegged.peg.literal!("%"), pegged.peg.literal!("/")), "CLOP.MultiplicativeOperator")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("*"), pegged.peg.literal!("%"), pegged.peg.literal!("/")), "CLOP.MultiplicativeOperator"), "MultiplicativeOperator")(TParseTree("", false,[], s));
        }
    }
    static string MultiplicativeOperator(GetName g)
    {
        return "CLOP.MultiplicativeOperator";
    }

    static TParseTree AdditiveExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MultiplicativeExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AdditiveOperator, Spacing), pegged.peg.wrapAround!(Spacing, AdditiveExpr, Spacing)), Spacing))), "CLOP.AdditiveExpr")(p);
        }
        else
        {
            if(auto m = tuple(`AdditiveExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MultiplicativeExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AdditiveOperator, Spacing), pegged.peg.wrapAround!(Spacing, AdditiveExpr, Spacing)), Spacing))), "CLOP.AdditiveExpr"), "AdditiveExpr")(p);
                memo[tuple(`AdditiveExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AdditiveExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MultiplicativeExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AdditiveOperator, Spacing), pegged.peg.wrapAround!(Spacing, AdditiveExpr, Spacing)), Spacing))), "CLOP.AdditiveExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MultiplicativeExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AdditiveOperator, Spacing), pegged.peg.wrapAround!(Spacing, AdditiveExpr, Spacing)), Spacing))), "CLOP.AdditiveExpr"), "AdditiveExpr")(TParseTree("", false,[], s));
        }
    }
    static string AdditiveExpr(GetName g)
    {
        return "CLOP.AdditiveExpr";
    }

    static TParseTree AdditiveOperator(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+")), "CLOP.AdditiveOperator")(p);
        }
        else
        {
            if(auto m = tuple(`AdditiveOperator`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+")), "CLOP.AdditiveOperator"), "AdditiveOperator")(p);
                memo[tuple(`AdditiveOperator`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AdditiveOperator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+")), "CLOP.AdditiveOperator")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+")), "CLOP.AdditiveOperator"), "AdditiveOperator")(TParseTree("", false,[], s));
        }
    }
    static string AdditiveOperator(GetName g)
    {
        return "CLOP.AdditiveOperator";
    }

    static TParseTree ShiftExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AdditiveExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftOperator, Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpr, Spacing)), Spacing))), "CLOP.ShiftExpr")(p);
        }
        else
        {
            if(auto m = tuple(`ShiftExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AdditiveExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftOperator, Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpr, Spacing)), Spacing))), "CLOP.ShiftExpr"), "ShiftExpr")(p);
                memo[tuple(`ShiftExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ShiftExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AdditiveExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftOperator, Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpr, Spacing)), Spacing))), "CLOP.ShiftExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AdditiveExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftOperator, Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpr, Spacing)), Spacing))), "CLOP.ShiftExpr"), "ShiftExpr")(TParseTree("", false,[], s));
        }
    }
    static string ShiftExpr(GetName g)
    {
        return "CLOP.ShiftExpr";
    }

    static TParseTree ShiftOperator(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("<<", ">>"), "CLOP.ShiftOperator")(p);
        }
        else
        {
            if(auto m = tuple(`ShiftOperator`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("<<", ">>"), "CLOP.ShiftOperator"), "ShiftOperator")(p);
                memo[tuple(`ShiftOperator`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ShiftOperator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("<<", ">>"), "CLOP.ShiftOperator")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("<<", ">>"), "CLOP.ShiftOperator"), "ShiftOperator")(TParseTree("", false,[], s));
        }
    }
    static string ShiftOperator(GetName g)
    {
        return "CLOP.ShiftOperator";
    }

    static TParseTree RelationalExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelationalOperator, Spacing), pegged.peg.wrapAround!(Spacing, RelationalExpr, Spacing)), Spacing))), "CLOP.RelationalExpr")(p);
        }
        else
        {
            if(auto m = tuple(`RelationalExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelationalOperator, Spacing), pegged.peg.wrapAround!(Spacing, RelationalExpr, Spacing)), Spacing))), "CLOP.RelationalExpr"), "RelationalExpr")(p);
                memo[tuple(`RelationalExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RelationalExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelationalOperator, Spacing), pegged.peg.wrapAround!(Spacing, RelationalExpr, Spacing)), Spacing))), "CLOP.RelationalExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelationalOperator, Spacing), pegged.peg.wrapAround!(Spacing, RelationalExpr, Spacing)), Spacing))), "CLOP.RelationalExpr"), "RelationalExpr")(TParseTree("", false,[], s));
        }
    }
    static string RelationalExpr(GetName g)
    {
        return "CLOP.RelationalExpr";
    }

    static TParseTree RelationalOperator(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("<=", ">=", "<", ">"), "CLOP.RelationalOperator")(p);
        }
        else
        {
            if(auto m = tuple(`RelationalOperator`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("<=", ">=", "<", ">"), "CLOP.RelationalOperator"), "RelationalOperator")(p);
                memo[tuple(`RelationalOperator`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RelationalOperator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("<=", ">=", "<", ">"), "CLOP.RelationalOperator")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("<=", ">=", "<", ">"), "CLOP.RelationalOperator"), "RelationalOperator")(TParseTree("", false,[], s));
        }
    }
    static string RelationalOperator(GetName g)
    {
        return "CLOP.RelationalOperator";
    }

    static TParseTree EqualityExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelationalExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualityOperator, Spacing), pegged.peg.wrapAround!(Spacing, EqualityExpr, Spacing)), Spacing))), "CLOP.EqualityExpr")(p);
        }
        else
        {
            if(auto m = tuple(`EqualityExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelationalExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualityOperator, Spacing), pegged.peg.wrapAround!(Spacing, EqualityExpr, Spacing)), Spacing))), "CLOP.EqualityExpr"), "EqualityExpr")(p);
                memo[tuple(`EqualityExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EqualityExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelationalExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualityOperator, Spacing), pegged.peg.wrapAround!(Spacing, EqualityExpr, Spacing)), Spacing))), "CLOP.EqualityExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelationalExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualityOperator, Spacing), pegged.peg.wrapAround!(Spacing, EqualityExpr, Spacing)), Spacing))), "CLOP.EqualityExpr"), "EqualityExpr")(TParseTree("", false,[], s));
        }
    }
    static string EqualityExpr(GetName g)
    {
        return "CLOP.EqualityExpr";
    }

    static TParseTree EqualityOperator(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("==", "!="), "CLOP.EqualityOperator")(p);
        }
        else
        {
            if(auto m = tuple(`EqualityOperator`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("==", "!="), "CLOP.EqualityOperator"), "EqualityOperator")(p);
                memo[tuple(`EqualityOperator`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EqualityOperator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("==", "!="), "CLOP.EqualityOperator")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("==", "!="), "CLOP.EqualityOperator"), "EqualityOperator")(TParseTree("", false,[], s));
        }
    }
    static string EqualityOperator(GetName g)
    {
        return "CLOP.EqualityOperator";
    }

    static TParseTree ANDExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualityExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, ANDExpr, Spacing)), Spacing))), "CLOP.ANDExpr")(p);
        }
        else
        {
            if(auto m = tuple(`ANDExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualityExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, ANDExpr, Spacing)), Spacing))), "CLOP.ANDExpr"), "ANDExpr")(p);
                memo[tuple(`ANDExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ANDExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualityExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, ANDExpr, Spacing)), Spacing))), "CLOP.ANDExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualityExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, ANDExpr, Spacing)), Spacing))), "CLOP.ANDExpr"), "ANDExpr")(TParseTree("", false,[], s));
        }
    }
    static string ANDExpr(GetName g)
    {
        return "CLOP.ANDExpr";
    }

    static TParseTree ExclusiveORExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ANDExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, ExclusiveORExpr, Spacing)), Spacing))), "CLOP.ExclusiveORExpr")(p);
        }
        else
        {
            if(auto m = tuple(`ExclusiveORExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ANDExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, ExclusiveORExpr, Spacing)), Spacing))), "CLOP.ExclusiveORExpr"), "ExclusiveORExpr")(p);
                memo[tuple(`ExclusiveORExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExclusiveORExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ANDExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, ExclusiveORExpr, Spacing)), Spacing))), "CLOP.ExclusiveORExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ANDExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, ExclusiveORExpr, Spacing)), Spacing))), "CLOP.ExclusiveORExpr"), "ExclusiveORExpr")(TParseTree("", false,[], s));
        }
    }
    static string ExclusiveORExpr(GetName g)
    {
        return "CLOP.ExclusiveORExpr";
    }

    static TParseTree InclusiveORExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExclusiveORExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, InclusiveORExpr, Spacing)), Spacing))), "CLOP.InclusiveORExpr")(p);
        }
        else
        {
            if(auto m = tuple(`InclusiveORExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExclusiveORExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, InclusiveORExpr, Spacing)), Spacing))), "CLOP.InclusiveORExpr"), "InclusiveORExpr")(p);
                memo[tuple(`InclusiveORExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree InclusiveORExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExclusiveORExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, InclusiveORExpr, Spacing)), Spacing))), "CLOP.InclusiveORExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExclusiveORExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, InclusiveORExpr, Spacing)), Spacing))), "CLOP.InclusiveORExpr"), "InclusiveORExpr")(TParseTree("", false,[], s));
        }
    }
    static string InclusiveORExpr(GetName g)
    {
        return "CLOP.InclusiveORExpr";
    }

    static TParseTree LogicalANDExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InclusiveORExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, LogicalANDExpr, Spacing)), Spacing))), "CLOP.LogicalANDExpr")(p);
        }
        else
        {
            if(auto m = tuple(`LogicalANDExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InclusiveORExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, LogicalANDExpr, Spacing)), Spacing))), "CLOP.LogicalANDExpr"), "LogicalANDExpr")(p);
                memo[tuple(`LogicalANDExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LogicalANDExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InclusiveORExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, LogicalANDExpr, Spacing)), Spacing))), "CLOP.LogicalANDExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InclusiveORExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, LogicalANDExpr, Spacing)), Spacing))), "CLOP.LogicalANDExpr"), "LogicalANDExpr")(TParseTree("", false,[], s));
        }
    }
    static string LogicalANDExpr(GetName g)
    {
        return "CLOP.LogicalANDExpr";
    }

    static TParseTree LogicalORExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LogicalANDExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, LogicalORExpr, Spacing)), Spacing))), "CLOP.LogicalORExpr")(p);
        }
        else
        {
            if(auto m = tuple(`LogicalORExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LogicalANDExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, LogicalORExpr, Spacing)), Spacing))), "CLOP.LogicalORExpr"), "LogicalORExpr")(p);
                memo[tuple(`LogicalORExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LogicalORExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LogicalANDExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, LogicalORExpr, Spacing)), Spacing))), "CLOP.LogicalORExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LogicalANDExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, LogicalORExpr, Spacing)), Spacing))), "CLOP.LogicalORExpr"), "LogicalORExpr")(TParseTree("", false,[], s));
        }
    }
    static string LogicalORExpr(GetName g)
    {
        return "CLOP.LogicalORExpr";
    }

    static TParseTree ConditionalExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LogicalORExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing)), Spacing))), "CLOP.ConditionalExpr")(p);
        }
        else
        {
            if(auto m = tuple(`ConditionalExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LogicalORExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing)), Spacing))), "CLOP.ConditionalExpr"), "ConditionalExpr")(p);
                memo[tuple(`ConditionalExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ConditionalExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LogicalORExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing)), Spacing))), "CLOP.ConditionalExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LogicalORExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing)), Spacing))), "CLOP.ConditionalExpr"), "ConditionalExpr")(TParseTree("", false,[], s));
        }
    }
    static string ConditionalExpr(GetName g)
    {
        return "CLOP.ConditionalExpr";
    }

    static TParseTree AssignmentExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.wrapAround!(Spacing, AssignmentOperator, Spacing), pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing)), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing)), "CLOP.AssignmentExpr")(p);
        }
        else
        {
            if(auto m = tuple(`AssignmentExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.wrapAround!(Spacing, AssignmentOperator, Spacing), pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing)), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing)), "CLOP.AssignmentExpr"), "AssignmentExpr")(p);
                memo[tuple(`AssignmentExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AssignmentExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.wrapAround!(Spacing, AssignmentOperator, Spacing), pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing)), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing)), "CLOP.AssignmentExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.wrapAround!(Spacing, AssignmentOperator, Spacing), pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing)), pegged.peg.wrapAround!(Spacing, ConditionalExpr, Spacing)), "CLOP.AssignmentExpr"), "AssignmentExpr")(TParseTree("", false,[], s));
        }
    }
    static string AssignmentExpr(GetName g)
    {
        return "CLOP.AssignmentExpr";
    }

    static TParseTree AssignmentOperator(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|="), "CLOP.AssignmentOperator")(p);
        }
        else
        {
            if(auto m = tuple(`AssignmentOperator`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|="), "CLOP.AssignmentOperator"), "AssignmentOperator")(p);
                memo[tuple(`AssignmentOperator`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AssignmentOperator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|="), "CLOP.AssignmentOperator")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|="), "CLOP.AssignmentOperator"), "AssignmentOperator")(TParseTree("", false,[], s));
        }
    }
    static string AssignmentOperator(GetName g)
    {
        return "CLOP.AssignmentOperator";
    }

    static TParseTree Expression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing)), Spacing))), "CLOP.Expression")(p);
        }
        else
        {
            if(auto m = tuple(`Expression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing)), Spacing))), "CLOP.Expression"), "Expression")(p);
                memo[tuple(`Expression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Expression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing)), Spacing))), "CLOP.Expression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignmentExpr, Spacing)), Spacing))), "CLOP.Expression"), "Expression")(TParseTree("", false,[], s));
        }
    }
    static string Expression(GetName g)
    {
        return "CLOP.Expression";
    }

    static TParseTree IdentifierList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing))), "CLOP.IdentifierList")(p);
        }
        else
        {
            if(auto m = tuple(`IdentifierList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing))), "CLOP.IdentifierList"), "IdentifierList")(p);
                memo[tuple(`IdentifierList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IdentifierList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing))), "CLOP.IdentifierList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing))), "CLOP.IdentifierList"), "IdentifierList")(TParseTree("", false,[], s));
        }
    }
    static string IdentifierList(GetName g)
    {
        return "CLOP.IdentifierList";
    }

    static TParseTree Identifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.negLookahead!(Keyword), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"))))), "CLOP.Identifier")(p);
        }
        else
        {
            if(auto m = tuple(`Identifier`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.negLookahead!(Keyword), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"))))), "CLOP.Identifier"), "Identifier")(p);
                memo[tuple(`Identifier`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Identifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.negLookahead!(Keyword), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"))))), "CLOP.Identifier")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.negLookahead!(Keyword), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"))))), "CLOP.Identifier"), "Identifier")(TParseTree("", false,[], s));
        }
    }
    static string Identifier(GetName g)
    {
        return "CLOP.Identifier";
    }

    static TParseTree Keyword(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("NDRange", "apply", "auto", "break", "case", "char", "const", "continue", "default", "double", "do", "else", "enum", "extern", "float", "for", "goto", "if", "inline", "int", "long", "register", "restrict", "return", "short", "signed", "sizeof", "static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while"), "CLOP.Keyword")(p);
        }
        else
        {
            if(auto m = tuple(`Keyword`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("NDRange", "apply", "auto", "break", "case", "char", "const", "continue", "default", "double", "do", "else", "enum", "extern", "float", "for", "goto", "if", "inline", "int", "long", "register", "restrict", "return", "short", "signed", "sizeof", "static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while"), "CLOP.Keyword"), "Keyword")(p);
                memo[tuple(`Keyword`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Keyword(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("NDRange", "apply", "auto", "break", "case", "char", "const", "continue", "default", "double", "do", "else", "enum", "extern", "float", "for", "goto", "if", "inline", "int", "long", "register", "restrict", "return", "short", "signed", "sizeof", "static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while"), "CLOP.Keyword")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("NDRange", "apply", "auto", "break", "case", "char", "const", "continue", "default", "double", "do", "else", "enum", "extern", "float", "for", "goto", "if", "inline", "int", "long", "register", "restrict", "return", "short", "signed", "sizeof", "static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while"), "CLOP.Keyword"), "Keyword")(TParseTree("", false,[], s));
        }
    }
    static string Keyword(GetName g)
    {
        return "CLOP.Keyword";
    }

    static TParseTree Spacing(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(space, endOfLine, Comment))), "CLOP.Spacing")(p);
        }
        else
        {
            if(auto m = tuple(`Spacing`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(space, endOfLine, Comment))), "CLOP.Spacing"), "Spacing")(p);
                memo[tuple(`Spacing`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Spacing(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(space, endOfLine, Comment))), "CLOP.Spacing")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(space, endOfLine, Comment))), "CLOP.Spacing"), "Spacing")(TParseTree("", false,[], s));
        }
    }
    static string Spacing(GetName g)
    {
        return "CLOP.Spacing";
    }

    static TParseTree Comment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine)), "CLOP.Comment")(p);
        }
        else
        {
            if(auto m = tuple(`Comment`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine)), "CLOP.Comment"), "Comment")(p);
                memo[tuple(`Comment`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Comment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine)), "CLOP.Comment")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine)), "CLOP.Comment"), "Comment")(TParseTree("", false,[], s));
        }
    }
    static string Comment(GetName g)
    {
        return "CLOP.Comment";
    }

    static TParseTree IntegerLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.option!(IntegerSuffix))), "CLOP.IntegerLiteral")(p);
        }
        else
        {
            if(auto m = tuple(`IntegerLiteral`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.option!(IntegerSuffix))), "CLOP.IntegerLiteral"), "IntegerLiteral")(p);
                memo[tuple(`IntegerLiteral`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IntegerLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.option!(IntegerSuffix))), "CLOP.IntegerLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.option!(IntegerSuffix))), "CLOP.IntegerLiteral"), "IntegerLiteral")(TParseTree("", false,[], s));
        }
    }
    static string IntegerLiteral(GetName g)
    {
        return "CLOP.IntegerLiteral";
    }

    static TParseTree Integer(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(digit)), "CLOP.Integer")(p);
        }
        else
        {
            if(auto m = tuple(`Integer`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(digit)), "CLOP.Integer"), "Integer")(p);
                memo[tuple(`Integer`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Integer(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(digit)), "CLOP.Integer")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(digit)), "CLOP.Integer"), "Integer")(TParseTree("", false,[], s));
        }
    }
    static string Integer(GetName g)
    {
        return "CLOP.Integer";
    }

    static TParseTree IntegerSuffix(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("Lu", "LU", "uL", "UL", "L", "u", "U"), "CLOP.IntegerSuffix")(p);
        }
        else
        {
            if(auto m = tuple(`IntegerSuffix`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("Lu", "LU", "uL", "UL", "L", "u", "U"), "CLOP.IntegerSuffix"), "IntegerSuffix")(p);
                memo[tuple(`IntegerSuffix`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IntegerSuffix(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("Lu", "LU", "uL", "UL", "L", "u", "U"), "CLOP.IntegerSuffix")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("Lu", "LU", "uL", "UL", "L", "u", "U"), "CLOP.IntegerSuffix"), "IntegerSuffix")(TParseTree("", false,[], s));
        }
    }
    static string IntegerSuffix(GetName g)
    {
        return "CLOP.IntegerSuffix";
    }

    static TParseTree FloatLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), pegged.peg.option!(Integer), pegged.peg.option!(pegged.peg.and!(pegged.peg.keywords!("e", "E"), pegged.peg.option!(Sign), Integer)))), "CLOP.FloatLiteral")(p);
        }
        else
        {
            if(auto m = tuple(`FloatLiteral`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), pegged.peg.option!(Integer), pegged.peg.option!(pegged.peg.and!(pegged.peg.keywords!("e", "E"), pegged.peg.option!(Sign), Integer)))), "CLOP.FloatLiteral"), "FloatLiteral")(p);
                memo[tuple(`FloatLiteral`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FloatLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), pegged.peg.option!(Integer), pegged.peg.option!(pegged.peg.and!(pegged.peg.keywords!("e", "E"), pegged.peg.option!(Sign), Integer)))), "CLOP.FloatLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), pegged.peg.option!(Integer), pegged.peg.option!(pegged.peg.and!(pegged.peg.keywords!("e", "E"), pegged.peg.option!(Sign), Integer)))), "CLOP.FloatLiteral"), "FloatLiteral")(TParseTree("", false,[], s));
        }
    }
    static string FloatLiteral(GetName g)
    {
        return "CLOP.FloatLiteral";
    }

    static TParseTree StringLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(DQChar), doublequote)), "CLOP.StringLiteral")(p);
        }
        else
        {
            if(auto m = tuple(`StringLiteral`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(DQChar), doublequote)), "CLOP.StringLiteral"), "StringLiteral")(p);
                memo[tuple(`StringLiteral`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StringLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(DQChar), doublequote)), "CLOP.StringLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(DQChar), doublequote)), "CLOP.StringLiteral"), "StringLiteral")(TParseTree("", false,[], s));
        }
    }
    static string StringLiteral(GetName g)
    {
        return "CLOP.StringLiteral";
    }

    static TParseTree DQChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(EscapeSequence, pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "CLOP.DQChar")(p);
        }
        else
        {
            if(auto m = tuple(`DQChar`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(EscapeSequence, pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "CLOP.DQChar"), "DQChar")(p);
                memo[tuple(`DQChar`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DQChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(EscapeSequence, pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "CLOP.DQChar")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(EscapeSequence, pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "CLOP.DQChar"), "DQChar")(TParseTree("", false,[], s));
        }
    }
    static string DQChar(GetName g)
    {
        return "CLOP.DQChar";
    }

    static TParseTree EscapeSequence(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backslash, pegged.peg.or!(pegged.peg.literal!("a"), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v"))))), "CLOP.EscapeSequence")(p);
        }
        else
        {
            if(auto m = tuple(`EscapeSequence`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backslash, pegged.peg.or!(pegged.peg.literal!("a"), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v"))))), "CLOP.EscapeSequence"), "EscapeSequence")(p);
                memo[tuple(`EscapeSequence`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EscapeSequence(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backslash, pegged.peg.or!(pegged.peg.literal!("a"), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v"))))), "CLOP.EscapeSequence")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backslash, pegged.peg.or!(pegged.peg.literal!("a"), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v"))))), "CLOP.EscapeSequence"), "EscapeSequence")(TParseTree("", false,[], s));
        }
    }
    static string EscapeSequence(GetName g)
    {
        return "CLOP.EscapeSequence";
    }

    static TParseTree Sign(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("-", "+"), "CLOP.Sign")(p);
        }
        else
        {
            if(auto m = tuple(`Sign`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("-", "+"), "CLOP.Sign"), "Sign")(p);
                memo[tuple(`Sign`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Sign(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("-", "+"), "CLOP.Sign")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("-", "+"), "CLOP.Sign"), "Sign")(TParseTree("", false,[], s));
        }
    }
    static string Sign(GetName g)
    {
        return "CLOP.Sign";
    }

    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(TranslationUnit(p));
        result.children = [result];
        result.name = "CLOP";
        return result;
    }

    static TParseTree opCall(string input)
    {
        if(__ctfe)
        {
            return CLOP(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            memo = null;
            return CLOP(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "CLOP";
    }

    }
}

alias GenericCLOP!(ParseTree).CLOP CLOP;

