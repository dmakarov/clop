/++
This module was automatically generated from the following grammar:


CLOP:

TranslationUnit        <  ExternalDeclarations?
                          (SyncPattern :Spacing)?
                          RangeDecl :Spacing CompoundStatement
                          (:Spacing Transformations)?
ExternalDeclarations   <  ExternalDeclaration (:Spacing ExternalDeclaration)*
ExternalDeclaration    <  FunctionDefinition / Declaration
FunctionDefinition     <  TypeSpecifier Declarator CompoundStatement

DeclarationList        <  Declaration (:Spacing Declaration)*
Declaration            <  TypeSpecifier InitDeclaratorList? ';'
Declarator             <  (Identifier / '(' Declarator ')') ( '[' ']'
                                                            / '[' ConstantExpression ']'
                                                            / '(' ')'
                                                            / '(' ParameterList ')'
                                                            / '(' IdentifierList ')'
                                                            )*
TypeSpecifier          <- "void" / "char" / "short" / "int" / "long" / "float" / "double"
                       /  StructSpecifier
ParameterList          <  ParameterDeclaration (',' ParameterDeclaration)*
ParameterDeclaration   <  TypeSpecifier Declarator
IdentifierList         <  Identifier (',' Identifier)*
StructSpecifier        <  "struct" ( Identifier ('{' StructDeclarationList '}')?
                                   / '{' StructDeclarationList '}' )
StructDeclarationList  <  StructDeclaration (:Spacing StructDeclaration)*
StructDeclaration      <  TypeSpecifier StructDeclaratorList ';'
StructDeclaratorList   <  StructDeclarator (',' StructDeclarator)*
StructDeclarator       <  ( Declarator ConstantExpression? / ConstantExpression )
InitDeclaratorList     <  InitDeclarator (',' InitDeclarator)*
InitDeclarator         <  Declarator ('=' Initializer)?
Initializer            <  AssignExpr / '{' InitializerList ','? '}'
InitializerList        <  Initializer (',' Initializer)*

SyncPattern            <  "Antidiagonal" / "Horizontal" / "Stencil"
RangeDecl              <  "NDRange" '(' RangeList ')'
RangeList              <  RangeSpec ( ',' RangeSpec )*
RangeSpec              <  Identifier ':' Expression ".." Expression

Transformations        < "apply" '(' TransList ')'
TransList              < TransSpec ( ',' TransSpec )*
TransSpec              < Identifier ( '(' ')' / '(' ArgumentList ')' )

PrimaryExpr            <  Identifier / FloatLiteral / IntegerLiteral / '(' Expression ')'
UnaryExpr              <  PrimaryExpr ( ArrayIndex / FunctionCall )?
ArrayIndex             <  '[' ArgumentList ']'
FunctionCall           <  '(' ')' / '(' ArgumentList ')'
ArgumentList           <  Expression ( ',' Expression )*

MulExpr                <  [*/] UnaryExpr
Factor                 <  UnaryExpr MulExpr*
AddExpr                <  [-+] Factor
Expression             <  Factor AddExpr*
AssignExpr             <  UnaryExpr '=' AssignExpr /  ConditionalExpression
RelationalExpression   <  Expression (RelationalOperator RelationalExpression)*
RelationalOperator     <  "<=" / ">=" / "<" / ">"
EqualityExpression     <  RelationalExpression (EqualityOperator EqualityExpression)*
EqualityOperator       <  "==" / "!="
ANDExpression          <  EqualityExpression ('&' ANDExpression)*
ExclusiveORExpression  <  ANDExpression ('^' ExclusiveORExpression)*
InclusiveORExpression  <  ExclusiveORExpression ('|' InclusiveORExpression)*
LogicalANDExpression   <  InclusiveORExpression ("&&" LogicalANDExpression)*
LogicalORExpression    <  LogicalANDExpression ("||" LogicalORExpression)*
ConditionalExpression  <  LogicalORExpression ('?' Expression ':' ConditionalExpression)?
ConstantExpression     <  ConditionalExpression

CompoundStatement      <  '{' '}'
                       /  '{' DeclarationList '}'
                       /  '{' StatementList '}'
                       /  '{' DeclarationList StatementList '}'
ExpressionStatement    <  AssignExpr? ';'
IfStatement            <  "if" '(' EqualityExpression ')' Statement ("else" Statement)?
WhileStatement         <  "while" '(' EqualityExpression ')' Statement
ForStatement           <  "for" '(' AssignExpr? ';' EqualityExpression? ';' AssignExpr? ')' Statement
IterationStatement     <  WhileStatement / ForStatement
ReturnStatement        <  "return" ConditionalExpression ';'
Statement              <  CompoundStatement
                       /  ExpressionStatement
                       /  IfStatement
                       /  IterationStatement
                       /  ReturnStatement
StatementList          <  Statement ( :Spacing Statement )*

Identifier             <~ !Keyword [a-zA-Z_] [a-zA-Z0-9_]*
Keyword                <- "NDRange"
Spacing                <~ (space / endOfLine / Comment)*
Comment                <~ "//" (!endOfLine .)* endOfLine
IntegerLiteral         <~ Sign? Integer IntegerSuffix?
Integer                <~ digit+
IntegerSuffix          <- "Lu" / "LU" / "uL" / "UL" / "L" / "u" / "U"
FloatLiteral           <~ Sign? Integer "." Integer? (("e" / "E") Sign? Integer)?
Sign                   <- "-" / "+"



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
        rules["ExternalDeclarations"] = toDelegate(&ExternalDeclarations);
        rules["ExternalDeclaration"] = toDelegate(&ExternalDeclaration);
        rules["FunctionDefinition"] = toDelegate(&FunctionDefinition);
        rules["DeclarationList"] = toDelegate(&DeclarationList);
        rules["Declaration"] = toDelegate(&Declaration);
        rules["Declarator"] = toDelegate(&Declarator);
        rules["TypeSpecifier"] = toDelegate(&TypeSpecifier);
        rules["ParameterList"] = toDelegate(&ParameterList);
        rules["ParameterDeclaration"] = toDelegate(&ParameterDeclaration);
        rules["IdentifierList"] = toDelegate(&IdentifierList);
        rules["StructSpecifier"] = toDelegate(&StructSpecifier);
        rules["StructDeclarationList"] = toDelegate(&StructDeclarationList);
        rules["StructDeclaration"] = toDelegate(&StructDeclaration);
        rules["StructDeclaratorList"] = toDelegate(&StructDeclaratorList);
        rules["StructDeclarator"] = toDelegate(&StructDeclarator);
        rules["InitDeclaratorList"] = toDelegate(&InitDeclaratorList);
        rules["InitDeclarator"] = toDelegate(&InitDeclarator);
        rules["Initializer"] = toDelegate(&Initializer);
        rules["InitializerList"] = toDelegate(&InitializerList);
        rules["SyncPattern"] = toDelegate(&SyncPattern);
        rules["RangeDecl"] = toDelegate(&RangeDecl);
        rules["RangeList"] = toDelegate(&RangeList);
        rules["RangeSpec"] = toDelegate(&RangeSpec);
        rules["Transformations"] = toDelegate(&Transformations);
        rules["TransList"] = toDelegate(&TransList);
        rules["TransSpec"] = toDelegate(&TransSpec);
        rules["PrimaryExpr"] = toDelegate(&PrimaryExpr);
        rules["UnaryExpr"] = toDelegate(&UnaryExpr);
        rules["ArrayIndex"] = toDelegate(&ArrayIndex);
        rules["FunctionCall"] = toDelegate(&FunctionCall);
        rules["ArgumentList"] = toDelegate(&ArgumentList);
        rules["MulExpr"] = toDelegate(&MulExpr);
        rules["Factor"] = toDelegate(&Factor);
        rules["AddExpr"] = toDelegate(&AddExpr);
        rules["Expression"] = toDelegate(&Expression);
        rules["AssignExpr"] = toDelegate(&AssignExpr);
        rules["RelationalExpression"] = toDelegate(&RelationalExpression);
        rules["RelationalOperator"] = toDelegate(&RelationalOperator);
        rules["EqualityExpression"] = toDelegate(&EqualityExpression);
        rules["EqualityOperator"] = toDelegate(&EqualityOperator);
        rules["ANDExpression"] = toDelegate(&ANDExpression);
        rules["ExclusiveORExpression"] = toDelegate(&ExclusiveORExpression);
        rules["InclusiveORExpression"] = toDelegate(&InclusiveORExpression);
        rules["LogicalANDExpression"] = toDelegate(&LogicalANDExpression);
        rules["LogicalORExpression"] = toDelegate(&LogicalORExpression);
        rules["ConditionalExpression"] = toDelegate(&ConditionalExpression);
        rules["ConstantExpression"] = toDelegate(&ConstantExpression);
        rules["CompoundStatement"] = toDelegate(&CompoundStatement);
        rules["ExpressionStatement"] = toDelegate(&ExpressionStatement);
        rules["IfStatement"] = toDelegate(&IfStatement);
        rules["WhileStatement"] = toDelegate(&WhileStatement);
        rules["ForStatement"] = toDelegate(&ForStatement);
        rules["IterationStatement"] = toDelegate(&IterationStatement);
        rules["ReturnStatement"] = toDelegate(&ReturnStatement);
        rules["Statement"] = toDelegate(&Statement);
        rules["StatementList"] = toDelegate(&StatementList);
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
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ExternalDeclarations, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SyncPattern, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, RangeDecl, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, CompoundStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, Transformations, Spacing)), Spacing))), "CLOP.TranslationUnit")(p);
        }
        else
        {
            if(auto m = tuple(`TranslationUnit`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ExternalDeclarations, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SyncPattern, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, RangeDecl, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, CompoundStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, Transformations, Spacing)), Spacing))), "CLOP.TranslationUnit"), "TranslationUnit")(p);
                memo[tuple(`TranslationUnit`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TranslationUnit(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ExternalDeclarations, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SyncPattern, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, RangeDecl, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, CompoundStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, Transformations, Spacing)), Spacing))), "CLOP.TranslationUnit")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ExternalDeclarations, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SyncPattern, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, RangeDecl, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, CompoundStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, Transformations, Spacing)), Spacing))), "CLOP.TranslationUnit"), "TranslationUnit")(TParseTree("", false,[], s));
        }
    }
    static string TranslationUnit(GetName g)
    {
        return "CLOP.TranslationUnit";
    }

    static TParseTree ExternalDeclarations(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExternalDeclaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, ExternalDeclaration, Spacing)), Spacing))), "CLOP.ExternalDeclarations")(p);
        }
        else
        {
            if(auto m = tuple(`ExternalDeclarations`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExternalDeclaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, ExternalDeclaration, Spacing)), Spacing))), "CLOP.ExternalDeclarations"), "ExternalDeclarations")(p);
                memo[tuple(`ExternalDeclarations`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExternalDeclarations(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExternalDeclaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, ExternalDeclaration, Spacing)), Spacing))), "CLOP.ExternalDeclarations")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExternalDeclaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, ExternalDeclaration, Spacing)), Spacing))), "CLOP.ExternalDeclarations"), "ExternalDeclarations")(TParseTree("", false,[], s));
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

    static TParseTree DeclarationList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), Spacing))), "CLOP.DeclarationList")(p);
        }
        else
        {
            if(auto m = tuple(`DeclarationList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), Spacing))), "CLOP.DeclarationList"), "DeclarationList")(p);
                memo[tuple(`DeclarationList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DeclarationList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), Spacing))), "CLOP.DeclarationList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), Spacing))), "CLOP.DeclarationList"), "DeclarationList")(TParseTree("", false,[], s));
        }
    }
    static string DeclarationList(GetName g)
    {
        return "CLOP.DeclarationList";
    }

    static TParseTree Declaration(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InitDeclaratorList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.Declaration")(p);
        }
        else
        {
            if(auto m = tuple(`Declaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InitDeclaratorList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.Declaration"), "Declaration")(p);
                memo[tuple(`Declaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Declaration(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InitDeclaratorList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.Declaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeSpecifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InitDeclaratorList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.Declaration"), "Declaration")(TParseTree("", false,[], s));
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
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ConstantExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing))), "CLOP.Declarator")(p);
        }
        else
        {
            if(auto m = tuple(`Declarator`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ConstantExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing))), "CLOP.Declarator"), "Declarator")(p);
                memo[tuple(`Declarator`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Declarator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ConstantExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing))), "CLOP.Declarator")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ConstantExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing))), "CLOP.Declarator"), "Declarator")(TParseTree("", false,[], s));
        }
    }
    static string Declarator(GetName g)
    {
        return "CLOP.Declarator";
    }

    static TParseTree TypeSpecifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("void"), pegged.peg.literal!("char"), pegged.peg.literal!("short"), pegged.peg.literal!("int"), pegged.peg.literal!("long"), pegged.peg.literal!("float"), pegged.peg.literal!("double"), StructSpecifier), "CLOP.TypeSpecifier")(p);
        }
        else
        {
            if(auto m = tuple(`TypeSpecifier`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("void"), pegged.peg.literal!("char"), pegged.peg.literal!("short"), pegged.peg.literal!("int"), pegged.peg.literal!("long"), pegged.peg.literal!("float"), pegged.peg.literal!("double"), StructSpecifier), "CLOP.TypeSpecifier"), "TypeSpecifier")(p);
                memo[tuple(`TypeSpecifier`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TypeSpecifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("void"), pegged.peg.literal!("char"), pegged.peg.literal!("short"), pegged.peg.literal!("int"), pegged.peg.literal!("long"), pegged.peg.literal!("float"), pegged.peg.literal!("double"), StructSpecifier), "CLOP.TypeSpecifier")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("void"), pegged.peg.literal!("char"), pegged.peg.literal!("short"), pegged.peg.literal!("int"), pegged.peg.literal!("long"), pegged.peg.literal!("float"), pegged.peg.literal!("double"), StructSpecifier), "CLOP.TypeSpecifier"), "TypeSpecifier")(TParseTree("", false,[], s));
        }
    }
    static string TypeSpecifier(GetName g)
    {
        return "CLOP.TypeSpecifier";
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
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ConstantExpression, Spacing))), pegged.peg.wrapAround!(Spacing, ConstantExpression, Spacing)), Spacing), "CLOP.StructDeclarator")(p);
        }
        else
        {
            if(auto m = tuple(`StructDeclarator`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ConstantExpression, Spacing))), pegged.peg.wrapAround!(Spacing, ConstantExpression, Spacing)), Spacing), "CLOP.StructDeclarator"), "StructDeclarator")(p);
                memo[tuple(`StructDeclarator`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructDeclarator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ConstantExpression, Spacing))), pegged.peg.wrapAround!(Spacing, ConstantExpression, Spacing)), Spacing), "CLOP.StructDeclarator")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ConstantExpression, Spacing))), pegged.peg.wrapAround!(Spacing, ConstantExpression, Spacing)), Spacing), "CLOP.StructDeclarator"), "StructDeclarator")(TParseTree("", false,[], s));
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

    static TParseTree Initializer(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, InitializerList, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "CLOP.Initializer")(p);
        }
        else
        {
            if(auto m = tuple(`Initializer`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, InitializerList, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "CLOP.Initializer"), "Initializer")(p);
                memo[tuple(`Initializer`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Initializer(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, InitializerList, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "CLOP.Initializer")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, InitializerList, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "CLOP.Initializer"), "Initializer")(TParseTree("", false,[], s));
        }
    }
    static string Initializer(GetName g)
    {
        return "CLOP.Initializer";
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

    static TParseTree SyncPattern(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Antidiagonal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Horizontal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Stencil"), Spacing)), "CLOP.SyncPattern")(p);
        }
        else
        {
            if(auto m = tuple(`SyncPattern`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Antidiagonal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Horizontal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Stencil"), Spacing)), "CLOP.SyncPattern"), "SyncPattern")(p);
                memo[tuple(`SyncPattern`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SyncPattern(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Antidiagonal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Horizontal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Stencil"), Spacing)), "CLOP.SyncPattern")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Antidiagonal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Horizontal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Stencil"), Spacing)), "CLOP.SyncPattern"), "SyncPattern")(TParseTree("", false,[], s));
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
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), "CLOP.RangeSpec")(p);
        }
        else
        {
            if(auto m = tuple(`RangeSpec`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), "CLOP.RangeSpec"), "RangeSpec")(p);
                memo[tuple(`RangeSpec`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RangeSpec(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), "CLOP.RangeSpec")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), "CLOP.RangeSpec"), "RangeSpec")(TParseTree("", false,[], s));
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
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), "CLOP.TransSpec")(p);
        }
        else
        {
            if(auto m = tuple(`TransSpec`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), "CLOP.TransSpec"), "TransSpec")(p);
                memo[tuple(`TransSpec`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TransSpec(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), "CLOP.TransSpec")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), "CLOP.TransSpec"), "TransSpec")(TParseTree("", false,[], s));
        }
    }
    static string TransSpec(GetName g)
    {
        return "CLOP.TransSpec";
    }

    static TParseTree PrimaryExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "CLOP.PrimaryExpr")(p);
        }
        else
        {
            if(auto m = tuple(`PrimaryExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "CLOP.PrimaryExpr"), "PrimaryExpr")(p);
                memo[tuple(`PrimaryExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PrimaryExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "CLOP.PrimaryExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "CLOP.PrimaryExpr"), "PrimaryExpr")(TParseTree("", false,[], s));
        }
    }
    static string PrimaryExpr(GetName g)
    {
        return "CLOP.PrimaryExpr";
    }

    static TParseTree UnaryExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimaryExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ArrayIndex, Spacing), pegged.peg.wrapAround!(Spacing, FunctionCall, Spacing)), Spacing))), "CLOP.UnaryExpr")(p);
        }
        else
        {
            if(auto m = tuple(`UnaryExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimaryExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ArrayIndex, Spacing), pegged.peg.wrapAround!(Spacing, FunctionCall, Spacing)), Spacing))), "CLOP.UnaryExpr"), "UnaryExpr")(p);
                memo[tuple(`UnaryExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UnaryExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimaryExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ArrayIndex, Spacing), pegged.peg.wrapAround!(Spacing, FunctionCall, Spacing)), Spacing))), "CLOP.UnaryExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimaryExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ArrayIndex, Spacing), pegged.peg.wrapAround!(Spacing, FunctionCall, Spacing)), Spacing))), "CLOP.UnaryExpr"), "UnaryExpr")(TParseTree("", false,[], s));
        }
    }
    static string UnaryExpr(GetName g)
    {
        return "CLOP.UnaryExpr";
    }

    static TParseTree ArrayIndex(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "CLOP.ArrayIndex")(p);
        }
        else
        {
            if(auto m = tuple(`ArrayIndex`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "CLOP.ArrayIndex"), "ArrayIndex")(p);
                memo[tuple(`ArrayIndex`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArrayIndex(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "CLOP.ArrayIndex")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "CLOP.ArrayIndex"), "ArrayIndex")(TParseTree("", false,[], s));
        }
    }
    static string ArrayIndex(GetName g)
    {
        return "CLOP.ArrayIndex";
    }

    static TParseTree FunctionCall(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "CLOP.FunctionCall")(p);
        }
        else
        {
            if(auto m = tuple(`FunctionCall`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "CLOP.FunctionCall"), "FunctionCall")(p);
                memo[tuple(`FunctionCall`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionCall(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "CLOP.FunctionCall")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "CLOP.FunctionCall"), "FunctionCall")(TParseTree("", false,[], s));
        }
    }
    static string FunctionCall(GetName g)
    {
        return "CLOP.FunctionCall";
    }

    static TParseTree ArgumentList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "CLOP.ArgumentList")(p);
        }
        else
        {
            if(auto m = tuple(`ArgumentList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "CLOP.ArgumentList"), "ArgumentList")(p);
                memo[tuple(`ArgumentList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArgumentList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "CLOP.ArgumentList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "CLOP.ArgumentList"), "ArgumentList")(TParseTree("", false,[], s));
        }
    }
    static string ArgumentList(GetName g)
    {
        return "CLOP.ArgumentList";
    }

    static TParseTree MulExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("*"), pegged.peg.literal!("/")), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "CLOP.MulExpr")(p);
        }
        else
        {
            if(auto m = tuple(`MulExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("*"), pegged.peg.literal!("/")), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "CLOP.MulExpr"), "MulExpr")(p);
                memo[tuple(`MulExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MulExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("*"), pegged.peg.literal!("/")), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "CLOP.MulExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("*"), pegged.peg.literal!("/")), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "CLOP.MulExpr"), "MulExpr")(TParseTree("", false,[], s));
        }
    }
    static string MulExpr(GetName g)
    {
        return "CLOP.MulExpr";
    }

    static TParseTree Factor(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, MulExpr, Spacing))), "CLOP.Factor")(p);
        }
        else
        {
            if(auto m = tuple(`Factor`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, MulExpr, Spacing))), "CLOP.Factor"), "Factor")(p);
                memo[tuple(`Factor`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Factor(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, MulExpr, Spacing))), "CLOP.Factor")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, MulExpr, Spacing))), "CLOP.Factor"), "Factor")(TParseTree("", false,[], s));
        }
    }
    static string Factor(GetName g)
    {
        return "CLOP.Factor";
    }

    static TParseTree AddExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+")), Spacing), pegged.peg.wrapAround!(Spacing, Factor, Spacing)), "CLOP.AddExpr")(p);
        }
        else
        {
            if(auto m = tuple(`AddExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+")), Spacing), pegged.peg.wrapAround!(Spacing, Factor, Spacing)), "CLOP.AddExpr"), "AddExpr")(p);
                memo[tuple(`AddExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AddExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+")), Spacing), pegged.peg.wrapAround!(Spacing, Factor, Spacing)), "CLOP.AddExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+")), Spacing), pegged.peg.wrapAround!(Spacing, Factor, Spacing)), "CLOP.AddExpr"), "AddExpr")(TParseTree("", false,[], s));
        }
    }
    static string AddExpr(GetName g)
    {
        return "CLOP.AddExpr";
    }

    static TParseTree Expression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Factor, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, AddExpr, Spacing))), "CLOP.Expression")(p);
        }
        else
        {
            if(auto m = tuple(`Expression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Factor, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, AddExpr, Spacing))), "CLOP.Expression"), "Expression")(p);
                memo[tuple(`Expression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Expression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Factor, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, AddExpr, Spacing))), "CLOP.Expression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Factor, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, AddExpr, Spacing))), "CLOP.Expression"), "Expression")(TParseTree("", false,[], s));
        }
    }
    static string Expression(GetName g)
    {
        return "CLOP.Expression";
    }

    static TParseTree AssignExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing)), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), "CLOP.AssignExpr")(p);
        }
        else
        {
            if(auto m = tuple(`AssignExpr`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing)), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), "CLOP.AssignExpr"), "AssignExpr")(p);
                memo[tuple(`AssignExpr`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AssignExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing)), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), "CLOP.AssignExpr")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing)), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), "CLOP.AssignExpr"), "AssignExpr")(TParseTree("", false,[], s));
        }
    }
    static string AssignExpr(GetName g)
    {
        return "CLOP.AssignExpr";
    }

    static TParseTree RelationalExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelationalOperator, Spacing), pegged.peg.wrapAround!(Spacing, RelationalExpression, Spacing)), Spacing))), "CLOP.RelationalExpression")(p);
        }
        else
        {
            if(auto m = tuple(`RelationalExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelationalOperator, Spacing), pegged.peg.wrapAround!(Spacing, RelationalExpression, Spacing)), Spacing))), "CLOP.RelationalExpression"), "RelationalExpression")(p);
                memo[tuple(`RelationalExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RelationalExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelationalOperator, Spacing), pegged.peg.wrapAround!(Spacing, RelationalExpression, Spacing)), Spacing))), "CLOP.RelationalExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelationalOperator, Spacing), pegged.peg.wrapAround!(Spacing, RelationalExpression, Spacing)), Spacing))), "CLOP.RelationalExpression"), "RelationalExpression")(TParseTree("", false,[], s));
        }
    }
    static string RelationalExpression(GetName g)
    {
        return "CLOP.RelationalExpression";
    }

    static TParseTree RelationalOperator(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "CLOP.RelationalOperator")(p);
        }
        else
        {
            if(auto m = tuple(`RelationalOperator`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "CLOP.RelationalOperator"), "RelationalOperator")(p);
                memo[tuple(`RelationalOperator`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RelationalOperator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "CLOP.RelationalOperator")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "CLOP.RelationalOperator"), "RelationalOperator")(TParseTree("", false,[], s));
        }
    }
    static string RelationalOperator(GetName g)
    {
        return "CLOP.RelationalOperator";
    }

    static TParseTree EqualityExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelationalExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualityOperator, Spacing), pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing)), Spacing))), "CLOP.EqualityExpression")(p);
        }
        else
        {
            if(auto m = tuple(`EqualityExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelationalExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualityOperator, Spacing), pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing)), Spacing))), "CLOP.EqualityExpression"), "EqualityExpression")(p);
                memo[tuple(`EqualityExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EqualityExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelationalExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualityOperator, Spacing), pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing)), Spacing))), "CLOP.EqualityExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelationalExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualityOperator, Spacing), pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing)), Spacing))), "CLOP.EqualityExpression"), "EqualityExpression")(TParseTree("", false,[], s));
        }
    }
    static string EqualityExpression(GetName g)
    {
        return "CLOP.EqualityExpression";
    }

    static TParseTree EqualityOperator(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing)), "CLOP.EqualityOperator")(p);
        }
        else
        {
            if(auto m = tuple(`EqualityOperator`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing)), "CLOP.EqualityOperator"), "EqualityOperator")(p);
                memo[tuple(`EqualityOperator`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EqualityOperator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing)), "CLOP.EqualityOperator")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing)), "CLOP.EqualityOperator"), "EqualityOperator")(TParseTree("", false,[], s));
        }
    }
    static string EqualityOperator(GetName g)
    {
        return "CLOP.EqualityOperator";
    }

    static TParseTree ANDExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, ANDExpression, Spacing)), Spacing))), "CLOP.ANDExpression")(p);
        }
        else
        {
            if(auto m = tuple(`ANDExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, ANDExpression, Spacing)), Spacing))), "CLOP.ANDExpression"), "ANDExpression")(p);
                memo[tuple(`ANDExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ANDExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, ANDExpression, Spacing)), Spacing))), "CLOP.ANDExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, ANDExpression, Spacing)), Spacing))), "CLOP.ANDExpression"), "ANDExpression")(TParseTree("", false,[], s));
        }
    }
    static string ANDExpression(GetName g)
    {
        return "CLOP.ANDExpression";
    }

    static TParseTree ExclusiveORExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ANDExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, ExclusiveORExpression, Spacing)), Spacing))), "CLOP.ExclusiveORExpression")(p);
        }
        else
        {
            if(auto m = tuple(`ExclusiveORExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ANDExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, ExclusiveORExpression, Spacing)), Spacing))), "CLOP.ExclusiveORExpression"), "ExclusiveORExpression")(p);
                memo[tuple(`ExclusiveORExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExclusiveORExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ANDExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, ExclusiveORExpression, Spacing)), Spacing))), "CLOP.ExclusiveORExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ANDExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, ExclusiveORExpression, Spacing)), Spacing))), "CLOP.ExclusiveORExpression"), "ExclusiveORExpression")(TParseTree("", false,[], s));
        }
    }
    static string ExclusiveORExpression(GetName g)
    {
        return "CLOP.ExclusiveORExpression";
    }

    static TParseTree InclusiveORExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExclusiveORExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, InclusiveORExpression, Spacing)), Spacing))), "CLOP.InclusiveORExpression")(p);
        }
        else
        {
            if(auto m = tuple(`InclusiveORExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExclusiveORExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, InclusiveORExpression, Spacing)), Spacing))), "CLOP.InclusiveORExpression"), "InclusiveORExpression")(p);
                memo[tuple(`InclusiveORExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree InclusiveORExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExclusiveORExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, InclusiveORExpression, Spacing)), Spacing))), "CLOP.InclusiveORExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExclusiveORExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, InclusiveORExpression, Spacing)), Spacing))), "CLOP.InclusiveORExpression"), "InclusiveORExpression")(TParseTree("", false,[], s));
        }
    }
    static string InclusiveORExpression(GetName g)
    {
        return "CLOP.InclusiveORExpression";
    }

    static TParseTree LogicalANDExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InclusiveORExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, LogicalANDExpression, Spacing)), Spacing))), "CLOP.LogicalANDExpression")(p);
        }
        else
        {
            if(auto m = tuple(`LogicalANDExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InclusiveORExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, LogicalANDExpression, Spacing)), Spacing))), "CLOP.LogicalANDExpression"), "LogicalANDExpression")(p);
                memo[tuple(`LogicalANDExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LogicalANDExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InclusiveORExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, LogicalANDExpression, Spacing)), Spacing))), "CLOP.LogicalANDExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InclusiveORExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, LogicalANDExpression, Spacing)), Spacing))), "CLOP.LogicalANDExpression"), "LogicalANDExpression")(TParseTree("", false,[], s));
        }
    }
    static string LogicalANDExpression(GetName g)
    {
        return "CLOP.LogicalANDExpression";
    }

    static TParseTree LogicalORExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LogicalANDExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, LogicalORExpression, Spacing)), Spacing))), "CLOP.LogicalORExpression")(p);
        }
        else
        {
            if(auto m = tuple(`LogicalORExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LogicalANDExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, LogicalORExpression, Spacing)), Spacing))), "CLOP.LogicalORExpression"), "LogicalORExpression")(p);
                memo[tuple(`LogicalORExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LogicalORExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LogicalANDExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, LogicalORExpression, Spacing)), Spacing))), "CLOP.LogicalORExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LogicalANDExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, LogicalORExpression, Spacing)), Spacing))), "CLOP.LogicalORExpression"), "LogicalORExpression")(TParseTree("", false,[], s));
        }
    }
    static string LogicalORExpression(GetName g)
    {
        return "CLOP.LogicalORExpression";
    }

    static TParseTree ConditionalExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LogicalORExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), Spacing))), "CLOP.ConditionalExpression")(p);
        }
        else
        {
            if(auto m = tuple(`ConditionalExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LogicalORExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), Spacing))), "CLOP.ConditionalExpression"), "ConditionalExpression")(p);
                memo[tuple(`ConditionalExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ConditionalExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LogicalORExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), Spacing))), "CLOP.ConditionalExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LogicalORExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), Spacing))), "CLOP.ConditionalExpression"), "ConditionalExpression")(TParseTree("", false,[], s));
        }
    }
    static string ConditionalExpression(GetName g)
    {
        return "CLOP.ConditionalExpression";
    }

    static TParseTree ConstantExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing), "CLOP.ConstantExpression")(p);
        }
        else
        {
            if(auto m = tuple(`ConstantExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing), "CLOP.ConstantExpression"), "ConstantExpression")(p);
                memo[tuple(`ConstantExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ConstantExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing), "CLOP.ConstantExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing), "CLOP.ConstantExpression"), "ConstantExpression")(TParseTree("", false,[], s));
        }
    }
    static string ConstantExpression(GetName g)
    {
        return "CLOP.ConstantExpression";
    }

    static TParseTree CompoundStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclarationList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StatementList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclarationList, Spacing), pegged.peg.wrapAround!(Spacing, StatementList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "CLOP.CompoundStatement")(p);
        }
        else
        {
            if(auto m = tuple(`CompoundStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclarationList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StatementList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclarationList, Spacing), pegged.peg.wrapAround!(Spacing, StatementList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "CLOP.CompoundStatement"), "CompoundStatement")(p);
                memo[tuple(`CompoundStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CompoundStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclarationList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StatementList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclarationList, Spacing), pegged.peg.wrapAround!(Spacing, StatementList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "CLOP.CompoundStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclarationList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StatementList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclarationList, Spacing), pegged.peg.wrapAround!(Spacing, StatementList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "CLOP.CompoundStatement"), "CompoundStatement")(TParseTree("", false,[], s));
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
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.ExpressionStatement")(p);
        }
        else
        {
            if(auto m = tuple(`ExpressionStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.ExpressionStatement"), "ExpressionStatement")(p);
                memo[tuple(`ExpressionStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExpressionStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.ExpressionStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.ExpressionStatement"), "ExpressionStatement")(TParseTree("", false,[], s));
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
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), Spacing))), "CLOP.IfStatement")(p);
        }
        else
        {
            if(auto m = tuple(`IfStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), Spacing))), "CLOP.IfStatement"), "IfStatement")(p);
                memo[tuple(`IfStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IfStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), Spacing))), "CLOP.IfStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), Spacing))), "CLOP.IfStatement"), "IfStatement")(TParseTree("", false,[], s));
        }
    }
    static string IfStatement(GetName g)
    {
        return "CLOP.IfStatement";
    }

    static TParseTree WhileStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "CLOP.WhileStatement")(p);
        }
        else
        {
            if(auto m = tuple(`WhileStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "CLOP.WhileStatement"), "WhileStatement")(p);
                memo[tuple(`WhileStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree WhileStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "CLOP.WhileStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "CLOP.WhileStatement"), "WhileStatement")(TParseTree("", false,[], s));
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
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "CLOP.ForStatement")(p);
        }
        else
        {
            if(auto m = tuple(`ForStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "CLOP.ForStatement"), "ForStatement")(p);
                memo[tuple(`ForStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ForStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "CLOP.ForStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, EqualityExpression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "CLOP.ForStatement"), "ForStatement")(TParseTree("", false,[], s));
        }
    }
    static string ForStatement(GetName g)
    {
        return "CLOP.ForStatement";
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

    static TParseTree ReturnStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.ReturnStatement")(p);
        }
        else
        {
            if(auto m = tuple(`ReturnStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.ReturnStatement"), "ReturnStatement")(p);
                memo[tuple(`ReturnStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReturnStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.ReturnStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "CLOP.ReturnStatement"), "ReturnStatement")(TParseTree("", false,[], s));
        }
    }
    static string ReturnStatement(GetName g)
    {
        return "CLOP.ReturnStatement";
    }

    static TParseTree Statement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CompoundStatement, Spacing), pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing), pegged.peg.wrapAround!(Spacing, IfStatement, Spacing), pegged.peg.wrapAround!(Spacing, IterationStatement, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing)), "CLOP.Statement")(p);
        }
        else
        {
            if(auto m = tuple(`Statement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CompoundStatement, Spacing), pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing), pegged.peg.wrapAround!(Spacing, IfStatement, Spacing), pegged.peg.wrapAround!(Spacing, IterationStatement, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing)), "CLOP.Statement"), "Statement")(p);
                memo[tuple(`Statement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Statement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CompoundStatement, Spacing), pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing), pegged.peg.wrapAround!(Spacing, IfStatement, Spacing), pegged.peg.wrapAround!(Spacing, IterationStatement, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing)), "CLOP.Statement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CompoundStatement, Spacing), pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing), pegged.peg.wrapAround!(Spacing, IfStatement, Spacing), pegged.peg.wrapAround!(Spacing, IterationStatement, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing)), "CLOP.Statement"), "Statement")(TParseTree("", false,[], s));
        }
    }
    static string Statement(GetName g)
    {
        return "CLOP.Statement";
    }

    static TParseTree StatementList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), Spacing))), "CLOP.StatementList")(p);
        }
        else
        {
            if(auto m = tuple(`StatementList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), Spacing))), "CLOP.StatementList"), "StatementList")(p);
                memo[tuple(`StatementList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StatementList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), Spacing))), "CLOP.StatementList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), Spacing))), "CLOP.StatementList"), "StatementList")(TParseTree("", false,[], s));
        }
    }
    static string StatementList(GetName g)
    {
        return "CLOP.StatementList";
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
            return         pegged.peg.defined!(pegged.peg.literal!("NDRange"), "CLOP.Keyword")(p);
        }
        else
        {
            if(auto m = tuple(`Keyword`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!("NDRange"), "CLOP.Keyword"), "Keyword")(p);
                memo[tuple(`Keyword`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Keyword(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("NDRange"), "CLOP.Keyword")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("NDRange"), "CLOP.Keyword"), "Keyword")(TParseTree("", false,[], s));
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

