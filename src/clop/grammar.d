module clop.grammar;

import std.stdio;

import pegged.grammar;

enum clop_rules = q{
CLOP:

TranslationUnit        <  ExternalDeclarations? RangeDecl :Spacing StatementList
ExternalDeclarations   <  ExternalDeclaration (:Spacing ExternalDeclaration)*
ExternalDeclaration    <  FunctionDefinition / Declaration
FunctionDefinition     <  TypeSpecifier Declarator DeclarationList? CompoundStatement

DeclarationList        <- Declaration (:Spacing Declaration)*
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
StructDeclarationList  <- StructDeclaration (:Spacing StructDeclaration)*
StructDeclaration      <  SpecifierQualifierList StructDeclaratorList ';'
SpecifierQualifierList <- TypeSpecifier (:Spacing TypeSpecifier)*
StructDeclaratorList   <  StructDeclarator (',' StructDeclarator)*
StructDeclarator       <  ( Declarator ConstantExpression? / ConstantExpression )
InitDeclaratorList     <  InitDeclarator (',' InitDeclarator)*
InitDeclarator         <  Declarator ('=' Initializer)?
Initializer            <  AssignExpr / '{' InitializerList ','? '}'
InitializerList        <  Initializer (',' Initializer)*

RangeDecl              <  "NDRange" '(' RangeList ')' ';'
RangeList              <  RangeSpec ( ',' RangeSpec )*
RangeSpec              <  Identifier ';' Expression ".." Expression

PrimaryExpr            <  Identifier / FloatLiteral / IntegerLiteral / '(' Expression ')'
UnaryExpr              <  PrimaryExpr ( ArrayIndex / FunctionCall )*
ArrayIndex             <  '[' Expression ']'
FunctionCall           <  '(' ')' / '(' ArgumentList ')'
ArgumentList           <  Expression ( ',' Expression )*

MulExpr                <  [*/] UnaryExpr
Factor                 <  UnaryExpr MulExpr*
AddExpr                <  [-+] Factor
Expression             <  Factor AddExpr*
AssignExpr             <  UnaryExpr '=' AssignExpr
                       /  ConditionalExpression
RelationalExpression   <  Expression (("<=" / ">=" / "<" / ">") RelationalExpression)*
EqualityExpression     <  RelationalExpression (("==" / "!=") EqualityExpression)*
ANDExpression          <  EqualityExpression ('&' ANDExpression)*
ExclusiveORExpression  <  ANDExpression ('^' ExclusiveORExpression)*
InclusiveORExpression  <  ExclusiveORExpression ('|' InclusiveORExpression)*
LogicalANDExpression   <  InclusiveORExpression ("&&" LogicalANDExpression)*
LogicalORExpression    <  LogicalANDExpression ("||" LogicalORExpression)*
ConditionalExpression  <  LogicalORExpression ('?' Expression ':' ConditionalExpression)?
ConstantExpression     <- ConditionalExpression

ExpressionStatement    <  AssignExpr? ';'
ReturnStatement        <  "return" ConditionalExpression ';'
Statement              <  ExpressionStatement
                       /  ReturnStatement
StatementList          <- Statement ( :Spacing Statement )*
CompoundStatement      <  '{' '}'
                       /  '{' DeclarationList '}'
                       /  '{' StatementList '}'
                       /  '{' DeclarationList StatementList '}'

Identifier             <~ !Keyword [a-zA-Z_] [a-zA-Z0-9_]*
Keyword                <- "NDRange"
Spacing                <~ (space / endOfLine / Comment)*
Comment                <~ "//" (!endOfLine .)* endOfLine
IntegerLiteral         <~ Sign? Integer IntegerSuffix?
Integer                <~ digit+
IntegerSuffix          <- "Lu" / "LU" / "uL" / "UL" / "L" / "u" / "U"
FloatLiteral           <~ Sign? Integer "." Integer? (("e" / "E") Sign? Integer)?
Sign                   <- "-" / "+"

};

mixin( grammar( clop_rules ) );

void
print_parser()
{
  writeln( grammar( clop_rules ) );
}
