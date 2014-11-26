module clop.grammar;

import std.stdio;

import pegged.grammar;

enum clop_rules = q{
CLOP:

TranslationUnit        <  ExternalDeclarations?
                          (SyncPattern :Spacing)?
                          RangeDecl :Spacing CompoundStatement
                          (:Spacing Transformations)?
ExternalDeclarations   <  ExternalDeclaration (:Spacing ExternalDeclaration)*
ExternalDeclaration    <  FunctionDefinition / Declaration
FunctionDefinition     <  TypeSpecifier Declarator CompoundStatement

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

SyncPattern            <  "Antidiagonal" / "Horizontal" / "Stencil"
RangeDecl              <  "NDRange" '(' RangeList ')'
RangeList              <  RangeSpec ( ',' RangeSpec )*
RangeSpec              <  Identifier ':' Expression ".." Expression

Transformations        < "apply" '(' TransList ')'
TransList              < TransSpec ( ',' TransSpec )*
TransSpec              < Identifier ( '(' ')' / '(' ArgumentList ')' )

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
ConstantExpression     <- ConditionalExpression

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
StatementList          <- Statement ( :Spacing Statement )*

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
