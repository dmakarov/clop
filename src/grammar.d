module clop.grammar;

import pegged.grammar;

mixin( grammar( q{
CLOP:

TranslationUnit   <  RangeDecl :Spacing StatementList

RangeDecl         <  "NDRange" '(' RangeList ')' ';'
RangeList         <  RangeSpec ( ',' RangeSpec )*
RangeSpec         <  Identifier ';' Expression ".." Expression

PrimaryExpr       <  Identifier / FloatLiteral / IntegerLiteral / '(' Expression ')'
UnaryExpr         <  PrimaryExpr ( ArrayIndex / FunctionCall )*
ArrayIndex        <  '[' Expression ']'
FunctionCall      <  '(' ')' / '(' ArgumentList ')'
ArgumentList      <  Expression ( ',' Expression )*

MulExpr           <  [*/] UnaryExpr
Factor            <  UnaryExpr MulExpr*
AddExpr           <  [-+] Factor
Expression        <  Factor AddExpr*

AssignExpr        <  UnaryExpr '=' Expression
Statement         <  AssignExpr? ';'
StatementList     <- Statement ( :Spacing Statement )*

Identifier        <~ !Keyword [a-zA-Z_] [a-zA-Z0-9_]*
Keyword           <- "NDRange"
Spacing           <~ (space / endOfLine / Comment)*
Comment           <~ "//" (!endOfLine .)* endOfLine
IntegerLiteral    <~ Sign? Integer IntegerSuffix?
Integer           <~ digit+
IntegerSuffix     <- "Lu" / "LU" / "uL" / "UL" / "L" / "u" / "U"
FloatLiteral      <~ Sign? Integer "." Integer? (("e" / "E") Sign? Integer)?
Sign              <- "-" / "+"

} ) );
