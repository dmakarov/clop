module clop.grammar;

import pegged.grammar;
public import pegged.peg;

mixin(grammar(q{
 CLOP:

 TranslationUnit        <  ExternalDeclarations? (SyncPattern :Spacing)? RangeDecl :Spacing CompoundStatement (:Spacing Transformations)?
 ExternalDeclarations   <  ExternalDeclaration (:Spacing ExternalDeclaration)*
 ExternalDeclaration    <  FunctionDefinition / Declaration
 FunctionDefinition     <  TypeSpecifier Declarator CompoundStatement
 SyncPattern            <  Identifier
 RangeDecl              <  "NDRange" '(' RangeList ')'
 RangeList              <  RangeSpec (',' RangeSpec)*
 RangeSpec              <  Identifier ':' Expression ".." Expression
 Transformations        <  "apply" '(' TransList ')'
 TransList              <  TransSpec (',' TransSpec)*
 TransSpec              <  Identifier ('(' ')' / '(' ArgumentList ')')

# CLOP_Decl:
 DeclarationList        <  Declaration (:Spacing Declaration)*
 Declaration            <  TypeSpecifier InitDeclaratorList? ';'
 Declarator             <  (Identifier / '(' Declarator ')') ('[' ']' / '(' ')' / '[' ConstantExpression ']' / '(' ParameterList ')' / '(' IdentifierList ')')*
 TypeSpecifier          <- "void" / "char" / "short" / "int" / "long" / "float" / "double" / StructSpecifier
 StructSpecifier        <  "struct" (Identifier ('{' StructDeclarationList '}')? / '{' StructDeclarationList '}')
 StructDeclarationList  <  StructDeclaration (:Spacing StructDeclaration)*
 StructDeclaration      <  TypeSpecifier StructDeclaratorList ';'
 StructDeclaratorList   <  StructDeclarator (',' StructDeclarator)*
 StructDeclarator       <  (Declarator ConstantExpression? / ConstantExpression)
 InitDeclaratorList     <  InitDeclarator (',' InitDeclarator)*
 InitDeclarator         <  Declarator ('=' Initializer)?
 Initializer            <  AssignExpr / '{' InitializerList ','? '}'
 InitializerList        <  Initializer (',' Initializer)*
 ParameterList          <  ParameterDeclaration (',' ParameterDeclaration)*
 ParameterDeclaration   <  TypeSpecifier Declarator

# CLOP_Stmt:
 StatementList          <  Statement (:Spacing Statement)*
 CompoundStatement      <  '{' '}' / '{' DeclarationList '}' / '{' StatementList '}' / '{' DeclarationList StatementList '}'
 Statement              <  CompoundStatement / ExpressionStatement / IfStatement / IterationStatement / ReturnStatement
 ExpressionStatement    <  AssignExpr? ';'
 IfStatement            <  "if" '(' EqualityExpression ')' Statement ("else" Statement)?
 WhileStatement         <  "while" '(' EqualityExpression ')' Statement
 ForStatement           <  "for" '(' AssignExpr? ';' EqualityExpression? ';' AssignExpr? ')' Statement
 IterationStatement     <  WhileStatement / ForStatement
 ReturnStatement        <  "return" ConditionalExpression ';'

# CLOP_Expr:
 ConditionalExpression  <  LogicalORExpression ('?' Expression ':' ConditionalExpression)?
 LogicalORExpression    <  LogicalANDExpression ("||" LogicalORExpression)*
 LogicalANDExpression   <  InclusiveORExpression ("&&" LogicalANDExpression)*
 InclusiveORExpression  <  ExclusiveORExpression ('|' InclusiveORExpression)*
 ExclusiveORExpression  <  ANDExpression ('^' ExclusiveORExpression)*
 ANDExpression          <  EqualityExpression ('&' ANDExpression)*
 EqualityExpression     <  RelationalExpression (EqualityOperator EqualityExpression)*
 EqualityOperator       <- "==" / "!="
 RelationalExpression   <  Expression (RelationalOperator RelationalExpression)*
 RelationalOperator     <- "<=" / ">=" / "<" / ">"
 Expression             <  Factor AddExpr*
 Factor                 <  UnaryExpr MulExpr*
 AddExpr                <  [-+] Factor
 UnaryExpr              <  PrimaryExpr (ArrayIndex / FunctionCall)?
 MulExpr                <  [*/] UnaryExpr
 AssignExpr             <  UnaryExpr '=' AssignExpr / ConditionalExpression
 ArrayIndex             <  '[' ArgumentList ']'
 FunctionCall           <  '(' ')' / '(' ArgumentList ')'
 ArgumentList           <  Expression (',' Expression)*
 PrimaryExpr            <  Identifier / FloatLiteral / IntegerLiteral / '(' Expression ')'
 ConstantExpression     <  ConditionalExpression

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
 Sign                   <- "-" / "+"

})); //
