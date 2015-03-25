module clop.ct.grammar;

public import pegged.peg;
import pegged.grammar;

mixin(grammar(q{
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
 Declaration            <  TypeSpecifier InitDeclaratorList? ';'
 Declarator             <  (Identifier / '(' Declarator ')')
                           ('[' ']' / '(' ')' / '[' ConditionalExpr ']' / '(' ParameterList ')' / '(' IdentifierList ')')*
 TypeSpecifier          <- "void" / "char" / "short" / "int" / "long" / "float" / "double" / StructSpecifier
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
 PrimaryExpr            <  Identifier / FloatLiteral / IntegerLiteral / '(' Expression ')'
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
 Sign                   <- "-" / "+"

})); //
