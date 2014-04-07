module clop.compiler;

import pegged.grammar;

import clop.grammar;

string
compile( immutable string expr )
{
  auto p = CLOP( expr );

  string value( ParseTree p )
  {
    switch ( p.name )
    {
    case "CLOP":
      return value( p.children[0] );
    case "CLOP.Identifier":
    case "CLOP.FloatLiteral":
    case "CLOP.IntegerLiteral":
      return p.matches[0];
    case "CLOP.ArrayIndex":
      return "[" ~ value( p.children[0] ) ~ "]";
    case "CLOP.FunctionCall":
      if ( p.children.length > 0 )
        return "(" ~ value( p.children[0] ) ~ ")";
      return "()";
    case "CLOP.PrimaryExpr":
      string s = value( p.children[0] );
      if ( p.matches[0] == "(" ) return "(" ~ s ~ ")";
      return s;
    case "CLOP.Factor":
    case "CLOP.UnaryExpr":
    case "CLOP.Expression":
      string s = value( p.children[0] );
      for ( auto c = 1; c < p.children.length; ++c )
        s ~= value( p.children[c] );
      return s;
    case "CLOP.ArgumentList":
      string s = value( p.children[0] );
      for ( auto c = 1; c < p.children.length; ++c )
        s ~= "," ~ value( p.children[c] );
      return s;
    case "CLOP.MulExpr":
    case "CLOP.AddExpr":
      return p.matches[0] ~ value( p.children[0] );
    case "CLOP.AssignExpr":
      return value( p.children[0] ) ~ "=" ~ value( p.children[1] );
    case "CLOP.Statement":
      return value( p.children[0] ) ~ ";";
    case "CLOP.StatementList":
      string s = "{\n";
      foreach ( c; p.children ) s ~= "  " ~ value( c ) ~ "\n";
      return s ~ "}";
    case "CLOP.RangeSpec":
      string s = "foreach (";
      foreach (c ; p.matches) s ~= " " ~ c;
      return s ~ " )\n";
    case "CLOP.RangeList":
      string s = value( p.children[0] );
      for ( auto c = 1; c < p.children.length; ++c )
        s ~= value( p.children[c] );
      return s;
    case "CLOP.RangeDecl":
      return value( p.children[0] );
    case "CLOP.TranslationUnit":
      return value( p.children[0] ) ~ value( p.children[1] );
    default: return p.name;
    }
  }

  string code = value( p );
  return "writefln( \"Generated code:\\n%s\\n\", q{" ~ code ~ "} );\n" ~ code;
}
