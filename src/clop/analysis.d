module clop.analysis;

import pegged.grammar;

/++
 +  Interval arithmetic operations crate ParseTree objects.
 +  ParseTree has the following fields
 +
 +  string      name;       /// The node name
 +  bool        successful; /// Indicates whether a parsing was successful or not
 +  string[]    matches;    /// The matched input's parts. Some expressions match at more than one place, hence matches is an array.
 +  string      input;      /// The input string that generated the parse tree. Stored here for the parse tree to be passed to other expressions, as input.
 +  size_t      begin, end; /// Indices for the matched part from the very beginning of the first match to the last char of the last match.
 +  ParseTree[] children;   /// The sub-trees created by sub-rules parsing.
 +/

struct Interval
{
  /++
   +  min and max fields are always CLOP.Expression nodes.
   +/
  ParseTree min;
  ParseTree max;

  this( ParseTree min, ParseTree max )
  {
    Interval.min = min;
    Interval.max = max;
  }
  
  string toString()
  {
    string s = "[";
    foreach ( x; min.matches )
      s ~= x;
    s ~= ",";
    foreach ( x; max.matches )
      s ~= x;
    s ~= "]";
    return s;
  }
  
  Interval dup() @property
  {
    Interval result = this;
    result.min = result.min.dup();
    result.max = result.max.dup();
    return result;
  }
}

Interval
interval_arithmetic_operation( Interval a, string op, Interval b )
{
  switch ( op )
  {
  case "+":
    auto min = create_add_expr( a.min, b.min, "+" );
    auto max = create_add_expr( a.max, b.max, "+" );
    return Interval( min, max );
  case "-":
    auto min = create_add_expr( a.min, b.max, "-" );
    auto max = create_add_expr( a.max, b.min, "-" );
    return Interval( min, max );
  case "*":
    auto s1 = create_mul_expr( a.min, b.min, "*" );
    auto s2 = create_mul_expr( a.min, b.max, "*" );
    auto s3 = create_mul_expr( a.max, b.min, "*" );
    auto s4 = create_mul_expr( a.max, b.max, "*" );
    auto min = create_call( "min", [s1, s2, s3, s4] );
    auto max = create_call( "max", [s1, s2, s3, s4] );
    return Interval( min, max );
  case "/":
    auto s1 = create_mul_expr( a.min, b.min, "/" );
    auto s2 = create_mul_expr( a.min, b.max, "/" );
    auto s3 = create_mul_expr( a.max, b.min, "/" );
    auto s4 = create_mul_expr( a.max, b.max, "/" );
    auto min = create_call( "min", [s1, s2, s3, s4] );
    auto max = create_call( "max", [s1, s2, s3, s4] );
    return Interval( min, max );
  default: return Interval();
  }
}

/++
 +  The result of any IA operation is a CLOP.Expression node.
 +/
ParseTree
create_call( string name, ParseTree[] args )
{
  auto id = [name];
  auto al = args[0].matches;
  for ( auto i = 1; i < args.length; ++i )
    al ~= "," ~ args[i].matches;
  auto fc = "(" ~ al ~ ")";
  auto i = args[0].input;
  size_t s = 0;
  size_t e = i.length;
  return
  ParseTree( "CLOP.Expression", true, id ~ fc, i, s, e,
    [ParseTree( "CLOP.Factor", true, id ~ fc, i, s, e,
       [ParseTree( "CLOP.UnaryExpr", true, id ~ fc, i, s, e,
          [ParseTree( "CLOP.PrimaryExpr", true, id, i, s, e,
             [ParseTree( "CLOP.Identifier", true, id, i, s, e, [] )] ),
           ParseTree( "CLOP.FunctionCall", true, fc, i, s, e,
             [ParseTree( "CLOP.ArgumentList", true, al, i, s, e, args )] )] )] )] );
}

ParseTree
create_add_expr( ParseTree a, ParseTree b, string op )
{
  string i = a.input;
  size_t s = 0;
  size_t e = i.length;
  auto p1 = "(" ~ a.matches ~ ")";
  auto p2 = "(" ~ b.matches ~ ")";
  auto a1 = op ~ p2;
  return
  ParseTree( "CLOP.Expression", true, p1 ~ a1, i, s, e,
    [ParseTree( "CLOP.Factor", true, p1, i, s, e,
       [ParseTree( "CLOP.UnaryExpr", true, p1, i, s, e,
          [ParseTree( "CLOP.PrimaryExpr", true, p1, i, s, e, [a] )] )] ),
     ParseTree( "CLOP.AddExpr", true, a1, i, s, e,
       [ParseTree( "CLOP.Factor", true, p2, i, s, e,
          [ParseTree( "CLOP.UnaryExpr", true, p2, i, s, e,
             [ParseTree( "CLOP.PrimaryExpr", true, p2, i, s, e, [b] )] )] )] )] );
}

ParseTree
create_mul_expr( ParseTree a, ParseTree b, string op )
{
  string i = a.input;
  size_t s = 0;
  size_t e = i.length;
  auto p1 = "(" ~ a.matches ~ ")";
  auto p2 = "(" ~ b.matches ~ ")";
  auto a1 = op ~ p2;
  return
  ParseTree( "CLOP.Expression", true, p1 ~ a1, i, s, e,
    [ParseTree( "CLOP.UnaryExpr", true, p1, i, s, e,
       [ParseTree( "CLOP.PrimaryExpr", true, p1, i, s, e, [a] )] ),
     ParseTree( "CLOP.MulExpr", true, a1, i, s, e,
       [ParseTree( "CLOP.UnaryExpr", true, p2, i, s, e,
          [ParseTree( "CLOP.PrimaryExpr", true, p2, i, s, e, [b] )] )] )] );
}
