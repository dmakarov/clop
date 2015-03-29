module clop.ct.analysis;

import std.string;

import pegged.grammar;

/++
 +  Interval arithmetic operations crate ParseTree objects.
 +  ParseTree has the following fields
 +
 +  string      name;       /// The node name
 +  bool        successful; /// Indicates whether parsing was successful
 +  string[]    matches;    /// The matched input's parts. Some expressions match at more than one place, hence matches is an array.
 +  string      input;      /// The input string that generated the parse tree. Stored here for the parse tree to be passed to other expressions, as input.
 +  size_t      begin, end; /// Indices for the matched part from the very beginning of the first match to the last char of the last match.
 +  ParseTree[] children;   /// The sub-trees created by sub-rules parsing.
 +/

struct Box
{
  Interval[]    intervals;
  string[]      symbols;
  /// map range parameter name to dimension index; dimension 0 is the inner most
  ulong[string] s2i;

  this(this)
  {
    intervals = intervals.dup;
    symbols   = symbols.dup;
    s2i       = s2i.dup;
  }

  auto get_dimensions()
  {
    return intervals.length;
  }

  string get_size(ulong dimension)
  {
    if ( dimension >= intervals.length )
      return null;
    return intervals[dimension].get_size();
  }

  string get_interval_sizes()
  {
    import std.algorithm : reduce;
    return reduce!((a, b) => a ~ ", " ~ b.get_size())(intervals[0].get_size(), intervals[1 .. $]);
  }

  string toString()
  {
    auto s = "";
    foreach ( i, k; s2i.keys )
    {
      s ~= format( "// %d %d %s %s\n", i, s2i[k], k, intervals[s2i[k]].toString() );
    }
    return s;
  }

  Interval[] map(ref ParseTree t)
  {
    if (t.name != "CLOP.Expression" || t.children.length != get_dimensions())
    {
      return null;
    }
    Interval[] result;
    foreach (i, c; t.children)
    {
      result ~= evaluate_expression(c, intervals[i]);
    }
    return result;
  }

 private:

  Interval evaluate_expression(ref ParseTree t, Interval i)
  {
    switch (t.name)
    {
    case "CLOP.AssignmentExpr":
      return t.children.length == 1 ? evaluate_expression(t.children[0], i) : evaluate_expression(t.children[2], i);
    case "CLOP.ConditionalExpr":
    case "CLOP.LogicalORExpr":
    case "CLOP.LogicalANDExpr":
    case "CLOP.InclusiveORExpr":
    case "CLOP.ExclusiveORExpr":
    case "CLOP.ANDExpr":
    case "CLOP.EqualityExpr":
    case "CLOP.RelationalExpr":
    case "CLOP.ShiftExpr":
    case "CLOP.CastExpr":
    case "CLOP.PrimaryExpr":
      return evaluate_expression(t.children[0], i);
    case "CLOP.AdditiveExpr":
    case "CLOP.MultiplicativeExpr":
      auto r = evaluate_expression(t.children[0], i);
      if (t.children.length == 1)
        return r;
      return interval_arithmetic_operation(r, t.children[1].matches[0], evaluate_expression(t.children[2], i));
    case "CLOP.UnaryExpr":
      if (t.children.length == 1)
        return evaluate_expression(t.children[0], i);
      else // FIXME apply unary operator
        return evaluate_expression(t.children[1], i);
    case "CLOP.PostfixExpr":
      return evaluate_expression(t.children[0], i);
    case "CLOP.Identifier":
      auto x = s2i.get(t.matches[0], 0xffffffff);
      if (x != 0xffffffff)
        return intervals[x];
      return Interval(t, t);
    case "CLOP.FloatLiteral":
    case "CLOP.IntegerLiteral":
      return Interval(t, t);
    default:
      return Interval();
    }
  }
}

struct Interval
{
  /++
   +  min and max fields are always CLOP.Expression nodes.
   +/
  ParseTree min;
  ParseTree max;

  this( ParseTree t )
  {
    this( t, t );
  }

  this( ParseTree min, ParseTree max )
  {
    Interval.min = min;
    Interval.max = max;
  }

  string get_min()
  {
    auto s = "";
    foreach ( x; min.matches )
      s ~= x;
    return s;
  }

  string get_max()
  {
    auto s = "";
    foreach ( x; max.matches )
      s ~= x;
    return s;
  }

  string get_size()
  {
    ParseTree x = create_add_expr( max, min, "-" );
    auto s = "";
    foreach ( m; x.matches )
      s ~= m;
    return s;
  }

  string toString()
  {
    auto s = "[";
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
    auto result = this;
    result.min = result.min.dup();
    result.max = result.max.dup();
    return result;
  }

  bool is_constant( ParseTree t )
  {
    switch ( t.name )
    {
      case "CLOP.FloatLiteral":
      case "CLOP.IntegerLiteral":
        return true;
      default:
        auto result = false;
        if ( t.children.length > 0 )
        {
          result = true;
          foreach ( c; t.children )
            result = result && is_constant( c );
        }
        return result;
    }
  }

  int get_value( ParseTree t )
  {
    switch ( t.name )
    {
      case "CLOP.FloatLiteral":
      case "CLOP.IntegerLiteral":
        return to!(int)(t.matches[0]);
      default:
        int result = 0;
        if ( t.children.length > 0 )
        {
          foreach ( c; t.children )
            result = get_value( c );
        }
        return result;
    }
  }

  ParseTree simplify_expression( ParseTree t )
  {
    switch ( t.name )
    {
    case "CLOP.Expression":
      ParseTree[] children = [];
      bool[] constants = [];
      int[] values;
      auto only_const = true;
      foreach ( c; t.children )
      {
        auto s = simplify_expression( c );
        children ~= [s];
        auto b = is_constant( s );
        only_const = only_const && b;
        constants ~= [b];
        if ( b )
          values ~= [get_value( s )];
        else
          values ~= [0];
      }

      int sum = 0;
      for ( auto i = 0; i < t.children.length; ++i )
      {
        if ( constants[i] )
        {
          if ( i == 0 || t.children[i].matches[0] == "+" )
           sum += values[i];
          else
           sum -= values[i];
        }
      }
      if ( only_const )
      {
        auto m = [format("%d", sum)];
        auto u = ParseTree( "CLOP.Factor", true, m, "", 0, 0,
                   [ParseTree( "CLOP.UnaryExpr", true, m, "", 0, 0,
                      [ParseTree( "CLOP.PrimaryExpr", true, m, "", 0, 0,
                         [ParseTree( "CLOP.IntegerLiteral", true, m, "", 0, 0, [] )] )] )] );
        return ParseTree( "CLOP.Expression", true, m, "", 0, 0, [u] );
      }
      if ( sum == 0 )
      {
        ParseTree[] n = [];
        string[] p = [];
        if ( constants[0] )
        {
          auto m = [format("%d", sum)];
          auto u = ParseTree( "CLOP.Factor", true, m, "", 0, 0,
                     [ParseTree( "CLOP.UnaryExpr", true, m, "", 0, 0,
                        [ParseTree( "CLOP.PrimaryExpr", true, m, "", 0, 0,
                           [ParseTree( "CLOP.IntegerLiteral", true, m, "", 0, 0, [] )] )] )] );
          n ~= [u];
          p ~= m;
        }
        else
        {
          n ~= [children[0]];
          p ~= children[0].matches;
        }
        for ( auto i = 1; i < t.children.length; ++i )
        {
          if ( !constants[i] )
          {
            n ~= [children[i]];
            p ~= children[i].matches;
          }
        }
        return ParseTree( "CLOP.Expression", true, p, "", 0, 0, n );
      }
      ParseTree[] n = [];
      string[] p = [];
      if ( constants[0] )
      {
        auto m = [format("%d", sum)];
        auto u = ParseTree( "CLOP.Factor", true, m, "", 0, 0,
                   [ParseTree( "CLOP.UnaryExpr", true, m, "", 0, 0,
                      [ParseTree( "CLOP.PrimaryExpr", true, m, "", 0, 0,
                         [ParseTree( "CLOP.IntegerLiteral", true, m, "", 0, 0, [] )] )] )] );
        n ~= [u];
        p ~= m;
      }
      else
      {
        n ~= [children[0]];
        p ~= children[0].matches;
        auto m = [format( "%s", sum < 0 ? "-" : "+" ), format( "%d", sum < 0 ? -sum : sum )];
        auto u = ParseTree( "CLOP.AddExpr", true, m, "", 0, 0,
                   [ParseTree( "CLOP.Factor", true, [m[1]], "", 0, 0,
                      [ParseTree( "CLOP.UnaryExpr", true, [m[1]], "", 0, 0,
                         [ParseTree( "CLOP.PrimaryExpr", true,[ m[1]], "", 0, 0,
                            [ParseTree( "CLOP.IntegerLiteral", true, [m[1]], "", 0, 0, [] )] )] )] )] );
        n ~= [u];
        p ~= m;
      }
      for ( auto i = 1; i < t.children.length; ++i )
      {
        if ( !constants[i] )
        {
          n ~= [children[i]];
          p ~= children[i].matches;
        }
      }
      return ParseTree( "CLOP.Expression", true, p, "", 0, 0, n );
    case "CLOP.Factor":
      ParseTree[] p = [];
      string[] m = [];
      foreach ( c; t.children )
      {
        auto u = simplify_expression( c );
        p ~= [u];
        m ~= u.matches;
      }
      return ParseTree( "CLOP.Factor", true, m, "", 0, 0, p );
    case "CLOP.AddExpr":
    case "CLOP.MulExpr":
      ParseTree[] p = [];
      string[] m = [t.matches[0]];
      foreach ( c; t.children )
      {
        auto u = simplify_expression( c );
        p ~= [u];
        m ~= u.matches;
      }
      return ParseTree( t.name, true, m, "", 0, 0, p );
    case "CLOP.UnaryExpr":
      if ( t.matches[0] == "min" && t.children.length > 1 && t.children[1].name == "CLOP.FunctionCall" )
      {
        auto args = t.children[1].children[0].children;
        auto a0 = simplify_expression( args[0] );
        auto a1 = simplify_expression( args[1] );
        auto b0 = is_constant( a0 );
        auto b1 = is_constant( a1 );
        if ( b0 && b1 )
        {
          auto v0 = get_value( a0 );
          auto v1 = get_value( a1 );
          auto v = v0 < v1 ? v0 : v1;
          auto m = format( "%d", v );
          return ParseTree( "CLOP.UnaryExpr", true, [m], "", 0, 0,
                   [ParseTree( "CLOP.PrimaryExpr", true,[m], "", 0, 0,
                      [ParseTree( "CLOP.IntegerLiteral", true, [m], "", 0, 0, [] )] )] );
        }
      }
      if ( t.matches[0] == "max" && t.children.length > 1 && t.children[1].name == "CLOP.FunctionCall" )
      {
        auto args = t.children[1].children[0].children;
        auto a0 = simplify_expression( args[0] );
        auto a1 = simplify_expression( args[1] );
        auto b0 = is_constant( a0 );
        auto b1 = is_constant( a1 );
        if ( b0 && b1 )
        {
          auto v0 = get_value( a0 );
          auto v1 = get_value( a1 );
          auto v = v0 < v1 ? v1 : v0;
          auto m = format( "%d", v );
          return ParseTree( "CLOP.UnaryExpr", true, [m], "", 0, 0,
                   [ParseTree( "CLOP.PrimaryExpr", true,[m], "", 0, 0,
                      [ParseTree( "CLOP.IntegerLiteral", true, [m], "", 0, 0, [] )] )] );
        }
      }
      ParseTree[] p = [];
      string[] m = [];
      foreach ( c; t.children )
      {
        auto u = simplify_expression( c );
        p ~= [u];
        m ~= u.matches;
      }
      return ParseTree( t.name, true, m, "", 0, 0, p );
    case "CLOP.PrimaryExpr":
      ParseTree[] p = [];
      string[] m = [];
      foreach ( c; t.children )
      {
        auto u = simplify_expression( c );
        p ~= [u];
        m ~= u.matches;
      }
      return ParseTree( t.name, true, m, "", 0, 0, p );
    case "CLOP.FunctionCall":
      ParseTree[] p = [];
      string[] m = ["("];
      foreach ( c; t.children )
      {
        auto u = simplify_expression( c );
        p ~= [u];
        m ~= u.matches;
      }
      m ~= [")"];
      return ParseTree( t.name, true, m, "", 0, 0, p );
    case "CLOP.ArgumentList":
      ParseTree[] p = [];
      string[] m = [];
      foreach ( i, c; t.children )
      {
        auto u = simplify_expression( c );
        p ~= [u];
        if ( i > 0 ) m ~= [","];
        m ~= u.matches;
      }
      return ParseTree( t.name, true, m, "", 0, 0, p );
    case "CLOP.Identifier":
    case "CLOP.FloatLiteral":
    case "CLOP.IntegerLiteral":
      return t;
    default:
      ParseTree[] p = [];
      string[] m = [];
      foreach ( c; t.children )
      {
        auto u = simplify_expression( c );
        p ~= [u];
        m ~= u.matches;
      }
      return ParseTree( t.name, true, m, "", 0, 0, p );
    }
  }
}

Interval
interval_apply(ref ParseTree t, ref Box r)
{
  switch ( t.name )
  {
  case "CLOP.Expression":
    auto i = interval_apply( t.children[0], r );
    for ( auto c = 1; c < t.children.length; ++c )
      if ( "+" == t.children[c].matches[0] )
        i = interval_arithmetic_operation( i, "+", interval_apply( t.children[c], r ) );
      else
        i = interval_arithmetic_operation( i, "-", interval_apply( t.children[c], r ) );
    return i;
  case "CLOP.Factor":
    auto i = interval_apply( t.children[0], r );
    for ( auto c = 1; c < t.children.length; ++c )
      if ( "*" == t.children[c].matches[0] )
        i = interval_arithmetic_operation( i, "*", interval_apply( t.children[c], r ) );
      else
        i = interval_arithmetic_operation( i, "/", interval_apply( t.children[c], r ) );
    return i;
  case "CLOP.AddExpr":
  case "CLOP.MulExpr":
    return interval_apply( t.children[0], r );
  case "CLOP.UnaryExpr":
    return interval_apply( t.children[0], r );
  case "CLOP.PrimaryExpr":
    return interval_apply( t.children[0], r );
  case "CLOP.Identifier":
    auto x = r.s2i.get( t.matches[0], 0xffffffff );
    if ( x != 0xffffffff )
      return r.intervals[x];
    return Interval( t, t );
  case "CLOP.FloatLiteral":
  case "CLOP.IntegerLiteral":
    return Interval( t, t );
  default:
    return Interval();
  }
}

Interval
interval_union( Interval a, Interval b )
{
//  auto min = create_minmax_expr( a.min, b.min, "min" );
//  auto max = create_minmax_expr( a.max, b.max, "max" );
  auto min = create_call( "min", [a.min, b.min] );
  auto max = create_call( "max", [a.max, b.max] );
  return Interval( min, max );
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
create_minmax_expr( ParseTree a, ParseTree b, string op )
{
  ParseTree te = ( op == "min" ) ? a : b;
  ParseTree fe = ( op == "min" ) ? b : a;
  string i = a.input;
  size_t s = 0;
  size_t e = i.length;
  auto c = a.matches ~ ["<"] ~ b.matches;
  auto m = ["( ("] ~ c ~ [") ? ("] ~ te.matches ~ [") : ("] ~ fe.matches ~ [") )"];
  return
  ParseTree( "CLOP.ConditionalExpression", true, m, i, s, e,
    [ParseTree( "CLOP.LogicalORExpression", true, c, i, s, e,
       [ParseTree( "CLOP.LogicalANDExpression", true, c, i, s, e,
          [ParseTree( "CLOP.InclusiveORExpression", true, c, i, s, e,
             [ParseTree( "CLOP.ExclusiveORExpression", true, c, i, s, e,
                [ParseTree( "CLOP.ANDExpression", true, c, i, s, e,
                   [ParseTree( "CLOP.EqualityExpression", true, c, i, s, e,
                      [ParseTree( "CLOP.RelationalExpression", true, c, i, s, e,
                         [a,
                          ParseTree( "CLOP.RelationalOperator", true, ["<"], i, s, e, [] ),
                          ParseTree( "CLOP.RelationalExpression", true, b.matches, i, s, e, [b] )] )] )] )] )] )] )] ),
     te,
     ParseTree( "CLOP.ConditionalExpression", true, fe.matches, i, s, e,
       [ParseTree( "CLOP.LogicalORExpression", true, fe.matches, i, s, e,
          [ParseTree( "CLOP.LogicalANDExpression", true, fe.matches, i, s, e,
             [ParseTree( "CLOP.InclusiveORExpression", true, fe.matches, i, s, e,
                [ParseTree( "CLOP.ExclusiveORExpression", true, fe.matches, i, s, e,
                   [ParseTree( "CLOP.ANDExpression", true, fe.matches, i, s, e,
                      [ParseTree( "CLOP.EqualityExpression", true, fe.matches, i, s, e,
                         [ParseTree( "CLOP.RelationalExpression", true, fe.matches, i, s, e, [fe] )] )] )] )] )] )] )] )] );
}

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
