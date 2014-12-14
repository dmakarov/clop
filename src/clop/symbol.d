module clop.symbol;

import pegged.grammar;

import clop.analysis;

struct Symbol
{
  string      name;
  ParseTree   decl;
  bool        is_local;
  bool        is_array;
  ParseTree[] uses;
  ParseTree[] defs;
  string      shadow;
  
  this(this)
  {
    uses = uses.dup;
    defs = defs.dup;
  }
}

struct Range
{
  string[]      indices;
  Interval[]    intervals;
  ulong[string] s2i;       // map range parameter name to dimension index
                           // dimension 0 is the inner most
  this(this)
  {
    indices = indices.dup;
    intervals = intervals.dup;
    s2i = s2i.dup;
  }
}
