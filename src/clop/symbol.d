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
  Interval[]  box;

  this( this )
  {
    uses = uses.dup;
    defs = defs.dup;
  }
}
