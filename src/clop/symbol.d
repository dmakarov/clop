module clop.symbol;

import pegged.grammar;

import clop.analysis;

struct Symbol
{
  string      name;
  string      type;
  ParseTree   decl;
  bool        is_local;
  bool        is_array;
  bool        is_shared;
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

struct Argument
{
  string name;
  string type;
  string qual;
  string size;
  string back;
  bool   skip;
}
