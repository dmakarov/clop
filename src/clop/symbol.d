module clop.symbol;

import std.string;

import pegged.grammar;

import clop.analysis;

struct Symbol
{
  string      name;
  string      type;
  ParseTree   decl;
  bool        is_local;
  bool        is_array;
  ParseTree[] uses;
  ParseTree[] defs;
  string      shadow;
  Interval[]  box;

  this(this)
  {
    uses = uses.dup;
    defs = defs.dup;
  }

  string toString()
  {
    return format( "name %10s, type %10s, %6s, %6s, #uses %2d, #defs %2d, shadow %s",
                   name,
                   (type == null ? "N/A" : type),
                   (is_local ? "local" : "global"),
                   (is_array ? "array" : "scalar"),
                   (uses == null ? 0 : uses.length),
                   (defs == null ? 0 : defs.length),
                   (shadow == null ? "N/A" : shadow) );
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
