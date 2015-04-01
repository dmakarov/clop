module clop.ct.symbol;

import std.string;

import pegged.grammar;

import clop.ct.analysis;

struct Symbol
{
  string      name;
  string      type;
  ParseTree   decl;
  ParseTree[] uses;
  ParseTree[] defs;
  Interval[]  box;
  string      shadow;
  bool        is_local;
  bool        is_array;
  bool        can_cache;

  this(this)
  {
    uses = uses.dup;
    defs = defs.dup;
    box = box.dup;
  }

  string toString()
  {
    return format("name %16s, type %6s, #uses %2d, #defs %2d, %6s, %6s, cache %s",
                  name,
                  (type == null ? "N/A" : type),
                  (uses == null ? 0 : uses.length),
                  (defs == null ? 0 : defs.length),
                  (is_local ? "local" : "global"),
                  (is_array ? "array" : "scalar"),
                  (can_cache ? "can" : "cannot" ) ~ " put in local memory");
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
