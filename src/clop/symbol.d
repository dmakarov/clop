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
    return format( "name %10s, type %10s, local %5s, array %5s, shadow %s",
                   name, (type == null ? "null" : type), is_local, is_array, (shadow == null ? "null" : shadow) );
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
