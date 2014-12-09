module clop.symbol;

import pegged.grammar;

struct Symbol
{
  string      name;
  ParseTree   decl;
  bool        is_local;
  bool        is_array;
  ParseTree[] uses;
  ParseTree[] defs;
}
