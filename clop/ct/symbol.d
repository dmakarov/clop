/*
 *  The MIT License (MIT)
 *  =====================
 *
 *  Copyright (c) 2015 Dmitri Makarov <dmakarov@alumni.stanford.edu>
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the "Software"), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in all
 *  copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 *  SOFTWARE.
 */
module clop.ct.symbol;

import std.string, pegged.grammar, clop.ct.analysis;

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

  @property
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
  bool   is_macro;
  string address;
  string to_push;
  string to_pull;
  string to_release;
  bool   is_ndarray;
  uint   number;

  this(string n,
       string t = "",
       string q = "",
       string s = "",
       string b = "",
       bool k = false,
       bool m = false,
       string a = "",
       string p = "",
       string h = "",
       string e = "")
  {
    name = n;
    type = t;
    qual = q;
    size = s;
    back = b;
    skip = k;
    is_macro = m;
    address = (a == "") ? "&" ~ n : a;
    to_push = p;
    to_pull = h;
    to_release = e;
  }

  @property
  string toString()
  {
    return format("%s, %s, %s, %s, %s, %s, %s, %s", name, type, qual, size, back, skip, is_macro, address);
  }
}

// Local Variables:
// compile-command: "../../tests/test_module clop.ct.symbol"
// End:
