module clop.transform;

import std.algorithm : reduce;

import pegged.grammar;

/++
 +  Information about an optimizing transformation.
 +/
struct Transformation
{
  @property
  string[] get_parameters()
  {
    return parameters;
  }

  @property
  string get_name()
  {
    return name;
  }

  @property
  string toString()
  {
    return name ~ reduce!((a, b) => a ~ "_" ~ b)("", parameters);
  }

 private:
  string name;
  string[] parameters;
}
