module clop.tests.main;

import std.stdio;

import clop.compiler;

/**
 * unit tests driver.
 */
int main()
{
  version (unittest)
  {
    writefln("All unit tests passed.");
    return 0;
  }
  else
  {
    return 1;
  }
}
