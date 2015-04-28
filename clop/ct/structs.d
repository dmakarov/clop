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
module clop.ct.structs;

/++
 +
 +/
struct Set(T)
{
  @property
  bool empty()
  {
    // it == items.length when we just finished iterating over all items
    return items.length == 0 || it == items.length;
  }

  @property
  ref T front()
  {
    assert(items.length > 0);
    // reset the iterator to the first items, once the previous use of
    // the iterator has reached the end.  if somebody breaks from a
    // foreach before reaching the end of items, the next invocation
    // of foreach on this set will start from where we left off.
    if (it == items.length)
      it = 0;
    return items[it];
  }

  void popFront()
  {
    assert(it < items.length);
    ++it;
  }

  bool contains(T item)
  {
    foreach (n; items)
      if (n == item)
        return true;
    return false;
  }

  void insert(T item)
  {
    if (!contains(item))
      items ~= item;
  }

  T[] get_items_as_array()
  {
    return items;
  }

  @property
  string toString()
  {
    import std.algorithm : map, reduce;
    return reduce!((a, b) => a ~ "_" ~ b)("", map!(a => a.toString)(items));
  }

private:

  T[] items; /// the collection
  size_t it; /// iterator index
} // Set struct

unittest
{
}

// Local Variables:
// compile-command: "../../tests/test_module clop.ct.structs"
// End:
