module clop.structs;

struct Set(T)
{
  private T[] items;
  private size_t it;

  @property
  bool empty()
  {
    return items.length == 0 || it == items.length;
  }

  @property
  ref T front()
  {
    assert(items.length > 0);
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

  @property
  string toString()
  {
    auto s = "";
    if (items.length > 0)
    {
      s = items[0].toString;
      for (auto i = 1; i < items.length; ++i)
        s ~= "_" ~ items[i].toString;
    }
    return s;
  }
}

unittest
{
}
