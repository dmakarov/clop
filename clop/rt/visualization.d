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
module clop.rt.visualization;

import std.container.slist, std.stdio;

class History {
  struct Event {
    struct Update {
      long thread_id;
      long[] sources;
    }
    SList!Update[long] targets;
  }
  enum State { SOURCE, TARGET, UPDATED, WRITTEN }

  Event[long] events;
  File outfile;
  size_t matrix_width;

  alias make_list = make!(SList!(Event.Update));

  this(string filename, size_t width)
  {
    matrix_width = width;
    outfile = File(filename, "w");
    outfile.write("\\documentclass{article}\n\\usepackage{animate}\n\\usepackage[margin=1cm]{geometry}\n\\usepackage{tikz}\n\\usetikzlibrary{backgrounds,calc,matrix}\n\\begin{document}\n\\pagestyle{empty}\n");
  }

  ~this()
  {
    outfile.write("\\end{document}\n");
    outfile.close();
  }

  void add_event(long timestamp, long tid, long target, long[] inputs)
  {
    if (timestamp in events)
    {
      if (target in events[timestamp].targets)
      {
        events[timestamp].targets[target].insert(Event.Update(tid, inputs));
      }
      else
      {
        events[timestamp].targets[target] = make_list(Event.Update(tid, inputs));
      }
    }
    else
    {
      events[timestamp] = Event([target : make_list(Event.Update(tid, inputs))]);
    }
  }

  void save_animation()
  {
    version (LDC) import std.algorithm;
    else {
      import std.algorithm.iteration;
      import std.algorithm.sorting : sort;
    }
    auto cells = new long[matrix_width * matrix_width];
    auto cache = new long[matrix_width * matrix_width];
    auto started = false;
    outfile.write("\\begin{animateinline}[controls]{2}\n");
    foreach (ts; sort(events.keys))
    {
      auto it = events[ts];
      auto boundary = false;
      foreach (t, el; it.targets)
      {
        if (t == -1)
        {
          boundary = true;
          break;
        }
        auto tid = 10 * el.front().thread_id;
        cells[t] = tid < 0 ? tid - 1 : tid + 1;
        cache[t] = 3;
        foreach (e; el)
          foreach (s; e.sources)
            cells[s] = tid < 0 ? tid - 2 : tid + 2;
      }
      if (boundary)
      {
        foreach (ref c; cache) if (c == 3) c = 4;
      }
      else
      {
        if (started) outfile.writeln("  \\newframe");
        else started = true;
        output_frame(cells);
      }
      cells[] = cache[];
    }
    outfile.write("\\end{animateinline}\n");
    auto keys = events.keys;
    foreach (k; keys) events.remove(k);
  }

  void output_frame(long[] cells)
  {
    import std.math : abs;
    outfile.write("  \\begin{tikzpicture}\n    \\matrix (magic) [matrix of nodes, nodes={text height=3mm, text width=3mm, font=\\footnotesize, draw}, nodes in empty cells]\n    {\n");
    foreach (i; 0 .. matrix_width)
    {
      auto started = false;
      foreach (j; 0 .. matrix_width)
      {
        if (started)
        {
          outfile.write(" &");
        }
        else
        {
          started = true;
        }
        auto encoded = cells[i * matrix_width + j];
        auto decoded = encoded < 0 ? -encoded : encoded;
        auto color = decoded % 10;
        auto tid = encoded < 0 ? -1 : decoded / 10;
        if      (color == 1) outfile.write(" |[fill=red]|   ");
        else if (color == 2) outfile.write(" |[fill=green]| ");
        else if (color == 3) outfile.write(" |[fill=yellow]|");
        else if (color == 4) outfile.write(" |[fill=gray]|  ");
        else                 outfile.write("                ");
        if (tid >= 0 && color > 0 && color < 3)
        {
          outfile.write(tid);
        }
      }
      outfile.writeln(" \\\\");
    }
    outfile.write("    };\n  \\end{tikzpicture}\n");
  }
}
