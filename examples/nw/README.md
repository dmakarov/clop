-*- org -*-

* General information
  This program generate two sequences randomly. Please specify your own
  sequences for different uses.  At the current stage, the program only supports
  two sequences with the same length, which must be divisible by 16.

* Usage
#+BEGIN_SRC sh
  ./bin/nw 2048 10
  2048     // the length of the sequence
  10       // penalty value
#+END_SRC
