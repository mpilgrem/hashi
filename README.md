# hashi

A solver of Hashiwokakero (橋をかけろ - _Build Bridges!_) logic puzzles.

A puzzle comprises a rectangular grid. Some cells of the grid are 'islands' and
other cells are 'water'. Each island has a constraint of the number of bridges,
1 to 8.

Neighbouring islands (above, below, to the left or to the right) are separated
by water. Optionally, they can be linked by one or two bridges. Bridges cannot
cross; the following is not allowed:

~~~text
┌─┬─┬─┐
│ │1│ │
├─┼┼┼─┤
│2╪╪╪2│
├─┼┼┼─┤
│ │1│ │
└─┴─┴─┘
~~~

Bridges are placed between neighbouring islands. The puzzle is solved when every
island is connected to all the others via bridges that are consistent with the
constraints.

~~~text
┌─┬─┬─┐
│3╪═╪4│
├┼┼─┼╫┤
│││ │║│
├┼┼─┼╫┤
│1│ │2│
└─┴─┴─┘
~~~

Puzzles are specified in text files using characters `.` (water) and `1` to `8` (island with constraint). For example `7x10easy` contains:

~~~text
2.2..2.
......1
6.5.3..
.1....3
3.1..1.
.3..8.5
4.2....
.2..5.2
2..1...
..2.5.3
~~~

To build with Stack, command:
~~~text
stack build
~~~

To use the executable with Stack with `7x10easy` (for example), command:
~~~text
stack exec -- hashi-solve 7x10easy
~~~

This project is a fork of [`hashi`](https://github.com/ctbo/hashi/tree/bd38e6f37635d74bd2fd3528821db04b9edf6643) by Harald Bögeholz.

See `CHANGELOG.md` for a description of changes and additions to that project.
The principal changes are:

* intended to make it easier to identify and follow the algorithm; and
* output in SVG rather than EPS.
