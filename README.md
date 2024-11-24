# hashi

A maker and solver of Hashiwokakero (橋をかけろ - _Build Bridges!_) logic puzzles.

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

Puzzles are specified by clicking in the sea. Click repeatedly to increase the
constraint.

To build with Stack, command:
~~~text
stack build
~~~

This project is a fork of
[`hashi`](https://github.com/ctbo/hashi/tree/bd38e6f37635d74bd2fd3528821db04b9edf6643)
by Harald Bögeholz.

See `CHANGELOG.md` for a description of changes and additions to that project.
The principal changes are:

* intended to make it easier to identify and follow the algorithm; and
* output to a Pixbuf rather than EPS.
