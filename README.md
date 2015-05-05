# Interactive Tree Demos

These are a series of interactive browser applications that
demonstrate several CS concepts concerning trees.  They were written
as a part of my 2015 SLU Computer Science capstone project, and are
based very closely on a series of Java applets written by
[Michael Goldwasser](http://cs.slu.edu/~goldwasser/), my project
supervisor.

The demos use [GHCJS](https://github.com/GHCJS/GHCJS) to compile to
JavaScript for the browser, and use my intermediary graphics library
**Hyper-Canvas** to, manage the graphics and layout (at this stage,
Hyper-Canvas is in an early alpha stage and lives solely inside of
this project).

## Try them out!

I host the latest versions of these demos (and more?!)
[right here](http://octalsrc.net/demos).

## Build from source

(The following assumes you are using a regular-ish Linux distribution)

1. Make sure you have [GHCJS](https://github.com/GHCJS/GHCJS) and
   ```cabal-install``` (with version >=1.22.2) in your path See the
   GHCJS link for instructions on installing it (it's not that hard
   anymore!).

2. Clone this project and run ```make``` in the project directory.

3. The HTML and JavaScript has hopefully been placed in ```www/```.

## Featuring...

### [Tree Rotations](http://octalsrc.net/demos/rotate.html)

Match the trees as fast as you can using rotation operations.  It's
kind of a game!  You can even challenge your friends to a race by
sharing a seed value to play on identical trees.

### [Heap Management](http://octalsrc.net/demos/heap.html)

Keep the heap valid by performing *upheap* and *downheap* operations
as you insert and remove elements.  How big can you grow your heap
before you mess up?

### [Heap Construction](http://octalsrc.net/demos/twoheaps.html)

Compare the efficiency of heap construction using *upheap* operations
and using *downheap* operations by building two heaps from the same
source tree.
