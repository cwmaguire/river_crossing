river_crossing
==============

This is my solution, in Erlang, to the following problem, including the
bonus parts:

------------------------------------------------------------------------

A farmer is travelling with his dog, a chicken, and a bag of grain.  He comes to a river, which has a small ferry boat.  The boat has room for the farmer, and one other item (dog, chicken, or grain).  The farmer knows the dog will eat the chicken if left alone with it; likewise, the chicken will eat the grain.  How does he get everything to the other side uneaten?

The current state of the puzzle can be represented by a string:
* f   farmer
* d   dog
* c   chicken
* g   grain
* ~   the river

And moves by using an arrow: xy> or <xy

```
fdcg~
fc>
dg~fc
```

Your program should take an initial state, and a sequence of moves, one per line.  Compute the final state resulting from applying the moves, or stop whenever something gets eaten.

Bonus parts:
* generalize -- use a list of items, and disallowed pairings (e.g. c eats g)
* solve -- generate a winning sequence of moves

Sample Input 1:
```
fdcg~
fc>
<f
fd>
```

Sample Output 1:
```
g~fcd
```

Sample Input 2:
```
fdcg~
fc>
<f
fg>
<f
```

Sample Output 2:
```
fd~cg
Grain was eaten.
```
