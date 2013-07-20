# TST - Erlang ternary search tree

[![Build Status](https://travis-ci.org/blt/tst.png)](https://travis-ci.org/blt/tst)

The [ternary search tree](http://en.wikipedia.org/wiki/Ternary_search_tree) is a
prefix binary search tree. Compared to similar tries, it is more memory
efficient at the cost of execution speed.

This library provides an Erlang module for creating, manipulating and searching
in-memory TSTs. The following operations are supported:

  * membership testing (`tst:contains/2`)
  * insertion (`tst:insert/2`)
  * partial match searches (`tst:partial_matches/2`)
  * near neighbor searches (`tst:near_neighbors/3`)
  * word size of given tree (`tst:wc/1`)
  * coercion from list of strings (`tst:from_list/1`)
  * coercion to list of strings (`tst:to_list/1`)

Binaries are not supported, though this is an intended feature. This TST is
case-sensitive. The words "moose", "Moose" and "MoOsE" are all considered
distinct words.

## Example

```
> erl -pa ebin
Erlang R15B03 (erts-5.9.3) [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9.3  (abort with ^G)
1> TST = tst:from_list(["moose", "goose", "house", "computer"]).
...
2> tst:contains("moose", TST).
true
3> tst:partial_matches(".oose", TST).
["goose","moose"]
4> tst:partial_matches(".o.se", TST).
["goose","house","moose"]
5> tst:to_list(TST).
["computer","goose","house","moose"]
6> tst:to_list(tst:insert("abba", TST)).
["abba","computer","goose","house","moose"]
```

Please see in-line documentation and in-line test cases for more.

## Future Work

Because `tst` uses Erlang strings--lists of machine words as
character--internally to represent strings the search tree will consume more
memory than reading the source words into memory as binaries would. Consider:

```
> du -sh priv/words
priv/words    2.4M
```

The test suite reads this file in to run benchmarks; the size of the TST
generated from `priv/words` is reported in megabytes.

```
src/tst.erl:459:<0.50.0>: TST size :: 42.339 megabytes
```

Future work will attempt to get the resulting TST to consume no more than the
same amount of memory as an on-disk representation.

- - -

This data-structure is implemented from the discussion given in Bentley and
Sedgewick's
[Fast Algorithsm for Searching and Sorting](http://www.cs.tufts.edu/~nr/comp150fp/archive/bob-sedgewick/fast-strings.pdf).
It is released under the MIT license.
