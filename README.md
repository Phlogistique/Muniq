Muniq
=====

This program converts a list of items into a tree exhibiting patterns of
repetitions into the list. The names comes from the 'uniq' utility from
UNIX. The M is for "multi-line".

For example, the list
 1 2 1 2 4 1 2 1 2 4 5
could become
 2×[ 2×[ 1 2 ] 4 ] 5

However, doing this is non-trivial, as there may be many ways to combine
patterns to describe the same list. The program shall implement several
strategies:

* Noop: does not detect any pattern
* Big-to-small: Always use the longest patterns
* Small-to-big: Always use the smallest patterns
* Efficiency-first: Always use the patterns that result in the list being
  the most shortened.
* Brute-force: Find the shortest description, no matter the cost.
