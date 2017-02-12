===================
Key Insertion
===================

Insertion cases that need to be handled when inserting keys into the trie. Spitting both keys on shared prefix - 0 indicates no remainder. Miss is also a Match Type but determined prior to prefix match.

TODO: Check Suffix, Query ordering. Re-implement if necessary

+----------+----------+--------------+
| Suffix   |  Query   | Match Type   |
+----------+----------+--------------+
| (0, 0)   | (dog, 0) | Miss         |
+----------+----------+--------------+
| (dog, 0) | (dog, 0) | Hit          |
+----------+----------+--------------+
| (dog, s) | (dog, 0) | PartialRight |
+----------+----------+--------------+
| (dog, 0) | (dog, s) | PartialLeft  |
+----------+----------+--------------+
| (dog, s) | (dog, e) | Partial      |
+----------+----------+--------------+

Can be reduced to a pair of booleans on the remainder of the key split being empty.

+--------+-------+--------------+
| Suffix | Query | Match Type   |
+--------+-------+--------------+
| true   | true  | Hit          |
+--------+-------+--------------+
| false  | true  | PartialRight |
+--------+-------+--------------+
| true   | false | PartialLeft  |
+--------+-------+--------------+
| false  | false | Partial      |
+--------+-------+--------------+

Match Types determine how the trie is altered.

+-------------------+-----------------+
| Miss -->          | Append          |
+-------------------+-----------------+
| Hit -->           | Explore         |
+-------------------+-----------------+
| Partial -->       | Expand          |
+-------------------+-----------------+
| PartialLeft -->   | Expand          |
+-------------------+-----------------+
| PartialRight -->  | ExploreOrExpand |
+-------------------+-----------------+

Some Examples

::

  /**
   * Partial(id, (an, d) t) - remainders on both query and suffix
   *
   * => Always Expand
   *
   *  (and)   put(ant,789) =>   (an)         (px.h._1)
   *    |                       /  \          /      \
   *   456                   (d)    (t)    (px.h._2)(px.t)
   *                          |      |        |       |
   *                         456    789     px.id    789
   *
   * Partial(id, (an, d) t) - with existing TS
   *
   *     (and)   put(ant,123) =>  (an)               (px.h._1)
   *     /   \                    /  \               /      \
   *   (TS) (over)              (d)  (t)        (px.h._2) (px.t)
   *    |     |                /   \    \           |        |
   *   456   789            (TS) (over) 123      (px.id)    123
   *                          |     |             |    |
   *                         456   789           456  789
   *
   * PartialLeft(id, (an, d) TS) - remainder on existing suffix
   *
   * => Always Expand
   *
   *  (and)   put(an,789) =>   (an)           (px.h._1)
   *    |                      /  \            /     \
   *   456                  (d)   (TS)     (px.h._2) (px.t)
   *                         |      |          |       |
   *                        456    789       px.id    789
   *
   * PartialLeft(id, (a, n) TS) - with existing TS
   *
   *     (an)    put(a, 345) =>  (a)             (px.h._1)
   *     /  \                   /   \             /     \
   *  (d)   (TS)              (n)   (TS)     (px.h._2)  (px.t)
   *   |      |               / \      \        / \       \
   *  456    789           (d)  (TS)   345    (px.id)     345
   *                        |     |           |     |
   *                       456   789         456   789
   *
   * PartialRight(id, (and, TS) over) - remainder on query
   *
   *  (and)  put(andover,789) =>  (and)          (px.h._1)
   *    |                         /  \             /    \
   *   456                     (TS)  (over)   (px.h._2) (px.t)
   *                             |     |          |       |
   *                            456   789       px.id    789
   *
   * Miss - append a non-matching key
   *
   * When a new key does not match any existing suffixes we can simply append
   * a new leaf onto this node
   *
   *       (and)      append(dig,789) =>     (and dig)
   *         |                                /     \
   *        456        
