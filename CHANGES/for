Anarki renames Arc 3.1's `for` loop to `up` (in symmetry with the
existing `down`) and makes `for` more C like.

For example, consider this `for` loop in Arc 3.1 (from http://arclanguage.org/tut.txt)
    arc> (for i 1 10
           (pr i " "))
    1 2 3 4 5 6 7 8 9 10 nil

To translate it to Anarki, use `up`:
    arc> (up i 1 10
           (pr i " "))
    1 2 3 4 5 6 7 8 9 10 nil

The corresponding version using Anarki's `for` would be:
    arc> (for i 1 (<= i 10) ++.i
           (pr i " "))
    1 2 3 4 5 6 7 8 9 10 nil

Type `(help for)` at the arc prompt for more details.
