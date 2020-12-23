# unit-test.arc

A unit test library for the [Arc](http://www.arclanguage.org/) programming language. It lets test suites be written and run simply.

_(This directory is a copy of [https://hg.sr.ht/~zck/unit-test.arc](https://hg.sr.ht/~zck/unit-test.arc).)_

## Quickstart

Let's get started and make some unit tests!

### Defining a suite

Each test must be defined inside a suite, to group together similar tests. Suites can also hold other suites, to allow for logical grouping of suites.

To declare a suite, give it a name, then a declare a bunch of tests. Tests are written as: `(test your-test-name your-test-body)` Use asserts (explained below) when you want to throw if the two things aren't equal to each other.

    (suite math
           (test this-will-pass (assert-same 4 (+ 2 2)))
           (test this-will-fail (assert-same 3 (+ 2 2))))


### Running tests

Tests don't do you any good if you can't run them! Sure, you wouldn't have any failures, but you wouldn't have any passes either.

    arc> (test math)
    Suite math:
    math.this-will-fail failed: (+ 2 2) should be 3 but instead was 4
    math.this-will-pass passed!
    In suite math, 1 of 2 tests passed.

    Oh dear, 1 of 2 failed.
    (1 2)

You can run multiple suites in `test`, or even call specific tests by name:

    arc> (test math.this-will-pass)
    Test math.this-will-pass: passed!

    Yay! The single test passed!
    (1 1)

## Asserts

There are four asserts:

* `assert-t` -- Arguments: `(actual (o fail-message))`. Throws when `actual` is nil. Accepts _any_ other value, whether `t`, or any other data type.

* `assert-nil` -- Arguments: `(actual (o fail-message))`. Throws when `actual` is *not* nil.

* `assert-error` -- Arguments: `(actual (o expected-error))`. Throws when `actual` does *not* error. If `expected-error` is given, also asserts that the error details is the same as `expected-error`.

* `assert-same` -- Arguments: `(expected actual (o fail-message))`. Throws if the `actual` value is not `expected`. `fail-message` is optional, and is used when you want to give more information about a failure.

Note that `assert-same` requires the expected value *before* the test-value. This is needed for pretty, pretty messages when the assert fails:

    arc> (assert-same 4 (- 2 2))
    Error: "(- 2 2) should be 4 but instead was 0"

### Custom error messages

You can also add custom error messages to your asserts. They get appended to the end of the aforementioned pretty, pretty error message.

    arc> (assert-same 42 27 "Those aren't equal?!")
    Error: "27 should be 42 but instead was 27. Those aren't equal?!"

## Nested suites

Suites can be nested, for the sake of organization, and to make them easier to run.

### Defining nested suites

Put a nested suite anywhere inside its parent suite. You can intermingle tests and suites, and it'll deal with it just fine:

    (suite math
           (test numbers-are-equal (assert-same 2 2))
           (suite adding
                  (test good (assert-same 4 (+ 2 2)))
                  (test bad (assert-same 3 (+ 2 2))))
           (test this-test-will-fail (assert-same 3 4))
           (suite subtracting
                  (test good (assert-same 0 (- 2 2)))
                  (test bad (assert-same 0 (- 2 42)))))

### Running nested suites

If you run a suite, it also runs all nested suites inside it.

    arc> (test math)
    Suite math:
    math.this-test-will-fail failed: 4 should be 3 but instead was 4
    math.numbers-are-equal passed!
    In suite math, 1 of 2 tests passed.
        Suite math.subtracting:
    math.subtracting.bad failed: (- 2 42) should be 0 but instead was -40
    math.subtracting.good passed!
    In suite math.subtracting, 1 of 2 tests passed.
        Suite math.adding:
    math.adding.bad failed: (+ 2 2) should be 3 but instead was 4
    math.adding.good passed!
    In suite math.adding, 1 of 2 tests passed.

    Oh dear, 3 of 6 failed.
    (3 6)

If you want to run only one suite that's nested inside another one, that's possible. Just call `test` with the full name of the suite you want to run. The full name is simply the names of all the parents of the suite concatenated together, with a period between them, then the suite's name:

    arc> (test math.adding)
    Suite math.adding:
    math.adding.bad failed: (+ 2 2) should be 3 but instead was 4
    math.adding.good passed!
    In suite math.adding, 1 of 2 tests passed.

    Oh dear, 1 of 2 failed.
    (1 2)

## Setup

If you need to set up some values to share across tests, declare a `setup` form alongside the tests in a suite. Just like a `with` block, insert a list containing `var val` pairs. For example:

    (suite math
           (setup x 6 y 2)
           (test adding-works (assert-same 8
                                           (+ x y)))
           (test multiplying-works (assert-same 12
                                                (* x y))))

    arc> (test math)
    Suite math: all 2 tests passed!

    Yay! All 2 tests passed!
    (2 2)


Under the hood, `setup` uses `withs`, so variables can depend on earlier variables.

    (suite math
           (setup x 3
                  y (+ x 2))
           (test lets-multiply (assert-same 15
                                            (* x y))))

    arc> (test math)
    Suite math: the single test passed!

    Yay! The single test passed!
    (1 1)


## Testing macros

Macros can be tested just like functions. The macro won't be expanded until the test is run, so the you can change it, re-run the test, and the test will use the up-to-date definition.

## Rerunning the last set of suites ran

You can rerun the last set of suites you ran with `(retest)`:

    arc> (retest)
    Suite math: the single test passed!

    Yay! The single test passed!
    (1 1)

## Suite management

### Examining suites

Often, it is useful to know what suites there are. Using the following suite:

    (suite math
           (test numbers-are-equal (assert-same 2 2))
           (suite adding
                  (test good (assert-same 4 (+ 2 2)))
                  (test bad (assert-same 3 (+ 2 2))))
           (test this-test-will-fail (assert-same 3 4))
           (suite subtracting
                  (test good (assert-same 0 (- 2 2)))
                  (test bad (assert-same 0 (- 2 42)))))

We can introspect with `list-suites`:

    arc> (list-suites)
    Here are all the suites that can be run.
    Each nested suite is indented under its parent.

    math: 2 tests
        subtracting: 2 tests
        adding: 2 tests
    nil

### Deleting suites

If you want to delete a suite or test, pass its name to `wipe-tests`:

    arc> (wipe-tests math.subtracting)
    nil
    arc> (list-suites)
    Here are all the suites that can be run.
    Each nested suite is indented under its parent.

    math: 2 tests
        adding: 2 tests
    nil
