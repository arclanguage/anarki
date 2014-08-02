Anarki: a publicly modifiable 'wiki-like' fork of PG's and RTM's Arc Lisp (http://www.paulgraham.com/arc.html)

The intention is to be extremely permissive in accepting patches. For commit
privileges, just submit your first pull request or ask on http://arclanguage.org/forum.

*Important*: If you are already running a HN-like site, migrating to this fork
might mess up your site's data. Come talk to us first, and be sure to make a
backup before the migration.

Anarki is explicitly not constrained to maintain compatibility with pg's
releases, and compatibility status may swing wildly from commit to commit
depending on who takes an interest.

To run, install Racket from http://racket-lang.org, then:

    $ git clone http://github.com/arclanguage/anarki
    $ cd anarki
    $ ./arc  # interactive repl
    arc> :a
    $

Anarki has thorough automated tests. To run them, install mercurial, then:

    # start in the anarki directory
    $ hg clone https://bitbucket.org/zck/unit-test.arc
    $ ./arc
    arc> (load "tests.arc")

If you run into trouble: http://sites.google.com/site/arclanguagewiki; http://arclanguage.org/forum

To run the HN server: lib/how-to-run-news
