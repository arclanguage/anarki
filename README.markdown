Anarki: a publicly modifiable 'wiki-like' fork of Arc Lisp (http://www.paulgraham.com/arc.html)

The intention is to be extremely permissive in accepting patches. For commit
privileges, just submit your first pull request or ask on http://arclanguage.org/forum.

*Important*: If you are already running a HN-like site, migrating to this fork
might mess up your site's data. Come [talk to us](http://arclanguage.org/forum)
first, and be sure to make a backup before the migration.

To run, install Racket from http://racket-lang.org, then:

    $ git clone http://github.com/arclanguage/anarki
    $ cd anarki
    $ ./arc  # interactive repl
    arc> (quit)
    $

Anarki has thorough automated tests. To run them, install mercurial, then:

    # start in the anarki directory
    $ hg clone https://bitbucket.org/zck/unit-test.arc
    $ ./arc
    arc> (load "tests.arc")

Anarki is explicitly not constrained to maintain compatibility with upstream
releases, and compatibility status may swing wildly from commit to commit
depending on who takes an interest. To see a list of what's different, type
`(incompatibilities)` at the repl. If you make an incompatible change, please
update this list.

If you run into trouble: http://sites.google.com/site/arclanguagewiki; http://arclanguage.org/forum

---

To run the HN server, first pick your (the admin's) username:

    $ mkdir www
    $ echo __username__ > www/admins

(You can have multiple admins. Add them all to www/admins, separated by whitespace.)

Now bring up the server:

    $ ./run-news

There'll be a pause while the server loads up, with some messages, then you'll
see the 'arc> ' prompt.

Go to [http://localhost:8080](http://localhost:8080). Click on login, and
create the account for your username. You should now be logged in as an admin.

Don't worry about "user break" or "error writing" messages.

To customize News, change the variables at the top of lib/news.arc. To change the
port your server runs at, modify lib/run-news.

Any interactive changes to the prompt will be reflected immediately in the
server, without needing to restart it. Don't forget to add them to the .arc
files as appropriate, otherwise they'll be lost when you stop the server or it
dies for some reason.
