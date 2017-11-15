# Anarki [![Travis build](https://travis-ci.org/arclanguage/anarki.svg?branch=master)](https://travis-ci.org/arclanguage/anarki)
## A publicly modifiable 'wiki-like' fork of [Arc Lisp](http://www.paulgraham.com/arc.html)

The intention is to be *extremely permissive* in accepting patches. For commit
privileges, just submit your first pull request or ask on [**Arc Language Forum**](http://arclanguage.org/forum).

## Getting started

### Installation

First [install **Racket** (v6.8 or later)](http://racket-lang.org), then

    $ git clone http://github.com/arclanguage/anarki
    $ cd anarki
    $ ./arc.sh    # start the interactive repl
    $ ./run-news  # or start News (also with the repl)

### Tutorial

If you're new to **Arc**, or **Lisp** in general, a good starting point is the [**Arc Language Tutorial**](http://www.arclanguage.org/tut.txt).


### Documentation and help

If you run into trouble, check

  * [**Arc Language Forum**](http://arclanguage.org/forum)
  * [**Arc Language Wiki**](http://sites.google.com/site/arclanguagewiki)
  * [**Arc Language Documentation**]( https://arclanguage.github.io/ref)
  * **Arc's built-in help system** by typing `(help)` at the repl

## News Site

**Anarki** comes bundled with **News**, a [**Hacker News**](https://news.ycombinator.com) style app.
 

### Admins

To run **News**, first pick your (the admin's) username,

    $ cd anarki
    $ mkdir www
    $ echo __username__ > www/admins

You can have multiple admins. Add them all to `www/admins`, separated by whitespace.


### Starting the server
 
**Warning**: *If you are already running a **News** site, migrating to this fork
might mess up your site's data. Come [talk to us](http://arclanguage.org/forum)
first, and be sure to make a backup before the migration.*

Now bring up the server,

    $ ./run-news

There'll be a pause while the server loads up, with some messages, then you'll
see the `arc>` prompt.

Go to [http://localhost:8080](http://localhost:8080). Click on **login**, and
create the account for your username. You should now be logged in as an admin.

(Don't worry about `user break` or `error writing` messages.)


### Customization

To customize **News**, change the variables at the top of `lib/news.arc`. To change the port your server runs at, modify `lib/run-news.arc`.

Any interactive changes to the prompt will be reflected immediately in the server, without needing to restart it. Hence if you make any changes to `lib/news.arc`, you can load them, by typing `(load "lib/news.arc)` in the repl. If you stop the server or it dies for some reason, previously entered commands are available in the command history (at least if you have **Readline** installed).


### HTTPS

You may want to serve **News** securely over **HTTPS**. An example configuration for running **Nginx** with **SSL** as a reverse proxy for **News** can be found in `anarki/extras/news.nginx.conf`.


### Windows specific

The **News** server will use the `openssl` command. If you're on Windows, first install **OpenSSL** and make sure it's on your `PATH`. Other systems should be distributed with **OpenSSL** already.


## Development

### Tests

**Anarki** has thorough automated tests. To run them, install [**Mercurial**](https://www.mercurial-scm.org), then,

    # start in the anarki directory
    $ hg clone https://bitbucket.org/zck/unit-test.arc
    $ ./arc.sh
    arc> (load "tests.arc")

**Anarki** is explicitly not constrained to maintain compatibility with upstream
releases, and compatibility status may swing wildly from commit to commit
depending on who takes an interest. To see a list of what's different, type
`(incompatibilities)` at the repl. If you make an incompatible change, please
update this list.

### Racket interop

**Racket** expressions can be evaluated with the `($ ...)` syntax. For instance, `(($ crypto-random-bytes) 16)` obtains the `crypto-random-bytes` function from **Racket** and makes a call to it.

If you write lots of **Racket** code and you just want to use **Anarki** for part of a bigger project, you can `raco pkg install anarki` and use **Anarki** to write **Racket** modules:

    #lang anarki
    (:provide function-for-use-in-racket)
    
    (= racket-import ($.dynamic-require ...))
    (load "relative/path.arc")
    
    (def utility-for-use-in-racket (x)
      (* x (racket-import x))

Note that **Anarki** does not have private module scopes; the `(:provide ...)` section is just there to make it easy to `require` an **Anarki**-based library from **Racket**.


