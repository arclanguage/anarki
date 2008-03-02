#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 1;

use File::Spec;

my $program_name =
    File::Spec->catfile(
        File::Spec->curdir(),
        "t", "files", "simple-loop.arc"
    );

# TEST
is(scalar(`bash arc.sh $program_name`),
   "Hello 1\nHello 2\nHello 3\nHello 4\nHello 5\n",
   "Testing that running a program using arc.sh works"
);
