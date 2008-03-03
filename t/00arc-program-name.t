#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 2;

use File::Spec;

sub get_prog_path
{
    my $prog = shift;
    return 
        File::Spec->catfile(
            File::Spec->curdir(),
            "t", "files", "$prog.arc"
        );
}

my $loop = get_prog_path("simple-loop");

# TEST
is (scalar(`bash arc.sh $loop`),
    "Hello 1\nHello 2\nHello 3\nHello 4\nHello 5\n",
    "Testing that running a program using arc.sh works"
);

my $two_stmts = get_prog_path("two-statements");

# TEST
is (scalar(`bash arc.sh $two_stmts`),
    "One\nTwo\n",
    "Testing that multiple separate statements in the same file work."
);

