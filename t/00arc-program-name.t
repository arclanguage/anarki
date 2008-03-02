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

print $program_name;

