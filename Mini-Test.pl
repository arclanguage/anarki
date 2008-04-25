#!/usr/bin/perl 
#===============================================================================
#
#         FILE:  Mini-Test.pl
#
#        USAGE:  ./Mini-Test.pl 
#
#  DESCRIPTION:  A minimalistic test harness for Arc using TAP::Parser
#
#      OPTIONS:  ---
# REQUIREMENTS:  ---
#         BUGS:  ---
#        NOTES:  ---
#       AUTHOR:  Shlomi Fish (SHLOMIF), <shlomif@iglu.org.il>
#      COMPANY:  None
#      VERSION:  1.0
#      CREATED:  25/04/08 22:18:23 IDT
#     REVISION:  ---
#===============================================================================

use strict;
use warnings;

use TAP::Parser;
use File::Glob qw(glob);

my $some_failed = 0;
foreach my $test_file (glob("t/*.t"))
{
    my $parser;
    if ($test_file =~ m{\.arc\.t})
    {
        $parser = TAP::Parser->new( 
            { exec => [ "bash", "arc.sh", $test_file,], },
        );
    }
    else
    {
        # A Perl Script.
        $parser = TAP::Parser->new(
            { source => $test_file, },
        );
    }
    print "$test_file\n";
    # Process all results.
    while (my $result = $parser->next())
    {
        if (! $result->is_ok())
        {
            print $result->as_string(), "\n";
            $some_failed++;
        }
    }
    if (my @failed = $parser->failed())
    {
        print STDERR "${test_file} failed: " . join(",",@failed) . "\n";
    }
}
if ($some_failed)
{
    print STDERR "\n\n\-----\n\n";
    die "$some_failed tests failed!\n";
}
exit(0);
