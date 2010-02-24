#!/usr/bin/perl
#
# xml2sexp.pl - convert XML to S-expression Meta Language (SML)
#
# Usage: xml2sexp.pl somefile.xml > somefile.sml
#
# lib/sml.arc has functions for converting SML back to XML
#
# Copyright (c) 2010 Jeffrey D. Brennan
# 
# License: http://www.opensource.org/licenses/mit-license.php
#
$/ = '\0';
$_ = <>;

s/<!--.*?-->//sg; # Remove XML comments
s/<\s*([\w:-]+)\s+(.+?)>/formatat($1,$2)/sge;
s/>(.*?)</formattext($1)/sge;
s|/>|) |g;
s|</[^>]*>|) |sg;
s/</(/g;
s/\?>/) /g;
s/>/ /g;
s/&lt;/</g;
s/&gt;/>/g;
s/&amp;/&/g;

print;

sub formatat {
    my ($tag,$attrs) = @_;
    $attrs =~ s/=/ /g;
    $attrs =~ s|/$|)|g;
    $attrs =~ s/"(.*?)"/formatstr($1)/ge;
    return "<$tag (@ " . $attrs . ")>";
}

sub formattext {
    my ($s) = @_;
    if ($s !~ /^\s*$/) {
	$s = formatstr($s) . " ";
    }
    return ">" . $s . "<";
}

sub formatstr {
    my ($s) = @_;
    $s =~ s/&#x(..);/pack("c",hex($1))/ge;
    $s =~ s/&#(\d+);/pack("c",int($1))/ge;
    $s =~ s/\\/\\\\/g;
    $s =~ s/"/\\"/g;
    return "\"$s\"";
}
