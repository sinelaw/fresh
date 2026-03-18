#!/usr/bin/env perl
use strict;
use warnings;

sub greet {
    my ($name) = @_;
    return "Hello, $name!";
}

my $message = greet("World");
print "$message\n";

my @items = (1, 2, 3);
my %config = (key => "value", count => 42);
