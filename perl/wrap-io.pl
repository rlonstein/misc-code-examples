#!/opt/local/bin/perl
# $Id: wrap-io.pl,v 1.1 2005/09/30 18:47:24 lonstein Exp lonstein $
#
# Clever (but incomplete) hack for timestamping on an existing filehandle.
# Useful for dealing with existing code that blats to STDERR/STDOUT
#

package StupidTimeStamper;

use strict;
use warnings;
use Tie::Handle;
use File::Basename;
use English qw(-no_match_vars);
use Carp qw(carp croak confess);

sub wrap {
    my $fh = shift;
    my $fh_name = $fh;
    $fh_name =~ s/^\*//;
    local *MYFH;
    open(MYFH, ">&$fh_name") or confess("Failed to dupe [$fh_name]: $ERRNO");
    tie(*$fh,'StupidTimeStamper', *MYFH);
}

sub TIEHANDLE {
    my ($class,$fh) = @_;
    my $self = bless [$fh], $class;
    return $self;
}

sub PRINT {
    my $self = shift;
    my $fh  = $self->[0];
    print {$fh} _tstamp(), ' ', @_;
}

sub PRINTF {
    my $self = shift;
    my $fh   = $self->[0];
    my ($fmt, @rest) = @_;
    print {$fh} _tstamp(), ' ', sprintf($fmt, @rest);
}

sub OPEN {
    my $self = shift;
    my $fh = $self->[0];
    return $fh;
}

sub _tstamp {
    my @t = localtime();
    return sprintf("[%4d/%02d/%02d %02d:%02d:%02d]",
                   $t[5]+1900,@t[4,3,2,1,0]);
}

##################################################################

package main;

use strict;
use warnings;
use English qw(-no_match_vars);
use Carp;

StupidTimeStamper::wrap(*STDERR);

print {*STDERR} "This is a print test\n";
printf {*STDERR} "This is a printf test [%d]\n", 1234;
warn "This too is a test\n";

open(my $err, ">&*STDERR") || croak($ERRNO);
print {$err} "This re-opened handle is NOT wrapped\n";

print "This goes to STDOUT unmolested.\n";

carp("What happened here?!");
croak("I'm melting! Oh, what at world...");

exit 0;
