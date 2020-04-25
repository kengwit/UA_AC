#!/usr/bin/perl
#
# index.pl - generate an HTML index for results in a directory
# No support is provided for this script.
#
# Copyright (C) 1999-2005 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# Author: Jason Glick
#
# $Id: index.pl 1416 2005-06-15 17:42:28Z cloyce $
#

use IO::File;

my $year = 2000;
my $suite = 'CPU'.$year;
my $lcsuite = lc($suite);

my $output = 'HEADER.html';

my $metric = 'spec.${lcsuite}.units';

my %fields = (
    'spec.${lcsuite}.company_name' => 0,
    'spec.${lcsuite}.hw_model'     => 1,
    'spec.${lcsuite}.hw_ncpu'      => 2,
    'spec.${lcsuite}.basemean'     => 3,
    'spec.${lcsuite}.peakmean'     => 4,
    $metric                     => 5,
);

my @fieldinfo = (
#      Field name         Align.   Font size adj
    [ 'Company Name',     'left',   -1],
    [ 'System Name',      'left',   -1],
    [ '#CPU',             'right',  -1],
    [ 'Base',             'right',  -1],
    [ 'Peak',             'right',  -1],
    [ 'Full Disclosures', 'right',  -1],
);

my $fields_display_cutoff = $fields{'spec.${lcsuite}.peakmean'};

my %roundfields = (
    'spec.${lcsuite}.basemean' => 1,
    'spec.${lcsuite}.peakmean' => 1
);

my @metrics = (
    'SPECint',
    'SPECfp',
    'SPECint_rate',
    'SPECfp_rate'
);

foreach my $m ( @metrics ) {
    $$m = {};
    $count{$m} = 0;
}

my @formats = (
    { 'Text'     => 'txt'  },
    { 'HTML'     => 'html' },
    { 'PDF'      => 'pdf'  },
    { 'PS'       => 'ps'   },
    { 'Config'   => 'cfg'  },
    { 'Raw'      => 'raw'  }
);


foreach my $sub ( glob( '*.sub' ) ) {
    my $ifh = new IO::File "<$sub"; # Closed when we leave scope
    next unless defined($ifh);

    my @f = ();       # This stores fields as they're read out
    my %f = %fields;  # This is to keep track of which fields we've seen

    foreach my $line ( <$ifh> ) {
        last unless %f;
        foreach my $key ( keys %f ) {
            if ( $line =~ /^$key: (.*)/ ) {
                my $val = $1;
                $val = round( $val ) if ( exists $roundfields{$key} );
                $f[$fields{$key}] = $val;
                delete( $f{$key} );
            }
        }
    }

    my $m = $f[$fields{$metric}];
    $$m->{"$f[0]:$f[1]:$sub"} = [@f];
    $count{$m}++;
}

my $total = 0;

foreach ( values( %count ) ) {
    $total += $_;
}


my $ofh = new IO::File ">$output";
die( "ERROR: cannot open $output for writing: $!\nStopped" ) unless (defined $ofh);

print $ofh "<HTML><BODY>\n";
print $ofh "<H1>SPEC $suite Submissions ($total):</H1>\n";

print $ofh "<P>|";
foreach (@metrics) {
    print $ofh " <A HREF=\"#$_\">$_</A> |";
}

print $ofh "</P><HR>\n";

foreach $m ( @metrics ) {

    next unless $count{$m};

    print $ofh "<A NAME=$m>\n";
    print $ofh "<H2>$m ($count{$m}):</H2>\n";

    print $ofh "<TABLE BORDER=0 CELLSPACING=4 CELLPADDING=1 WIDTH=100%>\n";
    print $ofh " <TR BGCOLOR=#fffbdc>\n";

    my $i = 0;

    foreach (@fieldinfo) {
        my ($name, $align, $size) = @{$_};
        print $ofh "  <TH NOWRAP ALIGN=$align>$name</TH>\n";
    }
    print $ofh " </TR>\n";

    foreach my $sub ( sort( keys( %{$$m} ) ) ) {

        $i = 0;

        print $ofh " <TR>\n";

        foreach ( @{$$m->{$sub}} ) {
            next if $i > $fields_display_cutoff;
            my ($name, $align, $size) = @{$fieldinfo[$i]};
            print $ofh "  <TD ALIGN=$align>";
            if ($size) {
              print $ofh "<FONT SIZE=$size>$_</FONT>";
            } else {
              print $ofh $_;
            }
            print $ofh "</TD>\n";
            $i++;
        }

        my ($name, $align, $size) = @{$fieldinfo[$i]};
        print $ofh "  <TD ALIGN=$align>";
        print $ofh "<FONT SIZE=$size>" if $size;
        print $ofh "&nbsp;|&nbsp;";

        $sub =~ s/.+:.+:(.+)\.sub$/$1/o;

        foreach $format ( @formats ) {
            while ( my ( $name, $ext ) = each %{$format} ) {
                my $file = "$sub.$ext";
                if ( -e $file ) {
                    print $ofh "<A HREF=\"$file\">$name</A>&nbsp;|&nbsp;";
                } else {
                    print $ofh "$name&nbsp;|&nbsp;";
                }
            }
        }
        print $ofh "</FONT>" if $size;
        print $ofh "</TD>\n </TR>\n";
    
    }

    print $ofh "</TABLE>\n";
#   print $ofh "<P><HR>\n" unless ($count{$m} > 0);

}

chomp( my $date = `/bin/date` );

print $ofh "<P><HR>Last updated: <EM>$date</EM><BR>\n";
print $ofh "<ADDRESS>SPEC Editor, 
	<A HREF=\"mailto:editor\@spec.org\">editor\@spec.org</A></ADDRESS>\n";

$ofh->close();

sub round {
    my( $number ) = @_;
    return $number if ( $number eq '--' );
    return sprintf( "%.0f", $number );
}

