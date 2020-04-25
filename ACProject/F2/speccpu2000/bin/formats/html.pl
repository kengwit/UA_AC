#
#  html.pl - produces HTML output
#  Copyright (C) 1999-2005 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Author:  Cloyce D. Spradling
#
# $Id: html.pl 1422 2005-06-15 18:37:20Z cloyce $

use strict;
use File::Basename;
use MIME::Base64;
use GD;
#use Data::Dumper;

use vars qw($name $extension);

$name      = 'HTML';
$extension = 'html';

my @invalid_gif = ();		# Forward reference.  See end of file.
my $prefix = $Spec::Format::raw::prefix;
my $html_version = 1.01;
my $image_path = (defined($::cpu2000_website_formatter) &&
		  $::cpu2000_website_formatter) ? $::image_path : 'images/';
my $image_url = (defined($::cpu2000_website_formatter) &&
		 $::cpu2000_website_formatter) ? $::image_url_path : 'images/';

my $logo_location = "logo037.gif";
my $basebar = "basebar.gif";
my $peakbar = "peakbar.gif";
my @logo_size = (37,55);	# We could use Image::Size,
                                # but it's not going to change.
my $table_width = 300;		# Table height will be figgerd out later
my $written = [];		# List of files written
my $debug = 0;

sub format {
    my ($me, $r, $fn) = @_;
    $written = [];

    return HTML_report($me, $r, $fn);
}

sub HTML_report {
    my ($me, $r, $fn) = @_;
    my @output = ();
    my $tmpstr = '';
    my $invgif = '';
    my $invalid = $r->{'invalid'} ||
	((ref($r->{'errors'}) eq 'ARRAY') && @{$r->{'errors'}});
    my ($barename, $outputpath) = fileparse($fn, ".$extension");
    $outputpath = '' if ($outputpath =~ /^\.[\/\\]$/o ||
			 (defined($::cpu2000_website_formatter) &&
			  $::cpu2000_website_formatter));

    # Make sure the images directory exists, in case we have to write
    # something in it.
    if (!-d "$outputpath$image_path") {
	mkdir "$outputpath$image_path", 0777;
    }
    # Make sure that the base and peak bar images exist.  If not, create them.
    if (!-f "$outputpath$image_path$basebar") {
	if (makebar($outputpath.$image_path.$basebar, $table_width, 10, 0,0,0)) {
	    push @{$written}, $image_path.$basebar;
	}
    }
    if (!-f "$outputpath$image_path$peakbar") {
	if (makebar($outputpath.$image_path.$peakbar, $table_width, 10, 127,127,127)) {
	    push @{$written}, $image_path.$peakbar;
	}
    }

    if ($invalid) {		# Make the nice background
	if (!-f "$outputpath${image_path}invalid.gif") {
	    my $igfh = new IO::File (">$outputpath${image_path}invalid.gif");
	    if (!defined($igfh)) {
		::Log(0, "Couldn't open $outputpath${image_path}invalid.gif for output: $!\n");
	    } else {
		print $igfh decode_base64(join('', @invalid_gif));
		$igfh->close();
		push @{$written}, $outputpath.$image_path.'invalid.gif';
	    }
	}
	$invgif = qq| BACKGROUND="${image_url}invalid.gif"|;
    }
    
    $invgif = qq| BACKGROUND="${image_url}nc.gif"| if (@{$r->{'nc'}}+0);

    push @output, HTML_head($r, $invalid);
    # Start the body
    push @output, '<BODY TEXT="#000000" BGCOLOR="#F7F7F7" LINK="#0000FF" VLINK="#0000FF" ALINK="#ff0000"'.$invgif.'>';

    # Make the top title bar thing
    push @output, HTML_title($r, $invalid);

    # Make the result bar
    push @output, HTML_result($r);

    # Make the info bar that shows the dates
    push @output, HTML_date($r);

    if (@{$r->{'nc'}}+0) {
	# Make the NC explanation section
	push @output, '<TABLE BORDER=1 CELLSPACING=0 CELLPADDING="25%" WIDTH="100%">';
	push @output, ' <TR>', '    <TD ALIGN="center"><STRONG>';
	push @output, join('<BR>', @{$r->{'nc'}});
	push @output, '    </STRONG></TD>', ' </TR>', '</TABLE>';
    }

    # Make the table of results and the graph
    push @output, HTML_table($r, $outputpath, $barename), '<BR CLEAR="ALL">';

    # Make the hardware and software tables
    push @output, HTML_info($r);

    # Do notes & errors
    push @output, HTML_notes($r), '<BR CLEAR="ALL">';

    push @output, '<DIV ALIGN="LEFT">';
    push @output, '<HR NOSHADE>';
    push @output, 'For questions about this result, please contact the tester.<BR>';
    push @output, 'For other inquiries, please contact <A HREF="mailto:webmaster@spec.org">webmaster@spec.org</A><BR>';
    push @output, 'Copyright &copy; 1999-2005 Standard Performance Evaluation Corporation<BR>';
    push @output, "Generated on ".&::ctime(time)." by SPEC CPU2000 HTML formatter v$html_version<BR>";
    push @output, '</DIV>';
    if (@{$r->{'nc'}}+0 == 0) {
      push @output, '<!-- The following is an encoded version of the raw file that was used to', '     produce this result. Use the extract_raw script to retrieve it. -->';
      if (defined($r->{'compraw'})) {
          push @output, "<!-- BEGIN GZIP $barename.raw", $r->{'compraw'};
      } elsif (defined($r->{'rawfile'})) {
          push @output, "<!-- BEGIN $barename.raw", $r->{'rawfile'};
      } else {
          push @output, '<!-- BEGIN', 'There must have been a problem with the encoding.  This is not a raw file.';
      }
      push @output, 'END -->';
    }
    push @output, qw(</BODY> </HTML>);

    foreach my $line (@output) {
	$line =~ tr/\015\012//d; # More reliable than the double chomp
    }

    return (\@output, $written);
}

sub HTML_table($$$) {
    # Make the table of results
    my ($r, $path, $name) = @_;
    my ($maxlen, $maxratio, $min_peak_dp, $min_base_dp) = (0, 0, 500, 500);
    my $font = gdSmallFont;
    my ($twidth, $theight) = ($table_width, 13 + $font->height);
    my @output = ();
    my $tmpstr = '';
    my $skip = 0;
    my $graph_align = 'ALIGN="LEFT"';
    $graph_align = 'ALIGN="RIGHT"' if $r->rate;
    my @tunes = @{$r->tunelist};
    my $showbase = grep { /^base$/io } @tunes;
    my $showpeak = grep { /^peak$/io } @tunes;
    # Figure out some maximums, the table height, etc
    foreach my $bench (keys %{$r->benchmarks}) {
	my $tmp = $r->ratio($bench, 'base');
	$maxratio = $tmp if ($tmp > $maxratio);
	$tmp = significant($tmp, undef, 1);
	$min_base_dp = $tmp if ($tmp < $min_base_dp);
	$tmp = $r->ratio($bench, 'peak');
	$maxratio = $tmp if ($tmp > $maxratio);
	$tmp = significant($tmp, undef, 1);
	$min_peak_dp = $tmp if ($tmp < $min_peak_dp);
    }
    # Thanks again, Chris
    my $htick_val = 0;
    my $hticks    = 0;
    {
	my $mult = 1;
	my $found = 0;
      find_tickval:
	while (1) {
	    for my $i ( 1, 2, 3, 5 ) {
		$htick_val = $i * $mult;
		for my $j ( 5, 6, 7, 8, 9, 10 ) {
		    $hticks = $j;
		    if ($hticks * $htick_val > $maxratio) {
			last find_tickval;
		    }
		}
	    }
	    $mult *= 10;
	}
    }
    $maxratio = $hticks * $htick_val;
    $maxratio = 0 if (@{$r->{'nc'}}+0);
    $maxratio = 5 unless $maxratio;
    my $scale = $twidth / $maxratio;
    my $fname = '';
    ($maxratio, $fname) = draw_scale($r, $maxratio, $twidth, $theight, $scale,
				     $font, $path, $skip);

    # Recalculate the scale in case draw_scale has changed maxratio
    $scale = $twidth / $maxratio;
    push @output, '<TABLE BORDER=1 CELLSPACING=0 CELLPADDING=0 WIDTH="100%" ALIGN="LEFT">';
    push @output, ' <TR VALIGN="BOTTOM">';
    
    $tmpstr = "  <TD WIDTH=$twidth $graph_align>";
    $tmpstr .= "<IMG SRC=\"$image_url$fname\" BORDER=0 ALIGN=\"BOTTOM\" ALT=\"Graph Scale\">" if defined($fname);
    $tmpstr .= '</TD>';
    push @output, $tmpstr if $r->rate;
    push @output, '  <TH>Benchmark</TH>';
    if ($r->rate) {
	push @output, '  <TH>Base<BR>Copies</TH>';
    } else {
	push @output, '  <TH>Reference<BR>Time</TH>';
    }
    push @output, '  <TH>Base<BR>Runtime</TH>';
    push @output, '  <TH>Base<BR>Ratio</TH>';
    push @output, '  <TH>Copies</TH>' if ($r->rate);
    push @output, '  <TH>Runtime</TH>';
    push @output, '  <TH>Ratio</TH>';
    push @output, $tmpstr unless $r->rate;
    push @output, ' </TR>';
    foreach my $bench (sort keys %{$r->benchmarks}) {
	my $tmp = '';
	my ($ref, $bcopies, $brun, $brat,
	    $pcopies, $prun, $prat) = (
			     $r->reference($bench) || '&nbsp;',
			     $r->copies($bench, 'base') || '&nbsp;',
			     $r->runtime($bench, 'base', 0) || '&nbsp;',
			     $r->ratio($bench, 'base') || '&nbsp;',
			     $r->copies($bench, 'peak') || '&nbsp;',
			     $r->runtime($bench, 'peak', 0) || '&nbsp;',
			     $r->ratio($bench, 'peak') || '&nbsp;',
			     );
	# Now fix up some of the values.
	if ($r->size ne 'ref') {
	    $brat = '--' unless ($brat eq '&nbsp;');
	    $prat = '--' unless ($prat eq '&nbsp;');
	    $ref = '--';
	}
	($brun, $brat) = (pad_rear($brun), pad_rear($brat));
	($prun, $prat) = (pad_rear($prun), pad_rear($prat));
	$brun = 'X' if (($brun ne '&nbsp;') && !$r->valid($bench,'base') && $showbase);
	$prun = 'X' if (($prun ne '&nbsp;') && !$r->valid($bench,'peak') && $showpeak);

	if (@{$r->{'nc'}}+0) {
	    ($brun, $brat, $prun, $prat) = ('NC')x4;
	}
	my $barstr = "  <TD $graph_align WIDTH=$twidth>";
	my $bwidth = int($brat * $scale);
	if (@{$r->{'nc'}}+0 == 0) {
	    $bwidth++ unless $bwidth;
	    ($tmp = $brat) =~ s/(\&nbsp\;)+$//;
	    $barstr .= "<IMG SRC=\"$image_url$basebar\" BORDER=1 WIDTH=$bwidth HEIGHT=10 ALT=\"$bench base result bar ($tmp)\">" if ($brat > 0);
	    $barstr .= "<BR>\n";
	    $bwidth = int($prat * $scale);
	    $bwidth++ unless $bwidth;
	    ($tmp = $prat) =~ s/(\&nbsp\;)+$//;
	    $barstr .= "<IMG SRC=\"$image_url$peakbar\" BORDER=1 WIDTH=$bwidth HEIGHT=10 ALT=\"$bench peak result bar ($tmp)\">" if ($prat > 0);
	} else {
	    $barstr .= '&nbsp;';
	}
	$barstr .= '</TD>';
	push @output, ' <TR>';
	push @output, $barstr if $r->rate;
	push @output, "  <TD BGCOLOR=\"#999999\">$bench</TD>";
	if ($r->rate) {
	    push @output, "  <TD BGCOLOR=\"#999999\">$bcopies</TD>";
	} else {
	    push @output, "  <TD>$ref</TD>";
	}
	push @output, "  <TD BGCOLOR=\"#999999\" ALIGN=\"RIGHT\">$brun</TD><TD BGCOLOR=\"#999999\" ALIGN=\"RIGHT\">$brat</TD>";
	push @output, "  <TD>$pcopies</TD>" if ($r->rate);
	push @output, "  <TD ALIGN=\"RIGHT\">$prun</TD><TD ALIGN=\"RIGHT\">$prat</TD>";
	push @output, $barstr unless $r->rate;
	push @output, ' </TR>';
    }
    push @output, ' <TR>';
    push @output, '  <TD BGCOLOR="#999999">&nbsp;</TD>' if $r->rate;
    push @output, '  <TD ALIGN="LEFT" COLSPAN=3>'.$r->baseunits.'</TD>';
    my $basemean = (@{$r->{'nc'}}+0 == 0) ? pad_rear($r->basemean) : 'NC';
    push @output, '  <TD ALIGN="RIGHT">'.$basemean.'</TD>';
    my $tmp = 3;
    push @output, "  <TD BGCOLOR=\"#999999\" COLSPAN=$tmp>&nbsp;</TD>";
    push @output, ' </TR>';
    push @output, ' <TR>';
    $tmp += ($r->rate) ? 1 : - 1;
    push @output, "  <TD BGCOLOR=\"#999999\" COLSPAN=$tmp>&nbsp;</TD>";
    push @output, '  <TD ALIGN="LEFT" COLSPAN=3>'.$r->peakunits.'</TD>';
    my $peakmean = (@{$r->{'nc'}}+0 == 0) ? pad_rear($r->peakmean) : 'NC';
    push @output, '  <TD ALIGN="RIGHT">'.$peakmean.'</TD>';
    push @output, '  <TD BGCOLOR="#999999">&nbsp;</TD>' unless $r->rate;
    push @output, ' </TR>';
    push @output, '</TABLE>';
    return @output;
}

sub HTML_info {
    # Make the hardware and software tables
    my ($r) = @_;
    my @output = ();
    
    push @output, '<TABLE BORDER=1 CELLSPACING=0 CELLPADDING=0 WIDTH="100%">';
    push @output, ' <TR VALIGN="TOP">';
    for my $foo ([ 'Hardware', $r->hardware ], [ 'Software', $r->software ]) {
	my ($heading, @refs) = @$foo;
	push @output, '  <TD WIDTH="50%">';
	push @output, '  <TABLE BORDER=0 CELLSPACING=0>';
	push @output, "   <TR><TH COLSPAN=2>$heading</TH></TR>";
	foreach my $ref (@refs) {
	    my ($name, @vals) = @$ref;
	    push @output, '   <TR ALIGN="LEFT">';
	    push @output, "    <TH VALIGN=\"TOP\">$name:</TH>";
	    push @output, '    <TD WIDTH="60%">'.join("<BR>\n        ", @vals).'</TD>';
	    push @output, '   </TR>';
	}
	push @output, '  </TABLE>';
	push @output, '  </TD>';
    }
    push @output, ' </TR>';
    push @output, '</TABLE>';
    
    return @output;
}

sub HTML_notes {
    # Print all the notes & errors
    my ($r) = @_;
    my @output = ();

    push @output, '<TABLE BORDER=1 CELLSPACING=0 CELLPADDING=0 WIDTH="100%">';
    push @output, ' <TR>';
    push @output, '  <TH WIDTH="100%">Notes / Tuning Information</TH>';
    push @output, ' </TR>';
    push @output, ' <TR>';
    push @output, '  <TD WIDTH="100%">';
    push @output, '  <PRE>';
    if (ref($r->{'notes'}) eq 'ARRAY') { # And it should...
	push @output, @{$r->{'notes'}};
    }
    push @output, '</PRE>';	# Shouldn't be indented; causes extra line
    push @output, ' </TR>';
    if ((ref($r->{'errors'}) eq 'ARRAY') &&
	@{$r->{'errors'}}) {
	push @output, ' <TR>';
	push @output, '  <TH WIDTH="100%"><FONT COLOR="#FF0000">Errors</FONT></TH>';
	push @output, ' </TR>';
	push @output, ' <TR>';
	push @output, '  <TD WIDTH="100%">';
	push @output, '  <PRE>';
	push @output, @{$r->{'errors'}};
	push @output, '  </PRE>';
	push @output, ' </TR>';
    }
    push @output, '</TABLE>';

    return @output;
}

sub HTML_head {
    my ($r, $invalid) = @_;
    my @output = ();

    push @output, qq|<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN">|;
    push @output, '<HTML>', '<HEAD>';
    push @output, qq| <META NAME="GENERATOR" CONTENT="SPEC CPU2000 Tools $::version (HTML v$html_version)">|;
    push @output, ' <META NAME="AUTHOR" CONTENT="'.$r->{'tester_name'}.'">';
    push @output, sprintf(' <TITLE>%s%s Result: %s %s%s</TITLE>',
			  (($invalid) ? 'Invalid ' : ''),
			  $r->{'metric'}, firstof($r->{'hw_vendor'}),
			  firstof($r->{'hw_model'}),
			  ($r->{'company_name'} !~ /^(|--)$/ &&
			   $r->{'company_name'} ne $r->{'hw_vendor'}) ? " (tested by ".$r->{'company_name'}.")" : '');
    push @output, '</HEAD>', '';
    return @output;
}

sub HTML_title {
    my ($r, $invalid) = @_;
    my @output = ();
    my $tmpstr = '';

    # Make the top title bar thing
    push @output, '<TABLE BORDER=1 WIDTH="100%" CELLSPACING=0 CELLPADDING=0>';
    push @output, ' <TR>';
    push @output, , "  <TD WIDTH=$logo_size[0] ALIGN=CENTER>";
    if (!$invalid &&		# Invalid results should never get the SPEC
        defined($::cpu2000_website_formatter) && # Seal of Reviewal
	$::cpu2000_website_formatter && 
        defined($::format_for_publication) &&
        $::format_for_publication) {
	# This must be a real result.  Put the cute little picture in
	push @output, "  <IMG SRC=\"$image_url$logo_location\" ALT=\"SPEC Seal of Reviewal\" WIDTH=$logo_size[0] HEIGHT=$logo_size[1]>";
    } else {
	push @output, '  &nbsp;&nbsp;&nbsp;';
    }
    push @output, '  </TD>';
    push @output, '  <TD ALIGN=CENTER>';
    $tmpstr = '  <BIG><STRONG>';
    if ($invalid) {
	$tmpstr .= '<FONT COLOR="#FF0000">Invalid</FONT> ';
    }
    push @output, $tmpstr.$r->{'metric'}." Result</STRONG></BIG><BR>";
    push @output, '  <SMALL>Copyright &copy; 1999-2005 Standard Performance Evaluation Corporation</SMALL>';
    push @output, '  </TD>', ' </TR>';
    push @output, '</TABLE>';

    return @output;
}


sub HTML_result {
    my ($r) = @_;
    my @output = ();

    # Make the result bar
    push @output, '<TABLE BORDER=1 WIDTH="100%" CELLSPACING=0 CELLPADDING=0>';
    push @output, '  <TR>';
    push @output, '  <TD ALIGN=LEFT WIDTH="100%">';
    push @output, '  <BIG>'.firstof($r->{'hw_vendor'}).'<BR>';
    push @output, '  '.firstof($r->{'hw_model'});
    push @output, '  </BIG></TD><TD>';
    push @output, '  <TABLE CELLSPACING=0 CELLPADDING=0 BORDER=0>';
    my $peakmean = (@{$r->{'nc'}}+0 == 0) ? pad_rear($r->peakmean) : 'NC';
    push @output, '   <TR><TD ALIGN=LEFT NOWRAP>'.$r->peakunits.' = </TD><TD ALIGN=RIGHT NOWRAP>'.$peakmean.'</TD></TR>';
    my $basemean = (@{$r->{'nc'}}+0 == 0) ? pad_rear($r->basemean) : 'NC';
    push @output, '   <TR BGCOLOR="#999999"><TD ALIGN=LEFT NOWRAP>'.$r->baseunits.' = '.'</TD><TD ALIGN=RIGHT NOWRAP>'.$basemean.'</TD></TR>';
    push @output, '  </TABLE>';
    push @output, '  </TD>';
    push @output, '  </TR>';
    push @output, '</TABLE>';

    return @output;
}


sub HTML_date {
    my ($r) = @_;
    my @output = ();

    # Make the info bar
    # Thanks, Chris
    push @output, '<TABLE BORDER=1 CELLSPACING=0 CELLPADDING=0 WIDTH="100%">';
    push @output, ' <TR ALIGN="CENTER">';
    for my $info (
		  [ 'SPEC license #'      , 'license_num'    , 12 ],
		  [ 'Tested by:'          , 'tester_name'    , 25 ],
		  [ 'Test date:'          , 'test_date',     , 21 ],
		  [ 'Hardware Avail:'     , 'hw_avail'       , 21 ],
		  [ 'Software Avail:'     , 'sw_avail'       , 21 ],
		  ) {
	my ($name, $key, $relwidth) = @$info;
	push @output, "    <TD WIDTH=\"${relwidth}%\">$name ".
	    firstof($r->{$key}).'</TD>';
    }
    push @output, ' </TR>';
    push @output, '</TABLE>';

    return @output;
}

sub draw_scale {
    my ($r, $maxratio, $twidth, $theight, $scale, $font, $path, $skip) = @_;
    my $xpos = 0;
    my $minor_ticks = 10;

    # Figure out what the filename should be
    my $name = sprintf('%s%sscale.%03d.gif', $image_path,
		       ($r->rate) ? 'r' : '', $maxratio);

    # Figure out what the scale should be in order to make a properly sized
    # scale.
    print "old $maxratio, scale $scale\n" if $debug;
    while ($scale < (length("$maxratio") * $font->width * 2)) {
	last if ($scale > (length("$maxratio") * $font->width * 1.8));
	$skip+=5;
	$scale = $twidth / int($maxratio / $skip + 0.5);
    }
    print "pre-tweak $maxratio, skip $skip, scale $scale\n" if $debug;
    if ($skip && ($maxratio % $skip)) {
	$maxratio += $skip - ($maxratio % $skip);
	$scale = $twidth / int($maxratio / $skip + 0.5);
    }
    $skip = 1 unless $skip;
    print "post-tweak $maxratio, skip $skip, scale $scale\n" if $debug;

    # Tweak the # of little ticks to have between the big ones
    if ($skip > 5) {
	$minor_ticks = (($maxratio / $skip) > 10) ? 5 : 10;
    } else {
	$minor_ticks = 10;
    }
    $minor_ticks /= 2 if (($scale % $minor_ticks) &&
			  !($minor_ticks % 2) &&
			  (($scale / $minor_ticks) < 5));
    print "post-tweak $maxratio, skip $skip, scale $scale, minor_ticks $minor_ticks\n" if $debug;

    # If it already exists, there's nothing else to do!
    return ($maxratio, basename($name)) if (-f $path.$name);

    # Set up the image object
    my $im = new GD::Image($twidth, $theight);
    my ($white, $black, $base, $peak) = (
					 $im->colorAllocate(255,255,255),
					 $im->colorAllocate(0,  0,  0),
					 $im->colorAllocate(0,  0,  0),
					 $im->colorAllocate(127,127,127),
			  );
    $im->transparent($white);
    $im->interlaced('false');
    $theight--;

    # Draw the scale

    # The base line
    $im->line(0, $theight, $twidth, $theight, $black);

    # Now for the tick marks and numbers
    my $cx = 0;
    for(my $tick = 0; $tick <= ($maxratio / $skip); $tick++) {
	my $tx = $cx;
	my $tickval = $tick * $skip;
	my $tickstr = ($r->rate) ? ($maxratio - $tickval) : $tickval;
	my $tickwidth = length("$tickstr") * $font->width;
	$tx -= int($tickwidth / 2 + 0.5) unless ($tick == 0);
	$tx -= int($tickwidth / 2 + 0.5) if ($tick >= ($maxratio/$skip));
	$im->string($font, $tx, 0, $tickstr, $black);
	$im->line($cx, $theight-12, $cx, $theight, $black); # Major tick
	my $mtx = $cx;
	for my $i (2..$minor_ticks) {
	    $mtx += $scale/$minor_ticks;
	    $im->line(int($mtx + 0.5), $theight-6,
		      int($mtx + 0.5), $theight,
		      $black); # Minor tick
	}
	$cx += $scale;
    }
    # Do the last big tick
    $im->line($twidth-1, $theight-12, $twidth-1, $theight, $black);

    my $fname = "$path$name";
    my $fh = new IO::File ">$fname";
    if (defined($fh)) {
	print $fh $im->gif;
	close $fh;
	push @{$written}, $name;
    } else {
	::Log(0, "\nCouldn't open $fname for writing: $!\n");
	::Log(0, "\nYour HTML result will have *NO* graphs!\n");
	# Let's not just fail, though...
	# If the correct image shows up, it'll still be right.
    }
    return ($maxratio, basename($name));
}

sub firstof($) {
    my ($val) = @_;
    $val = $val->[0] if (ref($val) eq 'ARRAY');
    return $val;
}

# significant() and floor() gleefully stolen wholesale from ps.pl
sub significant {
    my ($value, $min_log, $returnlog) = @_;
    my ($log);
    $min_log = 2 if !defined $min_log;
    if ($value == 0) {
	return $value if $value !~ /[\d.]/;
	$log = 0;
    } else {
	$log = &floor(log($value)/log(10)); 
    }
    return $log if ($returnlog);
    # STUPID STUPID HACK which circumvents the whole idea of
    # 3 significant digits.
    if ($value < 1000) {
	$value = int($value / (10**($log-$min_log))+.5) * (10**($log-$min_log));
    } else {
	# Round up; there won't be any decimal places
	$value = int($value + 0.5);
    }
    $value = sprintf ("%.*f", $min_log-$log, $value) if ($log < $min_log);
    return $value;
}
sub floor {
    my ($temp) = @_;
    my $inttemp = int($temp);
    if ($temp != $inttemp) { #  This is a bad test.
	if ($temp > 0) {
	    $temp = $inttemp;
	} else {
	    $temp = $inttemp-1;
	}
    }
    return $temp;
}

sub pad_rear {
    my ($val) = @_;
    $val = significant($val);
    my $log = significant($val, undef, 1);
    my $s = '';
    $s .= '&nbsp;' if ($val >= 100);
    $s .= '&nbsp;' x ($log+1) if $log >= 0;
    return "$val$s";
}

sub makebar {
    my ($fname, $width, $height, $r, $g, $b) = @_;

    # Set up the image object
    my $im = new GD::Image($width, $height);
    my $color = $im->colorAllocate($r, $g, $b);
    $im->interlaced('false');

    # The bar
    $im->filledRectangle(0, 0, $width, $height, $color);

    my $fh = new IO::File ">$fname";
    if (defined($fh)) {
	print $fh $im->gif;
	close $fh;
    } else {
	::Log(0, "\nCouldn't open $fname for writing: $!\n");
	return undef;
    }
    return $fname;
}

# This is needed in case it needs to be re-generated by the tools
# It's declared at the top of the file.
@invalid_gif = qw(
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
a9k5XLpglboc3i172RxgCnOYxOROAQAAOw==
);

1;
