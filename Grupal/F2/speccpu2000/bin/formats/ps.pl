#
#  ps.pl - produces PostScript output
#  Copyright (C) 1999-2005 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Authors:  Christopher Chan-Nui
#            Cloyce D. Spradling 
#
# $Id: ps.pl 1577 2005-08-22 19:04:38Z cloyce $

use strict;
use PSPDF;
use File::Basename;

use vars qw($name $extension $invalid);

my $min_font_size = 6;
my $ps_version = '$LastChangedRevision: 1577 $ '; # Make emacs happier
$ps_version =~ s/^\$LastChangedRevision: (\d+) \$ $/$1/;

my $debug = 0;

$name      = 'PostScript';
$extension = 'ps';
$invalid   = 0;			# Innocent until proven guilty

sub format () {
    my($me, $r, $path) = @_;
    my @output = split ("\n", SPEC_report($r, 'PS', $path));
    return (\@output, []);
}

############################################################################
# PS/PDF Formatting routines

my $table    = {};
my $results  = {};
my $smallest = {};
my $largest  = {};

sub debug_clone {
    my ($config, $sb, $st, $tb, $tt) = @_;
    %{$config->{'results'}{$tb}{$tt}{'data'}[0]} = %{$config->{'results'}{$sb}{$st}{'data'}[0]};
    bless $config->{'results'}{$tb}{$tt}{'data'}[0], 
	ref ($config->{'results'}{$sb}{$st}{'data'}[0]);
};

sub SPEC_report {
    my ($config, $mode, $path) = @_;
    my ($width, $height, $hsize);

    $mode = 'PS' if $mode eq '';
    if ($config->{'invalid'} ||
	(((ref($config->{'errors'}) eq 'ARRAY') && @{$config->{'errors'}}))) {
	$invalid = 1;
    }
    my $pspdf = new PSPDF $mode;
    if (exists ($pspdf->{'error'}) && $pspdf->{'error'}) {
	# PDF probably couldn't be initialized
	return '';
    }
    $pspdf->set_fontdir ("$ENV{'SPEC'}/bin/fonts") if exists $ENV{'SPEC'};

    my @gmtimes = gmtime(time);
    $gmtimes[5] += 1900;
    $gmtimes[4]++;
    $pspdf->CreationDate(sprintf("D:%04d%02d%02d%02d%02d+00'00'", @gmtimes[5,4,3,2,1,0]));
    $pspdf->Creator("SPEC CPU2000 tools $::version (PSPDF v$ps_version)");
    $pspdf->Author ($config->{'tester_name'});
    if ($config->{'company_name'} !~ /^(|--)$/ &&
	$config->{'company_name'} ne $config->{'hw_vendor'}) {
	$pspdf->Title  (firstof($config->{'hw_vendor'}.': '.$config->{'hw_model'}.' (tested by '.$config->{'company_name'}.')'));
    } else {
	$pspdf->Title  (firstof($config->{'hw_vendor'}.': '.$config->{'hw_model'}));
    }
    my $subj = sprintf('%sSPEC %s Result', ($invalid) ? 'Invalid ' : '',
		       joinup($config->{'metric'}));
    $pspdf->Subject($subj);

    $pspdf->open();

    ($width, $height) = SPEC_newpage($pspdf, $config);

    $smallest = {};
    $largest  = {};
    for my $res (sort $config->results_list) {
	my $tune = $res->tune;
	my $tmp;

	my $time  = $res->reported_time;
	my $ratio = $res->ratio;
	next if ! $res->selected;

	$smallest->{$tune}{'time'}  = $time  if $smallest->{$tune}{'time'}  < $time;
	$smallest->{$tune}{'ratio'} = $ratio if $smallest->{$tune}{'ratio'} < $ratio;
	$largest->{$tune}{'time'}   = $time  if $largest->{$tune}{'time'}   > $time;
	$largest->{$tune}{'ratio'}  = $ratio if $largest->{$tune}{'ratio'}  > $ratio;
    }

    use vars qw($inch);
    $inch = 72;

    # Draw the Table/Graph
    $hsize = SPEC_table ($pspdf, 0, $height, $width, $height, $config);
    $height -= $hsize;

    # Draw the Detail boxes
    $hsize = SPEC_detail($pspdf, 0, $height, $width, $height, $config);
    $height -= $hsize;

    my $border = 10;
    my $indent = 15+$border;
    $hsize = 20;
    $pspdf->set_font('Times-Bold', 16);
    $pspdf->set_text_pos($width/2, $height-$hsize);
    $pspdf->show_center('Notes/Tuning Information');
    $height -= $hsize;

    # Using a fixed-width font will help preserve notes formatting.
    # Thanks to John Henning for the suggestion.
    $pspdf->set_font('Courier', 10);

    my $char_height = $pspdf->string_CapHeight - $pspdf->string_Descender();

    $hsize = 10;
    my @notes = @{($config->notes)[0]};
    shift (@notes);
    for my $val (@notes) {
	$height -= $hsize;
	if ($height < $char_height) {
	    $pspdf->grestore();
	    $pspdf->end_page();

	    ($width, $height) = SPEC_newpage($pspdf, $config);

	    $pspdf->set_font('Times-Bold', 16);
	    $height -= 20;
	    $pspdf->set_text_pos($width/2, $height);
	    $pspdf->show_center('Notes/Tuning Information (Continued)');
	    $height -= 10;

	    # Make sure that second-page notes are the same as the others
	    # Thanks to J. Yang of Motorola for the spot.
	    $pspdf->set_font('Courier', 10);
	}
	my $rc = $pspdf->string_fit($val, $indent, $height, 
		$width-$indent-$border, $hsize, 
		$min_font_size, "A line in your Notes is is too long!\n");
	return 0 if $rc == 0;
    }

    if ((ref($config->{'errors'}) eq 'ARRAY') &&
	(@{$config->{'errors'}}+0 > 0)) {
	my @errors = @{$config->{'errors'}};
	# Figure out if we're going to spill a page, or figure out where to
	# start such that the last error ends up at the end of the page
	if (($height - (20 + 10 + 10 + ((@errors+0) * 10)) >= $char_height)) {
	    # We won't go on to the next page, so figure out how much lower
	    # down to start.
	    $height = $char_height + (20 + 10 + 10 + ((@errors+0) * 10)) - 5;
	    $pspdf->moveto(0, $height);
	} else {
	    # Just start where we are
	    $height -= 5;
	    $pspdf->moveto(0, $height);
	}
	$pspdf->lineto($width, $height);
	$pspdf->stroke();
	$height -= 5;
	$hsize = 20;
	$pspdf->set_font('Times-Bold', 16);
	$pspdf->set_text_pos($width/2, $height-$hsize);
	$pspdf->setrgbcolor(1,0,0);
	$pspdf->show_center('Errors');
	$pspdf->setgray(0);
	$height -= $hsize;

	$pspdf->set_font('Times-Roman', 10);

	$hsize = 10;
	for my $val (@errors) {
	    $height -= $hsize;
	    if ($height < $char_height) {
		$pspdf->grestore();
		$pspdf->end_page();

		($width, $height) = SPEC_newpage($pspdf, $config);

		$pspdf->set_font('Times-Bold', 16);
		$height -= 20;
		$pspdf->set_text_pos($width/2, $height);
		$pspdf->setrgbcolor(1,0,0);
		$pspdf->show_center('Errors (Continued)');
		$pspdf->setgray(0);
		$height -= 10;

		$pspdf->set_font('Times-Roman', 10);
	    }
	    my $rc = $pspdf->string_fit($val, $indent, $height, 
					$width-$indent-$border, $hsize, 
					$min_font_size,
					"A line in your Errors is is too long!\n");
	    return 0 if $rc == 0;
	}
    }

    $pspdf->grestore();
    $pspdf->end_page();
    if (@{$config->{'nc'}}+0 == 0) {
      my $fn = basename($path, '.'.lc($mode));
      # Now encode/attach the raw file
      if (defined($config->{'compraw'})) {
          $pspdf->add_raw("${fn}.raw", ' GZIP', $config->{'compraw'});
      } elsif (defined($config->{'rawfile'})) {
          $pspdf->add_raw("${fn}.raw", '', $config->{'rawfile'});
      } else {
          ::Log(0, "Encoding problem?  There is no raw file to embed in the result file.\n");
      }
    }
    $pspdf->close();
    my $output = $pspdf->output();
    return $output;
}

sub cross($$$;$) {
    my ($p, $x, $y, $w) = @_;
    $w = 10 if $w == 0;
    $p->gsave();
    $p->setrgbcolor(1,0,0);
    $p->moveto($x-$w,$y);
    $p->lineto($x+$w,$y);
    $p->moveto($x,$y-$w);
    $p->lineto($x,$y+$w);
    $p->stroke();
    $p->grestore();
    return 1;
}

sub SPEC_newpage($$) {
    my ($p, $config) = @_;

    $p->begin_page('letter');
    $p->set_font('Times-Roman', 18);

    my $width  = $p->{'width'};
    my $height = $p->{'height'};

    if ($invalid) {
	$p->gsave();
	$p->setgray(0.80);
	$p->rotate(50);
	my $width = sqrt($width*$width+$height*$height)*0.75;
	my $rc = $p->string_fit('Invalid Run', $width*0.25, 0, $width, 144);
	$p->grestore();
    }
    if (@{$config->{'nc'}}+0) {
	$p->gsave();
	$p->setgray(0.80);
	$p->rotate(50);
	my $width = sqrt($width*$width+$height*$height)*0.75;
	my $rc = $p->string_fit('Non-Compliant', $width*0.25, 0, $width, 144);
	$p->grestore();
    }

    $p->gsave();
    # We need a margin, let's use 0.5 inch
    my $inch = 72;
    my $border = 0.5 * $inch;
    $height -= $border * 2;
    $width  -= $border * 2;
    $p->translate($border,$border);

    # Draw a box for the page
    $p->rect(0,0, $width,$height);
    $p->stroke();

    # Draw the advertisement at the bottom
    my $hsize = 0.5 * $inch;
    SPEC_advertisement($p, 0, 0, $width, $hsize);
    $height -= $hsize;
    $p->translate(0,$hsize);

    # Draw the Banner Bar
    $hsize = 0.5 * $inch;
    SPEC_banner($p, 0, $height-$hsize, $width, $hsize, $config);
    $height -= $hsize;

    # Draw the Title Bar
    $hsize = 0.75 * $inch;
    SPEC_title($p, 0, $height-$hsize, $width, $hsize, $config);
    $height -= $hsize;

    # Draw the Info Bar
    $hsize = 0.125 * $inch;
    SPEC_infobar($p, 0, $height-$hsize, $width, $hsize, $config);
    $height -= $hsize;

    # Draw the NC box
    if (@{$config->{'nc'}}+0) {
	my $fontsize = 14;
	my @nc = @{$config->{'nc'}};
	my $indent = 0.375 * $inch;

	$p->gsave();
	$p->set_font('Times-Bold', $fontsize);

	# Blaze through and find the longest line -- it'll be used to
	# figure the font size
	my $maxsize = 0;
	my $badsize = 1;
	while ($badsize) {
	    $badsize = 0;
	    foreach my $line (@nc) {
		my $strwidth = $p->stringwidth($line, $fontsize);
		if ($strwidth > ($width - ($indent * 2))) {
		    $badsize = 1;
		    $maxsize = 0;
		    $fontsize--;
		    $p->set_font('Times-Bold', $fontsize);
		    last;
		} elsif ($strwidth > $maxsize) {
		    $maxsize = $strwidth;
		}
	    }
	}
	my $fonthsize = $p->string_XHeight('X') * 2.5;
	$hsize = $fonthsize * (@nc-1) + (0.5 * $inch);
	$p->translate(0, $height-$hsize);
	$p->rect(0,0, $width, $hsize);
	$p->stroke();
	my $currheight = $hsize - (0.25 * $inch);
	foreach my $line (@nc) {
	    $p->set_text_pos(0+$indent, $currheight);
	    $p->show($line);
	    $currheight -= $fonthsize;
	}
	$height -= $hsize;
	$p->grestore();
    }

    return ($width, $height);
}

sub SPEC_logo ($$$$$) {
    my ($p, $x, $y, $width, $height) = @_;
    my $fontsize=72;

    $p->gsave();
	$p->rect(0,0, $width,$height);
	$p->stroke();
	my $border = (($width < $height)?$width:$height) * 0.05;
	$p->translate($x + $border, $y + $border);
	$width -= $border * 2;
	$height -= $border * 2;

        # Find out what the correct scale for the text is
	my $i = 0.5;
	$p->set_font('Times-Bold', $fontsize);
	my $w = $p->stringwidth('spec');
	$fontsize *= ($width / $w);
	$fontsize = $height * 0.3 if $fontsize > $height * 0.3;

        # Print text for bottom
	$p->set_font('Times-Bold', $fontsize);
	my $XHeight = $p->string_CapHeight() * 1.1;
	my $Descender = $p->string_Descender() * 1.1;
	$p->set_text_pos($width/2, $XHeight /4);
	$p->show_center('spec');

        # Print Grid
	my $gridsize = $height - $fontsize;
	$gridsize = $width if $width < $gridsize;
	$p->translate(($width-$gridsize)/2, $XHeight + $Descender /2);

	for ($i = 0; $i <= 4; $i++) {
	    $p->moveto ($i/4 * $gridsize,                0);
	    $p->lineto ($i/4 * $gridsize,        $gridsize);
	    $p->moveto (               0, $i/4 * $gridsize);
	    $p->lineto (       $gridsize, $i/4 * $gridsize);
	}
	$p->stroke();

        # Print the curve
        $p->setrgbcolor(235,0,0);
	for ($i = 11; $i <= 11; $i++)  {
	    my $x1 = $i/16 * $gridsize;
	    my $y1 =  4/16 * $gridsize;
	    my $x2 = 12/16 * $gridsize; 
	    my $y2 = (16-$i)/16 * $gridsize;
	    $p->moveto ( 3/16 * $gridsize,  3/16 * $gridsize);
	    $p->curveto( $x1,$y1, $x2,$y2,
			13/16 * $gridsize, 13/16 * $gridsize);
	    $p->stroke();
	}
    $p->grestore();
    return 1;
}

sub SPEC_banner ($$$$$$) {
    my ($p, $x, $y, $width, $height, $config) = @_;
    my $bigfontsize = $height * 0.8;
    my $fontsize = $height * 0.2;
    my $copyright_char = "\343";
    my @copyright_msg  = ('Copyright ', 
			  '1999-2005, Standard Performance Evaluation Corporation');
    $p->gsave();
	$p->translate($x,$y);
	$p->rect(0,0, $width,$height);
	$p->stroke();

	if (!$invalid &&		# Invalid results should never get the
	    defined($::cpu2000_website_formatter) &&
	    $::cpu2000_website_formatter && # SPEC Seal of Reviewal
            defined($::format_for_publication) &&
            $::format_for_publication) {
	    SPEC_logo($p, 0, 0, $height, $height);
	}

	$p->set_font('Times-Bold', $bigfontsize);
	$p->set_text_pos($width/2, $height-$bigfontsize*0.9);
	$p->show_center($config->{'metric'}.' Result');

	$p->set_font("Symbol", $fontsize);
	my $w = $p->stringwidth($copyright_char);
	$p->set_font('Times-Roman', $fontsize);
	$w += $p->stringwidth ($copyright_msg[0]);
	$w += $p->stringwidth ($copyright_msg[1]);
	$p->set_text_pos(($width - $w)/2, $height-$bigfontsize*0.9 - $fontsize);
	$p->set_font ('Times-Roman', $fontsize);
	$p->show($copyright_msg[0]);
	$p->set_font("Symbol", $fontsize);
	$p->show($copyright_char);
	$p->set_font ('Times-Roman', $fontsize);
	$p->show($copyright_msg[1]);
    $p->grestore();
    return 1;
}

sub SPEC_advertisement {
    my ($p, $x, $y, $width, $height) = @_;
    $p->gsave();
	$p->translate($x,$y);
	$p->rect_fill(0,0, $width,$height, 0.9, undef, 1);

	my $font_size = $height / 4;
	$p->set_font("Times-Roman", $font_size);

	my $margin = $height/8;

	$x = $margin;
	$y = $height - $margin - $font_size;

	$p->set_text_pos($x, $y);

	$p->set_text_pos($width/2, $y);
	$p->show_center('Standard Performance Evaluation Corporation');
        # Address, phone #s removed because they change too often
	$p->continue_text_center('info@spec.org');
	$p->continue_text_center('http://www.spec.org/');

    $p->grestore();
    return 1;
}

sub SPEC_infobar ($$$$$$) {
    my ($p, $x, $y, $width, $height, $config) = @_;

    $p->gsave();
	$p->translate($x,$y);
	$p->rect(0,0, $width,$height);
	$p->stroke();
        $p->setlinewidth(0.1);
        my $border = min($width, $height) * 0.2;
        $p->set_font("Times-Roman", $height - $border - 0.5);
        my $string_base = (($height - $border) / 2) -
                         (($p->string_CapHeight() + $p->string_Descender) / 2) + 0.5; 
	my $xpos = 0;
        for my $info (
		      [ 'SPEC license #:'     , 'license_num'    , 0.12 ],
		      [ 'Tested by:'          , 'tester_name'    , 0.25 ],
		      [ 'Test date:'          , 'test_date',     , 0.21 ],
		      [ 'Hardware Avail:'     , 'hw_avail'       , 0.21 ],
		      [ 'Software Avail:'     , 'sw_avail'       , 0.21 ],
		   ) {
	    my ($name, $key, $relwidth) = @$info;
	    my $mywidth = $relwidth * $width;
	    $p->rect($xpos, 0, $mywidth, $height);
	    $p->stroke();
	    $p->set_text_pos($xpos+$border, $string_base);
	    $p->show($name);
	    $p->set_text_pos($xpos+$mywidth-$border, $string_base);
	    $p->show_right(firstof($config->{$key}));
	    $xpos += $mywidth;
	}
    $p->grestore();
    return 1;
}

# Given the upper left corner
sub SPEC_info_box {
    my ($p, $x, $y, $width, $height, $config, $list, $size) = @_;
    my $namewidth = 0;
    $p->set_font("Times-Roman", $size*1.2);

    # Do a quick run through the items to find the longest tag string
    for my $item (@$list) {
	my $reftype = ref($item);
	if ($reftype eq 'ARRAY') {
	    my ($name, $key) = @$item;
	    my $w = $p->stringwidth("$name:", $size);
	    $namewidth = $w if $w > $namewidth;
	}
    }

    my $valpos = $namewidth + $width / 20;

    $height = 0;		# This clobbers the parameter... okay?
    for my $item (@$list) {
	my $reftype = ref($item);
	if ($reftype eq 'ARRAY') {
	    my ($name, @vals) = @$item;
	    $height += $size;
	    $p->set_font("Times-Roman", $size);
	    $p->set_text_pos($x, $y - $height);
	    $p->show("$name:");

	    my $mysize = $size;
	    for my $subval (@vals) {
		my $rc = $p->string_calcsize($subval, $width-$valpos, $mysize, 
			$min_font_size, "Your '$name' field is too long; %s size font will be unreadable!\n");
		return 0 if $rc == 0;
		$mysize = $rc if $rc < $mysize;
	    }
	    $p->set_font("Times-Roman", $mysize);
	    for my $subval (@vals) {
		$p->set_text_pos($x + $valpos, $y - $height);
		$p->show($subval);
		$height += $mysize;
	    }
	    $height -= $mysize if @vals;
	} elsif ($reftype eq '') {
	    $p->set_font("Times-Bold", $size * 1.2);
	    $height += $size * 1.2;
	    $p->set_text_pos($x + $width / 2, $y - $height);
	    $height += $size * .2; # Nudge it down a little
	    $p->show_center($item);
	} else {
	    ::Log(0, "SPEC_info_length: list contains bogus element ref type!\n");
	    exit 1;
	}
    }
    return $height;
}
sub SPEC_table ($$$$$$) {
    my ($p, $x, $y, $width, $height, $config) = @_;
    my @titles;
    my $table_width = $width * 0.50;
    my $graph_width = $width - $table_width;

    my $table_x     = 0;
    my $graph_x     = $table_width;

    if (istrue($config->{'rate'})) {
	$table_x     = $graph_width;
	$graph_x     = $graph_width;
	$graph_width = -$graph_width+10;
    } else {
	$graph_width -= 10;
    }

    $height = 0;
    
    my $size = 10;
    if (istrue($config->{'rate'})) {
	@titles = (
	    [ '',     'Benchmark' ],
	    [ 'Base', 'Copies'    ],
	    [ 'Base', 'Runtime'   ],
	    [ 'Base', 'Ratio'     ],
	    [ '',     'Copies'    ],
	    [ '',     'Runtime'   ],
	    [ '',     'Ratio'     ],
	);
    } else {
	@titles = (
	    [ '',          'Benchmark' ],
	    [ 'Reference', 'Time'      ],
	    [ 'Base',      'Runtime'   ],
	    [ 'Base',      'Ratio'     ],
	    [ '',          'Runtime'   ],
	    [ '',          'Ratio'     ],
	);
    }

    my $label_width = 0.25 * $table_width;
    my $num_width   = ($table_width - $label_width) / (@titles-1);
    my @cols = ($table_x);
    for (my $i = 0; $i < @titles; $i++) {
	push (@cols, $table_x+$label_width+$num_width*$i);
    }
    $p->gsave();
    {
	$p->translate($x, $y);

	$height -= $size *2;
	$p->rect($table_x,$height, $table_width,-$height);
	$p->stroke();

	$size = 10;
	my $dp = { 'base' => [ 2147483647, 2147483647 ],
		   'peak' => [ 2147483647, 2147483647 ] };
	my $max = { 'ratio' => 0, 'time' => 0 };
	for my $bench (sort keys %{$config->benchmarks}) {
	    my $tmp = 0;
	    for my $tune (qw(peak base)) {
		if ($config->valid($bench, $tune)) {
		    $tmp = $config->runtime($bench, $tune);
		    $max->{'time'} = $tmp if $tmp > $max->{'time'};
		    $dp->{$tune}->[0] = $tmp if $tmp < $dp->{$tune}->[0];
		    $tmp = $config->ratio($bench, $tune);
		    $max->{'ratio'} = $tmp if $tmp > $max->{'ratio'};
		    $dp->{$tune}->[1] = $tmp if $tmp < $dp->{$tune}->[1];
		}
	    }
	}
        for my $tune (qw(base peak)) {
	    for (my $i = 0; $i < 2; $i++) {
		if ($dp->{$tune}->[$i] && ($dp->{$tune}->[$i] != 2147483647)) {
		    $dp->{$tune}->[$i] = log($dp->{$tune}->[$i])/log(10);
		}
	    }
	}

	if (@{$config->{'nc'}}+0) {
	    # Show them nothing!
	    $max->{'time'} = 0;
	    $max->{'ratio'} = 0;
	    $dp->{'base'} = [0,0];
	    $dp->{'peak'} = [0,0];
	}

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
			my $example_tickval = $hticks*$htick_val;
			my $tickinterval = $graph_width / $hticks;
			while (abs($tickinterval) <
			    ($p->stringwidth($example_tickval) * 1.5)) {
			    # So this combo of ticks and max value may
			    # be sufficient, but the tick labels will
			    # collide toward the upper end of the scale.
			    # So subtract one from the # of ticks and call
			    # it good.
			    if ($hticks != 1) {
				$hticks--;
			    } else { # There's a problem
				$htick_val += $mult;
			    }
			    $example_tickval = $hticks * $htick_val;
			    $tickinterval = $graph_width / $hticks;
			    # Don't do anything here... the following
			    # condition still needs to be true.
			}
			if ($hticks * $htick_val > $max->{'ratio'}) {
			    last find_tickval;
			}
		    }
		}
		$mult *= 10;
	    }
	}
	$max->{'ratio'} = $hticks * $htick_val;

	$p->set_font("Times-Bold", 8.5);
	my $Descender = $p->string_Descender();
	for (my $i = 0; $i < @titles; $i++) {
	    my $col_width = $cols[$i+1]-$cols[$i];
	    $p->rect($cols[$i], $height, $col_width, -$height);
	    $p->stroke();
	    $p->set_text_pos($cols[$i] + ($col_width / 2), $height + $size);
	    $p->show_center($titles[$i]->[0]);
	    $p->set_text_pos($cols[$i] + ($col_width / 2), $height - $p->string_Descender());
	    $p->show_center($titles[$i]->[1]);
	}

        # Draw the scale
	$p->set_font("Times-Roman", $size);
	my $char_height = $p->string_CapHeight() - $p->string_Descender();
	my $off = 6;
        my ($start_x, $interval) = ($graph_x,
				 ($htick_val/$max->{'ratio'}) * $graph_width);
	for (my $i = $htick_val; $i < $max->{'ratio'}; $i += $htick_val) {
	    my $x = $i / $max->{'ratio'} * $graph_width;
	    draw_minor_ticks($p, $start_x, $height+1, $interval);
	    $start_x = $graph_x + $x;
	    $p->set_text_pos($graph_x + $x, $height + ($char_height / 2) + $off);
	    $p->show_center($i);
	    $p->moveto($graph_x + $x, $height+1 );
	    $p->lineto($graph_x + $x, $height+6 );
	    $p->stroke();
	}
        draw_minor_ticks($p, $start_x, $height+1, $interval);
        $p->set_text_pos($graph_x + $graph_width, $height + ($char_height / 2) + $off);
	if (istrue($config->{'rate'})) {
	    $p->show($max->{'ratio'});
	} else {
	    $p->show_right($max->{'ratio'});
	}
	$p->moveto($graph_x + $graph_width, $height+1 );
	$p->lineto($graph_x + $graph_width, $height+5 );
	$p->stroke();

	my $top_y = $height;

        # Do the table
	my $hsize = $size * 1.5;
	for my $bench (sort keys %{$config->benchmarks}) {
	    my $border = 5;
	    $height -= $hsize;
	    my @tmp = ( [ 1, 'left',  $border, $bench ] );
	    if (grep { /^base$/o } @{$config->tunelist}) {
		if (istrue($config->{'rate'})) {
		    push (@tmp, [ 1, 'right', $border,
				  $config->copies($bench, 'base') ]);
		} else {
		    my $ref = ($config->size eq 'ref') ? $config->reference($bench) : '--';
		    push (@tmp, [ 0, 'right', $border, $ref ]);
		}
		if ($config->valid($bench, 'base')) {
		    if (@{$config->{'nc'}}+0) {
			push (@tmp, [ 0, 'right', $border,
				      'NC', 0 ]);
		    } else {
			push (@tmp, [ 1, 'decimal', $border,
				      $config->runtime($bench,'base',0),
				      $dp->{'base'}->[0] ]);
		    }
		    if ($config->size eq 'ref') {
			if (@{$config->{'nc'}}+0) {
			    push (@tmp, [ 0, 'right', $border,
					  'NC', 0 ]);
			} else {
			    push (@tmp, [ 1, 'decimal', $border,
					  $config->ratio($bench,'base'),
					  $dp->{'base'}->[1] ]);
			}
		    } else {
			push (@tmp, [ 0, 'right', $border, '--' ]);
		    }
		} else {
		    push (@tmp, [ 0, 'right', $border, 'X' ]);
		    push (@tmp, [ 0, 'right', $border, 'X' ]);
		}
	    } else {
		push (@tmp, [ 0, 'right', $border, '' ]);
		push (@tmp, [ 0, 'right', $border, '' ]);
		push (@tmp, [ 0, 'right', $border, '' ]);
	    }
	    if (grep { /^peak$/o } @{$config->tunelist}) {
		if (istrue($config->{'rate'})) {
		    push (@tmp, [ 0, 'right', $border,
				  $config->copies($bench, 'peak') ]);
		}
		if ($config->valid($bench, 'peak')) {
		    if (@{$config->{'nc'}}+0) {
			push (@tmp, [ 0, 'right', $border,
				      'NC', 0 ]);
		    } else {
			push (@tmp, [ 0, 'decimal', $border,
				      $config->runtime($bench,'peak',0),
				      $dp->{'peak'}->[0] ]);
		    }
		    if ($config->size eq 'ref') {
			if (@{$config->{'nc'}}+0) {
			    push (@tmp, [ 0, 'right', $border,
					  'NC', 0 ]);
			} else {
			    push (@tmp, [ 0, 'decimal', $border,
					  $config->ratio($bench,'peak'),
					  $dp->{'peak'}->[1] ]);
			}
		    } else {
			push (@tmp, [ 0, 'right', $border, '--' ]);
		    }
		} else {
		    push (@tmp, [ 0, 'right', $border, 'X' ]);
		    push (@tmp, [ 0, 'right', $border, 'X' ]);
		}
	    } else {
		push (@tmp, [ 0, 'right', $border, '' ]);
		push (@tmp, [ 0, 'right', $border, '' ]);
		push (@tmp, [ 0, 'right', $border, '' ]);
	    }		
	    for (my $i = 0; $i < @tmp; $i++) {
		my ($shade, $align, $off, $val, $arg1) = @{$tmp[$i]};
		my $col_width = $cols[$i+1]-$cols[$i];
		if ($shade) {
		    $p->gsave();
			$p->setgray(0.9);
			$p->rect($cols[$i], $height, $col_width, $hsize);
			$p->fill();
		    $p->grestore();
		}
		$p->rect($cols[$i], $height, $col_width, $hsize);
		$p->stroke();
		if ($align eq 'left') {
		    $p->set_text_pos($cols[$i] + $off, $height + ($char_height / 2));
		    $p->show($val);
		} elsif ($align eq 'right') {
		    $p->set_text_pos($cols[$i] + $col_width - $off, $height + ($char_height / 2));
		    $p->show_right($val);
		} elsif ($align eq 'decimal') {
		    $val = significant($val);
		    my $s = figure_dp($val, 3, $arg1);
		    my $w = $p->stringwidth($s);
		    $p->set_text_pos($cols[$i] + $col_width - $off - $w, $height + ($char_height / 2));
		    $p->show_right($val);
		}
	    }
	    my $base_ratio = $config->ratio($bench, 'base');
	    my $peak_ratio = $config->ratio($bench, 'peak');
	    # Draw the base bar
	    if ($base_ratio ne '') {
		my $w = $base_ratio/$max->{'ratio'}*$graph_width;
		$w = 0 if (@{$config->{'nc'}}+0);
		$p->gsave();
		    $p->setgray(0.8);
		    $p->rect($graph_x, $height+$hsize/2, $w, $hsize/3);
		    $p->fill();
		$p->grestore();
		$p->rect($graph_x, $height+$hsize/2, $w, $hsize/3);
		$p->stroke();
	    }
	    # Draw the peak bar
	    if ($peak_ratio ne '') {
		my $w = $peak_ratio/$max->{'ratio'}*$graph_width;
		$w = 0 if (@{$config->{'nc'}}+0);
		$p->rect($graph_x, $height+$hsize/2, $w, -$hsize/3);
		$p->stroke();
	    }
	}
        # Draw the mean ratio (vertical) lines
	$p->gsave();
	    $p->setdash(2,2);
	    $x = $config->basemean()/$max->{'ratio'}*$graph_width + $graph_x;
	    $p->moveto($x, $height+$hsize/3);
	    $p->lineto($x, $top_y-$hsize/3);
	    $p->stroke();
	$p->grestore();
	$x = $config->peakmean()/$max->{'ratio'}*$graph_width + $graph_x;
	$p->moveto($x, $height+$hsize/3);
	$p->lineto($x, $top_y-$hsize/3);
	$p->stroke();
    }
    $p->grestore();
    return -$height;
}

# This is an odd one, the Y position is the TOP of the box, not the bottom
# as with all of the others, and it returns 0 on error, otherwise it 
# returns the length (height) used.
sub SPEC_detail ($$$$$$) {
    my ($p, $x, $y, $width, $height, $config) = @_;
    my ($str, $size, $len);
    my $fontsize = 10;
    $p->gsave();
	$p->set_font("Times-Roman", 10);
	my $border = 10;

        my $split = $width * 0.5;
	my @hardware = $config->hardware;
	shift(@hardware); # Remove the hw_vendor field
	shift(@hardware); # Remove the hw_model field
	my $left_len = SPEC_info_box ($p, $x+$border, $y, 
		$split-$border*2, $height, $config, [
		'Hardware', @hardware, 
		], $fontsize);
	return 0 if $left_len == 0;

	my $right_len = SPEC_info_box ($p, $x+$border+$split, $y, 
		$width-$split-$border*2, $height, $config, [
		'Software', $config->software,
		], $fontsize);
	return 0 if $left_len == 0;

	$len = ($left_len > $right_len)?$left_len:$right_len;
	$len += $border;

	$p->rect($x, $y-$len, $width, $len);
	$p->rect($x, $y-$len, $split, $len);
	$p->stroke();

	if ($len > $height) {
	    ::Log(0, "Too many lines in the detail boxes!\n");
	}
    $p->grestore();
    return $len;
}

sub SPEC_title_right {
    my ($p, $x, $y, $width, $height, $config) = @_;
    my ($str, $size);
    $p->gsave();
	$p->translate($x,$y);
        # Shaded area for base
        $p->rect_fill(0, 0, $width, $height / 2, 0.8);
        # Box for the whole thing
	$p->rect(0,0, $width,$height);
	$p->stroke();

        # Leave a little space around the sides
	my $border = min($width, $height) * 0.1;
	$p->translate ($border, 0);
	$width  -= $border * 2;
        my $initial_size = 18;

	$p->set_font("Times-Roman", $initial_size);
        $size = $initial_size;

        # Figure out the size of the font that we're going to use for all of
        # the metrics (needed so that the things can be nicely centered
        # vertically)
        foreach my $tmpstr ($config->peakunits.' = '.significant($config->peakmean),
                            $config->baseunits.' = '.significant($config->basemean)) {
          my $tmpsize = $p->string_calcsize($tmpstr, $width, $height / 2, 0.1,
                                            "ERROR: metric string plus mean (\"$tmpstr\") is too long!");
          $size = $tmpsize if ($tmpsize < $size && $tmpsize > 0);
        }

	return 0 if ($size <= 0);
        $p->set_font("Times-Roman", $size, 1);
        my $string_base = ($height / 4) - ($p->string_CapHeight() / 2);

        $p->set_text_pos(0, $height/2 + $string_base);
        $p->show($config->peakunits .' = ');

        $str = (@{$config->{'nc'}}+0) ? 'NC' : significant($config->peakmean);
        $p->set_text_pos($width, $height/2 + $string_base);
        $p->show_right($str);

        $p->set_text_pos(0, $string_base);
        $p->show($config->baseunits .' = ');

        $str = (@{$config->{'nc'}}+0) ? 'NC' : significant($config->basemean);
        $p->set_text_pos($width, $string_base);
        $p->show_right($str);
        #$p->setgray(0);

    $p->grestore();
    return 1;
}

sub SPEC_title_left {
    my ($p, $x, $y, $width, $height, $config) = @_;
    my $initial_size = 25;  # Pretty big, eh?

    $p->gsave();
	$p->translate($x,$y);
        # Make the box outline
	$p->rect(0,0, $width,$height);
	$p->stroke();

	$p->set_font("Times-Roman", $initial_size);

        # Arrange for a little space on the sides
	my $border = min($width, $height) * 0.1;
	$width  -= $border * 2;
	$p->translate ($border, 1);

        # Treat the two areas separately in terms of font size.

        my $size = $p->strings_fit(0, $height / 2, $width, $height / 2,
                                  $min_font_size, $initial_size,
                                  "Your hw_vendor line is too long!\n",
                                  $config->{'hw_vendor'});
	return 0 if ($size <= 0);

        my $size = $p->strings_fit(0, 0, $width, $height / 2,
                                  $min_font_size, $initial_size,
                                  "Your hw_model line is too long!\n",
                                  $config->{'hw_model'});
	return 0 if ($size <= 0);
    $p->grestore();
    return 1;
}

sub SPEC_title {
    my ($p, $x, $y, $width, $height, $config) = @_;
    my $rc = 0;
    my $split = $width * 0.56;
    $rc = SPEC_title_left ($p, $x,            $y, $split, $height, $config);
    return 0 if $rc == 0;
    $rc = SPEC_title_right($p, $split, $y, $width-$split, $height, $config);
    return $rc;
}

sub firstof($) {
    my ($val) = @_;
    $val = $val->[0] if (ref($val) eq 'ARRAY');
    return $val;
}

sub istrue {
    return main::istrue(@_);
}

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
    # STUPID STUPID HACK which circumvents the whole idea
    # of 3 significant digits.
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

# Figure out how many (if any) decimal places to invisibly tack on to
# make sure that decimal alignment is.
sub figure_dp {
    my ($value, $min_log, $low_log) = @_;
    return '' unless ($value ne '');
    print " figure_dp($value, $min_log, $low_log);\n" if ($debug & 4);
    my $s = '';
    my ($real_dp, $wanted_dp, $dp, $space, $log);
    if ($value == 0) {
	if ($value !~ m/^\s*(\+|-)?[0-9.eE+]/) {
	    print "Returning '$value'\n" if ($debug & 4);
	    return $value;
	}
	$log = 0;
    } else {
	$log = &floor(log($value)/log(10)); 
    }
    $min_log--;
    print "  log=$log  min_log=$min_log\n" if ($debug & 4);
    print "  value=$value\n" if ($debug & 4);
    $dp        = ($low_log>=$min_log)?0:3-$low_log;
    $wanted_dp = ($log>=$min_log)?0:$min_log-$log;
    print "  dp=$dp   wanted_dp=$wanted_dp\n" if ($debug & 4);
    if ($dp > $wanted_dp) {
	$space = $dp - $wanted_dp;
	$real_dp = $wanted_dp;
    } else {
	$space = 0;
	$real_dp = $dp;
    }
    if ($real_dp == 0 && $dp > 0) {
	$s .= '.';
    }
    print "  space=$space  real_dp=$real_dp\n" if ($debug & 4);
    $s .= '0' x ($space) if $space > 0;
    return $s;
}

# Draw the smaller ticks on the graph
sub draw_minor_ticks {
    my ($p, $x, $y, $interval) = @_;
    # We gsave and grestore to negate any possible effects from cumulative
    # error, and also to be sure we don't screw anyone else up.
    $p->gsave();
        # First, the tick at the halfway point
        $p->moveto($x + ($interval/2), $y);
        $p->lineto($x + ($interval/2), $y + 4);
        $p->stroke();
        # Next, the two quarter point marks
        for (my $i = 0.25; $i < 1; $i += 0.5) {
            $p->moveto($x + ($i * $interval), $y);
	    $p->lineto($x + ($i * $interval), $y + 2);
	    $p->stroke();
        }
        # Finally, the eighth-point marks
        for (my $i = 0.125; $i < 1; $i += 0.25) {
 	    $p->moveto($x + ($i * $interval), $y);
	    $p->lineto($x + ($i * $interval), $y + 1);
	    $p->stroke();
        }
    $p->grestore();
}

sub min {
  my (@values) = @_;
  # Return the minimum of a list of values
  my $min = shift(@values);
  foreach my $val (@values) {
    $min = $val if ($val < $min);
  }
  return $min;
 }

sub max {
  my (@values) = @_;
  # Return the maximum of a list of values
  my $max = shift(@values);
  foreach my $val (@values) {
    $max = $val if ($val > $max);
  }
  return $max;
}

sub dist {
  my ($x1, $y1, $x2, $y2) = @_;
  my $dx = $x2 - $x1;
  my $dy = $y2 - $y1;

  return sqrt(($dx * $dx) + ($dy * $dy));
}

sub joinup {
  my ($ref) = @_;

  return $ref unless ref($ref);
  if (ref($ref) eq 'ARRAY') {
    return join(' ', @{$ref});
  }
}

