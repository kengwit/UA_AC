#
# PSPDF.pm
# Copyright (C) 1999-2005 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: PSPDF.pm 1585 2005-08-24 23:20:16Z cloyce $
#

require 'util.pl';

package PSPDF;

my $pdf_ok = [ ::pdf_ok(), '' ];
my $ps_ok = ::ps_ok();
eval "use PDF::API2::Lite;" if $pdf_ok;
$pdf_ok = [ 0, $@ ] if $@;
use Font::AFM;
use IO::File;
use strict;

use vars qw($indent %afms);
$indent = '';

sub pdf_ok () {
    return $pdf_ok->[0];
}

sub new {
    my ($class, $mode) = @_;
    if ($mode ne 'PS' && $mode ne 'PDF') {
	main::Log(0, "PSPDF::new: mode of '$mode' not supported, using 'PS'\n");
	$mode = 'PS';
    }
    if ($mode eq 'PDF' && !$pdf_ok->[0]) {
	main::Log(100, "PSPDF::new: Cannot load 'PDF::API2::Lite' module for PDF:\n  $pdf_ok->[1]\n");
	return bless { 'error' => 1 }, $class;
    }
    if ($mode eq 'PS' && !$ps_ok) {
	main::Log(100, "PSPDF::new: PostScript output is disabled\n");
	return bless { 'error' => 1 }, $class;
    }
    my $me = bless {
	'fontdir' => '.',     # Directory where fonts live
	'mode'    => $mode,   # This should be PS or PDF
	'output'  => '',
	'isopen'  => 0,
	'CreationDate' => '',
	'Creator' => '',
	'Author'  => '',
	'Title'   => '',
	'Subject' => '',
	'changedfont' => 0,
	'states'  => [],
    }, $class;
    return $me;
}

sub output {
    my ($me) = @_;
    if ($me->{'isopen'}) {
	main::Log(0, "PSPDF::output: called when object still open!\n");
	return '';
    }
    if ($me->{'mode'} eq 'PS') {	# Fix up the number of pages
	my $pages = $me->{'pagenum'} || 0;
	$me->{'output'} =~ s/%%PUTPAGESHERE%%/$pages/;
    }
    return $me->{'output'};
}

sub DESTROY {
    my ($me) = @_;
    unlink ($me->{'outputname'}) if $me->{'outputname'} ne '';
}

sub set_fontdir {
    my ($me, $dir) = @_;
    $me->{'fontdir'} = $dir;
    unshift (@Font::AFM::metrics_path, split (":", $dir));
}

sub anon_var {
    my ($me, $name, $val) = @_;
    my $old = $me->{$name};
    $me->{$name} = $val if defined $val;
    return $old;
}
sub CreationDate { my $me = shift; return $me->anon_var('CreationDate', @_); }
sub Creator { my $me = shift; return $me->anon_var('Creator', @_); }
sub Author  { my $me = shift; return $me->anon_var('Author' , @_); }
sub Title   { my $me = shift; return $me->anon_var('Title'  , @_); }
sub Subject { my $me = shift; return $me->anon_var('Subject', @_); }

sub open {
    my ($me) = @_;
    if ($me->{'mode'} eq 'PS') {
	$me->{'output'}="%!PS-Adobe-3.0\n";
	$me->{'output'}.="%%Pages: %%PUTPAGESHERE%%\n";
    } elsif ($me->{'mode'} eq 'PDF') {
	$me->{'pdf'} = PDF::API2::Lite->new();
        #$me->{'pdf'}->{'api'}->{'forcecompress'} = 0;
	$me->{'ip'} = {};
	for my $foo (qw(CreationDate Creator Author Title Subject)) {
	    $me->{'ip'}->{$foo} = $me->{$foo};
	}
        $me->{'ip'}->{'Producer'} = $me->{'ip'}->{'Creator'};
	$me->{'pdf'}->{'api'}->info(%{$me->{'ip'}});
    }
    $me->{'isopen'}=1;
}

sub close {
    my ($me) = @_;
    if ($me->{'mode'} eq 'PS') {
	$me->{'output'} .= "%%EOF\n";
    } elsif ($me->{'mode'} eq 'PDF' && $me->{'isopen'}) {
	$me->{'output'} = $me->{'pdf'}->saveas('-');
    }
    $me->{'isopen'}=0;
}

use vars qw(@pagenumtxt);
@pagenumtxt = qw(zero one two three four five size seven eight nine ten);

sub begin_page {
    my ($me, $size) = @_;

    if ($size eq 'letter') {
	$me->{'width'}  = 612;
	$me->{'height'} = 792;
    } elsif ($size eq 'a4') {
	$me->{'width'}  = 595;
	$me->{'height'} = 842;
    }
    if ($me->{'mode'} eq 'PDF') {
	$me->{'pdf'}->page($me->{'width'}, $me->{'height'});
	$me->{'pagenum'}++;
    } elsif ($me->{'mode'} eq 'PS') {
	my $num = ++$me->{'pagenum'};
	$me->{'output'} .= "%%Page: $pagenumtxt[$num] $num\n";
    }
}

sub end_page {
    my ($me) = @_;
    if ($me->{'mode'} eq 'PS') {
	$me->{'output'} .= "showpage\n";
    }
}

sub set_font {
    my ($me, $font, $size, $force) = @_;
    $size = $me->{'fontsize'} unless defined($size);
    if (exists $afms{$font} && ref($afms{$font}) eq 'Font::AFM') {
        $me->{'afm'} = $afms{$font};
    } else {
	$me->{'afm'} = new Font::AFM $font;
	$afms{$font} = $me->{'afm'};
    }
    return if (!(defined($force) && $force) && $me->{'fontsize'} == $size && $me->{'fontname'} eq $font );
    $me->{'fontsize'} = $size;
    $me->{'fontname'} = $font;
    $me->{'changedfont'} = 1;
    if ($me->{'mode'} eq 'PDF') {
	my $encoding = 'winansi';
	$encoding = 'builtin' if ($me->{'fontname'} eq 'Symbol');
	# It's hard to believe that PDF::API2 doesn't do this internally!
	if (exists $me->{'pdffonts'}->{$me->{'fontname'}.$encoding}) {
	    $me->{'currfont'} = $me->{'pdffonts'}->{$me->{'fontname'}.$encoding};
	} else {
	    $me->{'currfont'} = $me->{'pdffonts'}->{$me->{'fontname'}.$encoding} = 
		$me->{'pdf'}->corefont($me->{'fontname'},
				       '-encoding' => $encoding);
	}
    }
}

sub set_font_really {
    my ($me) = @_;
    if ($me->{'mode'} eq 'PDF') {
	$me->set_font($me->{'fontname'}, $me->{'fontsize'});
    } else {
	$me->{'output'} .= "/$me->{'fontname'} findfont $me->{'fontsize'} scalefont setfont\n";
    }
}

sub set_text_pos {
    my ($me, $x, $y) = @_;
    $me->{'text_x'} = $x;
    $me->{'text_y'} = $y;

    if ($me->{'mode'} eq 'PS') {
	$me->{'output'} .= "$x $y moveto\n";
    }
}

sub stringwidth {
    my ($me, $str, $size) = @_;
    my $rc = 0;
    $size = $me->{'fontsize'} if !defined $size;

    # Heck always use the AFM stuff, this prevents us from perhaps
    # inadvertantly growing the PDF file.
    $rc = $me->{'afm'}->stringwidth($str, $size);

    # Here's a hack...
    if ($rc == 0 && $str ne '' && $me->{'fontname'} eq 'Symbol') {
	# It seems unlikely that any symbol would have absolutely zero width...
	$rc = $me->{'afm'}->stringwidth(' ' x (length($str) + 2), $size);
    }
    return $rc;
}

sub string_fit {
    my ($me, $str, $x, $y, $width, $height, $minsize, $errtext) = @_;
#print "\nstring_fit($me, \"$str\", $x, $y, $width, $height, $minsize, $errtext) in $me->{'fontname'}\@$me->{'fontsize'} ";
    my $size = $me->string_calcsize($str, $width, $height, $minsize, $errtext);
    return 0 if ($size <= 0);
    $me->set_font($me->{fontname}, $size);
    $me->set_text_pos($x, $y+($height-$size)/2);
#print "=> ($x, ".($y+($height-$size)/2).") in $me->{'fontname'}\@$size\n";
    $me->show($str);
    return $size;
}
sub string_fit_right {
    my ($me, $str, $x, $y, $width, $height, $minsize, $errtext) = @_;
    my $size = $me->string_calcsize($str, $width, $height, $minsize, $errtext);
    return 0 if ($size <= 0);
    $me->set_font($me->{fontname}, $size);
    $me->set_text_pos($x, $y+($height-$size)/2);
#print "string_fit_right showing \"$str\" at ($x,".($y+($height - $size)/2).") in ".$me->{fontname}." $size\n";
    $me->show_right($str);
    return $size;
}

sub strings_fit {
    my ($me, $x, $y, $width, $height, $minsize, $maxsize, $errtext, @lines) = @_;
    # Do the right thing for array refs
    for(my $i = 0; $i < @lines+0; $i++) {
      if (ref($lines[$i]) eq 'ARRAY') {
	splice(@lines, $i, 1, @{$lines[$i]});
      }
    }
    my $numlines = @lines+0;
    return 1 if ($numlines == 0);
    if ($maxsize > $height / ($numlines * 1.1)) {
      $maxsize = $height / ($numlines * 1.1);
    }

    # Get a rough cut at the font size
    my $size = $maxsize;
    foreach my $line (@lines) {
      my $tmpsize = $me->string_calcsize($line, $width, $height / $numlines,
					 $minsize, $errtext);
      return 0 if ($tmpsize <= 0);
      $size = $tmpsize if ($tmpsize < $size);
    }
    $me->set_font($me->{fontname}, $size);

    # Arrange for some space for the descenders, plus a bit
    $y += abs($me->string_Descender()) + 1;
    $height -= ($me->string_Descender()) + 2;

    # Do the size calc over again with the newly revised height
    foreach my $line (@lines) {
      my $tmpsize = $me->string_calcsize($line, $width, $height / $numlines,
					 $minsize, $errtext);
      return 0 if ($tmpsize <= 0);
      $size = $tmpsize if ($tmpsize < $size);
    }
    $me->set_font($me->{fontname}, $size, 1);

    # Actually show the strings
    my $string_base = ($height / $numlines) - ($me->string_CapHeight() / 2) + 1;
    for(my $i = $numlines - 1; $i >= 0; $i--) {
      $me->set_text_pos($x, $y + ($string_base * ($numlines - $i - 1)));
      $me->show($lines[$i]);
    }
    return $size;
}
sub string_calcsize {
    my ($me, $str, $width, $height, $minsize, $errtext) = @_;
#print "\nstring_calcsize($me, \"$str\", $width, $height, $minsize, $errtext)\n";
    my $w = $me->{'afm'}->stringwidth($str, $height);
    return $height if $w == 0;
    my $size = $width / $w * $height;
    $size = $height if $size > $height;
#print "w=$w, size=$size\n";
    return $size if !defined $minsize;
    if ($size < $minsize && $errtext ne '') {
	my $txt = $errtext;
	$txt =~ s/%s/sprintf "%.2f", $size/eg;
	main::Log(0, "\n$txt\n");
    }
    return $size;
}

sub string_XHeight {
    my ($me, $size) = @_;
    $size = $me->{'fontsize'} if ! defined $size;
    return $me->{'afm'}->XHeight() * $size / 1000;
}
sub string_CapHeight {
    my ($me, $size) = @_;
    $size = $me->{'fontsize'} if ! defined $size;
    return $me->{'afm'}->CapHeight() * $size / 1000;
}
sub string_Descender {
    my ($me, $size) = @_;
    $size = $me->{'fontsize'} if ! defined $size;
    return $me->{'afm'}->Descender() * $size / 1000;
}
sub string_Ascender {
    my ($me, $size) = @_;
    $size = $me->{'fontsize'} if ! defined $size;
    return $me->{'afm'}->Ascender() * $size / 1000;
}

sub fontsize {
    my ($me) = @_;

    return $me->{'fontsize'};
}

sub show {
    my ($me, $str) = @_;

    if ($me->{'changedfont'}) {
	$me->set_font_really();
	$me->{'changedfont'} = 0;
    }
    if ($me->{'mode'} eq 'PDF') {
	$me->{'pdf'}->textstart;
	$me->{'pdf'}->textfont($me->{'currfont'}, $me->{'fontsize'});
	$me->{'pdf'}->transform(-translate => [ $me->{'text_x'}, $me->{'text_y'} ]);
	$me->{'pdf'}->text($str);
	$me->{'pdf'}->textend;
	$me->{'text_x'} += $me->stringwidth($str);
    } else {
	$str =~ s/([(%)])/\\$1/g;
#	$me->{'output'} .= "($str \(".sprintf("%.2f", $me->{'fontsize'})."\)) show\n";
	$me->{'output'} .= "($str) show\n";
    }
}

sub continue_text {
    my ($me, $str) = @_;

    # This is probably wrong for PDF
    $me->{'text_y'} -= $me->{'fontsize'};

    if ($me->{'changedfont'}) {
	$me->set_font_really();
	$me->{'changedfont'} = 0;
    }

    if ($me->{'mode'} eq 'PDF') {
	$me->show($str);
    } else {
	$str =~ s/([(%)])/\\$1/g;
	$me->{'output'} .= "$me->{'text_x'} $me->{'text_y'} moveto ($str) show\n";
    }
}

sub show_center {
    my ($me, $str) = @_;

    my $x = $me->{'text_x'};
    my $y = $me->{'text_y'};
    my $oldx = $x;

    $x -= $me->stringwidth($str)/2;

    $me->set_text_pos($x, $y);
    $me->show($str);
    $me->{'text_x'} = $oldx;
}

sub continue_text_center {
    my ($me, $str) = @_;
    $me->{'text_y'} -= $me->{'fontsize'};
    $me->show_center($str);
}

sub show_right {
    my ($me, $str) = @_;

    my $x = $me->{'text_x'};
    my $y = $me->{'text_y'};
    my $oldx = $x;


    $x -= $me->stringwidth($str);

    $me->set_text_pos($x, $y);
    $me->show($str);
    $me->{'text_x'} = $oldx;
}

sub continue_text_right {
    my ($me, $str) = @_;
    $me->{'text_y'} -= $me->{'fontsize'};
    $me->show_right($str);
}

sub moveto {
    my ($me, $x, $y) = @_;

    $me->{'x'} = $x;
    $me->{'y'} = $y;

    if ($me->{'mode'} eq 'PDF') {
	$me->{'pdf'}->move($x, $y);
    } else {
	$me->{'output'} .= "$x $y moveto\n";
    }
}

sub lineto {
    my ($me, $x, $y) = @_;

    $me->{'x'} = $x;
    $me->{'y'} = $y;

    if ($me->{'mode'} eq 'PDF') {
	$me->{'pdf'}->line($x, $y);
    } else {
	$me->{'output'} .= "$x $y lineto\n";
    }
}

sub curveto {
    my ($me, $x1, $y1, $x2, $y2, $x3, $y3) = @_;

    $me->{'x'} = $x3;
    $me->{'y'} = $y3;

    if ($me->{'mode'} eq 'PDF') {
	$me->{'pdf'}->curve($x1, $y1, $x2, $y2, $x3, $y3);
    } else {
	$me->{'output'} .= "$x1 $y1 $x2 $y2 $x3 $y3 curveto\n";
    }
}

sub circle {
    my ($me, $x, $y, $r) = @_;

    $me->{'x'} = $x;
    $me->{'y'} = $y;
    if ($me->{'mode'} eq 'PDF') {
        $me->{'pdf'}->circle($x, $y, $r);
    } else {
        $me->{'output'} .= "newpath $x $y $r 0 360 arc closepath\n";
    }
}

sub circle_fill {
    my ($me, $x, $y, $r, $gray, $width, $do_line) = @_;

    $me->{'x'} = $x;
    $me->{'y'} = $y;
    $me->gsave();
	$me->setgray($gray)       if defined $gray;
	$me->circle($x, $y, $r);
	$me->fill();
    $me->grestore();
    if ((defined($do_line) && $do_line) || defined($width)) {
        $me->gsave();
	    $me->setlinewidth($width) if defined $width;
	    $me->circle($x, $y, $r);
	    $me->stroke();
        $me->grestore();
    }
}

sub translate {
    my ($me, $x, $y) = @_;

    return if ($x == 0 && $y == 0);

    if ($me->{'mode'} eq 'PDF') {
	$me->{'pdf'}->transform(-translate => [ $x, $y ]);
    } else {
	$me->{'output'} .= "$x $y translate\n";
    }
}

sub rect {
    my ($me, $x, $y, $w, $h) = @_;

    $me->{'x'} = $x;
    $me->{'y'} = $y;

    my $nh = -$h;

    if ($me->{'mode'} eq 'PDF') {
	$me->{'pdf'}->rect($x, $y, $w, $h);
    } else {
	$me->{'output'} .= "$x $y moveto 0 $h rlineto $w 0 rlineto 0 $nh rlineto closepath\n";
    }
}

sub rectxy {
    my ($me, $x1, $y1, $x2, $y2) = @_;

    # Draw a rectange with opposite corners at (x1,y1) and (x2,y2)
    $me->rect($x1, $y1, ($x2 - $x1), ($y2 - $y1));
}

sub rect_fill {
    my ($me, $x, $y, $w, $h, $gray, $width, $do_line) = @_;
#print "rect_fill($me, $x, $y, $w, $h, $gray, $width, $do_line)\n";
    $me->{'x'} = $x;
    $me->{'y'} = $y;
    $me->gsave();
	$me->setgray($gray)       if defined $gray;
	$me->rect($x, $y, $w, $h);
	$me->fill();
    $me->grestore();
    if ((defined($do_line) && $do_line) || defined($width)) {
        $me->gsave();
	    $me->setlinewidth($width) if defined $width;
	    $me->rect($x, $y, $w, $h);
	    $me->stroke();
        $me->grestore();
    }
}

sub rectxy_fill {
    my ($me, $x1, $y1, $x2, $y2, $gray, $width, $do_line) = @_;

    # Draw & fill a rectange with opposite corners at (x1,y1) and (x2,y2)
    $me->rect_fill($x1, $y1, ($x2 - $x1), ($y2 - $y1), $gray, $width);
}

sub poly {
    my ($me, @coords) = @_;

    my ($x, $y) = @coords[0,1];
    return unless defined($x) && defined($y);

    # Snip dangling ordinate, if any
    if ($#coords % 2 == 0) {
        splice(@coords,-1);
    }

    my ($lastx, $lasty) = @coords[$#coords-1, $#coords];

    $me->{'x'} = $x;
    $me->{'y'} = $y;
    if ($me->{'mode'} eq 'PDF') {
	$me->{'pdf'}->poly(@coords);
    } else {
	($x, $y) = splice(@coords, 0, 2);
	return unless defined($x) && defined($y);
	$me->moveto($x, $y);
	while (@coords) {
	    ($x, $y) = splice(@coords, 0, 2);
	    last if (!defined($x) || !defined($y));
	    $me->lineto($x, $y);
	} 
    }
    $me->closepath if (($lastx != $me->{'x'}) || ($lasty != $me->{'y'}));
}

sub poly_fill {
    my ($me, $coords, $gray, $width, $do_line) = @_;
    return unless ref($coords) eq 'ARRAY';

    my ($x, $y) = @{$coords}[0,1];
    return unless defined($x) && defined($y);

    $me->{'x'} = $x;
    $me->{'y'} = $y;
    $me->gsave();
	$me->setgray($gray)       if defined $gray;
	$me->poly(@{$coords});
	$me->fill();
    $me->grestore();
    if ((defined($do_line) && $do_line) || defined($width)) {
        $me->gsave();
	    $me->setlinewidth($width) if defined $width;
            $me->poly(@{$coords});
            $me->stroke();
	$me->grestore();
    }
}
	
sub setdash {
    my ($me, $a, $b) = @_;

    if ($me->{'mode'} eq 'PDF') {
	$me->{'pdf'}->linedash($a, $b);
    } else {
	$me->{'output'} .= "[ $a $b ] 0 setdash\n";
    }
}

sub setgray {
    my ($me, $gray) = @_;

    if ($me->{'mode'} eq 'PDF') {
	$me->setrgbcolor($gray, $gray, $gray);
    } else {
	$me->{'output'} .= "$gray setgray\n";
    }

    $me->{'color'} = $gray;
}

sub setrgbcolor {
    my ($me, $red, $green, $blue) = @_;

    $me->{'color'} = [ $red, $green, $blue ];

    if ($me->{'mode'} eq 'PDF') {
	$me->{'pdf'}->fillcolor(sprintf("#%02x%02x%02x",
					$red * 255, $green * 255, $blue * 255));
	$me->{'pdf'}->strokecolor(sprintf("#%02x%02x%02x",
					  $red * 255, $green * 255, $blue * 255));
    } else {
	$me->{'output'} .= "$red $green $blue setrgbcolor\n";
    }
}

sub setlinewidth {
    my ($me, $w) = @_;

    if ($me->{'mode'} eq 'PDF') {
	$me->{'pdf'}->linewidth($w);
    } else {
	$me->{'output'} .= "$w setlinewidth\n";
    }
}

sub rotate {
    my ($me, $w) = @_;

    if ($me->{'mode'} eq 'PDF') {
	$me->{'pdf'}->transform(-rotate => $w);
    } else {
	$me->{'output'} .= "$w rotate\n";
    }
}

sub fill {
    my ($me) = @_;

    if ($me->{'mode'} eq 'PDF') {
	$me->{'pdf'}->fill;
    } else {
	$me->{'output'} .= "fill\n";
    }
}

sub stroke {
    my ($me) = @_;

    if ($me->{'mode'} eq 'PDF') {
	$me->{'pdf'}->stroke;
    } else {
	$me->{'output'} .= "stroke\n";
    }
}

sub closepath {
    my ($me) = @_;

    if ($me->{'mode'} ne 'PDF') {
	$me->{'output'} .= "closepath\n";
    }
}

my @state_vars = qw(color x y text_x text_y fontsize fontname);
sub gsave {
    my ($me) = @_;
#print "${indent}gsave\n"; $indent .= '  ';
    my $state = {};
    for (@state_vars) {
	$state->{$_} = $me->{$_};
    }
    push (@{$me->{'states'}}, $state);
    if ($me->{'mode'} eq 'PDF') {
	$me->{'pdf'}->savestate;
    } else {
	$me->{'output'} .= "gsave\n";
    }
}
sub grestore {
    my ($me) = @_;
#print "${indent}grestore\n"; substr($indent, 0, 2) = '';
    my $state = pop(@{$me->{'states'}});
    for (keys %$state) {
	$me->{$_} = $state->{$_};
    }
    if ($me->{'mode'} eq 'PDF') {
	$me->{'pdf'}->restorestate;
    } else {
	$me->{'output'} .= "grestore\n";
    }
}

sub add_raw {
    my ($me, $filename, $compression, $rawlines) = @_;

    # Add the raw file in.  This must happen before everything is closed out,
    # because when the PDFs are compressed, text after the %%EOF chokes
    # some interpreters.
    my $rawfilestring = "% The following is an encoded version of the raw file that was used to produce\n% this result.  Use the extract_raw script to retrieve it.\n% BEGIN";
    $rawfilestring .= "$compression $filename\n";
    $rawfilestring .= '% '.join("\n% ", split("\n", $rawlines))."\n";
    $rawfilestring .= "% END\n";
    if ($me->{'mode'} eq 'PS') {
        $me->{'output'} .= $rawfilestring;
    } elsif ($me->{'mode'} eq 'PDF') {
        # Fake up an XML container for data that the tools will still be able
        # to decode.
        my $xml = "<rawfile name=\"$filename\">\n";
        $xml .= "<content>\n$rawfilestring\n</content>\n</rawfile>\n";
        $me->{'pdf'}->{'api'}->xmpMetadata($xml);
    } else {
        # Do nothing; this shouldn't happen!
    }
}

