#
#  pdf.pl - produces PDF output
#  Copyright (C) 1999-2005 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Author:  Christopher Chan-Nui
#
# $Id: pdf.pl 1585 2005-08-24 23:20:16Z cloyce $

use strict;
use PSPDF;

use vars qw($name $extension $binary);

$name      = 'pdf';
$extension = 'pdf';
$binary    = 1;

sub format () {
    my($me, $r, $path) = @_;
    my @output = split ("\n", Spec::Format::ps::SPEC_report($r, 'PDF', $path));
    return (\@output, []);
}
