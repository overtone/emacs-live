#!/usr/bin/perl

$failures = 0;

while ($page = shift) {
system "mv $page $page.orig";
open IN,"<$page.orig" or die "Cannot read from $page.orig\n";
open OUT,">$page" or die "Cannot write to $page\n";

$toc = undef;

while (<IN>) {
  if (/<meta http-equiv="Content-Style-Type" content="text\/css">/) {
    print OUT;
    print OUT '<link rel="stylesheet" href="https://orgmode.org/org-manual.css" type="text/css" />';
  } elsif (/<div class="contents">/) {
    print OUT;
    print OUT '<p>This is the official manual for the latest <a href="https://orgmode.org">Org mode</a> release.</p><div id="table-of-contents">';
  } elsif (/<h2>Table of Contents<\/h2>|<h2 class="contents-heading">/) {
    print OUT;
    print OUT '<a href="https://orgmode.org">https://orgmode.org</a><br/><div id="text-table-of-contents">';
    $toc = 1;
  } elsif (/<\/div>/ and $toc) {
    print OUT "</div></div></div>";
    $toc = 0;
  } else {
    print OUT;
  }
}
system "rm $page.orig";

if (!defined($toc) || $toc != 0) {
  ++$failures;
  print STDERR "Patching of $page failed\n";
}
}

$failures == 0 or die "Patching of $failures files failed\n";
