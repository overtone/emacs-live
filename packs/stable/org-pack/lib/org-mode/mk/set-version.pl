#!/usr/bin/perl
$version = $ARGV[0];
if ($version eq "--all" or $version eq "-a") {
  $all = 1;
  $version = $ARGV[1]
}

if ($version eq "--only" or $version eq "-o") {
  $only = 1;
  $version = $ARGV[1]
}

die "No version given" unless $version=~/\S/;
$date = `date "+%B %Y"`; chomp $date;
$year = `date "+%Y"` ; chomp $year;

print STDERR "Changing version to \"$version\" and date to \"$date\" in all relevant files\n" ;

if (not $only) {

  print STDERR join("\n",glob("lisp/*.el")),"\n";
  $cmd = qq{s/^(;; Version:)\\s+(\\S+)[ \t]*\$/\$1 $version/;s/^(\\(defconst org-version )"(\\S+)"/\$1"$version"/};
  $c1 = "perl -pi -e '$cmd' lisp/*.el";
  system($c1);

  print STDERR "doc/org.texi\n";
  $cmd = qq{s/^(\\\@set VERSION)\\s+(\\S+)[ \t]*\$/\$1 $version/;s/^(\\\@set DATE)\\s+(.*)\$/\$1 $date/;};
  $c1 = "perl -pi -e '$cmd' doc/org.texi";
  system($c1);

  print STDERR "doc/orgguide.texi\n";
  $cmd = qq{s/^(\\\@set VERSION)\\s+(\\S+)[ \t]*\$/\$1 $version/;s/^(\\\@set DATE)\\s+(.*)\$/\$1 $date/;};
  $c1 = "perl -pi -e '$cmd' doc/orgguide.texi";
  system($c1);

  print STDERR "doc/orgcard.tex\n";
  $cmd = qq{s/^\\\\def\\\\orgversionnumber\\{\\S+\\}/\\\\def\\\\orgversionnumber{$version}/;s/\\\\def\\\\versionyear\\{\\S+\\}/\\\\def\\\\versionyear{$year}/;s/\\\\def\\\\year\\{\\S+\\}/\\\\def\\\\year{$year}/;};
  $c1 = "perl -pi -e '$cmd' doc/orgcard.tex";
  system($c1);

  print STDERR "README\n";
  $cmd = qq{s/^(The version of this release is:)\\s+(\\S+)[ \t]*\$/\$1 $version/;};
  $c1 = "perl -pi -e '$cmd' README";
  system($c1);
}
