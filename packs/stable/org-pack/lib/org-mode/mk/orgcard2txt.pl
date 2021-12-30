# orgcard2txt.pl - a script to generate orgcard.txt from orgcard.tex
# Copyright (C) 2010, 2013 Osamu OKANO
#
# Version: 0.1
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
# Usage:
# ======
#  perl orgcard2txt.pl orgcard.tex > orgcard.txt
use strict;
use warnings;

sub rep_esc{
  my $s = shift @_;
  $s =~ s/\\kbd{([^}]+)}/$1/g;
  $s =~ s/\$\^([0-9])\$/[$1]/g;
  $s =~ s/\\rm //g;
  $s =~ s/\\\///g;
  $s =~ s/\\\^{}/^/g;
  $s =~ s/\\}/}/g;
  $s =~ s/\\{/{/g;
  $s =~ s/\\\#/#/g;
  $s =~ s/\\\^/^/g;
  $s =~ s/\\\%/%/g;
  $s =~ s/\\\_/_/g;
  $s =~ s/\\\&/&/g;
  $s =~ s/\\\$/\$/g;
  $s =~ s/\$\\leftrightarrow\$/<->/g;
  $s =~ s/\$\\pm 1\$/±1/g;
  $s =~ s/``{\\tt ([^}]+)}''/`$1'/g;
  return $s;
}
my $page=0;
my $orgversionnumber;

open(IN,$ARGV[0]);
while(<IN>){
  last if(/\f/);
  $orgversionnumber = $1 if /\\def\\orgversionnumber{([^}]+)}/;
}
close(IN);

print <<HEAD;
================================================================================
Org Mode Reference Card (for version $orgversionnumber)
================================================================================
HEAD

my $key;
my $value;

format STDOUT =
@<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$key,$value
.

open(IN,$ARGV[0]);
while(<IN>){
  if(/\f/){
    $page = $page + 1;
    next;
  }
  next if($page != 1);
  next if(/^%/);
  next if /Org Mode Reference Card \([12]\/2\)/;
  next if /\\centerline{\(for version \\orgversionnumber\)}/;
  next if /\(for version  \)/;
  next if /\\newcolumn/;
  next if /\\copyrightnotice/;
  next if /\\bye/;
  next if /\\title{([^}]+)}/;
  chomp;
#  print "b:$_\n";
  s/([^\\])\%.+$/$1/;
#  print "a:$_\n";
  if (/\\section{(.+)}/){
    my $sec = rep_esc($1);
    print "================================================================================\n";
    print "$sec\n";
    print "================================================================================\n";
    next;
  }
  if (/{\\bf (.+)}/){
    my $bf = rep_esc($1);
    print "--------------------------------------------------------------------------------\n";
    print "$bf\n";
    print "--------------------------------------------------------------------------------\n";
    next;
  }
  if (/^{\\it (.+)}/){
    my $it = rep_esc($1);
    print "--------------------------------------------------------------------------------\n";
    print "$it\n";
    print "--------------------------------------------------------------------------------\n";
    next;
  }
  if(/^\\key{(.+)}\s*$/||/^\\metax{(.+)}\s*$/){
    my ($k,$v) = split(/}{/,$1);
    my $k2 = &rep_esc($k);
    my $v2 = &rep_esc($v);
#    print "$k2\t$v2\n";
    ($key,$value)=($k2,$v2);
    write;
    next;
  }
  my $line = rep_esc($_);
  $line =~ s/{\\it ([^}]+)}/$1/g;
  $line =~ s/{\\tt ([^}]+)}/$1/g;
  print "$line\n";
}
close(IN);
