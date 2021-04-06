#!/usr/bin/perl

$file1 = shift;
$file2 = shift;

open file1,"<$file1" or die;
while (<file1>) {
  if (m/^\s*\((defun|defsubst|defmacro|defcustom|defgroup|defface|defvar|defconst)\s+([-a-zA-Z0-9]+)/) {
    if ($1 eq "defun") {
      $fun1{$2}++;
    } elsif ($1 eq "defsubst") {
      $subst1{$2}++;
    } elsif ($1 eq "defmacro") {
      $macro1{$2}++;
    } elsif ($1 eq "defgroup") {
      $group1{$2}++;
    } elsif ($1 eq "defcustom") {
      $custom1{$2}++;
    } elsif ($1 eq "defface") {
      $face1{$2}++;
    } elsif ($1 eq "defvar") {
      $var1{$2}++;
    } elsif ($1 eq "defconst") {
      $const1{$2}++;
    }
  }
}
close file1;

open file2,"<$file2" or die;
while (<file2>) {
  if (m/^\s*\((defun|defsubst|defmacro|defcustom|defgroup|defface|defvar|defconst)\s+([-a-zA-Z0-9]+)/) {
    if ($1 eq "defun") {
      $fun2{$2}++;
    } elsif ($1 eq "defsubst") {
      $subst2{$2}++;
    } elsif ($1 eq "defmacro") {
      $macro2{$2}++;
    } elsif ($1 eq "defgroup") {
      $group2{$2}++;
    } elsif ($1 eq "defcustom") {
      $custom2{$2}++;
    } elsif ($1 eq "defface") {
      $face2{$2}++;
    } elsif ($1 eq "defvar") {
      $var2{$2}++;
    } elsif ($1 eq "defconst") {
      $const2{$2}++;
    }
  }
}
close file2;

foreach $type ("fun","subst","macro","group","custom","face","var","const") {
  $cmd1 = '%n1 = %' . $type . "1;";
  $cmd2 = '%n2 = %' . $type . "2;";
  eval $cmd1;
  eval $cmd2;
  
  print "$type added:\n";
  foreach (keys %n2) {
    unless (defined $n1{$_}) {
      print "  $_\n";
    }
  }
  print "$type removed:\n";
  foreach (keys %n1) {
    unless (defined $n2{$_}) {
      print "  $_\n";
    }
  }
}
