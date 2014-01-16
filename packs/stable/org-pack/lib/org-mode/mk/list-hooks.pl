#!/usr/bin/perl
@files = glob("lisp/org-*.el");
unshift @files,"lisp/org.el";

print "* Hooks and Function variables\n\n";

foreach $file (@files) {
  ($file1 = $file) =~ s|.*/||;
  open IN,"<$file" or die "Cannot open file $file\n";
  while (<IN>) {
    if (/^\((defvar|defcustom)\s+(org-.*?-(hook|functions?)\b)/) {
      $deftype = $1;
      $name = $2;
      $_=<IN> while (not m/^\s*"/);
      $doc = $_;
      while (not m/(?<!\\)"\)?\s*$/) {
	$_=<IN>;
	$doc .=$_;
      }
      $doc =~ s/\A\s*"//;
      $doc =~ s/"\)?\s*\Z//;
      print "** =$name=\n";
      print "Defined in: /$file1/\n";
      print "#+begin_example\n";
      @lines = split(/\n/,$doc);
      @lines = map { $_ = "    " . $_ } @lines;
      $doc = join("\n",@lines);
      print "$doc\n";
      print "#+end_example\n";
    }
  }
}
