#!/usr/bin/perl
# Work on the files that are created by makeinfo for html output
# split into many small files.

# This will walk though the files listed on the command line, install
# Sebastian Rose's key reader and add a small top-level-only table
# of contents that will be placed into a special region and visible
# in all subfiles.  The small contents is a constant and has to be updated
# by hand, currently.

$contents = <<EOF;
<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a name="toc_Top" href="index.html#Top">Org Mode Compact Guide</a>
<li><a name="toc_Introduction" href="Introduction.html#Introduction">1 Introduction</a>
<li><a name="toc_Document-Structure" href="Document-Structure.html#Document-Structure">2 Document Structure</a>
<li><a name="toc_Tables" href="Tables.html#Tables">3 Tables</a>
<li><a name="toc_Hyperlinks" href="Hyperlinks.html#Hyperlinks">4 Hyperlinks</a>
<li><a name="toc_TODO-Items" href="TODO-Items.html#TODO-Items">5 TODO Items</a>
<li><a name="toc_Tags" href="Tags.html#Tags">6 Tags</a>
<li><a name="toc_Properties" href="Properties.html#Properties">7 Properties</a>
<li><a name="toc_Dates-and-Times" href="Dates-and-Times.html#Dates-and-Times">8 Dates and Times</a>
<li><a name="toc_Capture" href="Capture-_002d-Refile-_002d-Archive.html#Capture-_002d-Refile-_002d-Archive">9 Capture-Refile-Archive</a>
<li><a name="toc_Agenda-Views" href="Agenda-Views.html#Agenda-Views">10 Agenda Views</a>
<li><a name="toc_Markup" href="Markup.html#Markup">11 Markup</a>
<li><a name="toc_Exporting" href="Exporting.html#Exporting">12 Exporting</a>
<li><a name="toc_Publishing" href="Publishing.html#Publishing">13 Publishing</a>
<li><a name="toc_Working-With-Source-Code" href="Working-With-Source-Code.html#Working-With-Source-Code">14 Source code</a>
<li><a name="toc_Miscellaneous" href="Miscellaneous.html#Miscellaneous">15 Miscellaneous</a>
</li></ul>
</div>
</div>
</div>
EOF

$script = <<'EOF';
</style><link rel="stylesheet" href="http://orgmode.org/org.css" type="text/css" />
<script type="text/javascript" src="http://orgmode.org/org-keys.js"></script>
<script type="text/javascript">
  <!--/*--><![CDATA[/*><!--*/
   OrgKeyReader.registerHref('h', 'index.html');
   OrgKeyReader.registerHref('t', 'index.html');
  /*]]>*/-->
</script>
EOF

while ($page = shift) {
system "mv $page $page.orig";
open IN,"<$page.orig" or die "Cannot read from $page.orig\n";
undef $/;
$all = <IN>;
close IN;

$all =~ s/<meta http-equiv="Content-Style-Type" content="text\/css">/$&\n$script/;
$all =~ s/^<body>/<body onload="OrgKeyReader.init();">\n$contents/m;

open OUT,">$page" or die "Cannot write to $page\n";
print OUT $all;
close OUT;
system "rm $page.orig";
}
