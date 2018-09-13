#! /usr/local/bin/perl

# converts etags output to ctags format

$infile = $#ARGV >= 0 ? $ARGV[0] : "TAGS";
$outfile = "tags";
open(I, $infile) || die "$infile: $!\n";
open(O, ">$outfile") || die "$outfile: $!\n";

while(<I>){
  chop;
  /^\014$/ &&
  do {
	$_ = <I>;
	$filename = (split(/,/))[0];
	next;
  };

  $line = (split(/\177/))[0];
  ($def, $funname) = split(/\s+/, $line);
  $line =~ s![/*.$\]\[]!\\$&!g;
  print O "$funname\t$filename\t/^$line/\n";
}
close I;
close O;

