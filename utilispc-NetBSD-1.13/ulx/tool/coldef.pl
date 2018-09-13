#! /usr/local/bin/perl
#
# remove/extract defuns listed in stdin from specified list of files.
#

$#ARGV >= 0 || die "Specify target input files\n";
# read function list from stdin
while($_ = $ARGV[0], /^-/){
  /^-B/ && ($outdir = $', shift, next );
  /^-f/ && ($listfile = $', shift, next );
  /^-v/ && ($verbose = 1, shift, next);
}
$outdir = "./remout" unless $outdir;
# read function list from stdin
# @funclist = <STDIN>;
while(<STDIN>){
  /^#/ || ( chop, push(@funclist, $_) );
}
$#funclist == -1 && die "No funclist\n";

FILE:
while ($file = shift){
  $bracelevel = 0;
  print stderr "FILE: $file\n";
  print ";;; from $file\n\n";
  open file || do { warn "open $file: $!\n"; next FILE; };
  $outfile = ">$outdir/$file";
  open outfile || die "wopen $outfile: $!\n";

  while(<file>){
    $line = $_;
    chop;
    s/;.*$//; s/"[^"]*"//;
    if($bracelevel == 0 && /^\s*\(\s*(defun|defmacro|macro)\s+(\S*)/){
      # it's a function/macro definition.
      $funname = $2;
      $printit = grep(/^$funname$/, @funclist); 
      print stderr "$1: $funname" if $verbose || $printit;
      print stderr " - extracted." if $printit;
      print stderr "\n" if $verbose || $printit;
    }
    print $line if $printit;
    print outfile $line unless $printit;
    $bracelevel += ( tr/(/(/ - tr/)/)/ );
    ( print "\n" ), $printit = 0 if $printit && $bracelevel <=0;
    $bracelevel = 0 if $bracelevel < 0;
  }
  close file;
  close outfile;
}

