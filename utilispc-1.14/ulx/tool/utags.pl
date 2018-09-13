#! /usr/local/bin/perl

foreach $f (pop @ARGV){
  open(F, $f) || do { warning("$f: $!\n"); next; };

  while(<F>){
    chop;
    s/;+.*//; # remove comment
    s/^\s+//; # remove leading space

    /^$/ && next;
    split(
    /^(def/
    /^(macro/
}
