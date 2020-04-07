#!/usr/bin/perl -w

#Extract module dependencies
foreach $file (<*.F *.F90>) {

  $objfile=$file;
  $objfile=~s/\..*$/\.o/;



  # Module dependencies
  open PIPE,"cat MODEL.CPP $file | /usr/bin/cpp -P 2>/dev/null |" or die "Cannot open pipe \n";
  while (<PIPE>) {
     chop;
     if (/^[ ]*use /i) {
        s/^[ ]*use[ ]*//i;
        s/[^a-z0-9_].*$//i;
        if (! /netcdf[ ,]*/i) {
           print "$objfile:      $_.o\n";
        }
     }
     if (/^[ ]*include/i) {
        s/^[ ]*include[ ]*//i;
        s/'//g;
        s/[^a-z0-9_].*$//i;
        print "$objfile:      $_.h\n";
     }
     #print "$_ \n";
  }
  close PIPE;

  
  # cpp include dependencies
  $found=0 ;
  if (open INPUT,"<$file") {
     while (<INPUT>) { 
        chop;
        if (/^#if||/i) { $found = 1} 
        if (/^#include/i) {
           s/^#include[ ]*//;
           s/[\<\>"]//g ;
           s/[ ]*//g ;
           if (! /mpif/) {
              print "$objfile:      $_\n";
           }
        }
     }
     close INPUT ;
  } else {
     print "Cannot open $file\n";
  }


  # MODEL.CPP dependencies
  if ( $found==1 ) { print "$objfile:      MODEL.CPP\n";
  }

  #makefile dependencies (all files)
  print "$objfile:      makefile\n";
}
