# Generated automatically from Makefile.in by configure.

# SUBDIR = src lib ulx
SUBDIR = src lib 

all : ${SUBDIR}

install : ${SUBDIR}
	cd src; make install 
	cd lib; make install 
#	cd ulx; make install 

clean :
	cd src; make clean
	cd lib; make clean
#	cd ulx; make clean

# .RECURSIVE: ${SUBDIR}

${SUBDIR}: FRC
	cd $@; $(MAKE) all

FRC:
