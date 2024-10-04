#------------------------------------------------------------------------------
# CZSparse Makefile
#------------------------------------------------------------------------------

default: C

include ../UFconfig/UFconfig.mk

C:
	( cd Lib ; $(MAKE) )

all: C

library:
	( cd Lib ; $(MAKE) )

clean:
	( cd Lib ; $(MAKE) clean )

purge:
	( cd Lib ; $(MAKE) purge )

distclean: purge

# do not install CZSparse
install:

# uninstall CZSparse: do nothing
uninstall:

