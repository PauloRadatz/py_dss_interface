
VERSION = 1.1.0

default: library

include ../UFconfig/UFconfig.mk

all: library

library:
	( cd Lib ; $(MAKE) )

clean:
	( cd Lib ; $(MAKE) clean )

purge:
	( cd Lib ; $(MAKE) purge )

distclean: purge

# install KLUSolve
install:
	$(CP) Lib/libklusolve.a $(INSTALL_LIB)/libklusolve.$(VERSION).a
	( cd $(INSTALL_LIB) ; ln -sf libklusolve.$(VERSION).a libklusolve.a )
	$(CP) Include/klusolve.h $(INSTALL_INCLUDE)
	chmod 644 $(INSTALL_LIB)/libklusolve*.a
	chmod 644 $(INSTALL_INCLUDE)/klusolve.h

# uninstall KLUSolve
uninstall:
	$(RM) $(INSTALL_LIB)/libklusolve*.a
	$(RM) $(INSTALL_INCLUDE)/klusolve.h
