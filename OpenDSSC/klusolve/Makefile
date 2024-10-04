#-------------------------------------------------------------------------------
# Makefile for all KLUSolve packages
#-------------------------------------------------------------------------------

include UFconfig/UFconfig.mk

# Compile the default rules for each package
default:
	- ( cd UFconfig/xerbla && $(MAKE) )
	- ( cd UFconfig && $(MAKE) )
#	- ( cd metis-4.0 && $(MAKE) )
	- ( cd AMD && $(MAKE) )
	- ( cd COLAMD && $(MAKE) )
	- ( cd BTF && $(MAKE) )
	- ( cd KLU && $(MAKE) )
	- ( cd CSparse && $(MAKE) )
	- ( cd CZSparse && $(MAKE) )
	- ( cd KLUSolve && $(MAKE) )

# install all packages in /usr/local/lib and /usr/local/include
install:
	- ( cd UFconfig && $(MAKE) install )
	- ( cd AMD && $(MAKE) install )
	- ( cd COLAMD && $(MAKE) install )
	- ( cd BTF && $(MAKE) install )
	- ( cd KLU && $(MAKE) install )
	- ( cd CZSparse && $(MAKE) install )
	- ( cd KLUSolve && $(MAKE) install )

# uninstall all packages
uninstall:
	- ( cd UFconfig && $(MAKE) uninstall )
	- ( cd AMD && $(MAKE) uninstall )
	- ( cd COLAMD && $(MAKE) uninstall )
	- ( cd BTF && $(MAKE) uninstall )
	- ( cd KLU && $(MAKE) uninstall )
	- ( cd CZSparse && $(MAKE) uninstall )
	- ( cd KLUSolve && $(MAKE) uninstall )

library:
	- ( cd UFconfig/xerbla && $(MAKE) )
	- ( cd UFconfig && $(MAKE) )
#	- ( cd metis-4.0 && $(MAKE) )
	- ( cd AMD && $(MAKE) library )
	- ( cd BTF && $(MAKE) library )
	- ( cd COLAMD && $(MAKE) library )
	- ( cd KLU && $(MAKE) library )
	- ( cd CSparse && $(MAKE) library )
	- ( cd CZSparse && $(MAKE) library )
	- ( cd KLUSolve && $(MAKE) library )

# Remove all files not in the original distribution
purge:
	- ( cd UFconfig/xerbla && $(MAKE) purge )
	- ( cd UFconfig && $(MAKE) purge )
#	- ( cd metis-4.0 && $(MAKE) realclean )
	- ( cd AMD && $(MAKE) purge )
	- ( cd COLAMD && $(MAKE) purge )
	- ( cd BTF && $(MAKE) purge )
	- ( cd KLU && $(MAKE) purge )
	- ( cd CSparse && $(MAKE) purge )
	- ( cd CZSparse && $(MAKE) purge )
	- ( cd KLUSolve && $(MAKE) purge )

# Remove all files not in the original distribution, but keep the libraries
clean:
	- ( cd UFconfig/xerbla && $(MAKE) clean )
	- ( cd UFconfig && $(MAKE) clean )
#	- ( cd metis-4.0 && $(MAKE) clean )
	- ( cd AMD && $(MAKE) clean )
	- ( cd COLAMD && $(MAKE) clean )
	- ( cd BTF && $(MAKE) clean )
	- ( cd KLU && $(MAKE) clean )
	- ( cd CSparse && $(MAKE) clean )
	- ( cd CZSparse && $(MAKE) clean )
	- ( cd KLUSolve && $(MAKE) clean )

distclean: purge

# statement coverage (Linux only); this requires a lot of time.
# The umfpack tcov requires a lot of disk space
cov:
	- ( cd CSparse && $(MAKE) cov )
	- ( cd KLU && $(MAKE) cov )
