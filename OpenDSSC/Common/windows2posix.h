#ifndef _WINDOWS2POSIX_H
#define _WINDOWS2POSIX_H

// This file should only be pulled in for compiling on POSIX platforms
// excluding MS Windows.
#include <cassert>	// assert
#include <cerrno>	// errno
#include <cstdint>	// uint_least64_t
#include <cstring>	// memset
#include "filesystem.hpp"
#include <iostream>	// cerr
#include <system_error>
#include <unistd.h>	// readlink
#include <time.h>	// clock_gettime

// Notes / Requirements:
//
// To minimize surprises, all functions must emulate their MS Windows
// equivalents in inputs, outputs, error reporting, and exception safety.
//
// Thus for functions which are not documented to throw exceptions (like
// CopyFile), these functions must be marked with the noexcept keyword.
//
// Also, every function shall have "static inline".  "static" is needed because
// we're keeping these functions in a header file at the moment, and we don't
// want name collisions between multiple source (.cpp or .c) files.  "inline"
// is a hint to the compiler to request inlining the functions wherever they're
// called, but the compiler is still free to not inline.
//
// If a function deviates from the docs, please note it with a brief
// explanation.

static inline bool CopyFile(const char *ExistingFileName, const char *NewFileName, bool FailIfExists) noexcept
{
	// Emulate behavior of this description:
	// https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-copyfile
	// return nonzero on success, and zero on failure with an error
	// number available via GetLastError

	// Here, we're assuming C++17.  If we only have C++11 available such as
	// possibly on RHEL 7.x, then the Boost library has the same
	// functionality.

#if 0
	std::error_code ec;
	bool retval = ghc::filesystem::copy_file(
		ExistingFileName,
		NewFileName,
		FailIfExists ? ghc::filesystem::copy_options::none : ghc::filesystem::copy_options::overwrite_existing,
		ec);
	// need to do: Put ec into errno since this is what Support/d2c_sysexcept.cpp will use for emulating GetLastError
	// This bug report gives for detail:  (read it for history)
	// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=60555

	// Instead of all that, do this:
#endif
	try {
		return ghc::filesystem::copy_file(
			ExistingFileName,
			NewFileName,
			FailIfExists ? ghc::filesystem::copy_options::none : ghc::filesystem::copy_options::overwrite_existing
			);
	}
	catch (const std::system_error &e) {
		errno = e.code().value(); // Will this always return an errno-compatible value on Linux systems?  How about other POSIX systems?
		return false;
	}
	catch (const std::exception &e) {

		// TODO: Extend our GetLastError mechanism to let us
		// dynamically allocate new error values as we encounter new
		// e.what() messages.

		std::cerr << "Encountered error in CopyFile(\"" <<
			ExistingFileName << "\", \"" << NewFileName << "\", \""
			<< FailIfExists << "\"): message=\"" << e.what() << "\"\n";

		errno = -1;  // This will cause strerror(errno) to return "Unknown error". (or the equivalent for the locale)
		return false;
	}
	catch (...) {
		std::cerr << "Encountered unknown error in CopyFile(\"" <<
			ExistingFileName << "\", \"" << NewFileName << "\", \""
			<< FailIfExists << "\")\n";

		errno = -1;  // This will cause strerror(errno) to return "Unknown error". (or the equivalent for the locale)
		return false;
	}
}


template<typename T>
static inline size_t GetModuleFileName(const T& must_be_zero, char *Filename, size_t max_Filename_len) noexcept
{
	// https://docs.microsoft.com/en-us/windows/win32/api/libloaderapi/nf-libloaderapi-getmodulefilenamea

	assert( !must_be_zero /* This function assumes the first parameter is zero or NULL. */ );

	// Deviation:  This function does not try to emulate the behavior of
	// Windows XP when the buffer size is too small.

	// Deviation:  When max_Filename_len is too small, this function sets
	// errno = ENOMEM (Out of memory) instead of ERROR_INSUFFICIENT_BUFFER,
	// because we don't have ERROR_INSUFFICIENT_BUFFER in POSIX.  Is there
	// a better standard value for errno?

	// GetDSSExeFile( ) in Forms/CmdForms.cpp happens to have similar code:
	ssize_t len = readlink("/proc/self/exe", Filename, max_Filename_len);
	if(len != -1)
		return 0; // readlink() has already set errno, so this covers GetLastError.

	// Emulate the rest of the MS behavior as best we can:
	if (size_t(len)<max_Filename_len)
		Filename[len] = '\0';
	else {
		Filename[max_Filename_len-1] = '\0'; // always terminate the string.
		errno = ENOMEM; // Out of memory: Closest equivalent to ERROR_INSUFFICIENT_BUFFER ?
	}

	return size_t(len); // The original function returns DWORD which is unsigned, so we convert ssize_t to size_t here.
}


static inline uint_least64_t GetTickCount64() noexcept
{
	// https://docs.microsoft.com/en-us/windows/win32/api/sysinfoapi/nf-sysinfoapi-gettickcount64
	// Return the # of milliseconds since boot.
	struct timespec t;
	std::memset(&t, 0, sizeof(t));
	int retval = clock_gettime(CLOCK_MONOTONIC, &t);
	if (retval) {
		// clock_gettime returns 0 on success or -1 on failure with errno set
		return 0;
	}
	return uint_least64_t(t.tv_sec)*1000u + uint_least64_t(t.tv_nsec)/1000000u;
}

#endif /* _WINDOWS2POSIX_H */
