#ifndef _DIRSEP_H
#define _DIRSEP_H

#ifdef windows
#define DIRSEP_STR "\\"
#define DIRSEP_CHAR '\\'
#else
#define DIRSEP_STR "/"
#define DIRSEP_CHAR '/'
#endif

#endif // _DIRSEP_H
