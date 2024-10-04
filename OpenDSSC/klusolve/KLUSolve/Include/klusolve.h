/* ------------------------------------------------------------------------- */
/* KLUSolve, Copyright (c) 2008, EnerNex Corporation. All rights reserved.   */
/* Licensed under the GNU Lesser General Public License (LGPL) v 2.1         */
/* ------------------------------------------------------------------------- */

#ifndef klusolve_included
#define klusolve_included

#ifndef _COMPLEX_DEFINED
#define _COMPLEX_DEFINED
typedef struct _complex {double x, y;} complex;
#endif

#include <stdint.h>

#ifndef KLUSOLVE_IMPORTS
#define KLUSOLVE_CONVENTION
#define KLUSOLVE_DLL
#else
	// KLUSOLVE_IMPORTS is defined:
	#define KLUSOLVE_CONVENTION __stdcall
	#if KLUSOLVE_IMPORTS
		// We need to import our functions:
		#define KLUSOLVE_DLL __declspec(dllimport)
	#else
		// We need to export our functions:
		#define KLUSOLVE_DLL __declspec(dllexport)
	#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef void* klusparseset_t;

// return handle of new sparse set, 0 if error
// be sure to DeleteSparseSet using the returned handle
KLUSOLVE_DLL klusparseset_t KLUSOLVE_CONVENTION NewSparseSet (unsigned int nBus);

// return 1 if successful, 0 if not
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION ZeroSparseSet (klusparseset_t hSparse);

// return 1 if successful, 2 if singular, 0 if other error
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION FactorSparseMatrix (klusparseset_t hSparse);

/* 
  input: current injections in zero-based _acxB
  output: node voltages in zero-based _acxX
  no provision for voltage sources
*/
// return 1 if successful, 2 if singular, 0 if other error
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION SolveSparseSet (klusparseset_t hSparse, complex *_acxX, complex *_acxB);

// return 1 if successful, 0 if not
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION DeleteSparseSet (klusparseset_t hSparse);

/* i and j are 1-based for these */
// return 1 if successful, 0 if not
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION AddMatrixElement (klusparseset_t hSparse, unsigned int i, unsigned int j, complex *pcxVal);
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION GetMatrixElement (klusparseset_t hSparse, unsigned int i, unsigned int j, complex *pcxVal);
// Sets a specific cell within the YBus matrix 
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION SetMatrixElement(klusparseset_t hSparse, unsigned int i, unsigned int j, complex* pcxVal);

// new functions
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION GetSize (klusparseset_t hSparse, unsigned int *pResult);
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION GetNNZ (klusparseset_t hSparse, unsigned int *pResult);
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION GetSparseNNZ (klusparseset_t hSparse, unsigned int *pResult);
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION GetRCond (klusparseset_t hSparse, double *pResult);
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION GetRGrowth (klusparseset_t hSparse, double *pResult);
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION GetCondEst (klusparseset_t hSparse, double *pResult);
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION GetFlops (klusparseset_t hSparse, double *pResult);
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION GetSingularCol (klusparseset_t hSparse, unsigned int *pResult);

KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION AddPrimitiveMatrix (klusparseset_t hSparse, unsigned int nOrder,
							unsigned int *pNodes, complex *pcY);
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION GetCompressedMatrix (klusparseset_t hSparse, unsigned int nColP, 
							 unsigned int nNZ, unsigned int *pColP, 
							 unsigned int *pRowIdx, complex *pcY);
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION GetTripletMatrix (klusparseset_t hSparse, unsigned int nNZ,
						  unsigned int *pRows, unsigned int *pCols, complex *pcY);
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION FindIslands (klusparseset_t hSparse, unsigned int nOrder, unsigned int *pNodes);

// iAction = 0 to close, 1 to rewrite, 2 to append
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION SetLogFile (const char *path, unsigned int iAction);

#ifdef __cplusplus
}
#endif

#endif // klusolve_included
