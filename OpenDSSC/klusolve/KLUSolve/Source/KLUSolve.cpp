/* ------------------------------------------------------------------------- */
/* KLUSolve, Copyright (c) 2008, EnerNex Corporation. All rights reserved.   */
/* Licensed under the GNU Lesser General Public License (LGPL) v 2.1         */
/* ------------------------------------------------------------------------- */

// KLUSolve.cpp : Defines the entry point for the DLL application.
//

//#include "stdafx.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>

#include "klusolve.h"
#include "klusystem.h"

#define SYMMETRIC_MATRIX

static FILE *lfp = NULL;

static void write_lfp (const char *fmt, ...)
{
	va_list args;
	va_start (args, fmt);

	if (lfp) {
		vfprintf (lfp, fmt, args);
		fflush (lfp);
	}

	va_end (args);
}

// iAction = 0 to close, 1 to rewrite, 2 to append
unsigned int KLUSOLVE_CONVENTION SetLogFile (const char *path, unsigned int iAction)
{
	unsigned int rc = 1;
	if (iAction == 0) {
		if (lfp) fclose (lfp);
	} else if (iAction == 1) {
		if (lfp) fclose (lfp);
		lfp = fopen (path, "w");
		if (!lfp) rc = 0;
	} else if (iAction == 2) {
		if (lfp) fclose (lfp);
		lfp = fopen (path, "a");
		if (!lfp) rc = 0;
	} else {
		rc = 0;
	}
	return rc;
}
/*
BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
	switch (ul_reason_for_call) {
		case DLL_PROCESS_ATTACH:
			break;
		case DLL_PROCESS_DETACH:
			break;
		case DLL_THREAD_ATTACH:
			break;
		case DLL_THREAD_DETACH:
			break;
	}
    return TRUE;
}
*/
// exported function definitions

klusparseset_t KLUSOLVE_CONVENTION NewSparseSet (unsigned int nBus)
{
    klusparseset_t rc = 0;

	write_lfp ("NewSparseSet %u\n", nBus);

    KLUSystem *pSys = new KLUSystem ();
    if (pSys) {
        pSys->Initialize(nBus, 0, nBus);
        rc = static_cast<klusparseset_t> (pSys);
    }
	return rc;
}

unsigned int KLUSOLVE_CONVENTION ZeroSparseSet (klusparseset_t hSparse)
{
    unsigned int rc = 0;

	write_lfp ("ZeroSparseSet\n");

	KLUSystem *pSys = static_cast<KLUSystem *> (hSparse);
	if (pSys) {
		pSys->zero();
		pSys->bFactored = false;
		rc = 1;
	}
	return rc;
}

unsigned int KLUSOLVE_CONVENTION FactorSparseMatrix (klusparseset_t hSparse)
{
    unsigned int rc = 0;

	write_lfp ("FactorSparseMatrix\n");

	KLUSystem *pSys = static_cast<KLUSystem *> (hSparse);
	if (pSys) {
		if (pSys->FactorSystem() == 0) { // success
			rc = 1;
		} else { // singular
			rc = 2;
		}
	}
	return rc;
}

/* 
  input: current injections in zero-based _acxB
  output: node voltages in zero-based _acxX
  no provision for voltage sources
*/
unsigned int KLUSOLVE_CONVENTION SolveSparseSet(klusparseset_t hSparse, complex *_acxX, complex *_acxB)
{
    unsigned int rc = 0;

	KLUSystem *pSys = static_cast<KLUSystem *> (hSparse);
	if (pSys) {
		if (pSys->bFactored == false) {
			pSys->FactorSystem();
		}
		if (pSys->bFactored) {
			pSys->SolveSystem (_acxX, _acxB);
			rc = 1;
		} else {
			rc = 2;
		}
	}
	write_lfp ("SolveSparseSet returning %u\n", rc);

	return rc;
}

unsigned int KLUSOLVE_CONVENTION DeleteSparseSet(klusparseset_t hSparse)
{
    unsigned int rc = 0;

	write_lfp ("DeleteSparseSet %u\n", hSparse);

	KLUSystem *pSys = static_cast<KLUSystem *> (hSparse);
	if (pSys) {
		delete pSys;
		rc = 1;
    }

	return rc;
}

/* i and j are 1-based for these */
unsigned int KLUSOLVE_CONVENTION AddMatrixElement(klusparseset_t hSparse, unsigned int i, unsigned int j, complex *pcxVal)
{
    unsigned int rc = 0;

	write_lfp ("AddMatrixElement [%u,%u] = %G + j%G\n", i, j, pcxVal->x, pcxVal->y);

	KLUSystem *pSys = static_cast<KLUSystem *> (hSparse);
	if (pSys) {
		pSys->AddElement (i, j, *pcxVal, 1);
#ifdef SYMMETRIC_MATRIX
		if (i != j) pSys->AddElement (j, i, *pcxVal, 1);
#endif
		pSys->bFactored = false;
		rc = 1;
	}
	return rc;
}

unsigned int KLUSOLVE_CONVENTION GetMatrixElement(klusparseset_t hSparse, unsigned int i, unsigned int j, complex *pcxVal)
{
    unsigned int rc = 0;

	KLUSystem *pSys = static_cast<KLUSystem *> (hSparse);
	if (pSys) {
		pSys->GetElement (i, j, *pcxVal);
		rc = 1;
	}
	return rc;
}

// Fills the cell specified with the given value, no adding involved
KLUSOLVE_DLL unsigned int KLUSOLVE_CONVENTION SetMatrixElement(klusparseset_t hSparse, unsigned int i, unsigned int j, complex* pcxVal)
{
	unsigned int rc = 0;

	write_lfp("SetMatrixElement [%u,%u] = %G + j%G\n", i, j, pcxVal->x, pcxVal->y);

	KLUSystem* pSys = static_cast<KLUSystem*> (hSparse);
	if (pSys) 
	{
		pSys->AddElement(i, j, *pcxVal, 1);
		pSys->bFactored = false;
		rc = 1;
	}
	return rc;
}

// new functions
unsigned int KLUSOLVE_CONVENTION GetSize (klusparseset_t hSparse, unsigned int *pResult)
{
    unsigned int rc = 0;
	*pResult = 0;
	KLUSystem *pSys = static_cast<KLUSystem *> (hSparse);
	if (pSys) {
		*pResult = pSys->GetSize();
		rc = 1;
	}
	return rc;
}

unsigned int KLUSOLVE_CONVENTION GetNNZ (klusparseset_t hSparse, unsigned int *pResult)
{
    unsigned int rc = 0;
	*pResult = 0;
	KLUSystem *pSys = static_cast<KLUSystem *> (hSparse);
	if (pSys) {
		*pResult = pSys->GetNNZ();
		rc = 1;
	}
	return rc;
}

unsigned int KLUSOLVE_CONVENTION GetSparseNNZ (klusparseset_t hSparse, unsigned int *pResult)
{
    unsigned int rc = 0;
	*pResult = 0;
	KLUSystem *pSys = static_cast<KLUSystem *> (hSparse);
	if (pSys) {
		*pResult = pSys->GetSparseNNZ();
		rc = 1;
	}
	return rc;
}

unsigned int KLUSOLVE_CONVENTION GetRCond (klusparseset_t hSparse, double *pResult)
{
    unsigned int rc = 0;
	*pResult = 0.0;
	KLUSystem *pSys = static_cast<KLUSystem *> (hSparse);
	if (pSys) {
		*pResult = pSys->GetRCond();
		rc = 1;
	}
	return rc;
}

unsigned int KLUSOLVE_CONVENTION GetRGrowth (klusparseset_t hSparse, double *pResult)
{
    unsigned int rc = 0;
	*pResult = 0.0;
	KLUSystem *pSys = static_cast<KLUSystem *> (hSparse);
	if (pSys) {
		*pResult = pSys->GetRGrowth();
		rc = 1;
	}
	return rc;
}

unsigned int KLUSOLVE_CONVENTION GetCondEst (klusparseset_t hSparse, double *pResult)
{
    unsigned int rc = 0;
	*pResult = 0.0;
	KLUSystem *pSys = static_cast<KLUSystem *> (hSparse);
	if (pSys) {
		*pResult = pSys->GetCondEst();
		rc = 1;
	}
	return rc;
}

unsigned int KLUSOLVE_CONVENTION GetFlops (klusparseset_t hSparse, double *pResult)
{
    unsigned int rc = 0;
	*pResult = 0.0;
	KLUSystem *pSys = static_cast<KLUSystem *> (hSparse);
	if (pSys) {
		*pResult = pSys->GetFlops();
		rc = 1;
	}
	return rc;
}

unsigned int KLUSOLVE_CONVENTION GetSingularCol (klusparseset_t hSparse, unsigned int *pResult)
{
    unsigned int rc = 0;
	*pResult = 0;
	KLUSystem *pSys = static_cast<KLUSystem *> (hSparse);
	if (pSys) {
		*pResult = pSys->GetSingularCol();
		rc = 1;
	}
	return rc;
}

unsigned int KLUSOLVE_CONVENTION AddPrimitiveMatrix (klusparseset_t hSparse, unsigned int nOrder,
							unsigned int *pNodes, complex *pcY)
{
    unsigned int rc = 0;

	if (lfp) {
		write_lfp ("AddPrimitiveMatrix, nOrder = %u\n", nOrder);
		for (unsigned i = 0; i < nOrder; i++) {
			unsigned idx = i;
			for (unsigned j = 0; j < nOrder; j++) {
				write_lfp ("\tLocal [%u,%u] System [%u,%u] Val(%u) = %G + j%G\n", 
					i, j, pNodes[i], pNodes[j], idx, pcY[idx].x, pcY[idx].y);
				idx += nOrder;
			}
		}
	}

	KLUSystem *pSys = static_cast<KLUSystem *> (hSparse);
	if (pSys) {
		rc = pSys->AddPrimitiveMatrix (nOrder, pNodes, pcY);
		pSys->bFactored = false;
	}
	return rc;
}

unsigned int KLUSOLVE_CONVENTION GetCompressedMatrix (klusparseset_t hSparse, unsigned int nColP, unsigned int nNZ,
				   unsigned int *pColP, unsigned int *pRowIdx, complex *pcY)
{
    unsigned int rc = 0;
	KLUSystem *pSys = static_cast<KLUSystem *> (hSparse);
	if (pSys) {
		if (pSys->GetCompressedMatrix (nColP, nNZ, pColP, pRowIdx, pcY)) {
			rc = 1;
		} else { // probably a size mismatch
			rc = 2;
		}
	}
	return rc;
}

unsigned int KLUSOLVE_CONVENTION GetTripletMatrix (klusparseset_t hSparse, unsigned int nNZ,
						  unsigned int *pRows, unsigned int *pCols, complex *pcY)
{
    unsigned int rc = 0;
	KLUSystem *pSys = static_cast<KLUSystem *> (hSparse);
	if (pSys) {
		if (pSys->GetTripletMatrix (nNZ, pRows, pCols, pcY)) {
			rc = 1;
		} else { // probably a size mismatch
			rc = 2;
		}
	}
	return rc;
}

unsigned int KLUSOLVE_CONVENTION FindIslands (klusparseset_t hSparse, unsigned int nOrder, unsigned int *pNodes)
{
    unsigned int rc = 0;
	KLUSystem *pSys = static_cast<KLUSystem *> (hSparse);
	if (pSys && nOrder >= pSys->GetSize()) {
		rc = pSys->FindIslands (pNodes);
	}
	return rc;
}
