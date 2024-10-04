/* ------------------------------------------------------------------------- */
/* KLUSolve, Copyright (c) 2008, EnerNex Corporation. All rights reserved.   */
/* Licensed under the GNU Lesser General Public License (LGPL) v 2.1         */
/* ------------------------------------------------------------------------- */

//#include "stdafx.h"
#include <stdio.h>
#include <string.h>

#include "klusolve.h"
#include "klusystem.h"

complex czero = {0.0, 0.0};
static klu_common Common;
static int common_init = 0;

KLUSystem::KLUSystem()
{
	InitDefaults ();
}

KLUSystem::KLUSystem (unsigned nBus, unsigned nV, unsigned nI)
{
	InitDefaults ();
    Initialize (nBus, nV, nI);
}

KLUSystem::~KLUSystem()
{
	clear ();
}

void KLUSystem::zero_indices ()
{
	m_nBus = m_nX = 0;
	m_NZpre = m_NZpost = 0;
	m_fltBus = 0;
}

void KLUSystem::null_pointers ()
{
	Y22 = NULL;
	T22 = NULL;
	Numeric = NULL;
	Symbolic = NULL;
	acx = NULL;
}

void KLUSystem::InitDefaults ()
{
    m_nBus = 0;
    bFactored = false;
	acx = NULL;
	zero_indices ();
	null_pointers ();
	if (!common_init) {
		klu_defaults (&Common);
		Common.halt_if_singular = 0;
		common_init = 1;
	}
}

void KLUSystem::clear()
{
	if (Y22) cs_spfree (Y22);

	if (T22) cs_spfree (T22);

	if (acx) delete [] acx;

	if (Numeric) klu_z_free_numeric (&Numeric, &Common);
	if (Symbolic) klu_free_symbolic (&Symbolic, &Common);

	zero_indices ();
	null_pointers ();
}

int KLUSystem::Initialize (unsigned nBus, unsigned nV, unsigned nI)
{
	clear ();

	m_nBus = m_nX = nBus;

	if (m_nX > 0) {
		T22 = csz_spalloc (m_nX, m_nX, 2 * m_nX, 1, 1);
	}
	if (acx) delete [] acx;
	acx = new complex [m_nBus + 1];
	return 0;
}

int KLUSystem::FactorSystem()
{
	bFactored = false;
	
	int rc = Factor();
	
	if (rc == 1) {
		bFactored = true;
		return 0;
	}
	return 1;
}

int KLUSystem::SolveSystem (complex *_acxX, complex *_acxB)
{
    int rc = 0;
	unsigned i;

	acx[0] = czero;
	for (i = 0; i < m_nBus; i++) {
		acx[i+1] = _acxB[i];
	}

	Solve (acx);

	for (i = 0; i < m_nBus; i++) {
		_acxX[i] = acx[i+1];
	}

    return rc;
}

unsigned KLUSystem::AddPrimitiveMatrix (unsigned nOrder, unsigned *pNodes, complex *pMat)
{
	unsigned i, j, idRow, idCol, idVal;
	double re, im;

	// check the node numbers
	for (i = 0; i < nOrder; i++) {
		if (pNodes[i] > m_nBus) return 0;
	}

	// add the matrix transposed
	for (i = 0; i < nOrder; i++) {
		if (pNodes[i] < 1) continue; // skip ground
		idVal = i;
		idRow = pNodes[i] - 1;  // convert to zero-based
		for (j = 0; j < nOrder; j++) {
			if (pNodes[j]) {
				idCol = pNodes[j] - 1;
				re = pMat[idVal].x;
				im = pMat[idVal].y;
				if (re != 0.0 || im != 0.0)	{
					// stuff this value into the correct partition, transposed
					csz_entry (T22, idCol, idRow, re, im);
				}
			}
			// always step through values, even if we don't use them
			idVal += nOrder;
		}
	}
	return 1;
}

void KLUSystem::AddMatrix (unsigned *aidBus, matrix_complex *pcxm, int)
{
    unsigned    i, j;
    unsigned    idRow, idCol;
    unsigned    nDim = pcxm->nRow;
    complex     val;
	double		re, im;

	// add the full primitive matrix, transposed, since csz_compress called later
	// bus id 0 is reference node, but KLU indices are zero-based
	for (i = 0; i < nDim; i++) {
        idRow = aidBus[i];
		if (idRow < 1) continue;
		--idRow;
        for (j = 0; j < nDim; j++) {
            idCol = aidBus[j];
			if (idCol < 1) continue;
			--idCol;
            val = pcxm->get_acx (i, j);
			re = val.x;
			im = val.y;
			if (re == 0.0 && im == 0.0) {
				continue;
			}

			// stuff this value into the correct partition, transposed
			csz_entry (T22, idCol, idRow, re, im);
        }
	}
}

cs *KLUSystem::process_triplet (cs **T)
{
	cs *C, *A = NULL;

	if (!*T) return NULL;

	if ((*T)->nz > 0) {
//		csz_print (*T, 0);
		C = csz_compress (*T);
		csz_dupl (C);
		csz_dropzeros (C);
		A = csz_transpose (C, 1);
		cs_spfree (C);
		m_NZpre += A->p[A->n];
//		csz_print (A, 0);
	}

	cs_spfree (*T);
	*T = NULL;
	return A;
}

void KLUSystem::compress_partitions ()
{
	Y22 = process_triplet (&T22);
}

int KLUSystem::Factor()
{
	// first convert the triplets to column-compressed form, and prep the columns
	if (T22) {
		compress_partitions ();
	} else { // otherwise, compression and factoring has already been done
		if (m_fltBus) return -1;  // was found singular before
		return 1;  // was found okay before
	}

	// then factor Y22
	if (Numeric) klu_z_free_numeric (&Numeric, &Common);
	if (Symbolic) klu_free_symbolic (&Symbolic, &Common);
	Numeric = NULL;
	Symbolic = NULL;

	if (Y22) {
		Symbolic = klu_analyze (Y22->n, Y22->p, Y22->i, &Common);
		Numeric = klu_z_factor (Y22->p, Y22->i, Y22->x, Symbolic, &Common);
		m_fltBus = Common.singular_col;
		if (Common.singular_col < Y22->n) {
			++m_fltBus; // for 1-based NexHarm row numbers
			m_fltBus += 0; // skip over the voltage source buses
		} else {
			m_fltBus = 0;  // no singular submatrix was found
		}
		if (Common.status == KLU_OK) {
			// compute size of the factorization
			m_NZpost += (Numeric->lnz + Numeric->unz - Numeric->n + 
				((Numeric->Offp) ? (Numeric->Offp [Numeric->n]) : 0));
			return 1;
		} else if (Common.status == KLU_SINGULAR) {
			return -1;
		} else { // KLU_OUT_OF_MEMORY, KLU_INVALID, or KLU_TOO_LARGE
			if (!m_fltBus) {
				m_fltBus = 1;  // this is the flag for unsuccessful factorization
			}
			return 0;
		}
	}

	return 1;
}

void KLUSystem::Solve (complex *acxVbus)
{
	double *rhs = NULL;
	unsigned i, i1, offset;

	if (m_nX < 1) return; // nothing to do

	// load current injections into RHS
	rhs = (double *) cs_malloc (2 * m_nX, sizeof (double));
	offset = 1;
	for (i = 0; i < m_nX; i++) {
		i1 = 2 * i;
		rhs[i1] = acxVbus[i+offset].x;
		rhs[i1+1] = acxVbus[i+offset].y;
	}

	// solve and copy voltages into the output vector
	// relying on Y22->n == m_nX from T22 creation by csz_spalloc
	klu_z_solve (Symbolic, Numeric, Y22->n, 1, rhs, &Common);

	offset = 1;
	for (i = 0; i < m_nX; i++) {
		i1 = 2 * i;
		acxVbus[i+offset].x = rhs[i1];
		acxVbus[i+offset].y = rhs[i1+1];
	}
	
	cs_free (rhs);
}

double KLUSystem::GetRCond ()
{
	klu_z_rcond (Symbolic, Numeric, &Common);
	return Common.rcond;
}

double KLUSystem::GetRGrowth ()
{
	if (Y22 == NULL) return 0.0;
	klu_z_rgrowth (Y22->p, Y22->i, Y22->x, Symbolic, Numeric, &Common);
	return Common.rgrowth;
}

double KLUSystem::GetCondEst ()
{
	if (Y22 == NULL) return 0.0;
	if (Y22->n > 1) klu_z_condest (Y22->p, Y22->x, Symbolic, Numeric, &Common);
	return Common.condest;
}

double KLUSystem::GetFlops ()
{
	klu_z_flops (Symbolic, Numeric, &Common);
	return Common.flops;
}

unsigned int KLUSystem::FindDisconnectedSubnetwork()
{
	Factor ();

	return m_fltBus;
}

static void augment_column (int *Ai, cs *y, unsigned j, int offset, unsigned *p, int *count)
{
	int *yp = y->p;
	int *yi = y->i;
	int k;

	for (k = yp[j]; k < yp[j+1]; k++) {
		Ai[*p] = yi[k] + offset;
		*count += 1;
		*p += 1;
	}
}

// stack-based DFS from Sedgewick
static int *stack = NULL;
static int stk_p = 0;
static void push (int v) {stack[stk_p++] = v;}
static int pop () {return stack[--stk_p];}
static void stackfree () {cs_free (stack);}
static int stackempty () {return !stk_p;}
static void stackinit (int size) 
{
	stackfree ();
	stack = (int *) cs_malloc (size + 1, sizeof (int));
	stk_p = 0;
}

static void mark_dfs (unsigned j, int cnt, int *Ap, int *Ai, int *clique)
{
	int i, k;

	push (j);
	while (!stackempty()) {
		j = pop();
		clique[j] = cnt;
		for (k = Ap[j]; k < Ap[j+1]; k++) {
			i = Ai[k];
			if (clique[i] == 0) {
				push (i);
				clique[i] = -1; // to only push once
			}
		}
	}
}

// The KLU factorization might have some information about cliques in Y22 only,
//   but we want to consider the whole system, so this function
//   performs a new DFS on the compressed non-zero pattern
// This function could behave differently than before, 
//   since the compression process removes numerical zero elements
unsigned int KLUSystem::FindIslands(unsigned int *idClique)
{
	Factor ();

	int *clique = (int *) cs_malloc (m_nBus, sizeof (int));
	int *Ap = Y22->p;
	int *Ai = Y22->i;
	unsigned j;

	// DFS down the columns
	int cnt = 0;
	for (j = 0; j < m_nBus; j++) clique[j] = cnt; // use to mark the nodes with clique #
	stackinit (m_nBus);
	for (j = 0; j < m_nBus; j++) {
		if (clique[j] == 0) { // have not visited this column yet
			++cnt;
			mark_dfs (j, cnt, Ap, Ai, clique);
		}
	}

	for (j = 0; j < m_nBus; j++) idClique[j] = clique[j];

	// if there is more than one clique, stuff the row numbers (+1) into island lists
	// unsigned i, p;
	/* used to return allocated lists of nodes in each island
	if (cnt > 1) {
        *paaidBus = new unsigned * [cnt];
        unsigned **aaidBus = *paaidBus;

        for (i = 0; i < cnt; i++) {
            p = 0;
			for (j = 0; j < m_nBus; j++) { // count nodes in this clique
				if (clique[j] == i+1) ++p;
			}
            aaidBus[i] = new unsigned [p + 1];
            aaidBus[i][0] = p;
			p = 1;
	        for (j = 0; j < m_nBus; j++) {
				if (clique[j] == i+1) {
					aaidBus[i][p++] = j + 1;
				}
			}
		}
	} else {
		paaidBus = NULL;
	}
	*/

	cs_free (clique);

	return cnt;
}


// added for ESolv32
void KLUSystem::zero()
{
	Initialize (m_nBus, 0, m_nBus);
}

void KLUSystem::AddElement (unsigned iRow, unsigned iCol, complex &cpxVal, 
							   int bSum) // bSum ignored
{
	if (iRow > m_nBus || iCol > m_nBus) return;
	if (iRow <= 0 || iCol <= 0) return;
	--iRow;
	--iCol;

	double re = cpxVal.x;
	double im = cpxVal.y;
	
	if (re == 0.0 && im == 0.0) {
		return;
	}

	// stuff this value into the correct partition, transposed
	csz_entry (T22, iCol, iRow, re, im);
}

void KLUSystem::GetElement (unsigned iRow, unsigned iCol, complex &cpxVal)
{
	cpxVal = czero;
	if (iRow > m_nBus || iCol > m_nBus) return;
	if (--iRow < 0) return;
	if (--iCol < 0) return;

	double *Ax;
	int *Ap, *Ai;
	int i, p;

	if (T22) {  // have to search the triplet storage, which is not sorted
		Ax = T22->x;
		Ap = T22->p;
		Ai = T22->i;
		for (i = 0; i < T22->nz; i++) {
			if (Ap[i] == iCol && Ai[i] == iRow) {
				cpxVal.x += Ax[2*i];
				cpxVal.y += Ax[2*i + 1];
			}
		}
	} else if (Y22) {  // faster, duplicates already summed and elements are sorted
		Ax = Y22->x;
		Ap = Y22->p;
		Ai = Y22->i;
		for (p = Ap[iCol]; p < Ap[iCol+1]; ++p) {
			if (Ai[p] == iRow) {
				cpxVal.x = Ax[2*p];
				cpxVal.y = Ax[2*p + 1];
				return;
			}
		}
	}
}

// return in compressed column form, return m_NZpre for success, 0 for a failure
unsigned KLUSystem::GetCompressedMatrix (unsigned nColP, unsigned nNZ, unsigned *pColP, unsigned *pRowIdx, complex *pMat)
{
	unsigned int rc = 0;

	if (T22) Factor();
	if (Y22 && nNZ >= m_NZpre && nColP > m_nBus) {
		rc = m_NZpre;
		if (rc > 0) {
			memcpy (pMat, Y22->x, m_NZpre * sizeof (complex));
			memcpy (pColP, Y22->p, (m_nBus + 1) * sizeof (unsigned));
			memcpy (pRowIdx, Y22->i, m_NZpre * sizeof (unsigned));
		}
	}
	return rc;
}

unsigned KLUSystem::GetTripletMatrix (unsigned nNZ, unsigned *pRows, unsigned *pCols, complex *pMat)
{
	unsigned int rc = 0;

	if (T22) Factor();
	if (Y22 && nNZ >= m_NZpre) {
		rc = m_NZpre;
		if (rc > 0) {
			memcpy (pMat, Y22->x, m_NZpre * sizeof (complex));
			int *Ap = Y22->p;
			int *Ai = Y22->i;
			for (unsigned j = 0; j < m_nBus; j++) {
				for (int p = Ap[j]; p < Ap[j+1]; p++) {
					pRows[p] = Ai[p];
					pCols[p] = j;
				}
			}
		}
	}
	return rc;
}

