/* ------------------------------------------------------------------------- */
/* KLUSolve, Copyright (c) 2008, EnerNex Corporation. All rights reserved.   */
/* Licensed under the GNU Lesser General Public License (LGPL) v 2.1         */
/* ------------------------------------------------------------------------- */

#ifndef klusystem_included
#define klusystem_included

extern "C" {
#include "cs.h"
#include "csz.h"
#include "klu.h"
}

struct matrix_complex {
	complex *acx;
	unsigned nRow, nCol;
	complex get_acx (unsigned i, unsigned j) {
		return acx[i*nCol+j];
    }
};

/* Kron reduction not supported, this version just solves

|Y22| * |V| = |I|

KLU manages complex values as interleaved real/imag in double arrays
KLU arrays are zero-based
*/

class KLUSystem
{
private:
	complex		*acx;

	// admittance matrix blocks in compressed-column storage, like Matlab
	cs *Y22;

	// admittance matrix blocks in triplet storage
	cs *T22;

	klu_symbolic *Symbolic;
	klu_numeric *Numeric;

	unsigned int m_nBus;    // number of nodes
	unsigned int m_nX;      // number of unknown voltages, hardwired to m_nBus
	unsigned int m_NZpre;   // number of non-zero entries before factoring
	unsigned int m_NZpost;  // number of non-zero entries after factoring
	unsigned int m_fltBus;  // row number of a bus causing singularity

	void InitDefaults ();
    void clear ();
	void zero_indices ();
	void null_pointers ();
	cs *process_triplet (cs **T);
	void compress_partitions ();

protected:

public:
	KLUSystem ();
    KLUSystem (unsigned nBus, unsigned nV = 0, unsigned nI = 0);
    ~KLUSystem ();

    bool bFactored;      //  system has been factored

    int FactorSystem ();
    int SolveSystem (complex *_acxX, complex *_acxB);
	// this resets and reinitializes the sparse matrix, nI = nBus
    int Initialize (unsigned nBus, unsigned nV = 0, unsigned nI = 0);
	unsigned int GetSize () {return m_nBus;}

	// metrics
	unsigned int GetSparseNNZ () {return m_NZpost;}
	unsigned int GetNNZ () {return m_NZpre;}
	double GetRCond ();
	double GetRGrowth ();
	double GetCondEst ();
	double GetFlops ();
	unsigned int GetSingularCol () {return m_fltBus;}

	// bSum is ignored
    void AddMatrix (unsigned /*far*/ *aidBus, matrix_complex *pcxm, int bSum);

	// returns 1 for success, -1 for a singular matrix
	// returns 0 for another KLU error, most likely the matrix is too large for int32
    int Factor ();

	// input: acxVbus[0] is ground voltage
	//        acxVbus[1..nBus] are current injections
	// output: acxVbus[1..nBus] are solved voltages
    void Solve (complex *acxVbus);

	// returns the number of connected components (cliques) in the whole system graph
	//  (i.e., considers Y11, Y12, and Y21 in addition to Y22)
	// store the island number (1-based) for each node in idClique
	unsigned FindIslands (unsigned *idClique);

	// returns the row > 0 if a zero appears on the diagonal
	// calls Factor if necessary
	// note: the EMTP terminology is "floating subnetwork"
	unsigned FindDisconnectedSubnetwork ();

	// The following were added for ESolv32:
	// maintains allocations, zeros matrix values
    void zero();
	// bSum is ignored
    void AddElement(unsigned iRow, unsigned iCol, complex &cpxVal, int bSum);
	// return the sum of elements at 1-based [iRow, iCol]
    void GetElement(unsigned iRow, unsigned iCol, complex &cpxVal);
	// for OpenDSS, return 1 for success
	unsigned AddPrimitiveMatrix (unsigned nOrder, unsigned *pNodes, complex *pMat);
	// return in compressed triplet form, return 1 for success, 0 for a size mismatch
	unsigned GetCompressedMatrix (unsigned nColP, unsigned nNZ, unsigned *pColP,
		unsigned *pRowIdx, complex *pMat);
	unsigned GetTripletMatrix (unsigned nNZ, unsigned *pRows, unsigned *pCols, complex *pMat);
};

#endif // klusystem_included
