#ifndef UcmatrixH
#define UcmatrixH

#include "System.h"
#include "Ucomplex.h"

namespace Ucmatrix
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
   12-4-99 Added MvMultAccum
   2/4/03  Added Avg routines
*/

class TcMatrix : public System::TObject
{
public:
	typedef TObject inherited;	
//private:
    /* Private declarations */
	int Norder;
	std::vector <complex> Values;
public:
    /* Public declarations */
	int InvertError;
	TcMatrix(int n);
	virtual ~TcMatrix();
	void Invert();
	void Clear();  /*Zero out matrix*/
	void AddFrom(TcMatrix* OtherMatrix);
	void CopyFrom(TcMatrix* OtherMatrix);
	void SetElement(int i, int j, const Ucomplex::complex& Value);
	void SetElemsym(int i, int j, const Ucomplex::complex& Value);
	void AddElement(int i, int j, const Ucomplex::complex& Value);
	void AddElemsym(int i, int j, const Ucomplex::complex& Value);
	Ucomplex::complex GetElement(int i, int j);
	int GetErrorCode();
	Ucomplex::complex SumBlock(int row1, int row2, int col1, int col2);
	void MVmult(Ucomplex::pComplexArray B, Ucomplex::pComplexArray X);  /*b = Ax*/
	void MVmultAccum(Ucomplex::pComplexArray B, Ucomplex::pComplexArray X);  /*b = Ax*/
	Ucomplex::pComplexArray GetValuesArrayPtr(int& Order);
	void ZeroRow(int iRow);
	void ZeroCol(int iCol);
	Ucomplex::complex AvgDiagonal();   // Average of Diagonal Elements
	Ucomplex::complex AvgOffDiagonal();
	void MultByConst(double X);  // Multiply all elements by a constant
	TcMatrix* MtrxMult(TcMatrix* B); // Multiply two square matrices of same order.  Result = A*B
	TcMatrix* Kron(int EliminationRow);  // Perform Kron reduction on last row/col and return new matrix
	int get_Norder();

	TcMatrix();
};
/*--------------------------------------------------------------------------*/

typedef TcMatrix* pTcMatrix;

}  // namespace Ucmatrix

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Ucmatrix;
#endif

#endif // UcmatrixH




