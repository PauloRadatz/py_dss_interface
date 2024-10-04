
#pragma hdrstop

#include "Ucmatrix.h"

using namespace std;


namespace Ucmatrix
{

TcMatrix::TcMatrix() {}


//#pragma resource "-" 
  /* Turn off range checking*/
/*--------------------------------------------------------------------------*/

TcMatrix::TcMatrix(int n)
 : Norder(0),
   InvertError(0)
{
	Values.clear();
	int i = 0;
	try
	{
		int stop = 0;
		;
		Norder = n;
		InvertError = 0;
		Values.resize(Norder * Norder);    /*Allocate*/
		for(stop = (Norder * Norder), i = 1; i <= stop; i++)
		{
			Values[i - 1] = cmplx(0.0, 0.0);
		}
	}
	catch(...)
	{
		//Destroy();
	}
}

 /*--------------------------------------------------------------------------*/

TcMatrix::~TcMatrix()
{
	if(!Values.empty())
		Values.clear();
	// inherited::Destroy();
}

/*--------------------------------------------------------------------------*/

int TcMatrix::get_Norder()
{
	return Norder;
}

/*--------------------------------------------------------------------------*/

void TcMatrix::Clear()
{
	int i = 0;
	int stop = 0;
	for(stop = (Norder * Norder), i = 1; i <= stop; i++)
	{
		Values[i - 1] = cmplx(0.0, 0.0);
	}
}

/*--------------------------------------------------------------------------*/

void TcMatrix::MVmult(pComplexArray B, pComplexArray X)
{
	complex Sum = {};
	int i = 0;
	int j = 0;
	int stop = 0;
	for(stop = Norder, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		Sum = cmplx(0.0, 0.0);
		for(stop1 = Norder, j = 1; j <= stop1; j++)
		{
			caccum(Sum, cmul(Values[((j - 1) * Norder + i) - 1], X[j - 1]));
		}
		B[i - 1] = Sum;
	}
}
 /*--------------------------------------------------------------------------*/
   // Same as MVMult except accumulates b

void TcMatrix::MVmultAccum(pComplexArray B, pComplexArray X)
{
	complex Sum = {};
	int i = 0;
	int j = 0;
	int stop = 0;
	for(stop = Norder, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		Sum = cmplx(0.0, 0.0);
		for(stop1 = Norder, j = 1; j <= stop1; j++)
		{
			caccum(Sum, cmul(Values[((j - 1) * Norder + i) - 1], X[j - 1]));
		}
		caccum(B[i - 1], Sum);
	}
}

/*--------------------------------------------------------------------------*/

void TcMatrix::Invert()
{
	typedef std::vector < int > IntArray;
	typedef IntArray pIntArray;
	int j = 0;
	int k = 0;
	int l = 0;
	int LL = 0;
	int m = 0;
	int i = 0;
	pIntArray LT;
	double RMY = 0.0;
	complex T1 = {};
	std::vector <complex> A;

	A.clear();
	auto Index = [&](int i, int j) -> int 
	{
		int result = 0;
		result = ( (j - 1) * l + i );
		return result;
	};
	int stop = 0;
	l = Norder;
	InvertError = 0;
	A = Values;  /*  Assign pointer to something we can use*/

/*Allocate LT*/
//     LT:=nil;
	LT.resize(l + 1);
	if(LT.empty())
	{
		InvertError = 1;
		return;
	}

/*Zero LT*/
	for(stop = l, j = 1; j <= stop; j++)
	{
		LT[j] = 0;
	}
	T1 = cmplx(0.0, 0.0);
	k = 1;

/*M Loop */
	for(stop = l, m = 1; m <= stop; m++)
	{
		int stop1 = 0;
		for(stop1 = l, LL = 1; LL <= stop1; LL++)
		{
			if(LT[LL] != 1)
			{
				RMY = cabs(A[Index(LL, LL) - 1]) - cabs(T1);  /*Will this work??*/
				if(RMY > 0.0)
				{
					T1 = A[Index(LL, LL) - 1];
					k = LL;
				} /*RMY*/
			} /*IF LT*/
		} /*LL*/

/*Error Check.  If RMY ends up zero, matrix is non-inversible*/
		RMY = cabs(T1);
		if(RMY == 0.0)
		{
			InvertError = 2;
			return;
		}
		T1 = cmplx(0.0, 0.0);
		LT[k] = 1;
		for(stop1 = l, i = 1; i <= stop1; i++)
		{
			if(i != k)
			{
				int stop2 = 0;
				for(stop2 = l, j = 1; j <= stop2; j++)
				{
					if(j != k)
						A[Index(i, j) - 1] = csub(A[Index(i, j) - 1], cdiv(cmul(A[Index(i, k) - 1], A[Index(k, j) - 1]), A[Index(k, k) - 1]));
				}
			}
		}
		A[Index(k, k) - 1] = cnegate(cinv(A[Index(k, k) - 1])); /*Invert and negate k,k element*/
		for(stop1 = l, i = 1; i <= stop1; i++)
		{
			if(i != k)
			{
				A[Index(i, k) - 1] = cmul(A[Index(i, k) - 1], A[Index(k, k) - 1]);
				A[Index(k, i) - 1] = cmul(A[Index(k, i) - 1], A[Index(k, k) - 1]);
			}
		}  /*if*/
	} /*M loop*/
	for(stop = l, j = 1; j <= stop; j++)
	{
		int stop1 = 0;
		for(stop1 = l, k = 1; k <= stop1; k++)
		{
			A[Index(j, k) - 1] = cnegate(A[Index(j, k) - 1]);
		}
	}
	LT.clear(); //# FreeMemory accepts one parameter only;  /*Dispose of LT*/
	Values = A;
}
    
/*--------------------------------------------------------------------------*/

void TcMatrix::SetElement(int i, int j, const complex& cValue)
{
	complex Value = cValue;
	Values[ ( (j - 1) * Norder + i ) - 1 ] = Value;
}

/*--------------------------------------------------------------------------*/

void TcMatrix::AddElement(int i, int j, const complex& cValue)
{
	complex Value = cValue;
	caccum(Values[ ( (j - 1) * Norder + i ) - 1 ], Value);
}

/*--------------------------------------------------------------------------*/

void TcMatrix::SetElemsym(int i, int j, const complex& cValue)
{
	complex Value = cValue;
	Values[((j - 1) * Norder + i) - 1] = Value;
	if(i != j)
		Values[((i - 1) * Norder + j) - 1] = Value;  /*ensure symmetry*/
}

   /*--------------------------------------------------------------------------*/

void TcMatrix::AddElemsym(int i, int j, const complex& cValue)
{
	complex Value = cValue;
	caccum(Values[((j - 1) * Norder + i - 1)], Value);
	if (i != j)
		caccum(Values[((i - 1) * Norder + j - 1)], Value);  // Asegurar simetría

}

/*--------------------------------------------------------------------------*/

complex TcMatrix::GetElement(int i, int j)
{
	complex result = {};
	
	// Verify Index Limits 
	if (i < 1 || i > Norder || j < 1 || j > Norder)
	{
		// Handle Error, throw exception or return.
		return result;
	}

	// Calc matrix index element
	int index = (j - 1) * Norder + i-1;

	// Access the element in the matrix values
	result = Values[index];

	return result;
}




/*--------------------------------------------------------------------------*/

int TcMatrix::GetErrorCode()
{
	int result = 0;
	result = InvertError;
	return result;
}

/*--------------------------------------------------------------------------*/
    /* Sum all elements in a given block of the matrix*/

complex TcMatrix::SumBlock(int row1, int row2, int col1, int col2)
{
	complex result = {};
	int i = 0;
	int j = 0;
	int RowStart = 0;
	complex Sum = {};
	int stop = 0;
	Sum = cmplx(0.0, 0.0);
	for(stop = col2, j = col1; j <= stop; j++)
	{
		int stop1 = 0;
		RowStart = (j - 1) * Norder;
		for(stop1 = (RowStart + row2), i = (RowStart + row1); i <= stop1; i++)
		{
			Sum = cadd(Sum, Values[i - 1]);
		}
	}
	result = Sum;
	return result;
}
/*--------------------------------------------------------------------------*/

void TcMatrix::CopyFrom(TcMatrix* OtherMatrix)
{
	int i = 0;
	int j = 0;
	if(Norder == OtherMatrix->Norder)
	{
		int stop = 0;
		for(stop = Norder, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			for(stop1 = Norder, j = 1; j <= stop1; j++)
			{
				SetElement(i, j, OtherMatrix->GetElement(i, j));
			}
		}
	}
}

/*--------------------------------------------------------------------------*/

void TcMatrix::AddFrom(TcMatrix* OtherMatrix)
{
	int i = 0;
	int j = 0;
	if(Norder == OtherMatrix->Norder)
	{
		int stop = 0;
		for(stop = Norder, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			for(stop1 = Norder, j = 1; j <= stop1; j++)
			{
				AddElement(i, j, OtherMatrix->GetElement(i, j));
			}
		}
	}
}

/*--------------------------------------------------------------------------*/

pComplexArray TcMatrix::GetValuesArrayPtr(int& Order)
{
	pComplexArray result = &(Values[0]);
	Order = Norder;
	return result;
}

/*--------------------------------------------------------------------------*/

void TcMatrix::ZeroRow(int iRow)
{
	int i = 0;
	int j = 0;
	complex Zero = {};
	int stop = 0;
	Zero = cmplx(0.0, 0.0);
	j = iRow;
	for(stop = Norder, i = 1; i <= stop; i++)
	{
		Values[j - 1] = Zero;
		j += Norder;
	}
}

/*--------------------------------------------------------------------------*/

void TcMatrix::ZeroCol(int iCol)
{
	int i = 0;
	complex Zero = {};
	int stop = 0;
	Zero = cmplx(0.0, 0.0);
	for(stop = (iCol * Norder), i = ((iCol - 1) * Norder + 1); i <= stop; i++)
	{
		Values[i - 1] = Zero;
	}
}

complex TcMatrix::AvgDiagonal()
{
	complex result = {};
	int i = 0;
	int stop = 0;
	result = cmplx(0.0, 0.0);
	for(stop = Norder, i = 1; i <= stop; i++)
	{
		caccum(result, Values[((i - 1) * Norder + i) - 1]);
	}
	if(Norder > 0)
		result = cdivreal(result, (double) (Norder));
	return result;
}
// Average the upper triangle off diagonals

complex TcMatrix::AvgOffDiagonal()
{
	complex result = {};
	int i = 0;
	int j = 0;
	int Ntimes = 0;
	int stop = 0;
	result = cmplx(0.0, 0.0);
	Ntimes = 0;
	for(stop = Norder, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		for(stop1 = Norder, j = i + 1; j <= stop1; j++)
		{
			++Ntimes;
			caccum(result, Values[((j - 1) * Norder + i) - 1]);
		}
	}
	if(Ntimes > 0)
		result = cdivreal(result, (double) (Ntimes));
	return result;
}

/*Do Kron reduction on present matrix and return a new one*/
/*Eliminates specified row/column*/

TcMatrix* TcMatrix::Kron(int EliminationRow)
{
	TcMatrix* result = nullptr;
	int i = 0;
	int j = 0;
	int n = 0;
	int II = 0;
	int jj = 0;
	complex NNElement = {};
	result = nullptr;   // Nil result means it failed
	if((Norder > 1) && (EliminationRow <= Norder) && (EliminationRow > 0))
	{
		int stop = 0;
		result = new TcMatrix(Norder - 1);
		n = EliminationRow;
		NNElement = GetElement(n, n);
		II = 0;
		for(stop = Norder, i = 1; i <= stop; i++)
		{
			if(i != n)    // skip elimination row
			{
				int stop1 = 0;
				++II;
				jj = 0;
				for(stop1 = Norder, j = 1; j <= stop1; j++)
				{
					if(j != n)
					{
						++jj;
						result->SetElement(II, jj, csub(GetElement(i, j), cdiv(cmul(GetElement(i, n), GetElement(n, j)), NNElement)));
					}
				}
			}
		}
	}
	return result;
}
// multiply two scquare matrices of same order
// C (result) = A*B

TcMatrix* TcMatrix::MtrxMult(TcMatrix* B)
{
	TcMatrix* result = nullptr;
	int i = 0;
	int j = 0;
	pComplexArray cTemp1 = nullptr;
	pComplexArray cTemp2 = nullptr;
	result = nullptr;   // returns Nil pointer if illegal operation
	if(B->Norder == Norder)
	{
		int stop = 0;
		result = new TcMatrix(Norder);
		cTemp1 = new complex[Norder];   // Temp array to hold column
		cTemp2 = new complex[Norder];   // Temp array
		for(stop = Norder, j = 1; j <= stop; j++)
		{   // Column j
			int stop1 = 0;
			for(stop1 = Norder, i = 1; i <= stop1; i++)
			{
				cTemp2[i - 1] = B->GetElement(i, j);
			} // Row i
			MVmult(cTemp1, cTemp2);
			for(stop1 = Norder, i = 1; i <= stop1; i++)
			{
				result->SetElement(i, j, cTemp1[i - 1]);
			}
		}
		delete[] cTemp1;
		delete[] cTemp2;
	}
	return result;
}

void TcMatrix::MultByConst(double X)
{
	int i = 0;
	int stop = 0;
	for(stop = Norder * Norder, i = 1; i <= stop; i++)
	{
		Values[i - 1] = cmulreal(Values[i - 1], X);
	}
}




}  // namespace Ucmatrix




