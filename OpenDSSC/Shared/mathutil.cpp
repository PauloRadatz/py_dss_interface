

#pragma hdrstop

#include "mathutil.h"

#include "DSSGlobals.h"


using namespace std;


namespace mathutil
{


TcMatrix* As2p = nullptr;
TcMatrix* Ap2s = nullptr;
TcMatrix* ClarkeF = nullptr;
TcMatrix* ClarkeR = nullptr;

// PI controller

TPICtrl::TPICtrl()
	: kNum(0.0),
	kDen(0.0),
	Kp(0.0)
{
	//Initializes the constants for a rising function of 5 steps
	den.resize(2);
	num.resize(2);
	kNum	= 0.8647;
	kDen	= 0.1353;
	Kp		= 0.02;
	den[1]	= 0;
	num[1]	= 0;
}


TPICtrl::~TPICtrl()
{
	den.resize(0);
	num.resize(0);
}


double TPICtrl::SolvePI(double SetPoint)
{
	double result = 0.0;
	num[0] = num[1];
	num[1] = SetPoint * Kp;
	den[0] = den[1];
	den[1] = (num[0] * kNum) + (den[0] * kDen);
	result = den[1];
	return result;
}



   // Sqrt23:Double;

void ETKInvert(pDoubleArray A, int Norder, int& Error)
{
	int j = 0;
	int k = 0;
	int l = 0;
	int LL = 0;
	int m = 0;
	int i = 0;
	Arraydef::pIntegerArray LT = nullptr;
	double RMY = 0.0;
	double T1 = 0.0;

	auto Index = [&](int i, int j) -> int
	{
		int result = 0;
		result = (j - 1) * l + i;
		return result;
	};
	int stop = 0;
	l = Norder;
	Error = 0;

/*Allocate LT*/
	free(LT);
	LT = new longInt[l];
	if(LT == nullptr)
	{
		Error = 1;
		return;
	}

/*Zero LT*/
	for(stop = l, j = 1; j <= stop; j++)
	{
		LT[j - 1] = 0;
	}
	T1 = 0.0;

/*M Loop */
    // initialize a safe value of k
	k = 1;
	for(stop = l, m = 1; m <= stop; m++)
	{
		int stop1 = 0;
		for(stop1 = l, LL = 1; LL <= stop1; LL++)
		{
			if(LT[LL - 1] != 1)
			{
				RMY = Abs(A[Index(LL, LL) - 1]) - Abs(T1);
				if(RMY > 0.0)
				{
					T1 = A[Index(LL, LL) - 1];
					k = LL;
				} /*RMY*/
			} /*IF LT*/
		} /*LL*/

/*Error Check.  If RMY ends up zero, matrix is non-inversible*/
		RMY = Abs(T1);
		if(RMY == 0.0)
		{
			Error = 2;
			return;
		}
		T1 = 0.0;
		LT[k - 1] = 1;
		for(stop1 = l, i = 1; i <= stop1; i++)
		{
			if(i != k)
			{
				int stop2 = 0;
				for(stop2 = l, j = 1; j <= stop2; j++)
				{
					if(j != k)
						A[Index(i, j) - 1] = A[Index(i, j) - 1] - A[Index(i, k) - 1] * A[Index(k, j) - 1] / A[Index(k, k) - 1];
				}
			}
		}
		A[Index(k, k) - 1] = -1.0 / A[Index(k, k) - 1];
		for(stop1 = l, i = 1; i <= stop1; i++)
		{
			if(i != k)
			{
				A[Index(i, k) - 1] = A[Index(i, k) - 1] * A[Index(k, k) - 1];
				A[Index(k, i) - 1] = A[Index(k, i) - 1] * A[Index(k, k) - 1];
			}
		}  /*if*/
	} /*M loop*/
	for(stop = l, j = 1; j <= stop; j++)
	{
		int stop1 = 0;
		for(stop1 = l, k = 1; k <= stop1; k++)
		{
			A[Index(j, k) - 1] = -A[Index(j, k) - 1];
		}
	}
	free(LT);  /*Dispose of LT*/
}

/*
	Matrix= reference to matrix of DOUBLEs
        Norder=  order of matrix  (assumed square)
        Error 	= 0 if no error;
        	= 1 if not enough heap to alloc temp array
                = 2 if matrix can't be inverted

        This routine will invert a non-symmetric matrix.  Index is assumed to
        follow the FORTRAN standard, not the Pascal standard.  That is the data
        are ordered by first subscript first, then second subscript.  This routine
        computes its own indexing, leaving nothing to the whims of a cantankerous compiler.

        It assumes that the matrix is dimensioned to exactly the number of elements
        needed.  Apologies to Fortran users who are accustomed to over dimensioning
        stuff.

*/ /*Proc Invert*/

   

/*-------------------------------------------------------------*/

void Phase2SymComp(pComplexArray Vph, pComplexArray V012)
{
	/*# with Ap2s do */
	{
		auto& with0 = Ap2s;
		with0->MVmult(V012, Vph);
	}
}

/*-------------------------------------------------------------*/

void SymComp2Phase(pComplexArray Vph, pComplexArray V012)
{
	/*# with As2p do */
	{
		auto& with0 = As2p;
		with0->MVmult(Vph, V012);
	}
}

/*-------------------------------------------------------------*/

void SetClarkeMatrices()
{
	double Sin2pi3 = 0.0;
	Sin2pi3 = sin(2.0 * PI / 3.0);
	/*# with ClarkeF do */
	{
		auto& with0 = ClarkeF;       // Forward Clarke
		with0->SetElement(1, 1, cmplx(1.0, 0.0));
		with0->SetElement(1, 2, cmplx(-0.5, 0.0));
		with0->SetElement(1, 3, cmplx(-0.5, 0.0));
		with0->SetElement(2, 2, cmplx(Sin2pi3, 0.0));
		with0->SetElement(2, 3, cmplx(-Sin2pi3, 0.0));
		with0->SetElement(3, 1, cmplx(0.5, 0.0));
		with0->SetElement(3, 2, cmplx(0.5, 0.0));
		with0->SetElement(3, 3, cmplx(0.5, 0.0));
		with0->MultByConst(2.0 / 3.0);  // multiply all elements by a const  2/3
	}
	/*# with ClarkeR do */
	{
		auto& with1 = ClarkeR;       // Reverse Clarke
		with1->SetElement(1, 1, cmplx(1.0, 0.0));
		with1->SetElement(2, 1, cmplx(-0.5, 0.0));
		with1->SetElement(3, 1, cmplx(-0.5, 0.0));
		with1->SetElement(2, 2, cmplx(Sin2pi3, 0.0));
		with1->SetElement(3, 2, cmplx(-Sin2pi3, 0.0));
		with1->SetElement(1, 3, cmplx(1.0, 0.0));
		with1->SetElement(2, 3, cmplx(1.0, 0.0));
		with1->SetElement(3, 3, cmplx(1.0, 0.0));
	}
}
/*-------------------------------------------------------------*/

void Phase2AB0(pComplexArray Vph, pComplexArray VaB0)
{
	/*# with ClarkeF do */
	{
		auto& with0 = ClarkeF;
		with0->MVmult(VaB0, Vph);
	}
}     // Forward Clarke



/*-------------------------------------------------------------*/

void AB02Phase(pComplexArray Vph, pComplexArray VaB0)
{
	/*# with ClarkeR do */
	{
		auto& with0 = ClarkeR;
		with0->MVmult(Vph, VaB0);
	}
}     // Reverse Clarke

complex TerminalPowerIn(pComplexArray V, pComplexArray i, int NPhases)
{
	complex result = {};
	int j = 0;
	int stop = 0;
	result = CZero;
	for(stop = NPhases, j = 1; j <= stop; j++)
	{
		caccum(result, cmul(V[j - 1], conjg(i[j - 1])));
	}
	return result;
}
// Computes total complex power given terminal  voltages and currents

/*-------------------------------------------------------------*/

void CalcKPowers(pComplexArray kWkvar, pComplexArray V, pComplexArray i, int n)
{
	int j = 0;
	int stop = 0;
	for(stop = n, j = 1; j <= stop; j++)
	{
		kWkvar[j - 1] = cmulreal(cmul(V[j - 1], conjg(i[j - 1])), 0.001);
	}
}

/*Compute complex power in kW and kvar in each phase*/

/*-------------------------------------------------------------*/

void SetAMatrix(TcMatrix* Amat)
{
	complex A = {};
	complex AA = {};
	int i = 0;
	A = cmplx(-0.5, 0.866025403);
	AA = cmplx(-0.5, -0.866025403);
	/*# with Amat do */
	{
		auto with0 = Amat;
		int stop = 0;
		for(stop = 3, i = 1; i <= stop; i++)
		{
			with0->SetElemsym(1, i, cmplx(1.0,0));
		}
		with0->SetElement(2, 2, AA);
		with0->SetElement(3, 3, AA);
		with0->SetElemsym(2, 3, A);
	}
}


/*-------------------------------------------------------------*/

double Gauss(double Mean, double StdDev)
{
	double result = 0.0;
	int i = 0;
	double A = 0.0;
	int stop = 0;
	A = 0.0;
	for(stop = 12, i = 1; i <= stop; i++)
	{
		A = A + rand();
	}
	result = (A - 6.0) * StdDev + Mean;
	return result;
}
/*Returns a normally distributed random variable*/

/*-------------------------------------------------------------*/

double QuasiLogNormal(double Mean)
{
	double result = 0.0;
	result = exp(Gauss(0.0, 1.0)) * Mean;
	return result;
}

/*Generates a quasi-lognormal distribution with approx 50% of values from 0 to Mean and the remainder from Mean to infinity*/

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

long double /*register*/ RCDSUM(void* Data, int Count)
{
	long double result = 0.0L;    // now EDX contains Count - 1
      // count * sizeof(Double) = count * 8
//	_asm
	{
	/*  // IN: EAX = ptr to Data, EDX = High(Data) = Count - 1
     // Uses 4 accumulators to minimize read-after-write delays and loop overhead
     // 5 clocks per loop, 4 items per loop = 1.2 clocks per item
       FLDZ
       SUB      EDX, 1    // now EDX contains Count - 1
       MOV      ECX, EDX
       FLD      ST(0)
       AND      EDX, not 3
       FLD      ST(0)
       AND      ECX, 3
       FLD      ST(0)
       SHL      EDX, 3      // count * sizeof(Double) = count * 8
       JMP      @Vector.Pointer[ECX*4]
@Vector:
       DD @@1
       DD @@2
       DD @@3
       DD @@4
@@4:   fAdd     qword Ptr [EAX+EDX+24]    // 1
       FXCH     ST(3)                     // 0
@@3:   fAdd     qword Ptr [EAX+EDX+16]    // 1
       FXCH     ST(2)                     // 0
@@2:   fAdd     qword Ptr [EAX+EDX+8]     // 1
       FXCH     ST(1)                     // 0
@@1:   fAdd     qword Ptr [EAX+EDX]       // 1
       FXCH     ST(2)                     // 0
       SUB      EDX, 32
       JNS      @@4
       FADDP    ST(3),ST                  // ST(3) := ST + ST(3); Pop ST
       fAdd                               // ST(1) := ST + ST(1); Pop ST
       fAdd                               // ST(1) := ST + ST(1); Pop ST
       FWAIT
	*/
	}
	return result;
} 

// Sums an array of doubles quickly

/* With register convention first 3 parameters are passed EAX, EDX, ECX and
  remainder on stack*/

void RCDMeanAndStdDev(void* pData, int Ndata, double& Mean, double& StdDev)
{
	typedef double DoubleArray[100/*# range 1..100*/];
	typedef DoubleArray* pDoubleArray;
	pDoubleArray Data = nullptr;
	double s = 0.0;
	int i = 0;
	int stop = 0;
	Data = ((pDoubleArray) pData);  // make a double pointer
	if(Ndata == 1)
	{
		Mean = (*Data)[1 - 1];
		StdDev = (*Data)[1 - 1];
		return;
	}

	//Mean = RCDSUM(Data, (Ndata)) / Ndata;
	Mean = 0.0;
	for (stop = Ndata, i = 1; i <= stop; i++)
    {
		Mean = Mean + (*Data)[i - 1];
	}
    Mean = Mean / Ndata;

	s = 0;               // sum differences from the mean, for greater accuracy
	for(stop = Ndata, i = 1; i <= stop; i++)
	{
		s = s + pow((Mean - (*Data)[i - 1]), 2);
	}
	StdDev = sqrt(s / (Ndata - 1));
}

void CurveMeanAndStdDev(pDoubleArray pY, pDoubleArray pX, int n, double& Mean, double& StdDev)
{
	double s = 0.0;
	double dy1 = 0.0;
	double dy2 = 0.0;
	int i = 0;
	int stop = 0;
	if(n == 1)
	{
		Mean = pY[1 - 1];
		StdDev = pY[1 - 1];
		return;
	}
	s = 0;
	for(stop = n - 1, i = 1; i <= stop; i++)
	{
		s = s + 0.5 * (pY[i - 1] + pY[i + 1 - 1]) * (pX[i + 1 - 1] - pX[i - 1]);
	}
	Mean = s / (pX[n - 1] - pX[1 - 1]);
	s = 0;               // sum differences from the mean, for greater accuracy
	for(stop = n - 1, i = 1; i <= stop; i++)
	{
		dy1 = (pY[i - 1] - Mean);
		dy2 = (pY[i + 1 - 1] - Mean);
		s = s + 0.5 * (dy1 * dy1 + dy2 * dy2) * (pX[i + 1 - 1] - pX[i - 1]);
	}
	StdDev = sqrt(s / (pX[n - 1] - pX[1 - 1]));
}

double GetXR(const complex& A)
{
	double result = 0.0;
	if(A.re != 0.0)
	{
		result = A.im / A.re;
		if(Abs(result) > 9999.0)
			result = 9999.0;
	}
	else
	result = 9999.0;
	return result;
}

complex ParallelZ(const complex& Z1, const complex& Z2)
{
	complex result = {};
	complex Denom = {};
    /*Parallel two complex impedances*/
	Denom = cadd(Z1, Z2);
	if((Abs(Denom.re) > 0.0) || (Abs(Denom.im) > 0.0))
		result = cdiv(cmul(Z1, Z2), Denom);
	else
 /*Error*/
		result = CZero;
	return result;
}

// z = I0(a)

complex Bessel_I0(const complex& A)
{
	complex result = {};
	const int MaxTerm = 1000;
	const double EpsilonSqr = 1.0e-20;
	int i = 0;
	double SizeSqr = 0.0;
	complex term = {};
	complex zSQR25 = {};
	result = cONE;                // term 0
	zSQR25 = cmulreal(cmul(A, A), 0.25);
	term = zSQR25;
	caccum(result, zSQR25);      // term 1
	i = 1;
	do
	{
		term = cmul(zSQR25, term);
		++i;
		term = cdivreal(term, Sqr(i));
		caccum(result, term);          // sum := sum + term
		SizeSqr = Sqr(term.re) + Sqr(term.im);
	}
	while(!((i > MaxTerm) || (SizeSqr < EpsilonSqr)));
	return result;
} /*Bessel_I0*/

complex Bessel_I1(const complex& X)
{
	complex result = {};
	const double MaxTerm = 1000.0;
	const double EpsilonSqr = 1.0e-20;
	double i = 0.0;
	complex term = {};
	complex incterm = {};
	complex newterm = {};
	double SizeSqr = 0.0;
	term = cdivreal(X, 2.0);
	result = term;
	incterm = term;
	i = 4.0;
	do
	{
		newterm = cdivreal(X, i);
		term = cmul(term, cmul(incterm, newterm));
		caccum(result, term);
		incterm = newterm;
		i = i + 2.0;
		SizeSqr = Sqr(term.re) + Sqr(term.im);
	}
	while(!((i > MaxTerm) || (SizeSqr < EpsilonSqr)));
	return result;
}

double PctNemaUnbalance(pComplexArray Vph)
{
	double result = 0.0;
	int i = 0;
	double Vavg = 0.0;
	double MaxDiff = 0.0;
	double Vmag[3/*# range 1..3*/];
	int stop = 0;
	for(stop = 3, i = 1; i <= stop; i++)
	{
		Vmag[i - 1] = cabs(Vph[i - 1]);
	}
	Vavg = 0.0;
	for(stop = 3, i = 1; i <= stop; i++)
	{
		Vavg = Vavg + Vmag[i - 1];
	}
	Vavg = Vavg / 3.0;
	MaxDiff = 0.0;
	for(stop = 3, i = 1; i <= stop; i++)
	{
		MaxDiff = max(MaxDiff, Abs((Vmag[i - 1] - Vavg)));
	}
	if(Vavg != 0.0)  // pct difference
		result = MaxDiff / Vavg * 100.0;
	else
		result = 0.0;
	return result;
}

/*Return Nema unbalance */

/*#inline*/
void DblInc(double& X, double Y)
{
	X = X + Y;
}


void mathutil_initialization()
{
	rand();
	As2p = new TcMatrix(3);
	Ap2s = new TcMatrix(3);
	ClarkeF = new TcMatrix(3);
	ClarkeR = new TcMatrix(3);
	SetAMatrix(As2p);
	SetAMatrix(Ap2s);
	Ap2s->Invert();
	SetClarkeMatrices();
    // Sqrt23 := Sqrt(2.0/3.0); // for park
}

void mathutil_finalization()
{
	delete As2p;
	delete Ap2s;
	delete ClarkeF;
	delete ClarkeR;
}

		class 		mathutil_unit
		{
		public:
		mathutil_unit()
		{
			//AssertSystemInitialization();
			mathutil_initialization();
		}
		~		mathutil_unit(){mathutil_finalization(); }
		};
		mathutil_unit _mathutil_unit;

}  // namespace mathutil




