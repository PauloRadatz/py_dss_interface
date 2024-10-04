#ifndef mathutilH
#define mathutilH

#include "System.h"
#include "Sysutils.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "Ucmatrix.h"
#include <math.h>

namespace mathutil
{


   /*Math utilities*/
/*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
extern Ucmatrix::TcMatrix* As2p;
extern Ucmatrix::TcMatrix* Ap2s;
extern Ucmatrix::TcMatrix* ClarkeF;
extern Ucmatrix::TcMatrix* ClarkeR; /*Symmetrical Component Conversion Matrices*/
void AB02Phase(Ucomplex::pComplexArray Vph, Ucomplex::pComplexArray VaB0);     // Reverse Clarke
Ucomplex::complex Bessel_I0(const Ucomplex::complex& A);
Ucomplex::complex Bessel_I1(const Ucomplex::complex& X);
void CalcKPowers(Ucomplex::pComplexArray kWkvar, Ucomplex::pComplexArray V, Ucomplex::pComplexArray i, int n);
void ETKInvert(Arraydef::pDoubleArray A, int Norder, int& Error);  // Real Matrix Inversion
double Gauss(double Mean, double StdDev);
double GetXR(const Ucomplex::complex& A);
Ucomplex::complex ParallelZ(const Ucomplex::complex& Z1, const Ucomplex::complex& Z2);
void Phase2AB0(Ucomplex::pComplexArray Vph, Ucomplex::pComplexArray VaB0);     // Forward Clarke
void Phase2SymComp(Ucomplex::pComplexArray Vph, Ucomplex::pComplexArray V012);
double QuasiLogNormal(double Mean);
void RCDMeanAndStdDev(void* pData, int Ndata, double& Mean, double& StdDev);
void CurveMeanAndStdDev(Arraydef::pDoubleArray pY, Arraydef::pDoubleArray pX, int n, double& Mean, double& StdDev);
//         function  RCDSum( Data:Pointer; Count:Integer): Extended; register;
void SymComp2Phase(Ucomplex::pComplexArray Vph, Ucomplex::pComplexArray V012);
Ucomplex::complex TerminalPowerIn(Ucomplex::pComplexArray V, Ucomplex::pComplexArray i, int NPhases);
double PctNemaUnbalance(Ucomplex::pComplexArray Vph);
void DblInc(double& X, double Y);//#inline // increment a double

class TPICtrl : public TObject {
	typedef TObject inherited;
private:
	std::vector< double > den, num;
public:
	double kNum, kDen, Kp;
	double SolvePI(double SetPoint);
	TPICtrl();
	virtual ~TPICtrl();
};


typedef TPICtrl* PPICtrl;


}  // namespace mathutil

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace mathutil;
#endif

#endif // mathutilH




