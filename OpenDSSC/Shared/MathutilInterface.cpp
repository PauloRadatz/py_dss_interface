#include <vcl.h>
#pragma hdrstop

#include "MathutilInterface.h"
#include "../Shared/mathutil.h"
#include "cbess1.h"
#include "cbess2.h"
#include "ZComplex.h"

using namespace std;
using namespace d2c_system;
using namespace System;
using namespace Ucomplex;
using namespace ZComplex;
using namespace cbess1;
using namespace cbess2;
using namespace mathutil;

namespace MathutilInterface
{



void __stdcall I0(complex& result, complex& A)
{
	result = Bessel_I0(A);
}

void __stdcall I1(complex& result, complex& A)
{
	result = Bessel_I1(A);
}

void ZBESJ(double Zr, double Zi, double FNU, int KODE, int n, VEC& CYR, VEC& CYI, int& NZ, int& IERR)
{
	double AA = 0.0;
	double ALIM = 0.0;
	double Arg = 0.0;
	double CII = 0.0;
	double CSGNI = 0.0;
	double CSGNR = 0.0;
	double Dig = 0.0;
	double ELIM = 0.0;
	double FNUL = 0.0;
	double HPI = 0.0;
	double RL = 0.0;
	double R1M5 = 0.0;
	double Str = 0.0;
	double TOL = 0.0;
	double ZNI = 0.0;
	double ZNR = 0.0;
	double BB = 0.0;
	double FN = 0.0;
	double AZ = 0.0;
	int i = 0;
	int INU = 0;
	int INUH = 0;
	int IR = 0;
	int k = 0;
	int K1 = 0;
	int K2 = 0;
	int NL = 0;

/****FIRST EXECUTABLE STATEMENT  ZBESJ */
	int stop = 0;
	HPI = double(Pi) / 2.0;
	IERR = 0;
	NZ = 0;
	if(FNU < 0.0)
		IERR = 1;
	if((KODE < 1) || (KODE > 2))
		IERR = 1;
	if(n < 1)
		IERR = 1;
	if(IERR != 0)
		goto Return;
/*-----------------------------------------------------------------------
!     SET PARAMETERS RELATED TO MACHINE CONSTANTS.
!     TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18.
!     ELIM IS THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT.
!     EXP(-ELIM).LT.EXP(-ALIM):=EXP(-ELIM)/TOL    AND
!     EXP(ELIM).GT.EXP(ALIM):=EXP(ELIM)*TOL       ARE INTERVALS NEAR
!     UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED ARITHMETIC IS DONE.
!     RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z.
!     DIG := NUMBER OF BASE 10 DIGITS IN TOL = 10^(-DIG).
!     FNUL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC SERIES FOR LARGE FNU.
!----------------------------------------------------------------------*/
	TOL = DMAX(D1MACH(4), 1E-18);
	K1 = I1MACH(15);
	K2 = I1MACH(16);
	R1M5 = D1MACH(5);
	k = IMin(Abs(K1), Abs(K2));
	ELIM = 2.303 * (k * R1M5 - 3.0);
	K1 = I1MACH(14) - 1;
	AA = R1M5 * K1;
	Dig = DMIN(AA, 18.0);
	AA = AA * 2.303;
	ALIM = ELIM + DMAX(-AA, -41.45);
	RL = 1.2 * Dig + 3.0;
	FNUL = 10.0 + 6.0 * (Dig - 3.0);
/*-----------------------------------------------------------------------
!     TEST FOR PROPER RANGE
!----------------------------------------------------------------------*/
	AZ = ZABS(Zr, Zi);
	FN = FNU + 1.0 * (n - 1);
	AA = 0.5 / TOL;
	BB = I1MACH(9) * 0.5;
	AA = DMIN(AA, BB);
	if(AZ > AA)
		goto 260;
	if(FN > AA)
		goto 260;
	AA = Sqrt(AA);
	if(AZ > AA)
		IERR = 3;
	if(FN > AA)
		IERR = 3;
/*-----------------------------------------------------------------------
!     CALCULATE CSGN=EXP(FNU*HPI*I) TO MINIMIZE LOSSES OF SIGNIFICANCE
!     WHEN FNU IS LARGE
!----------------------------------------------------------------------*/
	CII = 1.0;
	INU = (int) Round(FNU);
	INUH = (int)(INU / 2);
	IR = INU - 2 * INUH;
	Arg = (FNU - 1.0 * (INU - IR)) * HPI;
	CSGNR = Cos(Arg);
	CSGNI = Sin(Arg);
	if((INUH % 2) == 0)
		goto 40;
	CSGNR = -CSGNR;
	CSGNI = -CSGNI;
/*-----------------------------------------------------------------------
!     ZN IS IN THE RIGHT HALF PLANE
!----------------------------------------------------------------------*/
	40:;
	ZNR = Zi;
	ZNI = -Zr;
	if(Zi >= 0.0)
		goto 50;
	ZNR = -ZNR;
	ZNI = -ZNI;
	CSGNI = -CSGNI;
	CII = -CII;
	50:;
	ZBINU(ZNR, ZNI, FNU, KODE, n, CYR, CYI, NZ, RL, FNUL, TOL, ELIM, ALIM);
	if(NZ < 0)
		goto 130;
	NL = n - NZ;
	if(NL == 0)
		goto Return;
	for(stop = NL, i = 1; i <= stop; i++)
	{
		Str = CYR[i] * CSGNR - CYI[i] * CSGNI;
		CYI[i] = CYR[i] * CSGNI + CYI[i] * CSGNR;
		CYR[i] = Str;
		Str = -CSGNI * CII;
		CSGNI = CSGNR * CII;
		CSGNR = Str;
	}
	goto Return;
	130:;
	if(NZ ==  - 2)
		goto 140;
	NZ = 0;
	IERR = 2;
	goto Return;
	140:;
	NZ = 0;
	IERR = 5;
	goto Return;
	260:;
	NZ = 0;
	IERR = 4;
	Return:;
}
/****BEGIN PROLOGUE  ZBESJ
!***DATE WRITTEN   830501   (YYMMDD)  (Original Fortran 77 version).
!***REVISION DATE  830501   (YYMMDD)
!***CATEGORY NO.  B5K
!***KEYWORDS  J-BESSEL FUNCTION,BESSEL FUNCTION OF COMPLEX ARGUMENT,
!             BESSEL FUNCTION OF FIRST KIND
!***AUTHOR  AMOS, DONALD E., SANDIA NATIONAL LABORATORIES
!***PURPOSE  TO COMPUTE THE J-BESSEL FUNCTION OF A COMPLEX ARGUMENT
!***DESCRIPTION
!
!                      ***A DOUBLE PRECISION ROUTINE***
!         ON KODE=1, CBESJ COMPUTES AN N MEMBER  SEQUENCE OF COMPLEX
!         BESSEL FUNCTIONS CY(I)=J(FNU+I-1,Z) FOR REAL, NONNEGATIVE
!         ORDERS FNU+I-1, I=1,...,N AND COMPLEX Z IN THE CUT PLANE
!         -PI < ARG(Z) <= PI. ON KODE=2, CBESJ RETURNS THE SCALED
!         FUNCTIONS:
!
!         CY(I)=EXP(-ABS(Y))*J(FNU+I-1,Z)   I = 1,...,N , Y=AIMAG(Z)
!
!         WHICH REMOVE THE EXPONENTIAL GROWTH IN BOTH THE UPPER AND
!         LOWER HALF PLANES FOR Z TO INFINITY. DEFINITIONS AND NOTATION
!         ARE FOUND IN THE NBS HANDBOOK OF MATHEMATICAL FUNCTIONS
!         (REF. 1).
!
!         INPUT      ZR,ZI,FNU ARE DOUBLE PRECISION
!           ZR,ZI  - Z:=CMPLX(ZR,ZI),  -PI.LT.ARG(Z).LE.PI
!           FNU    - ORDER OF INITIAL J FUNCTION, FNU.GE.0.0D0
!           KODE   - A PARAMETER TO INDICATE THE SCALING OPTION
!                    KODE= 1  RETURNS
!                             CY(I)=J(FNU+I-1,Z), I=1,...,N
!                        = 2  RETURNS
!                             CY(I)=J(FNU+I-1,Z)EXP(-ABS(Y)), I=1,...,N
!           N      - NUMBER OF MEMBERS OF THE SEQUENCE, N.GE.1
!
!         OUTPUT     CYR,CYI ARE DOUBLE PRECISION
!           CYR,CYI- DOUBLE PRECISION VECTORS WHOSE FIRST N COMPONENTS
!                    CONTAIN REAL AND IMAGINARY PARTS FOR THE SEQUENCE
!                    CY(I)=J(FNU+I-1,Z)  OR
!                    CY(I)=J(FNU+I-1,Z)EXP(-ABS(Y))  I=1,...,N
!                    DEPENDING ON KODE, Y:=AIMAG(Z).
!           NZ     - NUMBER OF COMPONENTS SET TO ZERO DUE TO UNDERFLOW,
!                    NZ= 0   , NORMAL RETURN
!                    NZ.GT.0 , LAST NZ COMPONENTS OF CY SET  ZERO DUE
!                             TO UNDERFLOW, CY(I):=CMPLX(0.0D0,0.0D0),
!                              I = N-NZ+1,...,N
!           IERR   - ERROR FLAG
!                    IERR=0, NORMAL RETURN - COMPUTATION COMPLETED
!                    IERR=1, INPUT ERROR   - NO COMPUTATION
!                    IERR=2, OVERFLOW      - NO COMPUTATION, AIMAG(Z)
!                            TOO LARGE ON KODE=1
!                    IERR=3, CABS(Z) OR FNU+N-1 LARGE - COMPUTATION DONE
!                            BUT LOSSES OF SIGNIFCANCE BY ARGUMENT
!                            REDUCTION PRODUCE LESS THAN HALF OF MACHINE
!                            ACCURACY
!                    IERR=4, CABS(Z) OR FNU+N-1 TOO LARGE - NO COMPUTA-
!                            TION BECAUSE OF COMPLETE LOSSES OF SIGNIFI-
!                            CANCE BY ARGUMENT REDUCTION
!                    IERR=5, ERROR              - NO COMPUTATION,
!                            ALGORITHM TERMINATION CONDITION NOT MET
!
!***LONG DESCRIPTION
!
!         THE COMPUTATION IS CARRIED OUT BY THE FORMULA
!
!         J(FNU,Z)=EXP( FNU*PI*I/2)*I(FNU,-I*Z)    AIMAG(Z).GE.0.0
!
!         J(FNU,Z)=EXP(-FNU*PI*I/2)*I(FNU, I*Z)    AIMAG(Z).LT.0.0
!
!         WHERE I^2 := -1 AND I(FNU,Z) IS THE I BESSEL FUNCTION.
!
!         FOR NEGATIVE ORDERS,THE FORMULA
!
!              J(-FNU,Z) = J(FNU,Z)*COS(PI*FNU) - Y(FNU,Z)*SIN(PI*FNU)
!
!         CAN BE USED. HOWEVER,FOR LARGE ORDERS CLOSE TO INTEGERS, THE
!         THE FUNCTION CHANGES RADICALLY. WHEN FNU IS A LARGE POSITIVE
!         INTEGER,THE MAGNITUDE OF J(-FNU,Z):=J(FNU,Z)*COS(PI*FNU) IS A
!         LARGE NEGATIVE POWER OF TEN. BUT WHEN FNU IS NOT AN INTEGER,
!         Y(FNU,Z) DOMINATES IN MAGNITUDE WITH A LARGE POSITIVE POWER OF
!         TEN AND THE MOST THAT THE SECOND TERM CAN BE REDUCED IS BY
!         UNIT ROUNDOFF FROM THE COEFFICIENT. THUS, WIDE CHANGES CAN
!         OCCUR WITHIN UNIT ROUNDOFF OF A LARGE INTEGER FOR FNU. HERE,
!         LARGE MEANS FNU.GT.CABS(Z).
!
!         IN MOST COMPLEX VARIABLE COMPUTATION, ONE MUST EVALUATE ELE-
!         MENTARY FUNCTIONS. WHEN THE MAGNITUDE OF Z OR FNU+N-1 IS
!         LARGE, LOSSES OF SIGNIFICANCE BY ARGUMENT REDUCTION OCCUR.
!         CONSEQUENTLY, IF EITHER ONE EXCEEDS U1:=SQRT(0.5/UR), THEN
!         LOSSES EXCEEDING HALF PRECISION ARE LIKELY AND AN ERROR FLAG
!         IERR:=3 IS TRIGGERED WHERE UR:=DMAX1(D1MACH(4),1.0D-18) IS
!         DOUBLE PRECISION UNIT ROUNDOFF LIMITED TO 18 DIGITS PRECISION.
!         IF EITHER IS LARGER THAN U2:=0.5/UR, THEN ALL SIGNIFICANCE IS
!         LOST AND IERR:=4. IN ORDER TO USE THE INT FUNCTION, ARGUMENTS
!         MUST BE FURTHER RESTRICTED NOT TO EXCEED THE LARGEST MACHINE
!         INTEGER, U3:=I1MACH(9). THUS, THE MAGNITUDE OF Z AND FNU+N-1 IS
!         RESTRICTED BY MIN(U2,U3). ON 32 BIT MACHINES, U1,U2, AND U3
!         ARE APPROXIMATELY 2.0E+3, 4.2E+6, 2.1E+9 IN SINGLE PRECISION
!         ARITHMETIC AND 1.3E+8, 1.8E+16, 2.1E+9 IN DOUBLE PRECISION
!         ARITHMETIC RESPECTIVELY. THIS MAKES U2 AND U3 LIMITING IN
!         THEIR RESPECTIVE ARITHMETICS. THIS MEANS THAT ONE CAN EXPECT
!         TO RETAIN, IN THE WORST CASES ON 32 BIT MACHINES, NO DIGITS
!         IN SINGLE AND ONLY 7 DIGITS IN DOUBLE PRECISION ARITHMETIC.
!         SIMILAR CONSIDERATIONS HOLD FOR OTHER MACHINES.
!
!         THE APPROXIMATE RELATIVE ERROR IN THE MAGNITUDE OF A COMPLEX
!         BESSEL FUNCTION CAN BE EXPRESSED BY P*10**S WHERE P:=MAX(UNIT
!         ROUNDOFF,1.0E-18) IS THE NOMINAL PRECISION AND 10**S REPRE-
!         SENTS THE INCREASE IN ERROR DUE TO ARGUMENT REDUCTION IN THE
!         ELEMENTARY FUNCTIONS. HERE, S:=MAX(1,ABS(LOG10(CABS(Z))),
!         ABS(LOG10(FNU))) APPROXIMATELY (I.E. S:=MAX(1,ABS(EXPONENT OF
!         CABS(Z),ABS(EXPONENT OF FNU)) ). HOWEVER, THE PHASE ANGLE MAY
!         HAVE ONLY ABSOLUTE ACCURACY. THIS IS MOST LIKELY TO OCCUR WHEN
!         ONE COMPONENT (IN ABSOLUTE VALUE) IS LARGER THAN THE OTHER BY
!         SEVERAL ORDERS OF MAGNITUDE. IF ONE COMPONENT IS 10**K LARGER
!         THAN THE OTHER, THEN ONE CAN EXPECT ONLY MAX(ABS(LOG10(P))-K,
!         0) SIGNIFICANT DIGITS; OR, STATED ANOTHER WAY, WHEN K EXCEEDS
!         THE EXPONENT OF P, NO SIGNIFICANT DIGITS REMAIN IN THE SMALLER
!         COMPONENT. HOWEVER, THE PHASE ANGLE RETAINS ABSOLUTE ACCURACY
!         BECAUSE, IN COMPLEX ARITHMETIC WITH PRECISION P, THE SMALLER
!         COMPONENT WILL NOT (AS A RULE) DECREASE BELOW P TIMES THE
!         MAGNITUDE OF THE LARGER COMPONENT. IN THESE EXTREME CASES,
!         THE PRINCIPAL PHASE ANGLE IS ON THE ORDER OF +P, -P, PI/2-P,
!         OR -PI/2+P.
!
!***REFERENCES  HANDBOOK OF MATHEMATICAL FUNCTIONS BY M. ABRAMOWITZ
!                 AND I. A. STEGUN, NBS AMS SERIES 55, U.S. DEPT. OF
!                 COMMERCE, 1955.
!
!               COMPUTATION OF BESSEL FUNCTIONS OF COMPLEX ARGUMENT
!                 BY D. E. AMOS, SAND83-0083, MAY, 1983.
!
!               COMPUTATION OF BESSEL FUNCTIONS OF COMPLEX ARGUMENT
!                 AND LARGE ORDER BY D. E. AMOS, SAND83-0643, MAY, 1983
!
!               A SUBROUTINE PACKAGE FOR BESSEL FUNCTIONS OF A COMPLEX
!                 ARGUMENT AND NONNEGATIVE ORDER BY D. E. AMOS, SAND85-
!                 1018, MAY, 1985
!
!               A PORTABLE PACKAGE FOR BESSEL FUNCTIONS OF A COMPLEX
!                 ARGUMENT AND NONNEGATIVE ORDER BY D. E. AMOS, TRANS.
!                 MATH. SOFTWARE, 1986
!
!***ROUTINES CALLED  ZABS,ZBINU,I1MACH,D1MACH
!***END PROLOGUE  ZBESJ
!
!     COMPLEX CI,CSGN,CY,Z,ZN */ /*ZBESJ*/


 /*
  n:=5;
  zr:=1.0; zi:=2.0;

  ZBESJ(zr,zi,0,1,n,cyr,cyi,nz,ierr);

 */




}  // namespace MathutilInterface




