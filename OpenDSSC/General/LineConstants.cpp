

#pragma hdrstop

#include "LineConstants.h"
#include "DSSGlobals.h"
#include "mathutil.h"
#include <math.h>

using namespace std;
using namespace Arraydef;
using namespace DSSGlobals;
using namespace LineUnits;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;

namespace LineConstants
{

TLineConstants::TLineConstants() {}


complex C1_j1 = {};
double B1 = 0.0;
double B2 = 0.0;
double B3 = 0.0;
double B4 = 0.0;
double D2 = 0.0;
double D4 = 0.0;
double C2 = 0.0;
double c4 = 0.0;



/* TLineConstants */
/*Compute base Z and YC matrices in ohms/m for this frequency and earth impedance*/

void TLineConstants::Calc(double f)
{
	bool PowerFreq = false;
	complex Temp = {};
	complex Zi = {};
	complex Zspacing = {};
	complex LFactor = {};
	int ReducedSize = 0;
	int i = 0;
	int j = 0;
	double Dij = 0.0;
	double Dijp = 0.0;
	double Pfactor = 0.0;

      // RhoEarth := rho;
	int stop = 0;
	Set_Frequency(f);  // this has side effects
	if(ASSIGNED(FZreduced))
	{
		ReducedSize = FZreduced->get_Norder();
		delete FZreduced;
	}
	else
	ReducedSize = 0;
	if(ASSIGNED(FYCreduced))
		delete FYCreduced;
	FZreduced = nullptr;
	FYCreduced = nullptr;
	FZmatrix->Clear();
	FYCmatrix->Clear();

      /*For less than 1 kHz use GMR to better match published data*/
	LFactor = cmplx(0.0, Fw * mu0 / TwoPi);
	if((f < 1000.0) && (f > 40.0))
		PowerFreq = true;
	else
		PowerFreq = false;

      /*Self Impedances*/
	for(stop = FNumConds, i = 1; i <= stop; i++)
	{
		Zi = Get_Zint(i);
		if(PowerFreq) // for less than 1 kHz, use published GMR
		{
			Zi.im = 0.0;
			Zspacing = cmulreal(LFactor, log(1.0 / FGMR[i - 1]));  // use GMR
		}
		else
		{
			Zspacing = cmulreal(LFactor, log(1.0 / Fradius[i - 1]));
		}
		Temp = Get_Ze(i, i);
		FZmatrix->SetElement(i, i, cadd(Zi, cadd(Zspacing, Get_Ze(i, i))));
	}

      /*Mutual IMpedances*/
	for(stop = FNumConds, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		for(stop1 = i - 1, j = 1; j <= stop1; j++)
		{
			Dij = sqrt(sqr(FX[i - 1] - FX[j - 1]) + sqr(FY[i - 1] - FY[j - 1]));
			FZmatrix->SetElemsym(i, j, cadd(cmulreal(LFactor, log(1.0 / Dij)), Get_Ze(i, j)));
		}
	}

      /*Capacitance Matrix*/
	Pfactor = -1.0 / TwoPi / E0 / Fw; // include frequency

      /*Construct P matrix and then invert*/

      /*Self uses capradius, which defaults to actual conductor radius. But
       in case of bundled conductors can be specified different in Wiredata.*/
	for(stop = FNumConds, i = 1; i <= stop; i++)
	{
		FYCmatrix->SetElement(i, i, cmplx(0.0, Pfactor * log(2.0 * FY[i - 1] / Fcapradius[i - 1])));
	}
	for(stop = FNumConds, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		for(stop1 = i - 1, j = 1; j <= stop1; j++)
		{
			Dij = sqrt(sqr(FX[i - 1] - FX[j - 1]) + sqr(FY[i - 1] - FY[j - 1]));
			Dijp = sqrt(sqr(FX[i - 1] - FX[j - 1]) + sqr(FY[i - 1] + FY[j - 1])); // distance to image j
			FYCmatrix->SetElemsym(i, j, cmplx(0.0, Pfactor * log(Dijp / Dij)));
		}
	}
	FYCmatrix->Invert(); // now should be nodal C matrix
	if(ReducedSize > 0)
		Kron(ReducedSize);  // Was reduced so reduce again to same size
		
    /*Else the Zmatrix is OK as last computed*/
	FRhoChanged = false;
}

bool TLineConstants::ConductorsInSameSpace(string& ErrorMessage)
{
	bool result = false;
	int i = 0;
	int j = 0;
	double Dij = 0.0;
/*Check all conductors to make sure none occupy the same space or are defined at 0,0*/
	int stop = 0;
	result = false;

     /*Check for 0 Y coordinate*/
	for(stop = FNumConds, i = 1; i <= stop; i++)
	{
		if(FY[i - 1] <= 0.0)
		{
			result = true;
			ErrorMessage = Format("Conductor %d height must be  > 0. ",i);
			return result;
		}
	}

     /*Check for overlapping conductors*/
	for(stop = FNumConds, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		for(stop1 = FNumConds, j = i + 1; j <= stop1; j++)
		{
			Dij = sqrt(sqr(FX[i - 1] - FX[j - 1]) + sqr(FY[i - 1] - FY[j - 1]));
			if(Dij < (Fradius[i - 1] + Fradius[j - 1]))
			{
				result = true;
				ErrorMessage = Format("Conductors %d and %d occupy the same space.",i, j);
				return result;
			}
		}
	}
	return result;
}

TLineConstants::TLineConstants(int NumConductors)
 : FNumConds(NumConductors),
			FNumPhases(0),
			FZmatrix(nullptr),
			FYCmatrix(nullptr),
			FZreduced(nullptr),
			FYCreduced(nullptr),
			FFrequency(0.0),
			Fw(0.0),
			FrhoEarth(0.0),
			FRhoChanged(false)
{
	int i = 0;
	int stop = 0;
	Set_NPhases(FNumConds);
	FX.resize(FNumConds, 0);
	FY.resize(FNumConds, 0);
	FGMR.resize(FNumConds, 0);
	Fradius.resize(FNumConds, 0);
	Fcapradius.resize(FNumConds, 0);
	FRDC.resize(FNumConds, 0);
	Frac.resize(FNumConds, 0);


     /*Initialize to  not set*/
	for(stop = FNumConds, i = 1; i <= stop; i++)
	{
		FGMR[i - 1] = -1.0;
	}
	for(stop = FNumConds, i = 1; i <= stop; i++)
	{
		Fradius[i - 1] = -1.0;
	}
	for(stop = FNumConds, i = 1; i <= stop; i++)
	{
		Fcapradius[i - 1] = -1.0;
	}
	for(stop = FNumConds, i = 1; i <= stop; i++)
	{
		FRDC[i - 1] = -1.0;
	}
	FZmatrix = new TcMatrix(FNumConds);
	FYCmatrix = new TcMatrix(FNumConds);
	FFrequency = -1.0;  // not computed
	FrhoEarth = 100.0;  // default value
	FRhoChanged = true;
	FZreduced = nullptr;
	FYCreduced = nullptr;
}

TLineConstants::~TLineConstants()
{
	if(ASSIGNED(FZmatrix))
		delete FZmatrix;
	if(ASSIGNED(FYCmatrix))
		delete FYCmatrix;
	if(ASSIGNED(FZreduced))
		delete FZreduced;
	if(ASSIGNED(FYCreduced))
		delete FYCreduced;
	FX.clear();
	FY.clear();
	FGMR.clear();
	Fradius.clear();
	Fcapradius.clear();
	FRDC.clear();
	Frac.clear();
	// inherited;
}


double TLineConstants::Get_Capradius(int i, int Units)
{
	double result = 0.0;
	result = Fcapradius[i - 1] * From_Meters(Units);
	return result;
}

double TLineConstants::Get_GMR(int i, int Units)
{
	double result = 0.0;
	result = FGMR[i - 1] * From_Meters(Units);
	return result;
}

double TLineConstants::Get_Rac(int i, int Units)
{
	double result = 0.0;
	result = Frac[i - 1] * From_per_Meter(Units);
	return result;
}

double TLineConstants::Get_radius(int i, int Units)
{
	double result = 0.0;
	result = Fradius[i - 1] * From_Meters(Units);
	return result;
}

double TLineConstants::Get_Rdc(int i, int Units)
{
	double result = 0.0;
	result = FRDC[i - 1] * From_per_Meter(Units);
	return result;
}

double TLineConstants::Get_X(int i, int Units)
{
	double result = 0.0;
	result = FX[i - 1] * From_Meters(Units);
	return result;
}

double TLineConstants::Get_Y(int i, int Units)
{
	double result = 0.0;
	result = FY[i - 1] * From_Meters(Units);
	return result;
}
/*Makes a new YCmatrix and correct for lengths and units as it copies*/
/*Uses the reduced Zmatrix by default if it exists*/

TcMatrix* TLineConstants::Get_YCmatrix(double f, double loggth, int Units)
{
	TcMatrix* result = nullptr;
	int NewSize = 0;
	int i = 0;
	double UnitLengthConversion = 0.0;
	TcMatrix* YC = nullptr;
	pComplexArray YCValues = nullptr;
	int stop = 0;
	if(ASSIGNED(FYCreduced))
	{
		YC = FYCreduced;
	}
	else
	{
		YC = FYCmatrix;
	}
	NewSize = YC->get_Norder();
	result = new TcMatrix(NewSize);
	result->CopyFrom(YC);
	YCValues = result->GetValuesArrayPtr(NewSize);
	UnitLengthConversion = From_per_Meter(Units) * loggth;
	for(stop = NewSize * NewSize, i = 1; i <= stop; i++)
	{
		cmulrealaccum(YCValues[i - 1], UnitLengthConversion);
	}
	return result;
}

complex TLineConstants::Get_Ze(int i, int j)
{
	complex result = {};
	complex logArg = {};
	complex hterm = {};
	complex xterm = {};
	double mij = 0.0;
	double thetaij = 0.0;
	double Dij = 0.0;
	double Fyi = 0.0;
	double Fyj = 0.0;
	double term1 = 0.0;
	double term2 = 0.0;
	double term3 = 0.0;
	double term4 = 0.0;
	double term5 = 0.0;
	Fyi = Abs(FY[i - 1]);
	Fyj = Abs(FY[j - 1]);
	switch(ActiveEarthModel[ActiveActor])
	{
		case 	SIMPLECARSON:
		{
			result = cmplx(Fw * mu0 / 8.0, (Fw * mu0 / TwoPi) * log(658.5L * sqrt(FrhoEarth / FFrequency)));
 // {****}             WriteDLLDebugFile(Format('Simple: Z(%d,%d) = %.8g +j %.8g',[i,j, Result.re, result.im]));
		}
		break;
         /*notation from Tleis book Power System Modelling and Fault Analysis*/
		case 	FULLCARSON:
		{
			if(i == j)
			{
				thetaij = 0.0;
				Dij = 2.0 * Fyi;
			}
			else
			{
				Dij = sqrt(sqr(Fyi + Fyj) + sqr(FX[i - 1] - FX[j - 1]));
				thetaij = acos((Fyi + Fyj) / Dij);
			}
			mij = 2.8099e-3 * Dij * sqrt(FFrequency / FrhoEarth);
			result.re = double(DSSGlobals::PI) / 8.0 - B1 * mij * cos(thetaij) + B2 * sqr(mij) * (log(exp(C2) / mij) * cos(2.0 * thetaij) + thetaij * sin(2.0 * thetaij)) + B3 * mij * mij * mij * cos(3.0 * thetaij) - D4 * mij * mij * mij * mij * cos(4.0 * thetaij);
			term1 = 0.5L * log(1.85138 / mij);
			term2 = B1 * mij * cos(thetaij);
			term3 = -D2 * sqr(mij) * cos(2.0 * thetaij);
			term4 = B3 * mij * mij * mij * cos(3.0 * thetaij);
			term5 = -B4 * mij * mij * mij * mij * (log(exp(c4) / mij) * cos(4.0 * thetaij) + thetaij * sin(4.0 * thetaij));
			result.im = term1 + term2 + term3 + term4 + term5;
			result.im = result.im + 0.5 * log(Dij);  // correction term to work with DSS structure
			result = cmulreal(result, Fw * mu0 / DSSGlobals::PI);

 //  {****}         WriteDLLDebugFile(Format('Full: Z(%d,%d) = %.8g +j %.8g; Dij=%.8g, thetaij=%.8g, mij=%.8g, Terms= %.8g, %.8g, %.8g, %.8g, %.8g',[i,j, Result.re, result.im, Dij, thetaij*180.0/pi, mij, term1, term2, term3, term4, term5]));
		}
		break;
		case 	DERI:
		{
			if(i != j)
			{
				hterm = cadd(cmplx(Fyi + Fyj, 0.0), cmulreal(cinv(Fme), 2.0));
				xterm = cmplx(FX[i - 1] - FX[j - 1], 0.0);
				logArg = csqrt(cadd(cmul(hterm, hterm), cmul(xterm, xterm)));
				result = cmul(cmplx(0.0, Fw * mu0 / TwoPi), CLn(logArg));
			}
			else
			{
				hterm = cadd(cmplx(Fyi, 0.0), cinv(Fme));
				result = cmul(cmplx(0.0, Fw * mu0 / TwoPi), CLn(cmulreal(hterm, 2.0)));
			}
 // {****}          WriteDLLDebugFile(Format('Deri: Z(%d,%d) = %.8g +j %.8g; hterm= %.8g + j %.8g',[i,j, Result.re, result.im, hterm.re, hterm.im]));
		}
		break;
		default:
		  ;
		break;
	}
	return result;
}

//---------------------------------------------------------------------------------------------------------------

int TLineConstants::get_FNumConds()
{
	return FNumConds;
}

//---------------------------------------------------------------------------------------------------------------

int TLineConstants::get_FNumPhases()
{
	return FNumPhases;
}

//---------------------------------------------------------------------------------------------------------------

complex TLineConstants::Get_Zint(int i)
{
	complex result = {};
	complex Alpha = {};
	complex I0I1 = {};
	switch(ActiveEarthModel[ActiveActor])
	{
		case 	SIMPLECARSON:
		{
			result = cmplx(Frac[i - 1], Fw * mu0 / (8 * DSSGlobals::PI));
		}
		break;      // no skin effect
		case 	FULLCARSON:
		{
			result = cmplx(Frac[i - 1], Fw * mu0 / (8 * DSSGlobals::PI));
		}
		break;   // with skin effect model
		
        /*Assume round conductor*/
		case 	DERI:
		{
			Alpha = cmulreal(C1_j1, sqrt(FFrequency * mu0 / FRDC[i - 1]));
			if(cabs(Alpha) > 35.0)
				I0I1 = cONE;
			else
				I0I1 = cdiv(Bessel_I0(Alpha), Bessel_I1(Alpha));
			result = cmulreal(cmul(C1_j1, I0I1), sqrt(FRDC[i - 1] * FFrequency * mu0) / 2.0);
		}
		break;
		default:
		  ;
		break;
	}
	return result;
}

//--------------------------------------------------------------------------------------------------

double TLineConstants::get_FrhoEarth()
{
	return FrhoEarth;
}

//--------------------------------------------------------------------------------------------------

/*Makes a new Zmatrix and correct for lengths and units as it copies*/
/*Uses the reduced Zmatrix by default if it exists*/

TcMatrix* TLineConstants::Get_Zmatrix(double f, double loggth, int Units)
{
	TcMatrix* result = nullptr;
	int NewSize = 0;
	int i = 0;
	double UnitLengthConversion = 0.0;
	TcMatrix* Z = nullptr;
	pComplexArray Zvalues = nullptr;
	int stop = 0;
	if((f != FFrequency) || FRhoChanged)
		Calc(f);  // only recalcs if f changed or rho earth changed
	if(ASSIGNED(FZreduced))
	{
		Z = FZreduced;
	}
	else
	{
		Z = FZmatrix;
	}
	NewSize = Z->get_Norder();
	result = new TcMatrix(NewSize);
	result->CopyFrom(Z);  // gets ohms/meter
	Zvalues = result->GetValuesArrayPtr(NewSize);  // ptr to the values in the new copy
    /*Convert the values by units and length*/
	UnitLengthConversion = From_per_Meter(Units) * loggth;
	for(stop = NewSize * NewSize, i = 1; i <= stop; i++)
	{
		cmulrealaccum(Zvalues[i - 1], UnitLengthConversion);
	}

	return result;
}

void TLineConstants::Kron(int Norder)
{
	TcMatrix* Ztemp = nullptr;
	bool FirstTime = false;
	int i = 0;
	int j = 0;
	Ztemp = FZmatrix;
	FirstTime = true;
	if((FFrequency >= 0.0) && (Norder > 0) && (Norder < FNumConds))
	{
		int stop = 0;
		if(ASSIGNED(FZreduced))
			free( FZreduced );
		if(ASSIGNED(FYCreduced))
			free( FYCreduced );

     /*Reduce computed matrix one row/col at a time until it is norder*/
		while(Ztemp->get_Norder() > Norder)
		{
			FZreduced = Ztemp->Kron(Ztemp->get_Norder());    // Eliminate last row
			if(!FirstTime)   // don't throw away original matrix
			{
				free( Ztemp );  // Ztemp now points to intermediate matrix
			}
			Ztemp = FZreduced;
			FirstTime = false;
		}

    /*Extract norder x norder portion of Yc matrx*/
		FYCreduced = new TcMatrix(Norder);
		for(stop = Norder, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			for(stop1 = Norder, j = 1; j <= stop1; j++)
			{
				FYCreduced->SetElement(i, j, FYCmatrix->GetElement(i, j));
			}
		}

      /*Left with reduced matrix*/
	}
}

/*Performs a Kron reduction to get rid of neutral conductors*/

void TLineConstants::Reduce()
{
	Kron(FNumPhases);
}

void TLineConstants::Set_Capradius(int i, int Units, double Value)
{
	if((i > 0) && (i <= FNumConds))
	{
		Fcapradius[i - 1] = Value * To_Meters(Units);
	}
}

void TLineConstants::Set_Frequency(double Value)
{
	FFrequency = Value;
	Fw = TwoPi * FFrequency;
	Fme = csqrt(cmplx(0.0, Fw * mu0 / FrhoEarth));
}

void TLineConstants::Set_Frhoearth(double Value)
{
	if(Value != FrhoEarth)
		FRhoChanged = true;
	FrhoEarth = Value;
	if(FFrequency >= 0.0)
		Fme = csqrt(cmplx(0.0, Fw * mu0 / FrhoEarth));
}

void TLineConstants::Set_GMR(int i, int Units, double Value)
{
	if((i > 0) && (i <= FNumConds))
	{
		FGMR[i - 1] = Value * To_Meters(Units);
		if(Fradius[i - 1] < 0.0)
			Fradius[i - 1] = FGMR[i - 1] / 0.7788; // equivalent round conductor
	}
}

void TLineConstants::Set_NPhases(int Value)
{
	FNumPhases = Value;
}

void TLineConstants::Set_Rac(int i, int Units, double Value)
{
	if((i > 0) && (i <= FNumConds))
		Frac[i - 1] = Value * To_per_Meter(Units);
}

void TLineConstants::Set_radius(int i, int Units, double Value)
{
	if((i > 0) && (i <= FNumConds))
	{
		Fradius[i - 1] = Value * To_Meters(Units);
		if(FGMR[i - 1] < 0.0)
			FGMR[i - 1] = Fradius[i - 1] * 0.7788; // Default to round conductor
	}
}

void TLineConstants::Set_Rdc(int i, int Units, double Value)
{
	if((i > 0) && (i <= FNumConds))
		FRDC[i - 1] = Value * To_per_Meter(Units);
}

void TLineConstants::Set_X(int i, int Units, double Value)
{
	if((i > 0) && (i <= FNumConds))
		FX[i - 1] = Value * To_Meters(Units);
}

void TLineConstants::Set_Y(int i, int Units, double Value)
{
	if((i > 0) && (i <= FNumConds))
		FY[i - 1] = Value * To_Meters(Units);
}


void LineConstants_initialization()
{
	C1_j1 = cmplx(1.0, 1.0);
	B1 = 1.0 / (3.0L * sqrt(2.0L));
	B2 = 1.0 / 16.0;
	B3 = B1 / 3.0 / 5.0;
	B4 = B2 / 4.0 / 6.0;
	D2 = B2 * DSSGlobals::PI / 4.0;
	D4 = B4 * DSSGlobals::PI / 4.0;
	C2 = 1.3659315;
	c4 = C2 + 1.0 / 4.0 + 1.0 / 6.0;
}

		class 		LineConstants_unit
		{
		public:
		LineConstants_unit()
		{
			//AssertSystemInitialization();
			LineConstants_initialization();
		}
		};
		LineConstants_unit _LineConstants_unit;

}  // namespace LineConstants





