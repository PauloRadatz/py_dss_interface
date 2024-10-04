
#pragma hdrstop

#include "CNLineConstants.h"
#include <math.h>
#include "Utilities.h"

using namespace std;
using namespace Arraydef;
using namespace CableConstants;
using namespace LineConstants;
using namespace LineUnits;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace Utilities;

namespace CNLineConstants
{

TCNLineConstants::TCNLineConstants() {}



int TCNLineConstants::Get_kStrand(int i)
{
	int result = 0;
	result = FkStrand[i - 1];
	return result;
}

double TCNLineConstants::Get_DiaStrand(int i, int Units)
{
	double result = 0.0;
	result = FDiaStrand[i - 1] * From_Meters(Units);
	return result;
}

double TCNLineConstants::Get_GmrStrand(int i, int Units)
{
	double result = 0.0;
	result = FGmrStrand[i - 1] * From_Meters(Units);
	return result;
}

double TCNLineConstants::Get_RStrand(int i, int Units)
{
	double result = 0.0;
	result = FRStrand[i - 1] * From_per_Meter(Units);
	return result;
}

void TCNLineConstants::Set_kStrand(int i, int Value)
{
	if((i > 0) && (i <= FNumConds))
		FkStrand[i - 1] = Value;
}

void TCNLineConstants::Set_DiaStrand(int i, int Units, double Value)
{
	if((i > 0) && (i <= FNumConds))
		FDiaStrand[i - 1] = Value * To_Meters(Units);
}

void TCNLineConstants::Set_GmrStrand(int i, int Units, double Value)
{
	if((i > 0) && (i <= FNumConds))
		FGmrStrand[i - 1] = Value * To_Meters(Units);
}

void TCNLineConstants::Set_RStrand(int i, int Units, double Value)
{
	if((i > 0) && (i <= FNumConds))
		FRStrand[i - 1] = Value * To_per_Meter(Units);
}
/*Compute base Z and YC matrices in ohms/m for this frequency and earth impedance*/

void TCNLineConstants::Calc(double f)
{
	complex Zi = {};
	complex Zspacing = {};
	bool PowerFreq = false;
	complex LFactor = {};
	int i = 0;
	int j = 0;
	double Dij = 0.0;
	double Yfactor = 0.0;
	int ReducedSize = 0;
	int n = 0;
	int idxi = 0;
	int idxj = 0;
	TcMatrix* Zmat = nullptr;
	TcMatrix* Ztemp = nullptr;
	double ResCN = 0.0;
	double RadCN = 0.0;
	double GmrCN = 0.0;
	double Denom = 0.0;
	double RadIn = 0.0;
	double RadOut = 0.0;
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

  // add concentric neutrals to the end of conductor list; they are always reduced
	n = FNumConds + FNumPhases;
	Zmat = new TcMatrix(n);

  /*For less than 1 kHz use GMR to better match published data*/
	LFactor = cmplx(0.0, Fw * mu0 / DSSGlobals::TwoPi);
	if((f < 1000.0) && (f > 40.0))
		PowerFreq = true;
	else
		PowerFreq = false;

  // Self Impedances - CN cores and bare neutrals
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
		Zmat->SetElement(i, i, cadd(Zi, cadd(Zspacing, Get_Ze(i, i))));
	}

  // CN self impedances
	for(stop = FNumPhases, i = 1; i <= stop; i++)
	{
		ResCN = FRStrand[i - 1] / FkStrand[i - 1];
		RadCN = 0.5 * (FDiaCable[i - 1] - FDiaStrand[i - 1]);
		GmrCN = pow(FGmrStrand[i - 1] * FkStrand[i - 1] * pow(RadCN, FkStrand[i - 1] - 1.0), 1.0 / FkStrand[i - 1]);
		Zspacing = cmulreal(LFactor, log(1.0 / GmrCN));
		Zi = cmplx(ResCN, 0.0);
		idxi = i + FNumConds;
		Zmat->SetElement(idxi, idxi, cadd(Zi, cadd(Zspacing, Get_Ze(i, i))));
	}

  // Mutual Impedances - between CN cores and bare neutrals
	for(stop = FNumConds, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		for(stop1 = i - 1, j = 1; j <= stop1; j++)
		{
			Dij = sqrt(sqr(FX[i - 1] - FX[j - 1]) + sqr(FY[i - 1] - FY[j - 1]));
			Zmat->SetElemsym(i, j, cadd(cmulreal(LFactor, log(1.0 / Dij)), Get_Ze(i, j)));
		}
	}

  // Mutual Impedances - CN to other CN, cores, and bare neutrals
	for(stop = FNumPhases, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		idxi = i + FNumConds;
		for(stop1 = i - 1, j = 1; j <= stop1; j++)
		{  // CN to other CN
			idxj = j + FNumConds;
			Dij = sqrt(sqr(FX[i - 1] - FX[j - 1]) + sqr(FY[i - 1] - FY[j - 1]));
			Zmat->SetElemsym(idxi, idxj, cadd(cmulreal(LFactor, log(1.0 / Dij)), Get_Ze(i, j)));
		}
		for(stop1 = FNumConds, j = 1; j <= stop1; j++)
		{ // CN to cores and bare neutrals
			idxj = j;
			RadCN = 0.5 * (FDiaCable[i - 1] - FDiaStrand[i - 1]);
			if(i == j) // CN to its own phase core
			{
				Dij = RadCN;
			}
			else
 // CN to another phase or bare neutral
			{
				Dij = sqrt(sqr(FX[i - 1] - FX[j - 1]) + sqr(FY[i - 1] - FY[j - 1]));
				Dij = pow(pow(Dij, (double) FkStrand[i - 1]) - pow(RadCN, (double) FkStrand[i - 1]), 1.0 / FkStrand[i - 1]);
			}
			Zmat->SetElemsym(idxi, idxj, cadd(cmulreal(LFactor, log(1.0 / Dij)), Get_Ze(i, j)));
		}
	}  

  // reduce out the CN
	while(Zmat->get_Norder() > FNumConds)
	{
		Ztemp = Zmat->Kron(Zmat->get_Norder());
		delete Zmat;
		Zmat = Ztemp;
	}
	FZmatrix->CopyFrom(Zmat);
	delete Zmat;  

  // for shielded cables, build the capacitance matrix directly
  // assumes the insulation may lie between semicon layers
	for(stop = FNumPhases, i = 1; i <= stop; i++)
	{
		Yfactor = DSSGlobals::TwoPi * E0 * FEpsR[i - 1] * Fw; // includes frequency so C==>Y
		RadOut = 0.5 * FDiaIns[i - 1];
		RadIn = RadOut - FInsLayer[i - 1];
		Denom = log(RadOut / RadIn);
		FYCmatrix->SetElement(i, i, cmplx(0.0, Yfactor / Denom));
	}
	if(ReducedSize > 0)
		Kron(ReducedSize);  // Was reduced so reduce again to same size
		
  /*Else the Zmatrix is OK as last computed*/
	FRhoChanged = false;
}

TCNLineConstants::TCNLineConstants(int NumConductors)
 : inherited(NumConductors)
{
	FkStrand	= new longInt[FNumConds];
	FDiaStrand	= new double[FNumConds];
	FGmrStrand	= new double[FNumConds];
	FRStrand	= new double[FNumConds];
}

TCNLineConstants::~TCNLineConstants()
{
	free(FkStrand);
	free(FDiaStrand);
	free(FGmrStrand);
	free(FRStrand);
	// inherited;
}





}  // namespace CNLineConstants





