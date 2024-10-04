
#pragma hdrstop

#include <cmath>
#include "CableConstants.h"


using namespace std;
using namespace Arraydef;
using namespace LineConstants;
using namespace LineUnits;
using namespace System;
using namespace Ucmatrix;

namespace CableConstants
{

TCableConstants::TCableConstants() {}



void TCableConstants::Kron(int Norder)
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
			delete FZreduced;
		if(ASSIGNED(FYCreduced))
			delete FYCreduced;
		while(Ztemp->get_Norder() > Norder)
		{
			FZreduced = Ztemp->Kron(Ztemp->get_Norder());    // Eliminate last row
			if(!FirstTime)
				delete Ztemp;  // Ztemp points to intermediate matrix
			Ztemp = FZreduced;
			FirstTime = false;
		}
    // now copy part of FYCmatrix to FYCreduced
		FYCreduced = new TcMatrix(Norder);
		for(stop = Norder, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			for(stop1 = Norder, j = 1; j <= stop1; j++)
			{
				FYCreduced->SetElement(i, j, FYCmatrix->GetElement(i, j));
			}
		}
	}
}

bool TCableConstants::ConductorsInSameSpace(String& ErrorMessage)
{
	bool result = false;
	int i = 0;
	int j = 0;
	double Dij = 0.0;
	double Ri = 0.0;
	double Rj = 0.0;
	int stop = 0;
	result = false;

/*   Height of cable doesn't matter
  Removed 5-25-2016 RcD
  For i := 1 to FNumConds do Begin
    if (FY^[i] >= 0.0) then Begin
      Result := TRUE;
      ErrorMessage :=
        Format('Cable %d height must be < 0. ', [ i ]);
      Exit
    End;
  End;
*/
	for(stop = FNumConds, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		if(i <= FNumPhases)
			Ri = Fradius[i - 1];
		else
			Ri = 0.5 * FDiaCable[i - 1];
		for(stop1 = FNumConds, j = i + 1; j <= stop1; j++)
		{
			if(j <= FNumPhases)
				Rj = Fradius[j - 1];
			else
				Rj = 0.5 * FDiaCable[j - 1];
			Dij = sqrt(sqr(FX[i - 1] - FX[j - 1]) + sqr(FY[i - 1] - FY[j - 1]));
			if(Dij < (Ri + Rj))
			{
				result = true;
				ErrorMessage = Format("Cable conductors %d and %d occupy the same space.", i, j);
				return result;
			}
		}
	}
	return result;
}

double TCableConstants::Get_EpsR(int i)
{
	double result = 0.0;
	result = FEpsR[i - 1];
	return result;
}

double TCableConstants::Get_InsLayer(int i, int Units)
{
	double result = 0.0;
	result = FInsLayer[i - 1] * From_Meters(Units);
	return result;
}

double TCableConstants::Get_DiaIns(int i, int Units)
{
	double result = 0.0;
	result = FDiaIns[i - 1] * From_Meters(Units);
	return result;
}

double TCableConstants::Get_DiaCable(int i, int Units)
{
	double result = 0.0;
	result = FDiaCable[i - 1] * From_Meters(Units);
	return result;
}

void TCableConstants::Set_EpsR(int i, double Value)
{
	if((i > 0) && (i <= FNumConds))
		FEpsR[i - 1] = Value;
}

void TCableConstants::Set_InsLayer(int i, int Units, double Value)
{
	if((i > 0) && (i <= FNumConds))
		FInsLayer[i - 1] = Value * To_Meters(Units);
}

void TCableConstants::Set_DiaIns(int i, int Units, double Value)
{
	if((i > 0) && (i <= FNumConds))
		FDiaIns[i - 1] = Value * To_Meters(Units);
}

void TCableConstants::Set_DiaCable(int i, int Units, double Value)
{
	if((i > 0) && (i <= FNumConds))
		FDiaCable[i - 1] = Value * To_Meters(Units);
}

TCableConstants::TCableConstants(int NumConductors)
 : inherited(NumConductors)
{
	FEpsR		= new double[FNumConds];
	FInsLayer	= new double[FNumConds];
	FDiaIns		= new double[FNumConds];
	FDiaCable	= new double[FNumConds];

	for (int i = 0; i < FNumConds; ++i)
	{
		FEpsR[i] = 0;
		FInsLayer[i] = 0;
		FDiaIns[i] = 0;
		FDiaCable[i] = 0;
	}
}

TCableConstants::~TCableConstants()
{
	delete[] FEpsR;
	delete[] FInsLayer;
	delete[] FDiaIns;
	delete[] FDiaCable;
	// inherited;
}





}  // namespace CableConstants





