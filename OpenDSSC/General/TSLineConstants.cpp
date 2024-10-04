
#pragma hdrstop

#include "TSLineConstants.h"

#include "DSSGlobals.h"


using namespace std;


namespace TSLineConstants
{

TTSLineConstants::TTSLineConstants() {}


const double RhoTS = 2.3718e-8;  // for copper tape shield

double TTSLineConstants::Get_DiaShield(int i, int Units)
{
	double result = 0.0;
	result = FDiaShield[i - 1] * From_Meters(Units);
	return result;
}

double TTSLineConstants::Get_TapeLayer(int i, int Units)
{
	double result = 0.0;
	result = FTapeLayer[i - 1] * From_Meters(Units);
	return result;
}

double TTSLineConstants::Get_TapeLap(int i)
{
	double result = 0.0;
	result = FTapeLap[i - 1];
	return result;
}

void TTSLineConstants::Set_DiaShield(int i, int Units, double Value)
{
	if((i > 0) && (i <= FNumConds))
		FDiaShield[i - 1] = Value * To_Meters(Units);
}

void TTSLineConstants::Set_TapeLayer(int i, int Units, double Value)
{
	if((i > 0) && (i <= FNumConds))
		FTapeLayer[i - 1] = Value * To_Meters(Units);
}

void TTSLineConstants::Set_TapeLap(int i, double Value)
{
	if((i > 0) && (i <= FNumConds))
		FTapeLap[i - 1] = Value;
}
/*Compute base Z and YC matrices in ohms/m for this frequency and earth impedance*/

void TTSLineConstants::Calc(double f)
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
	double ResTS = 0.0;
	double GmrTS = 0.0;
	double Denom = 0.0;
	double RadIn = 0.0;
	double RadOut = 0.0;
	int stop = 0;
	Set_Frequency(f);  // this has side effects
	if(FZreduced != NULL)
	{
		ReducedSize = FZreduced->get_Norder();
		delete FZreduced;
	}
	else
	ReducedSize = 0;
	if(FYCreduced != NULL)
		delete FYCreduced;
	FZreduced = nullptr;
	FYCreduced = nullptr;
	FZmatrix->Clear();
	FYCmatrix->Clear();

  // add concentric neutrals to the end of conductor list; they are always reduced
	n = FNumConds + FNumPhases;
	Zmat = new TcMatrix(n);

  /*For less than 1 kHz use GMR to better match published data*/
	LFactor = cmplx(0.0, Fw * mu0 / TwoPi);
	if((f < 1000.0) && (f > 40.0))
		PowerFreq = true;
	else
		PowerFreq = false;

  // Self Impedances - TS cores and bare neutrals
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

  // TS self impedances
	for(stop = FNumPhases, i = 1; i <= stop; i++)
	{
		ResTS = 0.3183 * RhoTS / (FDiaShield[i - 1] * FTapeLayer[i - 1] * sqrt(50.0 / (100.0 - FTapeLap[i - 1])));
		GmrTS = 0.5 * (FDiaShield[i - 1] - FTapeLayer[i - 1]);  // per Kersting, to center of TS
		Zspacing = cmulreal(LFactor, log(1.0 / GmrTS));
		Zi = cmplx(ResTS, 0.0);
		idxi = i + FNumConds;
		Zmat->SetElement(idxi, idxi, cadd(Zi, cadd(Zspacing, Get_Ze(i, i))));
	}

  // Mutual Impedances - between TS cores and bare neutrals
	for(stop = FNumConds, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		for(stop1 = i - 1, j = 1; j <= stop1; j++)
		{
			Dij = sqrt(pow(FX[i - 1] - FX[j - 1], 2) + pow(FY[i - 1] - FY[j - 1], 2));
			Zmat->SetElemsym(i, j, cadd(cmulreal(LFactor, log(1.0 / Dij)), Get_Ze(i, j)));
		}
	}

  // Mutual Impedances - TS to other TS, cores, and bare neutrals
	for(stop = FNumPhases, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		idxi = i + FNumConds;
		for(stop1 = i - 1, j = 1; j <= stop1; j++)
		{  // TS to other TS
			idxj = j + FNumConds;
			Dij = sqrt(pow(FX[i - 1] - FX[j - 1], 2) + pow(FY[i - 1] - FY[j - 1], 2));
			Zmat->SetElemsym(idxi, idxj, cadd(cmulreal(LFactor, log(1.0 / Dij)), Get_Ze(i, j)));
		}
		for(stop1 = FNumConds, j = 1; j <= stop1; j++)
		{ // CN to cores and bare neutrals
			idxj = j;
			GmrTS = 0.5 * (FDiaShield[i - 1] - FTapeLayer[i - 1]);  // per Kersting, to center of TS
			if(i == j) // TS to its own phase core
			{
				Dij = GmrTS;
			}
			else
 // TS to another phase or bare neutral
			{
				Dij = sqrt(pow(FX[i - 1] - FX[j - 1], 2) + pow(FY[i - 1] - FY[j - 1], 2));
			}
			Zmat->SetElemsym(idxi, idxj, cadd(cmulreal(LFactor, log(1.0 / Dij)), Get_Ze(i, j)));
		}
	}

  // reduce out the tape shields
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
		Yfactor = TwoPi * E0 * FEpsR[i - 1] * Fw; // includes frequency so C==>Y
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

TTSLineConstants::TTSLineConstants(int NumConductors)
 : inherited(NumConductors)
{
	FDiaShield	= new double[FNumConds];
	FTapeLayer	= new double[FNumConds];
	FTapeLap	= new double[FNumConds];
}

TTSLineConstants::~TTSLineConstants()
{
	free(FDiaShield);
	free(FTapeLayer);
	free(FTapeLap);
	// inherited;
}





}  // namespace TSLineConstants





