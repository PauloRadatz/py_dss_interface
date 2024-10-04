#pragma hdrstop

#include "Pstcalc.h"

#include "DSSGlobals.h"
#include <string>
#include <algorithm>

using namespace std;


namespace Pstcalc
{


const int MAXBINS = 50000;
typedef vector < double > BinArray;
typedef BinArray* pBinArray;
typedef double Double6Array[6/*# range 0..5*/];
double rms_reference = 0.0;	// internal rms reference value (do not change)
double FBase = 0.0;			//not needed for AC signal input
double Tstep = 0.0;		//internal timestep, may or may not equal DeltaT
double Pst_Time = 0.0;
double Pst_Timer = 0.0;
double Pst_Time_Max = 0.0;
double rms_input = 0.0;	// nominal line-to-neutral rms input
double RMS_sample = 0.0;
double DeltaT = 0.0;
int NumPstIntervals = 0;
Double6Array vin;
Double6Array X1;
Double6Array X2;
Double6Array X3;
Double6Array X4;
Double6Array X5;
Double6Array X6;
Double6Array X7;
Double6Array X8;
Double6Array X9;
Double6Array X10;
Double6Array RMSVin;
pBinArray Bins0;
pBinArray Bins1;
double bin_ceiling = 0.0;
int number_bins = 0;

    /*Filter Coefficients*/
double WA2 = 0.0;
double WB2 = 0.0;
double WC2 = 0.0;
double WD2 = 0.0;
double WE2 = 0.0;
double WF2 = 0.0;
double WG2 = 0.0;  // weighting filter coefficients
 // time constant of sliding mean filter
double IVAA = 0.0;
double IVAB = 0.0;
double IVAC = 0.0;
double IVAD = 0.0;
double IVAE = 0.0;
double BA = 0.0;
double BB = 0.0;
double BC = 0.0;
double BD = 0.0;
double BE = 0.0;
double BG = 0.0;
double BH = 0.0;
double BI = 0.0;
double BJ = 0.0;
double Bk = 0.0;
double bl = 0.0;
double BM = 0.0;
double BN = 0.0;
double BP = 0.0;
double SA = 0.0;
double internal_reference = 0.0;
int lamp_type = 0;  // 0 for 120V filters, 1 for 230V filters
int input_type = 0;  // 0 for AC, 1 for 1-cycle RMS, 6 for 6-cycle rms

///////////////////////////////////////////////////////////////////////////////
// searches through the specified array for a bin and then
// interpolates (if needed)
///////////////////////////////////////////////////////////////////////////////

double SB(double Y, pBinArray bins)
{
	double result = 0.0;
	int n = 0;
	bool Found = false;
	Found = false;
	n = 0;
	while(((!Found) && (n < number_bins)))
		if(Y <= (*bins)[n])
			Found = true;
		else
			n = n + 1;
	if(n > 0)
             // Interpolate
	{
		result = bin_ceiling * (n - 1) / number_bins + (Y - (*bins)[n]) * (bin_ceiling / number_bins) / ((*bins)[n] - (*bins)[n]);
	}
	else
	result = 0.0;
	return result;
}

void ZeroOutBins()
{
	int n = 0;
	int stop = 0;
	for(stop = number_bins - 1, n = 0; n <= stop; n++)
	{
		(*Bins0)[n] = 0.0;
	}
	for(stop = number_bins - 1, n = 0; n <= stop; n++)
	{
		(*Bins1)[n] = 0.0;
	}
}

///////////////////////////////////////////////////////////////////////////////
// Calculates the Pst
///////////////////////////////////////////////////////////////////////////////

double CalcPst()
{
	double result = 0.0;
	double num_pts = 0.0;  // ?? long double Why??
	int n = 0;
	double P01 = 0.0;
	double P1s = 0.0;
	double P3s = 0.0;
	double P10s = 0.0;
	double P50s = 0.0;
	int stop = 0;
	num_pts = 0;
	for(stop = number_bins - 1, n = 0; n <= stop; n++)
	{
		num_pts = num_pts + (*Bins0)[n];
		(*Bins1)[n] = num_pts;
	}
	for(stop = number_bins - 1, n = 0; n <= stop; n++)
	{
		(*Bins1)[n] = (*Bins1)[n] / num_pts;
	}
	P01 = SB(0.999, Bins1);
	P1s = (SB(0.993, Bins1) + SB(0.990, Bins1) + SB(0.985, Bins1)) / 3.0;
	P3s = (SB(0.978, Bins1) + SB(0.970, Bins1) + SB(0.960, Bins1)) / 3.0;
	P10s = (SB(0.940, Bins1) + SB(0.920, Bins1) + SB(0.900, Bins1) + SB(0.870, Bins1) + SB(0.830, Bins1)) / 5.0;
	P50s = (SB(0.700, Bins1) + SB(0.500, Bins1) + SB(0.200, Bins1)) / 3.0;

 // This is the Pst
	result = sqrt(0.0314 * P01 + 0.0525 * P1s + 0.0657 * P3s + 0.28 * P10s + 0.08 * P50s);
	return result;
}

//////////////////////////////////////////////////////////////////////
// Calculates the coefficients for the weighting filter
//////////////////////////////////////////////////////////////////////

void Set_Filter_Coefficients(int input_type)
{
	double k = 0.0;
	double Lambda = 0.0;
	double W1 = 0.0;
	double W2 = 0.0;
	double W3 = 0.0;
	double W4 = 0.0;

	// Coefficients for Input Voltage Adapter
	// L = 8.93125 H
	// C = 35.725 F
	// R = 1.0 Ohms
	IVAA = 8.93125 * 35.725;
	IVAB = 35.725;
	IVAC = 4.0 * IVAA / (Tstep * Tstep) + 1.0 - 2.0 * IVAB / Tstep;
	IVAD = 2.0 - 8.0 * IVAA / (Tstep * Tstep);
	IVAE = 4.0 * IVAA / (Tstep * Tstep) + 1.0 + 2.0 * IVAB / Tstep;

	 
	// Bandpass centered at 8.5Hz
	// 120V lamp       
	if(lamp_type == 0)
	{
		k = 1.6357;
		Lambda = 26.1843893695;
		W1 = 57.0335348916;
		W2 = 18.4719490509;
		W3 = 8.76170084893;
		W4 = 108.794107576;
	}
	else
    // Bandpass centered at 8.8Hz
            // 230V lamp
	{
		k = 1.74802;
		Lambda = 25.5085385419;
		W1 = 57.5221844961;
		W2 = 14.3243430315;
		W3 = 7.69910111615;
		W4 = 137.601758227;
	}

	// Coefficients for Bandpass
	// 1st set of substitutions
	BA = 0.314159265359;
	BB = 113.834561498;
	BC = 48361.06156533785;
	BD = 311.00180567;
	BE = 424.836367168;
    // 2nd set of substitutions
	BG = 1 + BA * Tstep / 2.0;
	BH = BA * Tstep / 2.0 - 1.0;
	BI = 4.0 / (Tstep * Tstep) + 2.0 * BB / Tstep + BC;
	BJ = -8.0 / (Tstep * Tstep) + 2.0 * BC;
	Bk = 4.0 / (Tstep * Tstep) - 2.0 * BB / Tstep + BC;
	bl = 4.0 / (Tstep * Tstep) + 2.0 * BD / Tstep + BC;
	BM = 4.0 / (Tstep * Tstep) - 2.0 * BD / Tstep + BC;
	BN = 4.0 / (Tstep * Tstep) + 2.0 * BE / Tstep + BC;
	BP = 4.0 / (Tstep * Tstep) - 2.0 * BE / Tstep + BC;

	// Coefficients for Weighting filter
	WA2 = 4.0 * k * W1 * W3 * W4 / (Tstep * Tstep);
	WB2 = 2.0 * k * W1 * W2 * W3 * W4 / Tstep;
	WC2 = 16.0 * W2 / pow(Tstep, 4);
	WD2 = 8.0 * W2 * (2.0 * Lambda + W3 + W4) / pow(Tstep, 3);
	WE2 = 4.0 * W2 * (W3 * W4 + W1 * W1 + 2.0 * Lambda * (W3 + W4)) / (Tstep * Tstep);
	WF2 = 2.0 * W2 * (2.0 * Lambda * W3 * W4 + W1 * W1 * (W3 + W4)) / Tstep;
	WG2 = W2 * W3 * W4 * W1 * W1;

	// time constant of sliding mean filter
	SA = 0.3;
	
	// internal reference
	if(input_type == 0)
		internal_reference = 676.372;  // See "new 868 testing and scaling.xls" for derivation
	if(input_type == 1)
		internal_reference = 0.01106784;	// new scaling factor 7/25/05, based on 1-cycle RMS
	// using greater than 1-cycle RMS may result in errors
	if(input_type == 3)
		internal_reference = 0.009;	    // new scaling factor 8/3/05, based on 3-cycle RMS
	if(input_type == 6)
		internal_reference = 0.008449;	// new scaling factor 7/25/05, based on 6-cycle RMS
}

//////////////////////////////////////////////////////////////////////
// Put samples that get through the filter in the proper bins
//////////////////////////////////////////////////////////////////////

void Gather_Bins(double X10_value, pBinArray bins)
{

	auto My_inc = [&](double& X) -> void 
	{
		X = X + 1.0;
	};  // special incrementer routine
	if(X10_value > bin_ceiling)   // increment count
		My_inc((*bins)[number_bins]);
	else
		My_inc((*bins)[Trunc(number_bins * X10_value / bin_ceiling)]);
}

/*Find out which bin the value belongs in and increment it.*/

///////////////////////////////////////////////////////////////////////////////
// shifts every array value up (back in time)
///////////////////////////////////////////////////////////////////////////////

void Sample_Shift()
{
	int n = 0;
	int stop = 0;
	for(stop = 1, n = 5; n >= stop; n--)
	{
		vin[n] = vin[n];
		RMSVin[n] = RMSVin[n];
		X1[n] = X1[n];
		X2[n] = X2[n];
		X3[n] = X3[n];
		X4[n] = X4[n];
		X5[n] = X5[n];
		X6[n] = X6[n];
		X7[n] = X7[n];
		X8[n] = X8[n];
		X9[n] = X9[n];
		X10[n] = X10[n];
	}
}

///////////////////////////////////////////////////////////////////////////////
// Main Flicker Calculation Function
///////////////////////////////////////////////////////////////////////////////

void Get_Pinst()
{


  /*RMS input*/
	RMSVin[0] = rms_reference * RMS_sample / rms_input; // per unitize rms value
	X1[0] = (RMSVin[0] + 2.0 * RMSVin[1] + RMSVin[2] - IVAD * X1[1] - IVAC * X1[2]) / IVAE;
	X3[0] = RMSVin[0] * (1.0 - (X1[0] - 120.0) / RMSVin[0]);

    // Bandpass (HP at .05Hz and 6th order Butteworth LP at 35Hz)
	X4[0] = (X3[0] - X3[1] - BH * X4[1]) / BG;
	X5[0] = (BC * (X4[0] + 2 * X4[1] + X4[2]) - (BJ * X5[1] + Bk * X5[2])) / BI;
	X6[0] = (BC * (X5[0] + 2 * X5[1] + X5[2]) - (BJ * X6[1] + BM * X6[2])) / bl;
	X7[0] = (BC * (X6[0] + 2 * X6[1] + X6[2]) - (BJ * X7[1] + BP * X7[2])) / BN;

    // Weighting filter
	X8[0] = ((WA2 + WB2) * X7[0] + 2 * WB2 * X7[1] - 2 * WA2 * X7[2] - 2 * WB2 * X7[3] + (WA2 - WB2) * X7[4] - (2 * WF2 + 4 * WG2 - 4 * WC2 - 2 * WD2) * X8[1] - (6 * WC2 - 2 * WE2 + 6 * WG2) * X8[2] - (2 * WD2 + 4 * WG2 - 4 * WC2 - 2 * WF2) * X8[3] - (WC2 - WD2 + WE2 - WF2 + WG2) * X8[4]) / (WC2 + WD2 + WE2 + WF2 + WG2);

    // Sliding Mean filter
	X9[0] = (X8[0] * X8[0] + X8[1] * X8[1] - (1 - 2 * SA / Tstep) * X9[1]) / (1 + 2 * SA / Tstep);
	X10[0] = X9[0] / internal_reference;
}

//*******************************************************************
//*******************************************************************

void Init6Array(Double6Array* Y, double V1, double V2, double V3, double V4, double V5, double V6)
{
	*Y[0] = V1;
	*Y[1] = V2;
	*Y[2] = V3;
	*Y[3] = V4;
	*Y[4] = V5;
	*Y[5] = V6;
}

//*******************************************************************
//*******************************************************************

int _Pst(pDoubleArray PstResult, pDoubleArray VArray, int npts)
{
	int result = 0;
	int PstInterval = 0;
	double max_flicker = 0.0;
	double Time = 0.0;   // long double???
	int VIndex = 0;
	int ipst = 0;
	double FirstSample = 0.0;
	double PST_STart_Time = 0.0;
	int SynthesizedSamples = 0;
	double SamplesPerDeltaT = 0.0;	// this value is used when RMS data is used as input


// DEBUG file
	int stop = 0;
	rms_reference = 120.0;	// internal rms reference value (do not change)
	Init6Array(&vin, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
	Init6Array(&RMSVin, rms_reference, rms_reference, rms_reference, rms_reference, rms_reference, rms_reference);	// RMS input voltage
	Init6Array(&X1, rms_reference, rms_reference, rms_reference, rms_reference, rms_reference, rms_reference);
	Init6Array(&X2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);		// Output of Block 1
	Init6Array(&X3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);		// Output of Block 2
	Init6Array(&X4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
	Init6Array(&X5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
	Init6Array(&X6, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
	Init6Array(&X7, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
	Init6Array(&X8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);		// Output of Block 3
	Init6Array(&X9, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
	Init6Array(&X10, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);	  // Output of Block 4
	bin_ceiling = 350.0;	// previously used 215, increased to be encompass high flicker levels
	number_bins = 16000;

  /*Allocate Memory for bins*/
	Bins0->resize(number_bins); // 50k is max. # of bins
	Bins1->resize(number_bins); // 50k is max. # of bins
	Time = 0.0;		// time clock
	Pst_Timer = 0.0;
	ZeroOutBins();
	Tstep = 1.0 / (16.0 * FBase);	// time step for each cycle, fixed to 16 samples/cycle
	Pst_Time_Max = npts * DeltaT; // - 6.0 ;  //use the entire data set sent to calculate the flicker
	Pst_Time = min(600.0, Pst_Time_Max);
	NumPstIntervals = max( (int64_t) 1 , Trunc(Pst_Time_Max / Pst_Time)); // At least one interval
	if(PstResult != NULL)
		PstResult = (pDoubleArray) realloc(PstResult, 0);
	PstResult = (pDoubleArray) realloc(PstResult,sizeof(double) * NumPstIntervals);  // allocate result array
	Set_Filter_Coefficients(input_type);

	// //*********************************************************************************
	// Main RMS routine
	// ///*********************************************************************************
	SamplesPerDeltaT = DeltaT / Tstep;
	max_flicker = 0.0;
	FirstSample = VArray[1];
	rms_input = FirstSample;
	RMS_sample = FirstSample;

    // inits filter to 1 PU for 30 s
	while((Time < 30.0))
	{
		Time = Time + Tstep;
		Get_Pinst(); // Computes what get's through filter (X10 )
		Sample_Shift();
	}
	PST_STart_Time = Time + 5.0;  // Give it 5 s to settle down after real data starts
	VIndex = 1;
	PstInterval = 0;
	for(stop = npts, ipst = 1; ipst <= stop; ipst++)
	{
		int stop1 = 0;
		RMS_sample = VArray[VIndex];
        // The following loop holds the rms input samples constant over the RMS period
		for(stop1 = Round(SamplesPerDeltaT), SynthesizedSamples = 1; SynthesizedSamples <= stop1; SynthesizedSamples++)
		{
			Get_Pinst();    // Computes what gets through filter (X10[0] )

            /*////////////// This starts the Pst calculations //////////////*/
			if(Time >= PST_STart_Time)
			{
				Pst_Timer = Pst_Timer + Tstep;
				max_flicker = max(max_flicker, X10[0]);
				Gather_Bins(X10[0], Bins0);
				if(Pst_Timer >= Pst_Time)
                // OK, we got everything in the bins, let's compute Pst
				{
					++PstInterval;
					if(PstInterval <= NumPstIntervals)
						PstResult[PstInterval] = CalcPst();
					Pst_Timer = 0.0;
					ZeroOutBins();   // Zero Bins0 and Bins1 out for next time
				}
			}
			Sample_Shift();
			Time = Time + Tstep;
		}
		++VIndex;
	}
	result = PstInterval;   // should be NumPstIntervals
	Bins0->clear();
	Bins1->clear();
	return result;
}

// Function call for executing PST calculator using RMS data

int PstRMS(pDoubleArray PstResult, pDoubleArray pVoltages, double Freqbase, int NcyclesperSample, int npts, int Lamp)
{
	int result = 0;
	FBase = Freqbase;

	// lamp_type  := 0;			// 0 for 120V filters, 1 for 230V filters
	input_type = 6;			// 0 for AC, 1 for 1-cycle RMS, 6 for 6-cycle rms

	//Check for the lamp type (120 or 230), default to 120 if not read properly
	if(Lamp == 230)
	{
		lamp_type = 1;
	}
	else
	{
		lamp_type = 0;
	}
	DeltaT = double(NcyclesperSample) / FBase;
	result = _Pst(PstResult, pVoltages, npts);
	return result;
}
      // returns number of Pst elements computed
      // will automatically clean up PstStruct when it is reallocated; Init to nil


/////////////////////////////////////////////////////////
//
//  RMS flickermeter implementation
//
/////////////////////////////////////////////////////////

void Fhp(int n, float TS, float whp, pSingleArray X, pSingleArray Y)
{
	float A = 0.0F;
	float A0 = 0.0F;
	float A1 = 0.0F;
	int j = 0;
	int stop = 0;
	Y[1] = 0.0F;
	A = (float) (0.5F * TS * whp);
	A0 = (float) (A + 1.0F);
	A1 = (float) (A - 1.0F);
	for(stop = n, j = 2; j <= stop; j++)
	{
		Y[j] = (float) ((1.0F / A0) * (X[j] - X[j - 1] - A1 * Y[j - 1]));
	}
}

void Flp(int n, float TS, float tau, pSingleArray X, pSingleArray Y)
{
	float A0 = 0.0F;
	float A1 = 0.0F;
	int j = 0;
	int stop = 0;
	Y[1] = 0.0F;
	A0 = (float) (1 + 2 * tau / TS);
	A1 = (float) (1 - 2 * tau / TS);
	for(stop = n, j = 2; j <= stop; j++)
	{
		Y[j] = (float) ((1.0F / A0) * (X[j] + X[j - 1] - A1 * Y[j - 1]));
	}
}

void Fw1(int n, float TS, float W1, float k, float lam, pSingleArray X, pSingleArray Y)
{
	float A0 = 0.0F;
	float A1 = 0.0F;
	float A2 = 0.0F;
	float B0 = 0.0F;
	float B2 = 0.0F;
	int j = 0;
	int stop = 0;
	Y[1] = 0.0F;
	Y[2] = 0.0F;
	B0 = (float) (2.0F * k * W1 * TS);
	B2 = (float) (-2.0F * k * W1 * TS);
	A0 = (float) (W1 * W1 * TS * TS + 4.0F * lam * TS + 4.0F);
	A1 = (float) (2.0F * W1 * W1 * TS * TS - 8.0F);
	A2 = (float) (W1 * W1 * TS * TS - 4.0F * lam * TS + 4.0F);
	for(stop = n, j = 3; j <= stop; j++)
	{
		Y[j] = (float) ((1.0F / A0) * (B0 * X[j] + B2 * X[j - 2] - A1 * Y[j - 1] - A2 * Y[j - 2]));
	}
}

void Fw2(int n, float TS, float W2, float W3, float W4, pSingleArray X, pSingleArray Y)
{
	float A0 = 0.0F;
	float A1 = 0.0F;
	float A2 = 0.0F;
	float B0 = 0.0F;
	float B1 = 0.0F;
	float B2 = 0.0F;
	int j = 0;
	int stop = 0;
	Y[1] = 0.0F;
	Y[2] = 0.0F;
	B0 = (float) (W3 * W4 * (TS * TS * W2 + 2.0F * TS));
	B1 = (float) (W3 * W4 * 2.0F * TS * TS * W2);
	B2 = (float) (W3 * W4 * (TS * TS * W2 - 2.0F * TS));
	A0 = (float) (W2 * (TS * TS * W3 * W4 + 2.0F * TS * (W3 + W4) + 4));
	A1 = (float) (W2 * (2.0F * TS * TS * W3 * W4 - 8.0F));
	A2 = (float) (W2 * (TS * TS * W3 * W4 - 2.0F * TS * (W3 + W4) + 4));
	for(stop = n, j = 3; j <= stop; j++)
	{
		Y[j] = (float) ((1.0F / A0) * (B0 * X[j] + B1 * X[j - 1] + B2 * X[j - 2] - A1 * Y[j - 1] - A2 * Y[j - 2]));
	}
}

void QuickSort(vector<float> List, int iLo, int iHi)
{
	int Lo = 0;
	int Hi = 0;
	float T = 0.0F;
	float mid = 0.0F;
	Lo = iLo;
	Hi = iHi;
	mid = List[floor((Lo + Hi) / 2)];
	do
	{
		while(List[Lo] < mid)
			++Lo;
		while(List[Hi] > mid)
			--Hi;
		if(Lo <= Hi)
		{
			T = List[Lo];
			List[Lo] = List[Hi];
			List[Hi] = T;
			++Lo;
			--Hi;
		}
	}
	while(!(Lo > Hi));
	if(Hi > iLo)
		QuickSort(List, iLo, Hi);
	if(Lo < iHi)
		QuickSort(List, Lo, iHi);
}

float Percentile(vector<float> List, int iLo, int iHi, float pctExceeded)
{
	float result = 0.0F;
	int nlo = 0;
	int nhi = 0;
	float xhst = 0.0F;
	float xlo = 0.0F;
	float xhi = 0.0F;
	float xfrac = 0.0F;
	float pct = 0.0F;
	pct = (float) (100.0F - pctExceeded);
	xhst = (float) (iHi - iLo + 1);
	xfrac = (float) Frac(0.01F * pct * xhst);
	nlo = Trunc(0.01F * pct * xhst);
	nhi = nlo + 1;
	xlo = List[nlo];
	xhi = List[nhi];
	result = (float) (xlo + xfrac * (xhi - xlo));
	return result;
}

void FlickerMeter(int n, double FBase, double VBase, pSingleArray Pt, pSingleArray pRms, pSingleArray pPst)
{
	int i = 0;
	int ipst = 0;
	int ihst = 0;
	float T = 0.0F;
	float tpst = 0.0F;
  // filter coefficients
	float whp = 0.0F;
	float W1 = 0.0F;
	float W2 = 0.0F;
	float W3 = 0.0F;
	float W4 = 0.0F;
	float k = 0.0F;
	float lam = 0.0F;
	float tau = 0.0F;
	float TS = 0.0F;
	float cf = 0.0F;
	pSingleArray PBuf = new float[1];
	vector<float> hst;
	float pst = 0.0F;
	float p01s = 0.0F;
	float P1s = 0.0F;
	float P3s = 0.0F;
	float P10s = 0.0F;
	float P50s = 0.0F;
	float p30 = 0.0F;
	float p50 = 0.0F;
	float p80 = 0.0F;
	float p17 = 0.0F;
	float p13 = 0.0F;
	float p10 = 0.0F;
	float p8 = 0.0F;
	float p6 = 0.0F;
	float p4 = 0.0F;
	float p3 = 0.0F;
	float p2p2 = 0.0F;
	float p1p5 = 0.0F;
	float P1 = 0.0F;
	float p0p7 = 0.0F;
	int stop = 0;
	whp = (float) (2.0 * PI * 0.05);
	tau = 0.3F;
	cf = (float) (1.0F / 1.285e-6F);
	if(FBase == 50.0)
	{
		k = 1.74802F;
		lam = (float) (2.0 * PI * 4.05981);
		W1 = (float) (2.0 * PI * 9.15494);
		W2 = (float) (2.0 * PI * 2.27979);
		W3 = (float) (2.0 * PI * 1.22535);
		W4 = (float) (2.0 * PI * 21.9);
	}
	else
	{
		k = (float) (1.6357F / 0.783F);
		lam = (float) (2.0 * PI * 4.167375);
		W1 = (float) (2.0 * PI * 9.077169);
		W2 = (float) (2.0 * PI * 2.939902);
		W3 = (float) (2.0 * PI * 1.394468);
		W4 = (float) (2.0 * PI * 17.31512);
	}
	tpst = 0.0F;
	ipst = 1;
	TS = (float) (Pt[2] - Pt[1]);
	for(stop = n, i = 1; i <= stop; i++)
	{
	pRms[i] = (float) (pRms[i] / VBase);
	}
	PBuf = (pSingleArray) realloc(PBuf, sizeof(float) * n);
	Fhp(n, TS, whp, pRms, PBuf);
	Fw1(n, TS, W1, k, lam, PBuf, pRms);
	Fw2(n, TS, W2, W3, W4, pRms, PBuf);
	for(stop = n, i = 1; i <= stop; i++)
	{
		PBuf[i] = (float) (PBuf[i] * PBuf[i]);
	}
	Flp(n, TS, tau, PBuf, pRms);
	for(stop = n, i = 1; i <= stop; i++)
	{
		pRms[i] = (float) (cf * pRms[i]);
	}

  // build the Blcok 5 Pst outputs from Block 4 instantaneous flicker levels
	hst.resize(Trunc(600.0F / TS) + 1);
	ihst = MinValue(&hst);
	for(stop = n, i = 1; i <= stop; i++)
	{
		T = Pt[i];
		hst[ihst] = pRms[i];
		if((T - tpst) >= 600.0) // append a new Pst value
		{
			auto it0 = MinValue(&hst);
			QuickSort(hst, 0, ihst);
			p80 = Percentile(hst, 0, ihst, 80.0F);
			p50 = Percentile(hst, 0, ihst, 50.0F);
			p30 = Percentile(hst, 0, ihst, 30.0F);
			p17 = Percentile(hst, 0, ihst, 17.0F);
			p13 = Percentile(hst, 0, ihst, 13.0F);
			p10 = Percentile(hst, 0, ihst, 10.0F);
			p8 = Percentile(hst, 0, ihst, 8.0F);
			p6 = Percentile(hst, 0, ihst, 6.0F);
			p4 = Percentile(hst, 0, ihst, 4.0F);
			p3 = Percentile(hst, 0, ihst, 3.0F);
			p2p2 = Percentile(hst, 0, ihst, 2.2F);
			p1p5 = Percentile(hst, 0, ihst, 1.5F);
			P1 = Percentile(hst, 0, ihst, 1.0F);
			p0p7 = Percentile(hst, 0, ihst, 0.7F);
			p01s = Percentile(hst, 0, ihst, 0.1F);
			P50s = (float) ((p30 + p50 + p80) / 3.0);
			P10s = (float) ((p6 + p8 + p10 + p13 + p17) / 5.0);
			P3s = (float) ((p2p2 + p3 + p4) / 3.0);
			P1s = (float) ((p0p7 + P1 + p1p5) / 3.0);
			pst = (float) sqrt(0.0314F * p01s + 0.0525F * P1s + 0.0657F * P3s + 0.28F * P10s + 0.08F * P50s);
			pPst[ipst] = pst;
			++ipst;
			tpst = T;
			ihst = MinValue(&hst);
		}
		else
		{
			++ihst;
		}
	}
}




}  // namespace Pstcalc




