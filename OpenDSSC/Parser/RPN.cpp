

#pragma hdrstop

#include "RPN.h"
#include <math.h>
#include "DSSGlobals.h"

using namespace std;



/* TRPNCalc */
double DegToRad = 0.0;
double RadToDeg = 0.0;

void TRPNCalc::aCosdeg()
{
	FStack[1 - 1] = RadToDeg * acos(FStack[1 - 1]);
}

void TRPNCalc::Add()
{
	FStack[2 - 1] = FStack[1 - 1] + FStack[2 - 1];
	RollDn();
}

void TRPNCalc::aSindeg()
{
	FStack[1 - 1] = RadToDeg * asin(FStack[1 - 1]);
}

void TRPNCalc::aTandeg()
{
	FStack[1 - 1] = RadToDeg * atan(FStack[1 - 1]);
}

void TRPNCalc::aTan2deg()
{
	FStack[2 - 1] = RadToDeg * atan2(FStack[2 - 1], FStack[1 - 1]);
	RollDn();
}

void TRPNCalc::Cosdeg()
{
	FStack[1 - 1] = cos(DegToRad * FStack[1 - 1]);
}

TRPNCalc::TRPNCalc()
{
	int i = 0;
	int stop = 0;
	for(stop = MaxStackSize, i = 1; i <= stop; i++)
	{
		FStack[i - 1] = 0.0;
	}
}

TRPNCalc::~TRPNCalc()
{
	// inherited;
}


void TRPNCalc::Divide()
{
	FStack[2 - 1] = FStack[2 - 1] / FStack[1 - 1];
	RollDn();
}

double TRPNCalc::Get_X()
{
	double result = 0.0;
	result = FStack[1 - 1];
	return result;
}

double TRPNCalc::Get_Y()
{
	double result = 0.0;
	result = FStack[2 - 1];
	return result;
}

double TRPNCalc::Get_Z()
{
	double result = 0.0;
	result = FStack[3 - 1];
	return result;
}

void TRPNCalc::Multiply()
{
	FStack[2 - 1] = FStack[2 - 1] * FStack[1 - 1];
	RollDn();
}

void TRPNCalc::RollDn()
{
	int i = 0;
	int stop = 0;
	for(stop = MaxStackSize, i = 2; i <= stop; i++)
	{
		FStack[i - 1 - 1] = FStack[i - 1];
	}
}

void TRPNCalc::RollUp()
{
	int i = 0;
	int stop = 0;
	for(stop = 2, i = MaxStackSize; i >= stop; i--)
	{
		FStack[i - 1] = FStack[i - 1 - 1];
	}
}

void TRPNCalc::Set_X(double Value)
{
	RollUp();
	FStack[1 - 1] = Value;
}

void TRPNCalc::Set_Y(double Value)
{
	FStack[2 - 1] = Value;
}

void TRPNCalc::Set_Z(double Value)
{
	FStack[3 - 1] = Value;
}

void TRPNCalc::Sindeg()
{
	FStack[1 - 1] = sin(DegToRad * FStack[1 - 1]);
}

void TRPNCalc::Sqrt()
{
	FStack[1 - 1] = sqrt(FStack[1 - 1]);
}

void TRPNCalc::Square()
{
	FStack[1 - 1] = pow(FStack[1 - 1],2);
}

void TRPNCalc::Subtract()
{
	FStack[2 - 1] = FStack[2 - 1] - FStack[1 - 1];
	RollDn();
}

void TRPNCalc::SwapXY()
{
	double Temp = 0.0;
	Temp = FStack[1 - 1];
	FStack[1 - 1] = FStack[2 - 1];
	FStack[2 - 1] = Temp;
}

void TRPNCalc::Tandeg()
{
	FStack[1 - 1] = tan(DegToRad * FStack[1 - 1]);
}

void TRPNCalc::YToTheXPower()
{
	FStack[2 - 1] = pow(FStack[2 - 1], FStack[1 - 1]);
	RollDn();
}

void TRPNCalc::EnterPi()
{
	RollUp();
	FStack[1 - 1] = PI;
}

void TRPNCalc::etothex()
{
	FStack[1 - 1] = exp(FStack[1 - 1]);
}

void TRPNCalc::NatLog()
{
	FStack[1 - 1] = log(FStack[1 - 1]);
}

void TRPNCalc::TenLog()
{
	FStack[1 - 1] = log10(FStack[1 - 1]);
}  // invert  1/X

void TRPNCalc::Inv()
{
	FStack[1 - 1] = 1.0 / FStack[1 - 1];
}


void RPN_initialization()
{
	DegToRad = PI / 180.0;
	RadToDeg = 1.0 / DegToRad;
}

		class 		RPN_unit
		{
		public:
		RPN_unit()
		{
			//AssertSystemInitialization();
			RPN_initialization();
		}
		};
		RPN_unit _RPN_unit;






