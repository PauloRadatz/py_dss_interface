#ifndef RPNH
#define RPNH

#include "System.h"
#include "Sysutils.h"


namespace RPN
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*RPN Calculator*/

/*$M+*/
const int MaxStackSize = 10;

class TRPNCalc : public System::TObject
{
public:
	typedef TObject inherited;	
//private:
	double FStack[10/*# range 1..MaxStackSize*/];
	double Get_X();
	double Get_Y();
	double Get_Z();
	void Set_X(double Value);
	void Set_Y(double Value);
	void Set_Z(double Value);
//protected:
public:
	void Multiply();
	void Divide();
	void Sqrt();
	void Square();
	void Add();
	void Subtract();
	void YToTheXPower();
	void Sindeg();
	void Cosdeg();
	void Tandeg();
	void aSindeg();
	void aCosdeg();
	void aTandeg();
	void aTan2deg();
	void NatLog();
	void TenLog();
	void etothex();
	void EnterPi();
	void Inv();
	void SwapXY();
	void RollUp();
	void RollDn();
	TRPNCalc();
	virtual ~TRPNCalc();
//__published:
};


}  // namespace RPN

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace RPN;
#endif

#endif // RPNH




