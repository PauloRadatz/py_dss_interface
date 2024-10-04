#ifndef LineConstantsH
#define LineConstantsH

#include "System.h"
#include "Sysutils.h"

#include "Arraydef.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "LineUnits.h"

namespace LineConstants
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*Manages the geometry data and calculates the impedance matrices for an overhead line*/

/*Usage: Create with Number of conductors you want
        Specify the number of phases. The first conductors you define with
        be the phases. Other conductors may be considered neutral.

        Uses GMR for power frequency calcs so that answers match published
        data.

        You only have to set R or GMR. The other will default. However, you should set
        both for better accuracy.

        When you ask for Zmatrix or YCmatrix you get the full matrix unless you have executed
        a Kron reduction or Reduce function. Reduce eleminates all non phases. If you
        want the full detailed model, DO NOT REDUCE!

*/

/*This class returns a matrix ordered by phases first then remaining conductors
 Assumes phases are defined first*/

class TLineConstants : public System::TObject
{
public:
	typedef TObject inherited;	
//private:
//protected:
	int FNumConds;
	int FNumPhases;
	std::vector <double> FX;
	std::vector <double> FY;
	std::vector <double> FRDC;   // ohms/m
	std::vector <double> Frac;   // ohms/m
	std::vector <double> FGMR;   // m
	std::vector <double> Fradius;
	std::vector <double> Fcapradius;  // if different than radius; defaults to radius
                                   // Primarily for bundled conductors
	Ucmatrix::TcMatrix* FZmatrix;   // in ohms/m
	Ucmatrix::TcMatrix* FYCmatrix;   // siemens/m   --- jwC
	Ucmatrix::TcMatrix* FZreduced;  // These two do not exist until Kron Reduction
	Ucmatrix::TcMatrix* FYCreduced;  // is executed
	double FFrequency;  // Frequency for which impedances are computed
	double Fw;  // 2piF
	double FrhoEarth;  // ohm-m
	Ucomplex::complex Fme; // factor for earth impedance
	bool FRhoChanged;
	double Get_GMR(int i, int Units);
	double Get_radius(int i, int Units);
	double Get_Rdc(int i, int Units);
	double Get_Rac(int i, int Units);
	double Get_X(int i, int Units);
	double Get_Y(int i, int Units);
	Ucmatrix::TcMatrix* Get_YCmatrix(double f, double Lngth, int Units);
	Ucomplex::complex Get_Ze(int i, int j);
	Ucomplex::complex Get_Zint(int i);
	Ucmatrix::TcMatrix* Get_Zmatrix(double f, double Lngth, int Units);
	void Set_GMR(int i, int Units, double Value);
	void Set_radius(int i, int Units, double Value);
	void Set_Rdc(int i, int Units, double Value);
	void Set_Rac(int i, int Units, double Value);
	void Set_X(int i, int Units, double Value);
	void Set_Y(int i, int Units, double Value);
	void Set_Frequency(double Value);
	void Set_Frhoearth(double Value);  // m
    // This allows you to compute capacitance using a different radius -- for bundled conductors
	double Get_Capradius(int i, int Units);
	void Set_Capradius(int i, int Units, double Value);

   /*These can only be called privately*/
	void Set_NPhases(int Value);
public:
	virtual bool ConductorsInSameSpace(std::string& ErrorMessage);
	virtual void Calc(double f); // force a calc of impedances
	virtual void Kron(int Norder); // Performs a Kron reduction leaving first Norder  rows
	void Reduce();  // Kron reduce to Numphases only
	double get_FrhoEarth();
	int get_FNumConds();
	int get_FNumPhases();

	TLineConstants(int NumConductors);
	virtual ~TLineConstants();
	TLineConstants();
};
const double E0 = 8.854e-12;  // dielectric constant  F/m
const double mu0 = 12.56637e-7; // hy/m
//const double TwoPi = 6.283185307;


}  // namespace LineConstants

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace LineConstants;
#endif

#endif // LineConstantsH





