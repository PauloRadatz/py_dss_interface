#ifndef PDELementH
#define PDELementH

#include "System.h"
#include "Sysutils.h"

#include "CktElement.h"
#include "Ucomplex.h"
#include "Ucmatrix.h"
#include "DSSClass.h"
#include "MeterElement.h"
#include "Arraydef.h"
#include "DSSClassDefs.h"
#include "PCElement.h"
#include "Bus.h"

namespace PDELement
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
   Change Log
   1/10/00  Fixed bug where OverLoad_EEN, _UE was not being set for elements
            where the rating was not specified.
   4/11/01  Fixed error in computin excess kVAs (factor readjustment)
*/

	class TPDElement : public CktElement::TDSSCktElement
	{
	public:
		typedef CktElement::TDSSCktElement inherited;	
	//private:
		Ucomplex::complex Get_ExcessKVANorm(int idxTerm, int ActorID);
		Ucomplex::complex Get_ExcessKVAEmerg(int idxTerm, int ActorID);
	public:  // annual faults per year
		// percent of faults that are permanent in this element
		// net failure rate for this branch
	  // accumulated failure rate for this branch
	  // length in miles if line
	 // total miles downstream
		double NormAmps;
		double EmergAmps;
		double FaultRate;
		double PctPerm;
		double BranchFltRate;
		double AccumulatedBrFltRate;
		double MilesThisLine;
		double AccumulatedMilesDownStream;
		double HrsToRepair;
		int FromTerminal;
		int ToTerminal;  // Set by Meter zone for radial feeder
		bool IsShunt;
		int BranchNumCustomers;
		int BranchTotalCustomers;
		double BranchCustWeight; // Weighting factor for customers on this elemebt
		int BranchSectionID; // ID of the section that this PD element belongs to
		TPDElement* ParentPDElement;                     /*Upline energymeter*/
		MeterElement::TMeterElement* MeterObj;
		MeterElement::TMeterElement* SensorObj; // Upline Sensor for this element  for allocation and estimation
		double Overload_UE;
		double OverLoad_EEN;  // Indicate amount of branch overload
		int NumAmpRatings;
		Arraydef::TRatingsArray AmpRatings;
		TPDElement(TDSSClass* ParClass);
		virtual ~TPDElement();
		virtual void InitPropertyValues(int ArrayOffset);
		virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present values of terminal
		virtual void CalcFltRate();  // Calc failure rates for section and buses
		void AccumFltRate();
		void CalcNum_Int(int& SectionCount, bool AssumeRestoration);  // Calc Number of Interruptions in forward sweep
		void CalcCustInterrupts();
		void ZeroReliabilityAccums(); // Zero out reliability accumulators
		void CalcNumCustomers(int ActorID, TPDElement* pElems);
		TPDElement(String ClassName);
		TPDElement();
	};


}  // namespace PDELement

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace PDELement;
#endif

#endif // PDELementH




