#ifndef FeederH
#define FeederH
 /*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*  Feeder Class

   User cannot instantiate this object.  Feeders are created on the fly when
   a radial system is specified.  Feeders are created from Energymeters and are
   given the same name.


 6-24-04  Created from Isource (a simple PC Element)
 8-13-2006  Radial circuit solution removed

 Feeders get created from energy meters if Radial is set to yes and meter zones
 are already computed.  If Radial=Yes and the meterzones are reset, then the feeders
 are redefined.  If Radial is subsequently set to NO or a solution mode is used
 that doesn't utilize feeders, the get currents routines will not do anything.

 Feeders cannot be re-enabled unless the energymeter object allows them to be.

 Feeders are not saved.  This is implicit with the Energymeter saving.

*/


//#include <System.hpp>

#include "DSSClass.h"
#include "PCClass.h"
#include "PCElement.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "PointerList.h"
#include "CktElement.h"
#include "CktTree.h"

namespace Feeder
{

	//class TFeeder;
	//class TFeederObj;



	// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

	class TFeeder : public TPCClass {
	public:
		typedef TPCClass inherited;
		friend class TFeederObj;
//	private:
	protected:
		void DefineProperties();
		virtual int MakeLike(const String OtherFeederName);
	public:
		TFeeder();
		virtual ~TFeeder();
		virtual int Edit(int ActorID);
		virtual int Init(int Handle, int ActorID);
		virtual int NewObject(const String ObjName);
	};

	// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

	using namespace DSSClass;

	class TFeederObj : public PCElement::TPCElement {
		typedef TPCElement inherited;
		friend class TFeeder;
	public:
		//  private:
		PointerList::TPointerList sequenceList, ShuntList;
		TDSSCktElement RootElement;
		int FromTerminalOffset;
	public:
		bool IsSynched;
		void InitializeFeeder(TCktTree BranchList, int ActorID);
		void SetCktElementFeederFlags(bool Value);
		TFeederObj(DSSClass::TDSSClass* ParClass, const String MeterName);
		virtual ~TFeederObj();
		virtual void RecalcElementData(int ActorID);
		virtual void CalcYPrim(int ActorID);
		virtual void MakePosSequence(int ActorID)  // Make a positive Sequence Model  - N/A
			;
		virtual int InjCurrents(int ActorID);
		virtual void GetInjCurrents(pComplexArray Curr, int ActorID);
		virtual void GetCurrents(pComplexArray Curr, int ActorID);
		virtual void InitPropertyValues(int ArrayOffset);
		virtual void DumpProperties(Textfile& F, bool Complete);
	};


	extern TFeederObj* ActiveFeederObj;

	// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
}

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Feeder;
#endif

#endif //  FeederH








