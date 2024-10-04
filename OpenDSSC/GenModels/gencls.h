#ifndef GENCLS_H
#define GENCLS_H

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "ControlClass.h"
#include "ControlElem.h"
#include "CktElement.h"
#include "Bus.h"
#include "DSSClass.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "d2c_structures.h"
#include "gencls.h"


using namespace std;

namespace Gencls
{

	/*
	  ----------------------------------------------------------
	  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
	  All rights reserved.
	  ----------------------------------------------------------
	*/

	/*
	   Change Log
	   6/6/2022 - B. Leonardi - created class

	*/

	/*
	  A Gencls is a class that represents a classical model of a synchronous generator.

	  It can be invoked as follows in the input file:

	  New Gencls.Name=myname generator=Generator.name terminal=[ 1|2|...] Parameter_List

	  Controlled generator must already exist in the case.
	*/






	class TGencls : public ControlClass::TControlClass {
		friend class TGenclsObj;
	public:
		typedef ControlClass::TControlClass inherited;
	private:

	protected:
		void DefineProperties();
		int MakeLike(const String GenclsName);
	public:
		TGencls();
		virtual ~TGencls();
		int Edit(int ActorID);					// uses global parser
		int NewObject(const String ObjName);
		int FindGenerator();

	};

	class TGenclsObj : public ControlElem::TControlElem {
	public:
		typedef ControlElem::TControlElem inherited;


	public:
		int64_t nstates;
		int64_t nchannels;
		//	States, stored tates and state derivatives
		double *s, *ss, *ds, *dss;
		vector <double> ps;
		//	Parameters
		double h;
		double d;
		//	Variables
		double pmech, tm;
		double pgen, qgen, te;
		double dspd;
		double ang;
		double spd;
		double w0;
		double ed;	//	internal voltage source direct axis voltage in pu
		double eq;	//	internal voltage source quadrature axis voltage in pu
		double id;	//	internal current source direct axis current in pu
		double iq;	//	internal current source direct axis current in pu
		complex zsorce, zsorcepu;
		complex vsorce, vsorcepu;
		complex isorce, isorcepu;
		complex isorce2, isorce2pu;
		complex iterminit[3];
		//	Bases
		double	vbase, mbase, ibase;

		int FListSize;
		String gen_name;									//	Name of generator model is connected to
		String bus_name;									//	Name of bus where model is connected to
		Generator::TGeneratorObj* pGen;						//	Pointer to controlled generator
		std::vector <complex> cBuffer;    // Complexarray buffer


	public:
		TGenclsObj(DSSClass::TDSSClass* ParClass, const String GenclsName);
		TGenclsObj();
		virtual ~TGenclsObj();
		// void MakePosSequence(int ActorID);  // Make a positive Sequence Model
		//virtual void RecalcElementData(int ActorID);
		//virtual void CalcYPrim(int ActorID);    // Always Zero for a GenDispatcher
		void sample(int ActorID);    // Sample control quantities and set action times in Control Queue
		void DoPendingAction(int Code, int ProxyHdl, int ActorID);   // Do the action that is pending from last sample
		void Reset(int ActorID);  // Reset to initial defined state
		//void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present value of terminal Curr
		//void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);   // Returns Injextion currents
		void InitPropertyValues(int ArrayOffset);
		// void DumpProperties(System::TTextRec& f, bool Complete);
		void LoadDefaultParameters();
		// bool MakeGenList();
		// void SetMachineModel();
		virtual int InitializeStates(int ActorID);
		virtual int CalculateRate(int ActorID);
		virtual int StateIntegration(int ActorID);
		virtual int StateIntegration_correction(int ActorID);
		virtual complex* CalculateIsorce(int ActorID, complex *I012);
		virtual complex GetIsorce1(int ActorID);
		virtual complex GetIsorce2(int ActorID);
		virtual double get_channel(int channel_num);
		virtual String get_channel_header(int channel_num);
		virtual int64_t get_number_channels();


	};

	extern TGenclsObj* ActiveGenclsObj;

}

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Gencls;
#endif

#endif // !GENCLS_H
