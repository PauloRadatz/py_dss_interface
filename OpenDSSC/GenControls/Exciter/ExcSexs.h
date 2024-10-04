#ifndef EXCSEXS_H
#define EXCSEXS_H

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



using namespace std;

namespace ExcSexs
{

	/*
	  ----------------------------------------------------------
	  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
	  All rights reserved.
	  ----------------------------------------------------------
	*/

	/*
	   Change Log
	   11/15/2022 - J. Taylor 

	*/

	/*
	  The SEXS model represents a simplified excitation system.

	  It can be invoked as follows in the input file:

	  New ExcSexs.Name=myname generator=Generator.name terminal=[ 1|2|...] Parameter_List  
	  // need to revise the call in order to ensure the connection to the generator is correct
	  // need to define the exciter, than the type of exciter --- precidence in opendSS???

	  The generator model must already exist in the case.
	*/






	class TExcSexs : public ControlClass::TControlClass {
		friend class TExcSexsObj;
	public:
		typedef ControlClass::TControlClass inherited;
	private:

	protected:
		void DefineProperties();
		int MakeLike(const String ExcSexsName);
	public:
		TExcSexs();
		virtual ~TExcSexs();
		int Edit(int ActorID);					// uses global parser
		int NewObject(const String ObjName);
		int FindGenerator();

	};

	class TExcSexsObj : public ControlElem::TControlElem {
	public:
		typedef ControlElem::TControlElem inherited;


	public: //---Why are these variables not declared as private??
		int64_t nstates;
		int64_t nchannels;

		//	Pointers to states, stored states and state derivatives
		double* s, * ss, * ds, * dss;
		vector <double> ps;

		//	Parameters
		double TaOverTb;	// Ratio of Ta and Tb, this parameter definition follows formulation in PSSE
		double Tb;			// must be greater than zero, units in seconds
		double K;			//
		double Te;			// units in seconds
		double Emin;		//
		double Emax;		//

		//	Variables
		double efd;
		double Y;
		double U;
		double Vs;
		double Vref;
		double Ec;

		//Miscellaneous - Remove after validation
		int stepCount;

		//	Bases
		double	vbase, mbase, ibase;

		int FListSize;
		String gen_name;									//	Name of generator the model is connected to
		String bus_name;									//	Name of bus where the model is connected to
		Generator::TGeneratorObj* pGen;						//	Pointer to controlled generator
		std::vector <complex> cBuffer;					// Complexarray buffer


	public:
		TExcSexsObj(DSSClass::TDSSClass* ParClass, const String ExcSexsName);
		TExcSexsObj();
		virtual ~TExcSexsObj();

		void sample(int ActorID);    // Sample control quantities and set action times in Control Queue
		void DoPendingAction(int Code, int ProxyHdl, int ActorID);   // Do the action that is pending from last sample
		void Reset(int ActorID);  // Reset to initial defined state

		void InitPropertyValues(int ArrayOffset);

		void LoadDefaultParameters();

		virtual int InitializeStates(int ActorID);
		virtual int CalculateRate(int ActorID);
		virtual int StateIntegration(int ActorID);
		virtual int StateIntegration_correction(int ActorID);
//		virtual complex* CalculateIsorce(int ActorID, complex* I012);
		//virtual complex GetIsorce1(int ActorID);
		//virtual complex GetIsorce2(int ActorID);
		virtual double get_channel(int channel_num);
		virtual String get_channel_header(int channel_num);
		virtual int64_t get_number_channels();
		virtual double get_efield();					     	//  Returns calculated efield


	};

	extern TExcSexsObj* ActiveExcSexsObj;

}

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace ExcSexs;
#endif

#endif // !ExcSexs_H
