#ifndef TGOV_H
#define TGOV_H

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "ControlClass.h"
#include "ControlElem.h"
#include "CktElement.h" // May not need this
#include "Bus.h" // May not need this
#include "DSSClass.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "d2c_structures.h"

using namespace std;

namespace Tgov // Create a namespace so similar functions from others namespaces will not be crosswired
{
	/*
	  ----------------------------------------------------------
	  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
	  All rights reserved.
	  ----------------------------------------------------------
	*/

	/*
	   Change Log
	   11/17/2022 - B. Schenkman - created class

	*/

	/*
	  A Tgov is a class that represents a simple steam turbine governor.

	  It can be invoked as follows in the input file:

	  New Tgov.Name=myname generator=Generator.name terminal=[ 1|2|...] Parameter_List

	  Controlled generator must already exist in the case.
	*/

	// TTgov Class
	class TTgov : public ControlClass::TControlClass // Create class TTgov and pull in the TControlClass from ControlClass namespace 
	{ //
		friend class TTgovObj;
	public:
		typedef ControlClass::TControlClass inherited; // inherited becomes the ControlClass::TControlClass (Why is this here?)
	private:

	protected:
		void DefineProperties(); // Define the properties of the model in tgov.cpp
		int MakeLike(const String TgovName); // Creates a new Tgov object if none other exists
	public:
		TTgov();                        // Constructor to be performed when the TTGov class is called (initialization)
		virtual ~TTgov();             // Destructor to be performed when TTGov class is called (initialization)
		int Edit(int ActorID);	        // uses global parser
		int NewObject(const String ObjName); // Create Tgov model in Tgov class
		int FindGenerator(); // Find the generator where Tgov model is tied to

	};

	// TTgovObj Class 
	class TTgovObj : public ControlElem::TControlElem 
	{
	public:
		typedef ControlElem::TControlElem inherited; // Sets inherited as ControlElem::TControlElem
	public:
		int64_t nstates;
		int64_t nchannels;
		//	States, stored states and state derivatives
		double *s, *ss, *ds, *dss;
		vector <double> ps;
		//	Parameters
		double R;
		double Dt;
		double T1;
		double T2;
		double T3;
		double Vmax;
		double Vmin;
		//	Variables
		double pmech; // mechanical power output in pu
		double pmref; // mechanical power reference
		double dspd;  // speed deviation from generator
		double Y;
		double U;

		//Miscellaneous - Remove after validation
		int stepCount=0;

		//	Bases
		double	vbase, mbase, ibase;

		int FListSize;
		String gen_name;									//	Name of generator model is connected to
		String bus_name;									//	Name of bus where model is connected to
		Generator::TGeneratorObj* pGen;						//	Pointer to controlled generator
		std::vector <complex> cBuffer;						// Complexarray buffer


	public:
		TTgovObj(DSSClass::TDSSClass* ParClass, const String TgovName);
		//TTgovObj();
		virtual ~TTgovObj();
		void sample(int ActorID);										//	Sample control quantities and set action times in Control Queue
		void DoPendingAction(int Code, int ProxyHdl, int ActorID);		//	Do the action that is pending from last sample
		void Reset(int ActorID);										//	Reset to initial defined state
		void InitPropertyValues(int ArrayOffset);						//	Initialize property values
		void LoadDefaultParameters();									//	Loads default model parameters
		virtual int InitializeStates(int ActorID);						//	Initializes values of states based on steady state power flow solution
		virtual int CalculateRate(int ActorID);							//	Calculate state derivatives prior to integration
		virtual int StateIntegration(int ActorID);						//	Calls method to integrate states
		virtual int StateIntegration_correction(int ActorID);			//	Calls method to integrate states when predictor corrector method is used
		//virtual complex* CalculateIsorce(int ActorID, complex* I012);	//	Calculates current injections from model for network solution
		//virtual complex GetIsorce1(int ActorID);						//	Get positive sequence current
		//virtual complex GetIsorce2(int ActorID);						//	Get negative sequence current
		virtual double get_channel(int channel_num);					//	Gets channel values from model
		virtual String get_channel_header(int channel_num);				//	Gets channel header names in model
		virtual int64_t get_number_channels();							//	Gets number of channels in model
		virtual double get_pmech();
		//virtual double get_dspd();
					//	Implements Newton-Raphson to initialize model



	};

	extern TTgovObj* ActiveTgovObj;

}

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Tgov;
#endif

#endif // !TGOV_H
