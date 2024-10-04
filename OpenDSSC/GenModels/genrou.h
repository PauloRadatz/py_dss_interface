#ifndef GENROU_H
#define GENROU_H

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
//#include "genrou.h"


using namespace std;

namespace Genrou
{

	/*
	  ----------------------------------------------------------
	  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
	  All rights reserved.
	  ----------------------------------------------------------
	*/

	/*
	   Change Log
	   8/22/2022 - B. Leonardi - created genrou model

	*/

	/*
	  A genrou is a class that represents a round rotor model of a synchronous generator.

	  It can be invoked as follows in the input file:

	  New Genrou.Name=myname generator=Generator.name terminal=[ 1|2|...] Parameter_List

	  The controlled generator must already be declared and exist in the case.
	*/



	class TGenrou : public ControlClass::TControlClass {
	public:
		typedef ControlClass::TControlClass inherited;
	private:

	protected:
		void DefineProperties();
		int MakeLike(const String GenrouName);
	public:
		TGenrou();
		virtual ~TGenrou();
		int Edit(int ActorID);					// uses global DSS parser
		int NewObject(const String ObjName);
		int FindGenerator();

	};

	class TGenrouObj : public ControlElem::TControlElem {
		
	public:
		typedef ControlElem::TControlElem inherited;


	public:
		int64_t nstates;
		int64_t nchannels;
		//	States, stored states and state derivatives
		double* s, * ss, * ds, * dss;
		vector <double> ps;
		//	Parameters
		double tpdo;		//	d-axis transient rotor time constant, s
		double tppdo;		//	d-axis subtransient rotor time constant, s
		double tpqo;		//	q-axis transient time constant, s
		double tppqo;		//	q-axis subtransient time constant, s
		double h;			//	inertia constant, sec
		double d;			//	damping factor, pu
		double xd;			//	d-axis synchornous reactance, pu
		double xq;			//	q-axis synchronous reactance, pu
		double xpd;			//	d-axis transient reactance, pu
		double xpq;			//	q-axis transient reactance, pu
		double xppd;		//	d-axis transient reactance, pu
		double xl;			//	leakage reactance, pu
		double s1;			//	saturation factor at 1pu flux
		double s12;			//	saturation factor at 1.2 pu flux
		double ra;			//	armature or stator resistance, pu
		double rcomp;		//	compounding resitance for voltage control, pu (currently not implemented)
		double xcomp;		//	compounding reactance for voltage control, pu (currently not implemented)
		//	Variables
		double pmech, tm;
		double pgen, qgen, te;
		double dspd;
		double ang;
		double spd;
		double w0;
		double ed;				//	internal voltage source direct axis voltage in pu
		double eq;				//	internal voltage source quadrature axis voltage in pu
		double id;				//	internal current source direct axis current in pu
		double iq;				//	internal current source direct axis current in pu
		double asat, bsat;		//	saturation parameters
		double efd;				//	Field voltage in pu
		double ladifd;			//	Field current in pu
		double fq, fpq, fppq;	//	Steady state, transient and subtransient flux on q axis
		double fd, fpd, fppd;	//	Steady state, transient and subtransient flux on d axis
		double epd, epq;		//	Transient direct and quadrature axis voltages
		double vtd, vtq;		//	Terminal voltage in dq axis and pu
		double plast, qlast;	//	Last pgen and qgen from time domain simulation

		complex zsorce, zsorcepu;
		complex vsorce, vsorcepu;
		complex isorce, isorcepu;
		complex isorce2, isorce2pu;
		complex iterminit[3];
		//	Bases
		double	vbase, mbase, ibase;

		int FListSize;
		String gen_name;												//	Name of generator model is connected to
		String bus_name;												//	Name of bus where model is connected to
		Generator::TGeneratorObj* pGen;									//	Pointer to controlled generator
		std::vector <complex> cBuffer;									//	Complexarray buffer


	public:
		TGenrouObj(DSSClass::TDSSClass* ParClass, const String GenrouName);
		TGenrouObj();
		virtual ~TGenrouObj();
		void sample(int ActorID);										//	Sample control quantities and set action times in Control Queue
		void DoPendingAction(int Code, int ProxyHdl, int ActorID);		//	Do the action that is pending from last sample
		void Reset(int ActorID);										//	Reset to initial defined state
		void InitPropertyValues(int ArrayOffset);						//	Initialize property values
		void LoadDefaultParameters();									//	Loads default model parameters
		virtual int InitializeStates(int ActorID);						//	Initializes values of states based on steady state power flow solution
		virtual int CalculateRate(int ActorID);							//	Claculate state derivatives prior to integration
		virtual int StateIntegration(int ActorID);						//	Calls method to integrate states
		virtual int StateIntegration_correction(int ActorID);			//	Calls method to integrate states when predictor corrector method is used
		virtual complex* CalculateIsorce(int ActorID, complex* I012);	//	Calculates current injections from model for network solution
		virtual complex GetIsorce1(int ActorID);						//	Get positive sequence current
		virtual complex GetIsorce2(int ActorID);						//	Get negative sequence current
		virtual double get_channel(int channel_num);					//	Gets channel values from model
		virtual String get_channel_header(int channel_num);				//	Gets channel header names in model
		virtual int64_t get_number_channels();							//	Gets number of channels in model
		double QuadraticSaturation(double &fppmag);						//	Implements quadratic saturation in genrou
		//int64_t call_iterative_solver(void);							//	Implements Newton-Raphson to initialize model
		virtual double get_efield();								 	//  Returns calculated efield, primarily for initialization
		virtual double get_pmech();
		virtual double get_dspd();

		//	
		virtual double get_plast();
		virtual double get_qlast();

		
		/*
		* 
		*	These functions may not be needed so they have been commented out for the time being
		* 		
		*/

		// void MakePosSequence(int ActorID);  // Make a positive Sequence Model
		// virtual void RecalcElementData(int ActorID);
		// virtual void CalcYPrim(int ActorID);    // Always Zero for a GenDispatcher
		// void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present value of terminal Curr
		// void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);   // Returns Injextion currents
		// void DumpProperties(System::TTextRec& f, bool Complete);
		// bool MakeGenList();
		// void SetMachineModel();

	};

	extern TGenrouObj* ActiveGenrouObj;

}

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Genrou;
#endif

#endif // !GENROU_H

