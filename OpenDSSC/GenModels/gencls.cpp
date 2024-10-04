
#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "ControlClass.h"
#include "ControlElem.h"
#include "CktElement.h"
#include "DSSClass.h"
#include "DSSGlobals.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "PointerList.h"
#include "d2c_structures.h"
#include "gencls.h"
#include <string>
#include "IntegrationMethods.h"


using namespace std;
using namespace std;
using namespace Bus;
using namespace Circuit;
using namespace CktElement;
using namespace Command;
using namespace ControlClass;
using namespace ControlElem;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace ParserDel;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;


namespace Gencls
{

	TGenclsObj* ActiveGenclsObj = nullptr;
	const int NumPropsThisClass = 4;		//	Number of properties in this class

	void TGencls::DefineProperties()
	{
		NumProperties = NumPropsThisClass;
		CountProperties();   // Get inherited property count
		AllocatePropertyArrays();


		// Define Property names
		PropertyName[0] = "bus_name";
		PropertyName[1] = "gen_name";
		PropertyName[2] = "h";
		PropertyName[3] = "d";
		PropertyHelp[0] = "Bus name where the model is connected to";
		PropertyHelp[1] = "Generator name where model is connected to";
		PropertyHelp[2] = "Inertia constant";
		PropertyHelp[3] = "Damping constant";
		ActiveProperty = NumPropsThisClass - 1;
		inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
	}

	int TGencls::MakeLike(const String GenclsName)
	{
		int result = 0;
		TGenclsObj* OtherGencls = nullptr;
		int i = 0;
		result = 0;
		/*See if we can find this GenclsName in the present collection*/
		OtherGencls = ((TGenclsObj*)Find(GenclsName));
		if (OtherGencls != nullptr)
		{
			auto thisobj = ActiveGenclsObj;
			int stop = 0;
			thisobj->bus_name = OtherGencls->bus_name;

			for (i = 1; i <= thisobj->ParentClass->NumProperties; i++)
			{
				thisobj->Set_PropertyValue(i,OtherGencls->Get_PropertyValue(i));
			}
		}
		else
			DoSimpleMsg(String("Error in Gencls MakeLike: \"") + GenclsName
				+ "\" Not Found.", 9001);
		return result;
	}
	//	Constructor and Destructor
	TGencls::TGencls() {

		Class_Name = "Gencls";
		DSSClassType = DSSClassType + GENCLS_ELEMENT;
		DefineProperties();
		CommandList = TCommandList(Slice((PropertyName), NumProperties), NumProperties);
		CommandList.set_AbbrevAllowed(true);

	}

	TGencls::~TGencls() {

	}


	int TGencls::Edit(int ActorID)
	{
		int result = 0;
		int ParamPointer = 0;
		String ParamName;
		String Param;
		int i = 0;
		int ret = -1;

		// continue parsing WITH contents of Parser
		ActiveGenclsObj = (TGenclsObj*)ElementList.Get_Active();
		ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveGenclsObj);
		result = 0;

		{
			auto thisobj = ActiveGenclsObj;
			ParamPointer = 0;
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
			while (Param.size() > 0)
			{
				if (ParamName.size() == 0)
					++ParamPointer;
				else
					ParamPointer = CommandList.Getcommand(ParamName);
				if ((ParamPointer > 0) && (ParamPointer <= NumProperties))
					thisobj->Set_PropertyValue(ParamPointer,Param);
				switch (ParamPointer)
				{
				case 	0:
					DoSimpleMsg(String("Unknown parameter \"") + ParamName
						+ "\" for Object \""
						+ Class_Name
						+ "."
						+ thisobj->get_Name()
						+ "\"", 9000);
					break;
				case 	1:
					thisobj->bus_name = Parser[ActorID]->MakeString_();
					break;
				case 	2:
					thisobj->gen_name = Parser[ActorID]->MakeString_();
					// Try to find generator in generator list
					ret = FindGenerator();
					break;
				case 	3:
					thisobj->h = Parser[ActorID]->MakeDouble_();
					break;
				case 	4:
					thisobj->d = Parser[ActorID]->MakeDouble_();
					break;
				default:
					ClassEdit(ActiveGenclsObj, ParamPointer - NumPropsThisClass);
					break;
				}

				ParamName = Parser[ActorID]->GetNextParam();
				Param = Parser[ActorID]->MakeString_();
			}
			//thisobj->RecalcElementData(ActorID);

		}

		return result;
	}

	int TGencls::NewObject(const String ObjName) {

		// Make a new Gencls model and add it to Gencls class list

		int result = 0;
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TGenclsObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
		/*if(result)
			result = FindGenerator();
		*/
		return result;
	}

	int TGencls::FindGenerator()
	{
		auto thisobj = ActiveGenclsObj;		// For the active gencls model, set a pointer to its generator model
		TDSSClass* GenClass = nullptr;
		TGeneratorObj* Gen = nullptr;
		GenClass = (TDSSClass*)GetDSSClassPtr("generator");
		Gen = (TGeneratorObj*)GenClass->Find(thisobj->gen_name);
		if (ASSIGNED(Gen) && ((TDSSCktElement*)Gen)->Get_Enabled()) {		// If generator is found and is enabled, then set pointer
			Gen->gen_model = thisobj;										// C++ will allow this just fine since thisobj is a derived class from T
			thisobj->pGen = Gen;											// Set this object generator pointer for easy access
			if (Gen->gen_model != nullptr)
				return 0;
		}
		else {
			// If it gets to this point, generator was either not found, or casting was incorrect, or was not online/enabled
			DoSimpleMsg(String("Gencls model \"") + thisobj->get_Name()
				+ "\" could not find generator \"" + thisobj->gen_name
				+ "\". Generator could be missing or not enabled \"", 9002);
			return -1;
		}
		return -1;
	}


	//--------------------------------------------------------------//
	//																//
	//					TGenclsObj Functions						//
	//																//
	//--------------------------------------------------------------//



	TGenclsObj::TGenclsObj() :
		ibase(1.0),
		mbase(1.0),
		vbase(1.0),
		isorce{ 1.0, 1.0 },
		isorcepu{ 1.0,1.0 },
		isorce2{ 1.0,1.0 },
		isorce2pu{ 1.0,1.0 },
		iterminit{ 1.0,1.0 },
		vsorce{ 1.0,1.0 },
		vsorcepu{ 1.0,1.0 },
		zsorce{ 1.0,1.0 },
		zsorcepu{ 1.0,1.0 }
	{
		nstates = 2;
		nchannels = 6;
		// States
		ps.reserve(nstates);
		s = new double(nstates);
		ss = new double(nstates);
		ds = new double(nstates);
		dss = new double(nstates);
		// Parameters
		h = 0.0;
		d = 0.0;
		//	Variables
		pmech = 0.;
		pgen = 0.;
		qgen = 0.;
		tm = 0.;
		te = 0.;
		dspd = 0.0;
		ang = 0.0;
		spd = 0.0;
		w0 = 0.0;
		ed = 0.0;
		eq = 0.0;
		id = 0.0;
		iq = 0.0;
		// Others
		pGen = nullptr;
		FListSize = 0;
	}

	TGenclsObj::TGenclsObj(DSSClass::TDSSClass* ParClass, const String GenclsName) :
		inherited(ParClass),
		ibase(1.0),
		mbase(1.0),
		vbase(1.0),
		isorce{ 1.0, 1.0 },
		isorcepu{ 1.0,1.0 },
		isorce2{ 1.0,1.0 },
		isorce2pu{ 1.0,1.0 },
		iterminit{ 1.0,1.0 },
		vsorce{ 1.0,1.0 },
		vsorcepu{ 1.0,1.0 },
		zsorce{ 1.0,1.0 },
		zsorcepu{ 1.0,1.0 }
	{
		Set_Name(LowerCase(GenclsName));
		DSSObjType = ParClass->DSSClassType;
		cBuffer.clear();
		//{
		//	auto thisobj = ActiveGenclsObj;
		//	thisobj->bus_name = "none";
		//	thisobj->gen_name = "none";
		//	thisobj->h = 2.0;
		//	thisobj->d = 1.0;
		//}
		InitPropertyValues(0);

		//  RecalcElementData;

		//	Initialize members
		nstates = 2;
		nchannels = 6;
		// States
		s = new double[nstates];
		ss = new double[nstates];
		ds = new double[nstates];
		dss = new double[nstates];
		// Parameters
		h = 0.0;
		d = 0.0;
		//	Variables
		pmech = 0.;
		pgen = 0.;
		qgen = 0.;
		tm = 0.;
		te = 0.;
		dspd = 0.0;
		ang = 0.0;
		spd = 0.0;
		w0 = 0.0;
		ed = 0.0;
		eq = 0.0;
		id = 0.0;
		iq = 0.0;
		// Others
		pGen = nullptr;
		FListSize = 0;
	}




	TGenclsObj::~TGenclsObj() {
		delete[] s;
		delete[] ss;
		delete[] ds;
		delete[] dss;
	}

	void TGenclsObj::LoadDefaultParameters() {
		// These are the deafault parameters for this model
		h = 3.0;
		d = 0.0;
	}

	int TGenclsObj::InitializeStates(int ActorID)
	{
		// Get generator terminal current and terminal voltage

		/*VNeut,*/
		int i = 0;
		complex V012[3/*# range 0..2*/]{ 0.0,0.0,0.0,0.0,0.0,0.0 };
		complex I012[3/*# range 0..2*/]{ 0.0,0.0,0.0,0.0,0.0,0.0 };
		complex Vabc[4/*# range 1..3*/]{ 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0 };
		complex vterm;								//	Terminal voltage in V and abc ref frame
		complex iterm{ 0.0,0.0 }, itermpu{ 0.0,0.0 };	//	Terminal current in A and abc ref frame
		complex sgen;
		complex Edq;
		complex Idq;				//	Terminal current in dq reference frame
		//double ed, eq;			//	Source voltages in dq reference frame



		//	Get voltage and MVA bases for generator.  All generator variables and parameters will be in pu and these are the bases used to 
		double vbase = pGen->GenVars.kVGeneratorBase * 1000;				//	Voltage base LL in Volts
		double mbase = pGen->GenVars.kVArating * 1000;					//	MVA base of generator, in VA
		double ibase = mbase / (sqrt(3) * vbase);							//	Ibase in A for current pu-nitization
		double zbase = vbase * vbase / mbase;							//	Impedance base in Ohms
		pGen->Set_YprimInvalid(ActorID,true);  // Force rebuild of YPrims on the generator

		// This stuff will have to come from the new generator models, so should come from gencls, genrou, etc
		auto& thisgen = pGen->GenVars;
		thisgen.Zthev = cmplx(thisgen.Xdp / thisgen.XRdp, thisgen.Xdp);			//	Set Zthevenin based on Xdp and XRdp, where XRdp = Xdp/Rdp 

		pGen->Yeq = cinv(thisgen.Zthev);										//	Rebuilds Yeq or Ythevenin
		zsorce = thisgen.Zthev;													//	In ohms
		zsorcepu = cdivreal(zsorce, zbase);										//	In ohms

		/*Compute nominal Positive sequence voltage behind transient reactance*/
		if (pGen->GenON)
		{

			//	Get terminal current and put it in 012 reference frame
			vterm = pGen->GetVterminal(ActorID);		// Positive sequence voltage - LN

			// Calculate terminal current and convert from abc to 012
			pGen->ComputeIterminal(ActorID);
			Phase2SymComp(&(pGen->Iterminal[0]), & (I012[0]));
			iterm = cmulreal(I012[1], -1.0);		//	Making iterm convention positive when leaving the generator terminals
			iterminit[0] = I012[0];
			iterminit[1] = I012[1];
			iterminit[2] = I012[2];

			//	Calculate vsource - in Volts
			vsorce = cadd(vterm, cmul(iterm, zsorce));

			//	Convert put it in DQ reference frame with network angle as the reference - q axis leading d axis
			//	The angle theta is used to convert from positive sequence to dq reference frame
			//	theta is the angle of the terminal bus voltage

			double theta = cang(vsorce);

			//	Put vsorce in pu and calculate isorce in pu;
			vsorcepu = cdivreal(vsorce, (vbase / SQRT3));
			Edq = seq2dq(vsorcepu, theta);			//	LN
			ed = Edq.re;							//	ed in pu
			eq = Edq.im;							//	eq in pu

			//	Calculate isorce in Amps and pu
			isorce = cdiv(vsorce, zsorce);
			isorcepu = cdiv(vsorcepu, zsorcepu);
			Idq = seq2dq(isorcepu, theta);
			id = Idq.re;
			iq = Idq.im;

			//	Get total apparent power at the terminal (sgen) and put in pu
			sgen = pGen->Get_Power(1, ActorID);		//	Total power in Watts and VAr
			sgen.re = -1 * sgen.re / (mbase);			//	mbase is in VA 
			sgen.im = -1 * sgen.im / (mbase);

			pgen = sgen.re;
			qgen = sgen.im;
			pmech = pgen;

			// Initialize state 0 - rotor angle
			ang = cang(vsorce);
			this->s[0] = ang;
			this->ss[0] = ang;
			this->ds[0] = 0.0;


			// Initialize state 1 - rotor speed
			spd = 1.0;
			dspd = 0.;
			this->s[1] = 1.0;
			this->ss[1] = 1.0;
			this->ds[1] = 0.0;

			//	Sanity check - Let's calculate sgen just for the sake of validation
			//double sout;
			//sout = 3 * cabs(vterm) * cabs(iterm);
			//double angdiff, pout, qout;
			//angdiff = cang(vterm) - cang(iterm);
			//pout = sout * cos(angdiff);
			//qout = sout * sin(angdiff);

		}
		else {
			return -1;
			DoSimpleMsg(Format(("Generator " + pGen->get_Name() + " is offline.").c_str()), 9004);
		}

		return 0;
	}

	int TGenclsObj::CalculateRate(int ActorID)
	{
		complex vterm;
		double err;
		double te;
		double w0 = 2 * PI * DefaultBaseFreq;
		vterm = pGen->GetVterminal(ActorID);

		double mbase = pGen->GenVars.kVArating * 1000.;		// MVA base of generator, converted from kVA
		complex sgen;

		//	Get total apparent power at the terminal (sgen) and put in pu
		sgen = pGen->Get_Power(1, ActorID);
		sgen.re = -1 * sgen.re / mbase;
		sgen.im = -1 * sgen.im / mbase;

		pgen = sgen.re;
		qgen = sgen.im;
		//	pmech = pgen;	---> this should come from the turbine governor model
		// te = pgen * this->s[1];
		te = pgen;

		dspd = this->s[1] - 1.0;
		spd = this->s[1];
		ang = this->s[0];


		//err = ((pmech - dspd * d) / spd) - te;
		err = ((pmech - dspd * d)) - te;

		//	Calculate ds[1]
		if (h > 0)
			this->ds[1] = err / (2 * h);
		else
			this->ds[1] = 0.;

		//	Calculate ds[0]
		this->ds[0] = (spd - 1.0) * w0;

		return 0;
	}

	int TGenclsObj::StateIntegration(int ActorID)
	{
		//	Call global integration method of choice

		double h = ActiveCircuit[ActorID]->Solution->DynaVars.h;

		//	HEre we are calling Adams-Bashforth second order.  
		//	Other methods can be implemented and each model can call a specific method.  
		//	That is the beauty of this modular implementation
		// IntegrationMethods::trapezoidal<TGenclsObj>(this, &h);
		IntegrationMethods::ab2order<TGenclsObj>(this, &h);
		// IntegrationMethods::euler<TGenclsObj>(this, &h);

		//this->nstates;
		return 0;
	}

	int TGenclsObj::StateIntegration_correction(int ActorID)
	{
		//	Call global integration method of choice

		double h = ActiveCircuit[ActorID]->Solution->DynaVars.h;

		//	HEre we are calling Adams-Bashforth second order.  
		//	Other methods can be implemented and each model can call a specific method.  
		//	That is the beauty of this modular implementation
		IntegrationMethods::trapezoidal<TGenclsObj>(this, &h);
		// IntegrationMethods::euler<TGenclsObj>(this, &h);
		// IntegrationMethods::ab2order<TGenclsObj>(this, &h);

		//this->nstates;
		return 0;
	}

	complex* TGenclsObj::CalculateIsorce(int ActorID, complex *I012)
	{
		//	Calculate Isorce
		complex V012[3];			//	sequence terminal voltage
		//complex I012[3];			//	sequence terminal voltage
		complex isorce2, isorce2pu;	//	neagtive sequence source currents
		complex ithev;				//	Current down zthev impedance
		complex iterm;				//	Current injected into the terminal
		double	vbase, ibase, zbase, mbase;
		complex vterm;				//	This is the positive sequence terminal voltage in Volts, abc reference frame
		complex vtdq;				//	This is the positive sequence terminal voltage in Volts, dq reference frame
		double theta;				//	This is the angular reference used to convert from/from abc/dq reference frames
		polar vtermll;
		complex Edq;
		complex iterm2, iterm0;

		//complex vsorce, isorce, vsorcepu, isorcepu, zsorce, zsorcepu;

		//	Define bases
		vbase = pGen->GenVars.kVGeneratorBase * 1000;	//	vbase LN in Volts
		mbase = pGen->GenVars.kVArating * 1000.;		//	mbase in VA
		ibase = mbase / (SQRT3 * vbase);				//	ibase in A
		zbase = vbase * vbase / mbase;					//	zbase in Ohms

		//	Update the rotation angle
		theta = s[0];

		zsorce = { pGen->GenVars.Zthev.re, pGen->GenVars.Zthev.im };
		zsorcepu = cdivreal(zsorce, zbase);

		//	Calculate internal voltage source 
		Edq = { ed, eq };								//	Retrieve values in pu
		vsorcepu = dq2seq(Edq, theta);
		vsorce = cmulreal(vsorcepu, (vbase / SQRT3));

		//	Calculate isorcepu - this is the positive sequence current injection in pu
		isorce = cdiv(vsorce, zsorce);					// In amps

		//	Get positive sequence terminal voltage in Volts
		vterm = pGen->GetVterminal(ActorID);	//	LN
		ithev = cdiv(vterm, zsorce);

		//Iterminal = Isource - Ithevenin (current on Zthev);
		iterm = csub(isorce, ithev);
		I012[1] = cmulreal(iterm, -1.0);

		//	Sanity check on total power out of generator
		complex sgen = cmulreal(cmul(vterm, conjg(iterm)), 3);

		//	iterm is the positive sequence current in Amps. 
		//	Now need to calculate zero and negative sequence current at the terminals, then update Iterminal

		//**************  fix this tomorrow ****************************

		Phase2SymComp(&(pGen->Vterminal[0]), &V012[0]);
		I012[2] = cdiv(V012[2], cmplx(0.0, pGen->GenVars.Xdpp));				// machine use Xd"		
		I012[0] = { 0.0,0.0 };	//	Setting zero seq current here to zero but this will be recalculated as soon as we stop out of this function
		//complex *pI012;
		//pI012 = I012;

		return I012;
	}

	complex TGenclsObj::GetIsorce1(int ActorID)
	{
		complex I1;
		I1 = cmulreal(this->isorcepu, ibase);

		return I1;
	}

	complex TGenclsObj::GetIsorce2(int ActorID)
	{
		return complex();
	}

	double TGenclsObj::get_channel(int i)
	{
		double result = 0.0;
		int n = 0;
		int k = 0;
		n = 0;
		result = -9999.99;  // error return value
		if (i < 1)
			return result;  // Someone goofed
		switch (i) {
		case 	1:
			result = this->spd;		//	speed in pu
			break;
		case 	2:
			result = this->ang;		//	ang in radians
			break;
		case 	3:
			result = this->ed;		//	ed in pu
			break;
		case 	4:
			result = this->pgen;	//	pgen in pu
			break;
		case 	5:
			result = this->qgen;	//	qgen in pu
			break;
		case 	6:
			result = this->pmech;	//	pmech in pu
			break;
		default:
			break;
		}
		return result;
	}

	String TGenclsObj::get_channel_header(int i)
	{
		String result;
		switch (i) {
		case 	1:
			result = "spd(pu)";
			break;
		case 	2:
			result = " ang(rad)";
			break;
		case 	3:
			result = " Ed(pu)";
			break;
		case 	4:
			result = " pgen(pu)";
			break;
		case 	5:
			result = " qgen(pu)";
			break;
		case 	6:
			result = " pmech(pu)";
			break;
		default:
			break;
		}
		return result;
	}

	int64_t TGenclsObj::get_number_channels()
	{
		return nchannels;
	}


	/*void TGenclsObj::MakePosSequence(int ActorID) {

	}*/

	/*void TGenclsObj::CalcYPrim(int ActorID){

	}*/

	void TGenclsObj::sample(int ActorID) {
		// Empty for now, but need to see if anything needs to be added here
	}
	
	void TGenclsObj::DoPendingAction(int Code, int ProxyHdl, int ActorID) {
		// Empty for now, but need to see if anything needs to be added here
	}
	
	void TGenclsObj::Reset(int ActorID) {
		// Empty for now, but need to see if anything needs to be added here
	}

	/*void TGenclsObj::RecalcElementData(int ActorID){

	}*/

	/*void TGenclsObj::GetCurrents(Ucomplex::pComplexArray Curr, int ActorID) {

	}

	void TGenclsObj::GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID) {

	}*/
	void TGenclsObj::InitPropertyValues(int ArrayOffset) {
		Set_PropertyValue(1,"");		// bus_name;
		Set_PropertyValue(2,"");		// gen_name;
		Set_PropertyValue(3,"3.0");	// h;
		Set_PropertyValue(4,"1.0");	// d;
		inherited::InitPropertyValues(NumPropsThisClass);
	}

	/*
	void TGenclsObj::DumpProperties(System::TTextRec& f, bool Complete) {

	}
	*/
}
