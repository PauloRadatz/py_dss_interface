
#include "System.h"
#include "Sysutils.h"
#include "fstream"

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
#include "genrou.h"
#include <string>
#include "IntegrationMethods.h"
#include "klusolve.h"



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


namespace Genrou
{

	TGenrouObj* ActiveGenrouObj = nullptr;
	const int NumPropsThisClass = 19;		//	Number of properties in this class

	void TGenrou::DefineProperties()
	{
		NumProperties = NumPropsThisClass;
		CountProperties();   // Get inherited property count
		AllocatePropertyArrays();


		// Define Property names
		PropertyName[0] = "bus_name";
		PropertyName[1] = "gen_name";
		PropertyName[2] = "tpdo";
		PropertyName[3] = "tppdo";
		PropertyName[4] = "tpqo";
		PropertyName[5] = "tppqo";
		PropertyName[6] = "h";
		PropertyName[7] = "d";
		PropertyName[8] = "xd";
		PropertyName[9] = "xq";
		PropertyName[10] = "xpd";
		PropertyName[11] = "xpq";
		PropertyName[12] = "xppd";
		PropertyName[13] = "xl";
		PropertyName[14] = "s1";
		PropertyName[15] = "s12";
		PropertyName[16] = "ra";
		PropertyName[17] = "rcomp";
		PropertyName[18] = "xcomp";
		PropertyHelp[0] = "Bus name where the model is connected to";
		PropertyHelp[1] = "Generator name where model is connected to";
		PropertyHelp[2] = "d-axis transient rotor time constant";
		PropertyHelp[3] = "d-axis subtransient rotor time constant";
		PropertyHelp[4] = "q-axis transient time constant";
		PropertyHelp[5] = "q-axis subtransient time constant";
		PropertyHelp[6] = "inertia constant";
		PropertyHelp[7] = "damping factor";
		PropertyHelp[8] = "d-axis synchornous reactance";
		PropertyHelp[9] = "q-axis synchronous reactance";
		PropertyHelp[10] = "d-axis transient reactance";
		PropertyHelp[11] = "q-axis transient reactance";
		PropertyHelp[12] = "d-axis subtransient reactance";
		PropertyHelp[13] = "stator leakage reactance";
		PropertyHelp[14] = "saturation factor at 1pu flux";
		PropertyHelp[15] = "saturation factor at 1.2pu flux";
		PropertyHelp[16] = "stator resistance";
		PropertyHelp[17] = "compounding resistance for voltage control";
		PropertyHelp[18] = "compounding reactance for voltage control";
		ActiveProperty = NumPropsThisClass - 1;
		inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
	}

	int TGenrou::MakeLike(const String GenrouName)
	{
		int result = 0;
		TGenrouObj* OtherGenrou = nullptr;
		int i = 0;
		result = 0;
		/*See if we can find this GenrouName in the present collection*/
		OtherGenrou = ((TGenrouObj*)Find(GenrouName));
		if (OtherGenrou != nullptr)
		{
			auto thisobj = ActiveGenrouObj;
			int stop = 0;
			thisobj->bus_name = OtherGenrou->bus_name;

			for (i = 1; i <= thisobj->ParentClass->NumProperties; i++)
			{
				thisobj->Set_PropertyValue(i,OtherGenrou->Get_PropertyValue(i));
			}
		}
		else
			DoSimpleMsg(String("Error in Genrou MakeLike: \"") + GenrouName
				+ "\" Not Found.", 9001);
		return result;
	}
	//	Constructor and Destructor
	TGenrou::TGenrou() {

		Class_Name = "Genrou";
		DSSClassType = DSSClassType + GENROU_ELEMENT;
		DefineProperties();
		CommandList = TCommandList(Slice((PropertyName), NumProperties), NumProperties);
		CommandList.set_AbbrevAllowed(true);

	}

	TGenrou::~TGenrou() {

	}


	int TGenrou::Edit(int ActorID)
	{
		int result = 0;
		int ParamPointer = 0;
		String ParamName;
		String Param;
		int i = 0;
		int ret = -1;

		// continue parsing WITH contents of Parser
		ActiveGenrouObj = (TGenrouObj*)ElementList.Get_Active();
		ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveGenrouObj);
		result = 0;

		{
			auto thisobj = ActiveGenrouObj;
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
					thisobj->tpdo = Parser[ActorID]->MakeDouble_();
					break;
				case 	4:
					thisobj->tppdo = Parser[ActorID]->MakeDouble_();
					break;
				case 	5:
					thisobj->tpqo = Parser[ActorID]->MakeDouble_();
					break;
				case 	6:
					thisobj->tppqo = Parser[ActorID]->MakeDouble_();
					break;
				case 	7:
					thisobj->h = Parser[ActorID]->MakeDouble_();
					break;
				case 	8:
					thisobj->d = Parser[ActorID]->MakeDouble_();
					break;
				case 	9:
					thisobj->xd = Parser[ActorID]->MakeDouble_();
					break;
				case 	10:
					thisobj->xq = Parser[ActorID]->MakeDouble_();
					break;
				case 	11:
					thisobj->xpd = Parser[ActorID]->MakeDouble_();
					break;
				case 	12:
					thisobj->xpq = Parser[ActorID]->MakeDouble_();
					break;
				case 	13:
					thisobj->xppd = Parser[ActorID]->MakeDouble_();
					break;
				case 	14:
					thisobj->xl = Parser[ActorID]->MakeDouble_();
					break;
				case 	15:
					thisobj->s1 = Parser[ActorID]->MakeDouble_();
					break;
				case 	16:
					thisobj->s12 = Parser[ActorID]->MakeDouble_();
					break;
				case 	17:
					thisobj->ra = Parser[ActorID]->MakeDouble_();
					break;
				case 	18:
					thisobj->rcomp= Parser[ActorID]->MakeDouble_();
					break;
				case 	19:
					thisobj->xcomp = Parser[ActorID]->MakeDouble_();
					break;
				default:
					ClassEdit(ActiveGenrouObj, ParamPointer - NumPropsThisClass);
					break;
				}

				ParamName = Parser[ActorID]->GetNextParam();
				Param = Parser[ActorID]->MakeString_();
			}
			//thisobj->RecalcElementData(ActorID);

		}

		return result;
	}

	int TGenrou::NewObject(const String ObjName) {

		// Make a new Genrou model and add it to Genrou class list

		int result = 0;
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TGenrouObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
		/*if(result)
			result = FindGenerator();
		*/
		return result;
	}

	int TGenrou::FindGenerator()
	{
		auto thisobj = ActiveGenrouObj;		// For the active genrou model, set a pointer to its generator model
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
			DoSimpleMsg(String("Genrou model \"") + thisobj->get_Name()
				+ "\" could not find generator \"" + thisobj->gen_name
				+ "\". Generator could be missing or not enabled \"", 9002);
			return -1;
		}
		return -1;
	}


	//--------------------------------------------------------------//
	//																//
	//					TGenrouObj Functions						//
	//																//
	//--------------------------------------------------------------//



	TGenrouObj::TGenrouObj() :
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
		tpdo = 0.0;
		tppdo = 0.0;
		tpqo = 0.0;
		tppqo = 0.0;
		h = 0.0;
		d = 0.0;
		xd = 0.0;
		xq = 0.0;
		xpd = 0.0;
		xpq = 0.0;
		xppd = 0.0;
		xl = 0.0;
		s1 = 0.0;
		s12 = 0.0;
		ra = 0.0;
		rcomp = 0.0;
		xcomp = 0.0;
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
		efd = 0.0;
		ladifd = 0.0;
		fd = 0.0;
		fpd = 0.0;
		fppd = 0.0;
		fq = 0.0;
		fpq = 0.0;
		fppq = 0.0;
		// Others
		pGen = nullptr;
		FListSize = 0;
	}

	TGenrouObj::TGenrouObj(DSSClass::TDSSClass* ParClass, const String GenrouName) :
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
		Set_Name(LowerCase(GenrouName));
		DSSObjType = ParClass->DSSClassType;
		cBuffer.clear();

		InitPropertyValues(0);

		//  RecalcElementData;

		//	Initialize members
		nstates = 6;
		nchannels = 6;
		// States
		s = new double[nstates];
		ss = new double[nstates];
		ds = new double[nstates];
		dss = new double[nstates];
		// Parameters
		tpdo = 0.0;
		tppdo = 0.0;
		tpqo = 0.0;
		tppqo = 0.0;
		h = 0.0;
		d = 0.0;
		xd = 0.0;
		xq = 0.0;
		xpd = 0.0;
		xpq = 0.0;
		xppd = 0.0;
		xl = 0.0;
		s1 = 0.0;
		s12 = 0.0;
		ra = 0.0;
		rcomp = 0.0;
		xcomp = 0.0;
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
		efd = 0.0;
		ladifd = 0.0;
		fd = 0.0;
		fpd = 0.0;
		fppd = 0.0;
		fq = 0.0;
		fpq = 0.0;
		fppq = 0.0;
		// Others
		pGen = nullptr;
		FListSize = 0;
	}




	TGenrouObj::~TGenrouObj() {
		delete[] s;
		delete[] ss;
		delete[] ds;
		delete[] dss;
	}

	void TGenrouObj::LoadDefaultParameters() {
		// These are the deafault parameters for this model
		tpdo = 7.0;
		tppdo = 0.030;
		tpqo = 0.75;
		tppqo = 0.05;
		h = 3.0;
		d = 0.0;
		xd = 2.1;
		xq = 2.0;
		xpd = 0.2;
		xpq = 0.5;
		xppd = 0.18;
		xl = 0.15;
		s1 = 0.05;
		s12 = 0.3;
		ra = 0.0;
		rcomp = 0.0;
		xcomp = 0.0;
	}

	int TGenrouObj::InitializeStates(int ActorID)
	{
		// Get generator terminal current and terminal voltage

		/*VNeut,*/
		int i = 0;
		complex V012[3/*# range 0..2*/]{ 0.0,0.0,0.0,0.0,0.0,0.0 };
		complex I012[3/*# range 0..2*/]{ 0.0,0.0,0.0,0.0,0.0,0.0 };
		complex Vabc[4/*# range 1..3*/]{ 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0 };
		complex vterm;													//	Terminal voltage in V and abc ref frame
		complex iterm{ 0.0,0.0 }, itermpu{ 0.0,0.0 };					//	Terminal current in A and abc ref frame
		complex sgen;													//	Complex power 
		complex Idq;													//	Terminal current in dq reference frame
		complex vtdq, vtermpu;											//	Terminal dq voltage and terminal in pu
		

		//	Get voltage and MVA bases for generator.  All generator variables and parameters will be in pu and these are the bases used to 
		this->vbase = (pGen->GenVars.kVGeneratorBase * 1000/SQRT3);	//	Voltage base LL in Volts
		this->mbase = pGen->GenVars.kVArating * 1000;					//	MVA base of generator, in VA
		this->ibase = mbase / (vbase * 3);								//	Ibase in A for current pu-nitization
		double zbase = (vbase) / ibase;									//	Impedance base in Ohms
		double sx;														//	Saturation adjustment to flux
		double aux1, aux2;												//	Local auxiliary variables
		double vmag, imag, delta;										//	Local variables for voltage magnitude, current magnitude and rotor angle
		
		pGen->Set_YprimInvalid(ActorID, true);								//	Force rebuild of YPrims on the generator 

		// This stuff will have to come from the new generator models, so should come from genrou, genrou, etc
		auto& thisgen = pGen->GenVars;
		if (this->xppd <= 0.0001) {
			DoSimpleMsg(Format(("Xppd value for Generator " + pGen->get_Name() + " is less or equal than zero.").c_str()), 90001);
			return -1;
		}
		else {
			thisgen.Zthev = cmulreal(cmplx(this->ra, this->xppd),zbase);						//	Set Zthev based on ra and x"d - zthev is is Ohms
			//thisgen.Zthev = cmplx(thisgen.Xdp / thisgen.XRdp, thisgen.Xdpp);
		}

		pGen->Yeq = cinv(thisgen.Zthev);										//	Rebuilds Yeq or Ythevenin
		zsorce = thisgen.Zthev;													//	In ohms
		zsorcepu = cdivreal(thisgen.Zthev,zbase);								//	In ohms

		/*Compute nominal Positive sequence voltage behind transient reactance*/
		if (pGen->GenON)
		{

			//	Get terminal current and put it in 012 reference frame
			vterm = pGen->GetVterminal(ActorID);		// Positive sequence voltage - LN
			vtermpu = cdivreal(vterm, vbase);
			vmag = cabs(vtermpu);
			
			// Calculate terminal current and convert from abc to 012
			pGen->ComputeIterminal(ActorID);
			Phase2SymComp(&(pGen->Iterminal[0]), & (I012[0]));
			iterm = cmulreal(I012[1], -1.0);		//	Extracting the positive sequence Making iterm convention positive when leaving the generator terminals
			itermpu = cdivreal(iterm, ibase);
			iterminit[0] = I012[0];
			iterminit[1] = I012[1];
			iterminit[2] = I012[2];
			imag = cabs(itermpu);
			
			sgen = cmulreal(cmul(vterm,conjg(iterm)), 3);
			complex sgenpu = cmul(vtermpu, conjg(itermpu));
			
			//	Calculate vsource - in Volts
			vsorce = cadd(vterm, cmul(iterm, zsorce));		// LN
			vsorcepu = cdivreal(vsorce, vbase);	
			aux1 = cang(vsorcepu);
			complex vsp = cadd(vtermpu, cmul(itermpu, zsorcepu));
			
			//	Initialize rotor angle - delta
			//	Step 1 - calculate er, ei
			double er = vtermpu.re + this->ra * itermpu.re - this->xppd * itermpu.im;
			double ei = vtermpu.im + this->ra * itermpu.im + this->xppd * itermpu.re;

			//	Step 2 - calculate saturation magnitude
			//	Calculate the saturation parameters asat and bsat
			if (s12 > 0.0) {
				this->asat = (1.0 - sqrt((this->s1*1.0) / (this->s12*1.2)) * 1.2) / (1.0 - sqrt((this->s1*1.0) / (this->s12*1.2)));
				this->bsat = this->s1 / (pow(1.0 - this->asat, 2));
			}
			else {
				this->asat = 0.0;
				this->bsat = 0.0;
				DoSimpleMsg(Format(("Saturation factor s12 for Generator " + pGen->get_Name() + " is less or equal than zero. s12 = " + to_string(this->s12)).c_str()), 90002);
			}
			double fppmag = sqrt(pow(er, 2) + pow(ei, 2));
			sx = QuadraticSaturation(fppmag);

			//	Step 3 - calculate intermediate variables
			double A = (sx * (this->xq - this->xl) / (this->xd - this->xl)) + 1;
			double B = (this->xq - this->xppd);

			//	Step 4 - Calculate rotor angle
			delta = atan((A * ei + B * itermpu.re) / (A * er - B * itermpu.im));

			//	Step 5 - calcualte ed, eq

			this->ed = er * sin(delta) - ei * cos(delta);		//	ed in pu
			this->eq = er * cos(delta) + ei * sin(delta);		//	eq in pu

			vtdq = seq2dq(vtermpu, delta);						//	terminal voltage in dq ref frame
			this->vtd = vtdq.re;
			this->vtq = vtdq.im;


			//	Calculate isorce in Amps and pu
			isorce = cdiv(vsorce, zsorce);
			isorcepu = cdiv(vsorcepu, zsorcepu);
						
			Idq = seq2dq(itermpu, delta);				//	This is the iterm current in pu -> iterm = isorce - ithev
			this->id = Idq.re;
			this->iq = Idq.im;

			//	Get total apparent power at the terminal (sgen) and put in pu
			sgen = pGen->Get_Power(1, ActorID);			//	Total power in Watts and VAr
			sgen.re = -1 * sgen.re / (mbase);			//	mbase is in VA 
			sgen.im = -1 * sgen.im / (mbase);

			this->pgen = sgen.re;
			this->qgen = sgen.im;
			this->pmech = this->pgen;
					
			//	Calculate machine flux 
			this->fppq = -this->ed;	
			this->fppd = this->eq;

			//	Calculate pq and pd to calculate electrical torque
			this->fq = this->fppq - this->iq * this->xppd;
			this->fd = this->fppd - this->id * this->xppd;
			this->te = this->fd * this->iq - this->fq * this->id;
					
			// Initialize state 0 - rotor angle
			ang = delta;
			this->s[0] = ang;
			this->ss[0] = ang;
			this->ds[0] = 0.0;

			// Initialize state 1 - rotor speed
			spd = 1.0;
			dspd = 0.0;
			this->s[1] = 1.0;
			this->ss[1] = 1.0;
			this->ds[1] = 0.0;

			//	Initialize state 2 - transient q-axis voltage (E'q)
			this->s[2] = this->fppd + this->id * (this->xpd - this->xppd);
			this->ss[2] = this->s[2];
			this->ds[2] = 0.;

			//	Initialize state 3 - transient d-axis flux (fpd)
			this->s[3] = this->s[2] - this->id * (this->xpd - this->xl);
			this->ss[3] = this->s[3];
			this->ds[3] = 0.;

			//// Initialize state 5
			this->s[5] = -this->fppq - this->iq * (this->xpq - this->xppd);
			this->ss[5] = this->s[5];
			this->ds[5] = 0.;
						
			// Initialize state 4
			this->s[4] = this->s[5] + this->iq * (this->xpq - this->xl);
			this->ss[4] = this->s[4];
			this->ds[4] = 0.;
						
			//	Set field current and voltage
			aux1 = this->s[2] - this->s[3] - this->id * (this->xpd - this->xl);
			aux2 = aux1 * ((this->xpd - this->xppd) / (pow((this->xpd - this->xl), 2.0))) + this->id * (this->xd - this->xpd);
			this->ladifd = this->s[2] + aux2 + sx * fppd;
			this->efd = this->ladifd;

		}
		else {
			DoSimpleMsg(Format(("Generator " + pGen->get_Name() + " is offline.").c_str()), 9004);
			return -1;
		}

		return 0;
	}

	int TGenrouObj::CalculateRate(int ActorID)
	{
		// Declaring variables
		complex I012[3/*# range 0..2*/]{ 0.0,0.0,0.0,0.0,0.0,0.0 };
		complex vterm, vtermpu;								//	Terminal voltage in V and abc ref frame
		complex iterm{ 0.0,0.0 }, itermpu{ 0.0,0.0 };		//	Terminal current in A and abc ref frame
		complex sgen;
		complex Edq;
		complex Idq;				//	Terminal current in dq reference frame

		double err;
		double aux1, aux2, aux3, aux4 = 0.0, aux5;
		double w0 = 2 * PI * DefaultBaseFreq;
		double vbase = pGen->GenVars.kVGeneratorBase * 1000/SQRT3;	//	Voltage base LL in Volts
		double mbase = pGen->GenVars.kVArating * 1000.;			// MVA base of generator, converted from kVA
		double ibase = mbase / (vbase*3);						//	Ibase in A for current pu-nitization
		double zbase = vbase / ibase;							//	Impedance base in Ohms
		double sx;

		vterm = pGen->GetVterminal(ActorID);

		//	Get total apparent power at the terminal (sgen) and put in pu
		sgen = pGen->Get_Power(1, ActorID);
		sgen.re = -1 * sgen.re / mbase;
		sgen.im = -1 * sgen.im / mbase;

		pgen = sgen.re;
		qgen = sgen.im;

		//	Save last value of pgen and qgen on plast, qlast
		//	These values will be used to initialize the Time series power flow generation profile
		this->plast = this->pgen;
		this->qlast = this->qgen;

		// If governor model exists, use pmech from governor model
		if (pGen->turb_model != nullptr)
		{
			this->pmech = pGen->turb_model->get_pmech();
		}
		else
		{
			this->pmech = this->pmech;
		}

		//	Get terminal current and put it in 012 reference frame
		vterm = pGen->GetVterminal(ActorID);		// Positive sequence voltage - LN
		vtermpu = cdivreal(vterm, vbase);

		// Calculate terminal current and convert from abc to 012
		pGen->ComputeIterminal(ActorID);
		Phase2SymComp(&(pGen->Iterminal[0]), & (I012[0]));
		iterm = cmulreal(I012[1], -1.0);			//	Making iterm convention positive when leaving the generator terminals
		iterminit[0] = I012[0];
		iterminit[1] = I012[1];
		iterminit[2] = I012[2];

		//	Calculate vsource - in Volts
		vsorce = cadd(vterm, cmul(iterm, zsorce));

		//	Convert put it in DQ reference frame with network angle as the reference - q axis leading d axis
		//	The angle theta is used to convert from positive sequence to dq reference frame
		//	theta is the angle of the terminal bus voltage

		double delta = this->s[0];

		//	Put vsorce in pu and calculate isorce in pu;
		vsorcepu = cdivreal(vsorce, vbase);
		Edq = seq2dq(vsorcepu, delta);				//	LN
		this->ed = Edq.re;							//	ed in pu
		this->eq = Edq.im;							//	eq in pu

		//	Calculate isorce in Amps and pu
		isorce = cdiv(vsorce, zsorce);
		isorcepu = cdiv(vsorcepu, zsorcepu);

		//	Calculate angle between terminal current and terminal voltage
		//sigma = atan(isorce.im / isorce.re);		//	Terminal current angle
		//alpha = atan(vterm.im / vterm.re);			//	Terminal voltage angle
		//phi = alpha - sigma;						//	Angle between voltage and current

		itermpu = cdivreal(iterm, ibase);

		Idq = seq2dq(itermpu, delta);				//	This is the terminal current - includes isorce + ithev
		this->id = Idq.re;
		this->iq = Idq.im;


		//	Must update fppd and fppq from states
		this->fppd =	this->s[3] * ((this->xpd - this->xppd) / (this->xpd - this->xl)) +
			this->s[2] * ((this->xppd-this->xl) / (this->xpd-this->xl));

		this->fppq = -1.*((this->s[4] * ((this->xpq - this->xppd) / (this->xpq - this->xl))) +
			(this->s[5] * (this->xppd - this->xl) / (this->xpq - this->xl)));
		
		//	Update fpd, fpq
		this->fpd = this->s[3];
		this->fpq = this->s[4];

		
		//	Calculate electrical torque - something doesn't add up here
		this->fq = this->fppq - this->iq * this->xppd;
		this->fd = this->fppd - this->id * this->xppd;
		this->te = this->fd*this->iq - this->fq*this->id;

		//	Assign delta speed and speed
		this->dspd = this->s[1] - 1.0;
		this->spd = this->s[1];
		this->ang = this->s[0];

		//err = ((pmech - dspd * d) / spd) - pe;
		err = ((this->pmech - this->dspd * this->d)/this->s[1]) - (this->fppd*iq - this->fppq*id);

		//	Calculate ds[0]
		this->ds[0] = (spd - 1.0) * w0;
		
		//	Calculate ds[1]
		if (h > 0)
			this->ds[1] = err / (2 * h);
		else
			this->ds[1] = 0.;

		//	Calculate ds[2]
		aux1 = this->s[2] - this->s[3] - this->id * (this->xpd - this->xl);
		aux2 = (aux1 * ((this->xpd - this->xppd) / (pow((this->xpd - this->xl), 2.0))) + this->id) * (this->xd - this->xpd);
		
		//	Saturation
		double er = vtermpu.re + this->ra * itermpu.re - this->xppd * itermpu.im;
		double ei = vtermpu.im + this->ra * itermpu.im + this->xppd * itermpu.re;
		double fppmag = sqrt(pow(this->fppd, 2) + pow(this->fppq, 2));

		sx = QuadraticSaturation(fppmag);
		
		//this->ladifd = sx * this->fppd + this->s[2] + aux2;
				
		//	If excitation system model exits, use efd from exciter model 
		if (pGen->exc_model != nullptr) {
			this->efd = pGen->exc_model->get_efield();
		}
		else {	//	If the generator doesn't have an excitation system model, leave the value of efd unchanged
			this->efd = this->efd;
		}

		//	Calculate ds[3]
		double err1 = this->s[2] - this->s[3] - this->id * (this->xpd - this->xl);
		this->ds[3] = err1/this->tppdo;
		
		//	Calculate ds[2]
		this->ladifd = sx*this->fppd + this->s[2] + (this->xd - this->xpd) * (this->id + err1 * (this->xpd - this->xppd) / pow(this->xpd - this->xl, 2.0));
		this->ds[2] = (this->efd - this->ladifd) / this->tpdo;
				
		//	Calculate ds[4]
		double err2 = this->s[5] - this->s[4] + this->iq * (this->xpq - this->xl);
		this->ds[4] = err2 / this->tppqo;

		//	Calculate ds[5]
		aux5 = (this->iq - (err2 * ((this->xpq - this->xppd) / pow(this->xpq - this->xl ,2))) ) * (this->xq - this->xpq);
		aux3 = sx * this->fppq * (this->xq - this->xl) / (this->xd - this->xl);
		this->ds[5] = (aux3 - this->s[5] + aux5)/this->tpqo;
				

		return 0;
	}

	int TGenrouObj::StateIntegration(int ActorID)
	{
		//	Call global integration method of choice

		double h = ActiveCircuit[ActorID]->Solution->DynaVars.h;

		//	HEre we are calling Adams-Bashforth second order.  
		//	Other methods can be implemented and each model can call a specific method.  
		//	That is the beauty of this modular implementation
		// IntegrationMethods::trapezoidal<TGenrouObj>(this, &h);
		IntegrationMethods::ab2order<TGenrouObj>(this, &h);
		// IntegrationMethods::euler<TGenrouObj>(this, &h);

		//this->nstates;
		return 0;
	}

	int TGenrouObj::StateIntegration_correction(int ActorID)
	{
		//	Call global integration method of choice

		double h = ActiveCircuit[ActorID]->Solution->DynaVars.h;

		//	HEre we are calling Adams-Bashforth second order.  
		//	Other methods can be implemented and each model can call a specific method.  
		//	That is the beauty of this modular implementation
		// IntegrationMethods::trapezoidal<TGenrouObj>(this, &h);
		IntegrationMethods::euler<TGenrouObj>(this, &h);
		// IntegrationMethods::ab2order<TGenrouObj>(this, &h);

		//this->nstates;
		return 0;
	}

	complex* TGenrouObj::CalculateIsorce(int ActorID, complex* I012)
	{
		//	Calculate Isorce
		complex V012[3]		= {CZero, CZero, CZero};			//	sequence terminal voltage
		complex isorce2		= CZero, 
				isorce2pu	= CZero,	//	neagtive sequence source currents
				ithev		= CZero,				//	Current down zthev impedance
				iterm		= CZero,				//	Current injected into the terminal
				vterm		= CZero,				//	This is the positive sequence terminal voltage in Volts, abc reference frame
				vtdq		= CZero,				//	This is the positive sequence terminal voltage in Volts, dq reference frame
				Edq			= CZero,
				iterm2		= CZero, 
				iterm0		= CZero;
		double	vbase		= 0.0, 
				ibase		= 0.0, 
				zbase		= 0.0, 
				mbase		= 0.0,
				theta		= 0.0;				//	This is the angular reference used to convert from/from abc/dq reference frames
		polar	vtermll		= ctopolar(CZero);


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
		this->ed = -this->fppq;
		this->eq = this->fppd;
		Edq = { this->ed, this->eq };								//	Retrieve values in pu
		vsorcepu = dq2seq(Edq, theta);
		vsorce = cmulreal(vsorcepu, (vbase / SQRT3));

		//	Calculate isorcepu - this is the positive sequence current injection
		isorce = cdiv(vsorce, zsorce);					// In amps

		//	Get positive sequence terminal voltage in Volts
		vterm = pGen->GetVterminal(ActorID);			//	LN
		ithev = cdiv(vterm, zsorce);

		//Iterminal = Isource - Ithevenin (current on Zthev);
		iterm = csub(isorce, ithev);
		I012[1] = cmulreal(iterm, -1.0);

		//	Sanity check on total power out of generator
		complex sgen = cmulreal(cmul(vterm, conjg(iterm)), 3);

		//	iterm is the positive sequence current in Amps. 
		//	Now need to calculate zero and negative sequence current at the terminals, then update Iterminal

		Phase2SymComp(&(pGen->Vterminal[0]), &V012[0]);
		I012[2] = cdiv(V012[2], cmplx(0.0, pGen->GenVars.Xdpp));				// machine use Xd"		
		I012[0] = { 0.0,0.0 };	//	Setting zero seq current here to zero but this will be recalculated as soon as we stop out of this function
		
		return I012;
	}

	complex TGenrouObj::GetIsorce1(int ActorID)
	{
		complex I1;
		I1 = cmulreal(this->isorcepu, ibase);

		return I1;
	}

	complex TGenrouObj::GetIsorce2(int ActorID)
	{
		//	nothing here yet
		return complex();
	}

	double TGenrouObj::get_channel(int i)
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
			result = this->s[0];	//	ang in radians
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

	String TGenrouObj::get_channel_header(int i)
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

	int64_t TGenrouObj::get_number_channels()
	{
		return nchannels;
	}

	double TGenrouObj::QuadraticSaturation(double &fppmag)
	{
		double satx = 0.0;								//	Incremental saturated voltage to be added to d and q axis
		double avgflux = 0.0;

		avgflux = fppmag;								//	Average flux

		satx = this->bsat * pow((avgflux - this->asat), 2.0);
		satx = satx / avgflux;

		return satx;
	}

	//int64_t TGenrouObj::call_iterative_solver(void)
	//{
	//	/*
	//	*	Build the Newton-Raphson solution to properly initialize model
	//	*/
	//	//KLUSystem* pSys;

	//	//
	//	//cs *T{};
	//	int *pTi, *pTj;
	//	double *pTx, *pb;
	//	int m, n, nzmax, values=0, triplet=0;
	//	int i;
	//	double se, efd=0;



	//	////	Calculate saturation factor se
	//	double fppmag = 1.0; 
	//	/**** fix this later ****/
	//	/**** fix this later ****/
	//	/**** fix this later ****/
	//	/**** fix this later ****/
	//	se = QuadraticSaturation(fppmag);

	//	//	Calculate Efd


	//	//m = number of rows, n=number of columns, nz=number of no zero entries
	//	m = 8;
	//	n = 8;
	//	nzmax = 24;
	//	
	//	//	This is the jacobian in triplet format
	//	int Ti[] = { 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7};
	//	int Tj[] = { 0, 2, 5, 2, 5, 6, 0, 2, 6, 1, 3, 4, 3, 4, 7, 1, 4, 7, 1, 6, 7, 0, 6, 7 };
	//	
	//	//	8x8 matrix -> A
	//	double Tx[] = { -1.0, (this->xpd - this->xppd) / (this->xpd - this->xl), (this->xppd - this->xl) / (this->xpd - this->xl),
	//					-1.0, 1.0, -1.0 * (this->xpd - this->xl),
	//					se, 1.0, (this->xd - this->xpd),
	//					1.0, (this->xpq-this->xppd) / (this->xpq-this->xl), (this->xppd-this->xl) / (this->xpq-this->xl),
	//					-1.0, 1.0, (this->xpq-this->xl),
	//					se*(this->xq-this->xl)/(this->xd-this->xl), -1.0, (this->xq-this->xpq),
	//					1.0, this->ra, -this->xppd,
	//					-1.0, this->xppd, this->ra};

	//	//	Initialize unkowns basaed on preliminary initializion - this may not be needed
	//	this->fppd = this->fppd;
	//	this->fppq = this->fppq;
	//	this->fpd = this->s[3];
	//	this->fpq = this->s[4];
	//	this->epd = this->s[5];
	//	this->epq = this->s[2];
	//	this->id = this->id;
	//	this->iq = this->iq;
	//	
	//	//	Calculate f(x) at the current operating point
	//	double num1, num2, den;
	//	num1 = this->xpd - this->xppd;
	//	num2 = this->xppd - this->xl;
	//	den = this->xpd - this->xl;
	//	double f1 = -this->fppd + this->fpd * (num1/den) + this->epq * (num2 / den);
	//	double f2 = this->epq - this->fpd - this->id * (this->xpd - this->xl);
	//	double f3 = -this->efd + this->fpd + (this->xd - this->xpd) * this->id + se * this->fppd;

	//	num1 = this->xpq - this->xppd;
	//	num2 = this->xppd - this->xl;
	//	den = this->xpq - this->xl;
	//	double f4 = this->fppq + this->fpq * (num1 / den) +this->epd*(num2/den);
	//	double f5 = this->epd - this->fpq + this->iq * (den);
	//	double f6 = -this->epd + this->iq * (this->xq - this->xpq) + se * this->fppq * (this->xq - this->xl) / (this->xd - this->xl);
	//	double f7 = this->vtd + this->ra * this->id - this->xppd * this->iq;
	//	double f8 = this->vtq + this->ra * this->iq + this->xppd * this->id;	
	//				
	//	double b[] = { -f1, -f2, -f3, -f4, -f5, -f6, -f7, -f8};		//	Negative sign is needed per formulation

	//	// Example
	//	//int Ti[] = { 0, 0, 1, 1 };
	//	//int Tj[] = { 0, 1, 0, 1 };
	//	//double  Tx[] = { 5.0, -3.0, 2.0, -1.0 };
	//	//double  b[]  = { 1.0, 1.0 };
	//	//m = 2;
	//	//n = 2;
	//	//nzmax = 4;
	//
	//	////	Load arrays into cs triplet matrix
	//	
	//	//	Send all data in for the solve and return the solution on array b
	//	pTi = Ti;
	//	pTj = Tj;
	//	pTx = Tx;
	//	pb = b;

	//	//	Dump matrices to file for debuggin in matlab or python
	//	ofstream myfile;
	//	myfile.open("D:\\Projects-D\\INDUCES\\Examples\\Matrix\\matrix.csv");
	//	//	Dump row indices
	//	for (i = 0; i < nzmax; i++) {
	//		myfile << Ti[i] << " ";
	//	}
	//	myfile << endl;
	//	//	Dump column indices
	//	for (i = 0; i < nzmax; i++) {
	//		myfile << Tj[i] << " ";
	//	}
	//	myfile << endl;
	//	//	Dump entries
	//	for (i = 0; i < nzmax; i++) {
	//		myfile << Tx[i] << " ";
	//	}
	//	myfile << endl;
	//	//	Dump b array
	//	for (i = 0; i < n; i++) {
	//		myfile << b[i] << " ";
	//	}
	//	myfile << endl;
	//	myfile.close();




	//	//i = CSparse_solve(m, n, nzmax, pTi, pTj, pTx, pb);

	//	return 0;
	//}

	void TGenrouObj::sample(int ActorID) {
		// Empty for now, but need to see if anything needs to be added here
	}

	void TGenrouObj::DoPendingAction(int Code, int ProxyHdl, int ActorID) {
		// Empty for now, but need to see if anything needs to be added here
	}

	void TGenrouObj::Reset(int ActorID) {
		// Empty for now, but need to see if anything needs to be added here
	}

	void TGenrouObj::InitPropertyValues(int ArrayOffset) {
		Set_PropertyValue(1,"");		// bus_name;
		Set_PropertyValue(2,"");		// gen_name;
		Set_PropertyValue(3,"7.0");	// tpdo;
		Set_PropertyValue(4,"0.030");	// tppdo;
		Set_PropertyValue(5,"0.75");	// tpqo;
		Set_PropertyValue(6,"0.05");	// tppqo;
		Set_PropertyValue(7,"3.0");	// h;
		Set_PropertyValue(8,"0.0");	// d;
		Set_PropertyValue(9,"2.1");	// xd;
		Set_PropertyValue(10,"2.0");	// xq;
		Set_PropertyValue(11,"0.2");	// xpd;
		Set_PropertyValue(12,"0.5");	// xpq;
		Set_PropertyValue(13,"0.18");	// xppd;
		Set_PropertyValue(14,"0.15");	// xl;
		Set_PropertyValue(15,"0.05");	// s1;
		Set_PropertyValue(16,"0.3");	// s12;
		Set_PropertyValue(17,"0.0");	// ra;
		Set_PropertyValue(18,"0.0");	// rcomp;
		Set_PropertyValue(19,"0.0");	// xcomp;
		inherited::InitPropertyValues(NumPropsThisClass);
	}


	double TGenrouObj::get_efield()
	{
		return this->efd;
	}


	double TGenrouObj::get_pmech()
	{
		return this->pmech;
	}

	double TGenrouObj::get_dspd()
	{
		return this->dspd;
	}

	double TGenrouObj::get_plast() 
	{
		return this->plast*this->mbase/1000.0;
	}
	
	double TGenrouObj::get_qlast() 
	{
		return this->qlast*this->mbase/1000.0;
	}

	/*void TGenrouObj::MakePosSequence(int ActorID) {

	}*/

	/*void TGenrouObj::CalcYPrim(int ActorID){

	}*/

	/*void TGenrouObj::RecalcElementData(int ActorID){

	}*/

	/*void TGenrouObj::GetCurrents(Ucomplex::pComplexArray Curr, int ActorID) {

	}*/

	/*void TGenrouObj::GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID) {

	}*/


	/*	void TGenrouObj::DumpProperties(System::TTextRec& f, bool Complete) {

	}*/
}
