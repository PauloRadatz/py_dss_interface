#ifndef Generic5OrderMachH
#define Generic5OrderMachH

#include "System.h"
#include "Sysutils.h"

#include "DSSClass.h"
#include "PCClass.h"
#include "PCElement.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "Arraydef.h"
#include "LoadShape.h"
#include "GrowthShape.h"
#include "Spectrum.h"
#include "Dynamics.h"
#include "GeneratorVars.h"
#include "d2c_structures.h"


namespace Generic5OrderMach
{



/*   Change Log

   November 3, 2017

   Created by
     Darhey  Xu

*/   // Base class for most DSS objects
    // Base class for collection manager for PC elements
  // Base class for PC  Elements
     // Unit for managing complex matrice (for Yprim, etc)
     // Complex math functions, type definitions
     // definitions of basic DSS arrays

    // common modules used in PC elements
    // class for supporting/representing loadshapes
  // Class for holding growth shapes
     // Definitions for harmonic spectra
     // for elements that interact with dynamics variables


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

/* Collection manager for this class of element */   /* Notes Andres: -- definition of the class -- */

class TGeneric5 : public PCClass::TPCClass
{
	friend class TGeneric5Obj;
public:
	typedef PCClass::TPCClass inherited;	
private:

      /*These private functions are generally helper functions for Edit procedure*/

      /* A typical function */
	void SetNcondsForConnection();
protected:
	void DefineProperties();    // Define the property names and help strings
	virtual int MakeLike(const String OtherIndMach012Name);  // copy properties of another similar object
public:
	TGeneric5();
	virtual ~TGeneric5();
	virtual int Edit(int ActorID);      // Definition of the main property editing function
	virtual int Init(int Handle, int ActorID);  // Initialize by handle (index), if necessary
	virtual int NewObject(const String ObjName); // This function is called by the DSS New command
	
     /*any public functions that might be called from other elements*/
};

/* Class definition for this class of element*/
typedef Ucomplex::complex TSymCompArray5[3/*# range 0..2*/];
    //pTDynamicsRec =  ^TDynamicsRec;
    //pTGeneratorVars = ^TGeneratorVars;

class TGeneric5Obj : public PCElement::TPCElement
{
	friend class TGeneric5;
public:
	typedef PCElement::TPCElement inherited;	
private:

      /*Private variables of this class*/
	int Connection;  /*0 = line-neutral; 1=Delta*/
	Ucomplex::complex Yeq;   // Y at nominal voltage

        //S1,        // Pos seq slip
        //S2,
  // limit for slip to prevent solution blowing up
        //dSdP,  // for power flow
	
        /*Dynamics variables*/ // Rotor time constant
	double puRs;
	double puXs;
	double puRr;
	double puXr;
	double puXm;
	double MaxSlip;
	double Xopen;
	double Xp;
	double T0p;
        //NOrder, //NOrder = 5 is defined as constant
 //  system order
	int NumOrderX;
	int NumOrderY; //  system output Y order
	        /*X,V*/
        /*  X_var:pComplexArray;
          Y_out_var :pComplexArray;
          V_in_var :pComplexArray;
        {A, B Matrix */
        /*  Amm:pComplexArray;
          Bmn:pComplexArray;
          Cnm:pComplexArray;
          Dnn:pComplexArray;
        {*/
	Arraydef::pDoubleArray X_var;
	Arraydef::pDoubleArray dX_vardt;
	Arraydef::pDoubleArray X_varn;//for tropdize integrate
	Arraydef::pDoubleArray dX_vardtn;
	Arraydef::pDoubleArray Y_out_var;
	Arraydef::pDoubleArray V_in_var;
	Arraydef::pDoubleArray pV_f_CC;
	bool CC_Switch;
          //Cluster_num : integer;
	        /*A, B Matrix */
	Arraydef::pDoubleArray Amm;
	Arraydef::pDoubleArray Bmn;
	Arraydef::pDoubleArray Cnm;
	Arraydef::pDoubleArray Dnn;
        /**/
	bool InDynamics;
	Ucomplex::complex Zs;
	Ucomplex::complex ZM;
	Ucomplex::complex Zr;    // Keep the last computed voltages and currents
	Ucomplex::complex Is1;
	Ucomplex::complex Ir1;
	Ucomplex::complex V1;
	Ucomplex::complex Is2;
	Ucomplex::complex Ir2;
	Ucomplex::complex V2;

        /*Complex variables for dynamics*/
        //E1, E1n, dE1dt, dE1dtn,
        //E2, E2n, dE2dt, dE2dtn,
	Ucomplex::complex Zsp;

        /**/
	double id;
	double Iq; //Id related to P ; Iq related to Q
	bool flag_dyna_Id_chg;
	double dIddt;
	double dIqdt;
	double Idn;
	double Iqn;
	double dIddtn;
	double dIqdtn; //save last time step for integration.
	        /*the input for control purpose*/
	double kcd;
	double kcq;
	double kcq_drp2; //the control gain in vi1, vi2
	double Volt_Trhd;
	int droop;//droop type: 2, Q = kcq_drp2 * (1-v_dg). others: integral droop with kcq
        //flag_drp2 : integer; //if it is 1, drp2
	double kqi; //control gain for Q_ref
	double vi1;
	double vi2; //the input of the control
	double vi1n;
	double vi2n; //the input of the control
	double dvi1dt;
	double dvi2dt;
	double dvi1dtn;
	double dvi2dtn;
	double Id_ref;
	double Iq_ref; // The pursued value of Id related to P ; Iq related to Q
	        /**/
	double P_ref;
	double Q_ref;
	double V_ref;//Power and voltage goal of the machine
	double DPX;
	int ctrl_mode; //ctrl_mode 0-local droop  V_ref = V_DG_0, P_ref = P_DG_0
	        /**/
	double P_DG;
	double Q_DG; //power of all phases totally in one
	double V_DG;// the voltage magetitude of current bus
	double Theta_DG; //the voltage angel of DG bus to slack
        //Cluster_Num : integer; //the cluster define         --move to PCElement
        //Num_in_Cluster : integer; // node num in cluster;   --move to PCElement
	int QV_flag; // 0 Q_ref; 1 V_ref
        //QV_flag_0 : integer;
        //QV_switch: integer; //if Q hits limits, PV to PQ, the QV_switch:= 1; each time Edit function runs, check this and set QV_flag back to user set.
	        /*--for 3 phases--*/
        //power, voltage, angle,
	double P_DG1;
	double P_DG2;
	double P_dg3;
	double Q_DG1;
	double Q_dg2;
	double Q_dg3;
	double V_DG1;
	double V_DG2;
	double V_DG3;
	double V_theta1;
	double V_theta2;
	double V_theta3;//operation values
	double ID1;
	double Iq1;
	double ID2;
	double Iq2;
	double Id3;
	double Iq3; //currents
        //set values
	double P_ref1;
	double P_ref2;
	double P_ref3;
	double Q_ref1;
	double Q_ref2;
	double Q_ref3;
	double V_ref1;
	double V_ref2;
	double V_ref3;// set values from outside
	        /*------Max Check-------*/
          //SMax; //  MachineData.kVArating; // 'g1.kva=100'
 // Activity power output limit
 //limit per phase
  //(0, default)   // 'g1.pmax=100'
 //
 //Reactive power output limit
 //(-Qmax, default)
	double PMax;
	double PMax_phase;
	double PMin;
	double Pmin_phase;
	double Qmax;
	double Qmax_phase;
	double Qmin;
	double Qmin_phase; //
	double IdMax_phase;
	double IqMax_phase;//phase current limit
	int PQpriority; //Active and reactive power control mode, control s
                            //'g1.pqvflag=0 Q, 1 P;
          ///////////////////
          //Gradient ; move to public
          //Alpha, // Alpha := Q_DG/Qmax;
          //dAlpha,
          //Gradient: double;
	          /*----------------------*/
            //equivalent frequency
	double Freq;
	double z_dfs_plot;
          /*----------------------*/
	bool FirstIteration;
	bool FixedSlip;

        //var_Remembered  :Double; //Q remembered of last calc
	double DQDV;  //for P_ref V_ref model
	double RandomMult;
        //Generic5SolutionCount : Integer;
	bool Generic5SwitchOpen;

        // Debugging
	System::TTextRec Tracefile;
	bool DebugTrace;
	GeneratorVars::TGeneratorVars MachineData;    // Use generator variable structure

        // Andres: NEW variables from generator
	bool MachineON;
	Ucomplex::complex ShapeFactor;
	bool ShapeIsActual;
        // Andres: end NEW variables from generator
	double VBase;
	double kWBase;
         /*---deal with -Update_Pmax_by_Ftrs-*///Pmpp, default value is 1.0;
 //Pbias, default value is 0.0;
//factors, default value all are 1.0;
	double Pmpp;
	double Pbias;
	double Pfctr1;
	double Pfctr2;
	double Pfctr3;
	double Pfctr4;
	double Pfctr5;
	double Pfctr6;

         /*----------------*/
                 //Gradient ; public
 // Alpha := Q_DG/Qmax;  //pos ctrl
	double Alpha;
	double dAlpha;
	double Gradient; // Alpha := Q_DG/Qmax;  //pos ctrl
	double Alpha1;
	double Alpha2;
	double Alpha3;
	double dAlpha1;
	double dAlpha2;
	double dAlpha3;
	double Gradient1;
	double Gradient2;
	double Gradient3;
	double AlphaP;
	double AlphaP1;
	double AlphaP2;
	double AlphaP3;// for active P control
	double GradientP;
	double GradientP1;
	double GradientP2;
	double GradientP3;
//        Procedure InterpretOption(s:String);

        //procedure set_Localslip(const Value: Double);

        //Procedure Get_PFlowModelCurrent(Const V:Complex; Const S:Double; var Istator, Irotor:Complex);
	void Get_DynamicModelCurrent();
        //procedure Set_Slip(const Value: Double);
	double GetRotorLosses();
	double GetStatorLosses();
        //Function  Compute_dSdP:Double;
	void Randomize(int Opt);
	void InitModel(const TSymCompArray5& V012, const TSymCompArray5& I012);
	void InitModelVIabc(int ActorID);
	void CalcYPrimMatrix(Ucmatrix::TcMatrix* Ymatrix, int ActorID);
	void CalcGeneric5ModelContribution(int ActorID);
	void CalcInjCurrentArray(int ActorID);
	void DoGeneric5Model(int ActorID);
	void CalcModel(Ucomplex::pComplexArray V, Ucomplex::pComplexArray i, int ActorID);


        // Andres: NEW procedures from generator
	void CalcDailyMult(double hr);
	void CalcYearlyMult(double hr);
	void CalcDutyMult(double hr);
        // Andres: NEW procedures from generator
	void InitTraceFile();
	void WriteTraceRecord(int ActorID);
	double Get_PresentkV();
	void Set_PresentkV(double Value);
	void SetPowerkW(double PkW);
        /**/
	void update_controlinput(int ActorID);
	void update_pV_f_CC(int ActorID);//  update cooperate control part, pV_f_CC
	void update_pV_f_CC_M2(int ActorID);//  update cooperate control part, pV_f_CC
	void update_system_abcd();
	void Set_P_Ref(double PrefkW, int ActorID);
	void Set_Q_Ref(double QrefkVAr);
	void Set_V_Ref(double VrefkV);
	void Update_kWbase_by_Fctrs();
	void Update_PQlimits(); //real time limits check
                                    // can also be used in power flow and simulation
	void InfoPublish(int ActorID);
        //Procedure Get_Bii;
	void CalGradient();
//        Procedure CalcDQDV;
protected:

        /*A couple of virtual procedures you can override*/
	virtual void Set_ConductorClosed(int Index, int ActorID, bool Value);
	virtual void GetTerminalCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	void DoDynamicMode(int ActorID);
	void DoHarmonicMode(int ActorID);
public:

        /*Variables and functions accessed by DSS and other objects*/

        // Andres: new variables from generator
	String DailyDispShape;  // Daily (24 HR) Generator shape
	LoadShape::TLoadShapeObj* DailyDispShapeObj;  // Daily Generator Shape for this load
	LoadShape::TLoadShapeObj* DutyShapeObj;  // Shape for this generator
	String DutyShape;  //
	String YearlyShape;  // ='fixed' means no variation  on all the time
	LoadShape::TLoadShapeObj* YearlyShapeObj;  // Shape for this Generator
        // Andres: New variables from generator
	TGeneric5Obj(DSSClass::TDSSClass* ParClass, const String Generic5ObjName);
	virtual ~TGeneric5Obj();
	virtual void RecalcElementData(int ActorID);   // Generally called after Edit is complete to recompute variables
	virtual void CalcYPrim(int ActorID);   // Calculate Primitive Y matrix
	        /*-----------*/
//        Procedure CalcABMatrix;
	        /*-----------*/
	void Integrate(int ActorID);
	void IntegrateABCD(int ActorID);
	void CalcDynamic(TSymCompArray5 V012, TSymCompArray5 I012, int ActorID);
	void CalcPFlow(TSymCompArray5 V012, TSymCompArray5 I012, int ActorID);
        // for abc phases: the below 2
	void CalcDynamicVIabc(Ucomplex::pComplexArray Vabc, Ucomplex::pComplexArray Iabc, int ActorID);
	void CalcPFlowVIabc(Ucomplex::pComplexArray Vabc, Ucomplex::pComplexArray Iabc, int ActorID);
	void SetNominalPower(int ActorID);
	void UpdateAlpha_qi(); // \alpha_qi := Q_DG/Qmax;
	virtual int InjCurrents(int ActorID);
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);

      	// State variable management functions, if any
        // You can omit these if your PC element model is not using these
        // Default behavior is to basically do nothing
	virtual int NumVariables();
	virtual void GetAllVariables(Arraydef::pDoubleArray States);
	virtual double Get_Variable(int i);
	virtual void Set_Variable(int i, double Value);
	virtual String VariableName(int i);

        // Support for Dynamics Mode
	virtual void InitStateVars(int ActorID);
	virtual void IntegrateStates(int ActorID);

        // Support for Harmonics Mode
	virtual void InitHarmonics(int ActorID);
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model, if possible

       // Functions required for managing values of properties
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual String GetPropertyValue(int Index);

       // Property LocalSlip:Double read S1 write set_Localslip;
        //Property Slip:Double              Write Set_Slip;
	TGeneric5Obj(DSSClass::TDSSClass* ParClass);
	TGeneric5Obj(String ClassName);
	TGeneric5Obj();
};
extern TGeneric5* Generic5Class;
extern TGeneric5Obj* ActiveGeneric5Obj;


}  // namespace Generic5OrderMach

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Generic5OrderMach;
#endif

#endif // Generic5OrderMachH




