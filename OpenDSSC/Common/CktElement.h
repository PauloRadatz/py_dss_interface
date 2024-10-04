#ifndef CktElementH
#define CktElementH



    /*
       ----------------------------------------------------------
      Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
      All rights reserved.
      ----------------------------------------------------------
    */
    /*
         2-17-00 Modified Get_ConductorClosed to handle Index=0
         7-14-01 Changed way Enabled property works.
         8-17-06 Caught BusnameRedefined error when nconds changed
    */


#include "System.h"
#include "Sysutils.h"
#include "d2c_structures.h"

#include "Ucomplex.h"
#include "Ucmatrix.h"
#include "Arraydef.h"
#include "Terminal.h"
#include "DSSObject.h"
#include "DSSClass.h"
#include "PointerList.h"

using namespace std;

namespace CktElement
{

    class TDSSCktElement : public TDSSObject {
        typedef TDSSObject inherited;
    public:
//    private:
        std::vector < std::string > FBusNames;
        bool FEnabled;
        int FEnabledProperty;
        int FActiveTerminal;
        bool FYPrimInvalid;
        int FHandle;
        void Set_Freq(double Value);  // set freq and recompute YPrim.
        void Set_Nconds(int Value);
        void Set_NPhases(int Value);
        void Set_ActiveTerminal(int Value);
        int Get_ActiveTerminal();
        bool Get_ConductorClosed(int Index, int ActorID, bool Value = false);
        void Set_YprimInvalid(int ActorID, const bool Value);
        bool Get_YprimInvalid(int ActorID, const bool Value);
        std::string Get_FirstBus();
        std::string Get_NextBus(); // null string if no more values
        complex Get_Losses(int ActorID);   // Get total losses for property...
        complex Get_Power(int idxTerm, int ActorID);    // Get total complex power in active terminal
        complex Get_MaxPower(int idxTerm, int ActorID);    // Get equivalent total complex power in active terminal based on phase with max current
        double Get_MaxCurrent(int idxTerm, int ActorID);    // Get equivalent total complex current on phase with max current
        double Get_MaxVoltage(int idxTerm, int ActorID);    // Get equivalent total complex voltage on phase
        double Get_MaxCurrentAng(int idxTerm, int ActorID);
        double Get_MaxVoltageAng(int idxTerm, int ActorID);
        void DoYprimCalcs(TcMatrix* Ymatrix);
    //protected:
        int Fnterms;
        int Fnconds;  // no. conductors per terminal
        int Fnphases;  // Phases, this device
        std::vector <complex> ComplexBuffer;
        int* IterminalSolutionCount;
        int BusIndex;
        TcMatrix* YPrim_Series;
        TcMatrix* YPrim_Shunt;
        TcMatrix* YPrim;   // Order will be NTerms * Ncond
        double FYprimFreq;     // Frequency at which YPrim has been computed
        virtual void Set_Enabled(bool Value);
        void Set_ConductorClosed(int Index, int ActorID, bool Value);
        virtual void Set_NTerms(int Value);
        void Set_Handle(int Value);
    public:
        void LossCalc(int& ActorID, complex& cLoss, int& k, int& n);
        /*Total Noderef array for element*/
        std::vector <unsigned int> NodeRef;  // Need fast access to this
        longInt Yorder;
        int LastTerminalChecked;  // Flag used in tree searches
        bool Checked, HasEnergyMeter, HasSensorObj, IsIsolated, HasControl, IsMonitored, IsPartofFeeder, Drawn;  // Flag used in tree searches etc
        bool HasOCPDevice; // Fuse, Relay, or Recloser
        bool HasAutoOCPDevice; // Relay or Recloser only
        bool HasSwtControl; // Has a remotely-controlled Switch
        PointerList::TPointerList ControlElementList; //Pointer to control for this device
        std::vector <complex> Iterminal;  // Others need this
        std::vector <complex> Vterminal;
        double BaseFrequency;
        pTerminalList Terminals;
        TPowerTerminal* ActiveTerminal;
        int PublicDataSize;  // size of PublicDataStruct
        void* PublicDataStruct;  // Generic Pointer to public data Block that may be access by other classes of elements
                                   // Accessing app has to know the structure
                                   // Inited to Nil.  If Nil, accessing app should ignore

        bool GFM_Mode;
        TDSSCktElement(TDSSClass* ParClass);
        virtual ~TDSSCktElement();
        virtual int GetYPrim(TcMatrix& Ymatrix, int Opt)  //returns values of array
            ;
        virtual pComplexArray GetYPrimValues(int Opt);
        double MaxTerminalOneIMag(int ActorID);   // Max of Iterminal 1 phase currents
        virtual void ComputeIterminal(int ActorID)   // Computes Iterminal for this device
            ;
        void ComputeVterminal(int ActorID);
        void ZeroITerminal();
        virtual void GetCurrents(pComplexArray Curr, int ActorID) //Get present value of terminal Curr for reports
            ;
        virtual void GetInjCurrents(pComplexArray Curr, int ActorID)   // Returns Injextion currents
            ;
        virtual int InjCurrents(int ActorID) // Applies to PC Elements Puts straight into Solution Array
            ;
        String GetBus(int i);  // Get bus name by index
        void SetBus(int i, const String S);  // Set bus name by index
        virtual void SetNodeRef(int iTerm, pIntegerArray NodeRefArray)  // Set NodeRef Array for fast solution with intrinsics
            ;
        virtual void RecalcElementData(int ActorID);
        virtual void CalcYPrim(int ActorID)
            // radial solution removed PROCEDURE BackwardSweep; Virtual;
            ;
        virtual void MakePosSequence(int ActorID)  // Make a positive Sequence Model
            ;
        int Get_Handle();
        bool Get_Enabled();
        double Get_YPrimFreq();
        int Get_NTerms();
        int Get_NConds();
        int Get_NPhases();
        void GetTermVoltages(int iTerm, pComplexArray VBuffer, int ActorID);
        int get_FActiveTerminal();

        virtual void GetPhasePower(pComplexArray PowerBuffer, int ActorID);
        virtual void GetPhaseLosses(int& Num_Phases, pComplexArray LossBuffer, int ActorID);
        virtual void GetLosses(complex& TotalLosses, complex& LoadLosses, complex& NoLoadLosses, int ActorID);
        virtual void GetSeqLosses(complex& PosSeqLosses, complex& NegSeqLosses, complex& ZeroModeLosses, int ActorID);
        virtual String GetPropertyValue(int Index);
        virtual void InitPropertyValues(int ArrayOffset);
        virtual void DumpProperties(Textfile& F, bool Complete);
        virtual double Get_PCEValue(int idxTerm, int ValType, int ActorID);
        //__declspec (property ( get = Get_YprimInvalid, put = Set_YprimInvalid ) ) bool YprimInvalid[]DECLSPEC_2D;
        //__declspec (property (get = Get_Enabled, put = Set_Enabled) ) bool Enabled;
        //__declspec (property (get = Get_NTerms, put = Set_NTerms)) int NTerms;
        //__declspec (property (get = Get_NConds, put = Set_NConds)) int NConds;
        //__declspec (property (get = Get_NPhases, put = Set_NPhases)) int NPhases;
        //__declspec (property(get = Get_Power)) complex Power[][];
        //__declspec (property(get = Get_MaxPower)) complex MaxPower[][];
        //__declspec (property(get = Get_MaxCurrent)) double MaxCurrent[][];
        //__declspec (property(get = Get_MaxVoltage)) double MaxVoltage[][];

        //__declspec (property(get = Get_MaxCurrentAng)) double MaxCurrentAng[][];
        //__declspec (property(get = Get_MaxVoltageAng)) double MaxVoltageAng[][];
        //__declspec (property(get = Get_ActiveTerminal, put = Set_ActiveTerminal)) int ActiveTerminalIdx;
        //__declspec (property(get = Get_ConductorClosed, put = Set_ConductorClosed)) bool Closed[][][];

        //__declspec (property (get = Get_PCEValue)) double PCEValue[][][];
        void SumCurrents(int ActorID);
        void Get_Current_Mags(pDoubleArray cMBuffer, int ActorID); // Returns the Currents vector in magnitude

        TDSSCktElement(String ClassName);
        TDSSCktElement();
        virtual int InitializeStates(int ActorID);
    };

}

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace CktElement;
#endif

#endif //  CktElementH









