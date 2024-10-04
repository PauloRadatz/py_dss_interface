#ifndef AutoAddH
#define AutoAddH

/*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
/*  Unit for processing the AutoAdd Solution FUNCTIONs

  Note: Make sure this class in instantiated after energymeter class

  There is one of these per circuit

  6/11/00 - reorganized object
  6/14/00 - resolved sign issue with normal and Newton solution in AddCurrents
  9/13/03 - Modified to use pu improvement in losses and EEN instead of kW
*/

/*$M+*/


#include "System.h"

#include "Ucomplex.h"
#include "HashList.h"
#include "Arraydef.h"
#include "DSSClassDefs.h"
#include "Sysutils.h"
#include "Capacitor.h"



//class TAutoAdd;


namespace AutoAdd
{

    class TAutoAdd {
    private:
        //****    GeneratorClass   :TGenerator;
        TCapacitor* CapacitorClass;
        pIntegerArray BusIdxList;
        int BusIdxListSize;
        bool BusIdxListCreated;
        int LastAddedGenerator, LastAddedCapacitor;
        int BusIndex, Phases;
        double Ycap;
        complex GenVA;
        double kWLosses, BaseLosses, puLossImprovement;
        double kWEEN, BaseEEN, puEENImprovement;
        TTextRec FLog;  // Log File
        int ProgressCount;
        double Get_WeightedLosses();
        void ComputekWLosses_EEN(int ActorID);
        void SetBaseLosses();
        String GetUniqueGenName();
        String GetUniqueCapName();
    protected:
    public:

        /*Autoadd mode Variables*/
        double GenkW, GenPF, Genkvar, Capkvar;
        int AddType;
        bool ModeChanged;
        TAutoAdd();
        virtual ~TAutoAdd();
        void MakeBusList(int ActorID);
        void AppendToFile(const String WhichFile, const String S);
        void AddCurrents(int SolveType, int ActorID);
        int Solve(int ActorID); // Automatically add caps or generators
        //  __published:
    };

} // namespace autoadd

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace AutoAdd;
#endif

#endif //  AutoAddH











