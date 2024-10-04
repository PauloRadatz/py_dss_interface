
#pragma hdrstop

#include "DSSClassDefs.h"

#include "DSSGlobals.h"
#include "DSSObject.h"
#include "ParserDel.h"
#include "MyDSSClassDefs.h"
#include "Solution.h"
#include "Bus.h"
#include "Line.h"
#include "VSource.h"
#include "Isource.h"
#include "vccs.h"
#include "LineCode.h"
#include "Spectrum.h"
#include "WireData.h"
#include "CNData.h"
#include "TSData.h"
#include "LineGeometry.h"
#include "LineSpacing.h"
#include "Load.h"
#include "LoadShape.h"
#include "TempShape.h"
#include "PriceShape.h"
#include "XYcurve.h"
#include "Monitor.h"
#include "EnergyMeter.h"
#include "GrowthShape.h"
#include "TCC_Curve.h"
#include "Transformer.h"
#include "Capacitor.h"
#include "Reactor.h"
#include "Fault.h"
#include "generator.h"
#include "WindGen.h"
#include "RegControl.h"
#include "CapControl.h"
#include "GenDispatcher.h"
#include "Relay.h"
#include "Recloser.h"
#include "fuse.h"
#include "Sensor.h"
#include "Feeder.h"
#include "XfmrCode.h"
#include "Storage.h"
#include "StorageController.h"
#include "SwtControl.h"
#include "PVsystem.h"
#include "InvControl.h"
#include "GICLine.h"
#include "GICTransformer.h"
#include "VSConverter.h"
#include "ExpControl.h"
#include "UPFC.h"
#include "UPFCControl.h"
#include "ESPVLControl.h"
#include "IndMach012.h"
#include "GICsource.h"
#include "AutoTrans.h"
#include "DynamicExp.h"
     //by Dahei
#include "Generic5OrderMach.h"
     // By Dahei
#include "fMonitor.h"
// BHSL
#include "gencls.h"
#include "genrou.h"
 // JT
#include "ExcSexs.h"
// BLS
#include "tgov.h"
    
namespace DSSClassDefs
{

    unsigned int BaseClassMask = 0x00000007;
    unsigned int CLASSMASK = 0xFFFFFFF8;

    /*Basic element types*/
    int NumIntrinsicClasses = 0, NumUserClasses = 0;

    /*--------------------------------------------------------------*/

    void CreateDSSClasses()
    {
        ClassNames[ActiveActor] = THashList(25);   // Makes 5 sub lists
        DSSClassList[ActiveActor] = TPointerList(10);  // 10 is initial size and increment
        if (!(DSSClasses != NULL))
            DSSClasses = new TDSSClasses();  // class to handle junk for defining DSS classes

             /*General DSS objects, not circuit elements*/
        DSSObjs[ActiveActor] = TPointerList(25);  // 25 is initial size and increment

           /*instantiate all Intrinsic Object Classes*/

           /*Generic Object classes first in case others refer to them*/
        DSSClasses->Set_New(new TDSSSolution());
        SolutionClass[ActiveActor] = (TDSSSolution*) ActiveDSSClass[ActiveActor];     // this is a special class
        DSSClasses->Set_New(new TLineCode());
        LoadShapeClass[ActiveActor] = new TLoadShape();
        DSSClasses->Set_New(LoadShapeClass[ActiveActor]);
        TShapeClass[ActiveActor] = new TTShape();
        DSSClasses->Set_New(TShapeClass[ActiveActor]);
        PriceShapeClass[ActiveActor] = new TPriceShape();
        DSSClasses->Set_New(PriceShapeClass[ActiveActor]);
        XYCurveClass[ActiveActor] = new TXYcurve();
        DSSClasses->Set_New(XYCurveClass[ActiveActor]);
        GrowthShapeClass[ActiveActor] = new TGrowthShape();
        DSSClasses->Set_New(GrowthShapeClass[ActiveActor]);
        TCC_CurveClass[ActiveActor] = new TTCC_Curve();
        DSSClasses->Set_New(TCC_CurveClass[ActiveActor]);
        SpectrumClass[ActiveActor] = new TSpectrum();
        DSSClasses->Set_New(SpectrumClass[ActiveActor]);
        WireDataClass[ActiveActor] = new TWireData();
        DSSClasses->Set_New(WireDataClass[ActiveActor]);
        CNDataClass[ActiveActor] = new TCNData();
        DSSClasses->Set_New(CNDataClass[ActiveActor]);
        TSDataClass[ActiveActor] = new TTSData();
        DSSClasses->Set_New(TSDataClass[ActiveActor]);
        DSSClasses->Set_New(new TLineGeometry());
        LineSpacingClass[ActiveActor] = new TLineSpacing();
        DSSClasses->Set_New(LineSpacingClass[ActiveActor]);
        DSSClasses->Set_New(new TXfmrCode());

        /*Circuit Element Classes*/
        DSSClasses->Set_New(new TLine());
        ActiveVSource[ActiveActor] = new TVsource();
        DSSClasses->Set_New(ActiveVSource[ActiveActor]);   // 2-terminal Vsource
        DSSClasses->Set_New(new TIsource());              // 2-terminal Isource
        DSSClasses->Set_New(new TVCCS());
        DSSClasses->Set_New(new TLoad());
        DSSClasses->Set_New(new TTransf());
        DSSClasses->Set_New(new TRegControl());
        DSSClasses->Set_New(new TCapacitor());
        ReactorClass[ActiveActor] = new TReactor();
        DSSClasses->Set_New(ReactorClass[ActiveActor]);
        DSSClasses->Set_New(new TCapControl());
        DSSClasses->Set_New(new TFault());
        DSSClasses->Set_New(new TGenerator());
        WindGenClass[ActiveActor] = new TWindGen();
        DSSClasses->Set_New(WindGenClass[ActiveActor]);
        DSSClasses->Set_New(new TGenDispatcher());
        StorageClass[ActiveActor] = new TStorage();
        DSSClasses->Set_New(StorageClass[ActiveActor]);
        //     Storage2Class[ActiveActor]   := new TStorage2.Create;
        //     DSSClasses->New               := new Storage2Class[ActiveActor];
        DSSClasses->Set_New(new TStorageController());
        //     DSSClasses->New               := new TStorageController2.Create;
        DSSClasses->Set_New(new TRelay());
        DSSClasses->Set_New(new TRecloser());
        DSSClasses->Set_New(new TFuse());
        //     FeederClass    := new TFeeder.Create;
        //     DSSClasses->New := new FeederClass;
        DSSClasses->Set_New(new TSwtControl());
        PVSystemClass[ActiveActor] = new TPVSystem();
        DSSClasses->Set_New(PVSystemClass[ActiveActor]);
        //     PVSystem2Class[ActiveActor]  := new TPVSystem2.Create;
        //     DSSClasses->New               := new PVSystem2Class[ActiveActor];
        DSSClasses->Set_New(new TUPFC());
        DSSClasses->Set_New(new TUPFCControl());
        DSSClasses->Set_New(new TESPVLControl());
        DSSClasses->Set_New(new TIndMach012());
        DSSClasses->Set_New(new TGICsource()); // GIC source
        DSSClasses->Set_New(new TAutoTrans()); // Auto Transformer
        InvControlClass[ActiveActor] = new TInvControl();
        DSSClasses->Set_New(InvControlClass[ActiveActor]);
        //     InvControl2Class[ActiveActor]:= new TInvControl2();
        //     DSSClasses->New               := new InvControl2Class[ActiveActor];
        ExpControlClass[ActiveActor] = new TExpControl();
        DSSClasses->Set_New(ExpControlClass[ActiveActor]);
        DSSClasses->Set_New(new TGICLine());
        DSSClasses->Set_New(new TGICTransformer());
        DSSClasses->Set_New(new TVSConverter());
        MonitorClass[ActiveActor] = new TDSSMonitor();  // Have to do this AFTER Generator
        DSSClasses->Set_New(MonitorClass[ActiveActor]);
        EnergyMeterClass[ActiveActor] = new TEnergyMeter();  // Have to do this AFTER Generator
        DSSClasses->Set_New(EnergyMeterClass[ActiveActor]);
        SensorClass[ActiveActor] = new TSensor();      // Create state estimation sensors
        DSSClasses->Set_New(SensorClass[ActiveActor]);
        
        /*Add user-defined objects*/
        //by Dahei (UCF)
        FMonitorClass[ActiveActor] = new TDSSFMonitor();  // Have to do this AFTER Generator
        DSSClasses->Set_New(FMonitorClass[ActiveActor]);
        DSSClasses->Set_New(new TGeneric5());

        TDynamicExpClass[ActiveActor] = new TDynamicExp();
        DSSClasses->Set_New(TDynamicExpClass[ActiveActor]);

        /* Add new dynamic model capability */
        DSSClasses->Set_New(new TGencls());
        DSSClasses->Set_New(new TGenrou());
        DSSClasses->Set_New(new TExcSexs());
        DSSClasses->Set_New(new TTgov());

        /* Create Classes for custom implementations */
        CreateMyDSSClasses();
        NumIntrinsicClasses = DSSClassList[ActiveActor].get_myNumList();
        NumUserClasses = 0;


        /*This feature has been disabled - doesn't work in IIS*/

        // Check all DLLs in present directory and home DSS directory to see if they
        // are a user-defined DSS class

        //**** LoadUserClasses;
    }

    //----------------------------------------------------------------------------


    void DisposeDSSClasses(bool AllActors)
    {
        int k = 0, DSSCidx = 0, temp = 0, i = 0;
        TDSSObject* DSSObj;
        TDSSClass* DSSClass_idx;
        String TraceName, SuccessFree;
        if (!AllActors)
        {
            try
            {
                SuccessFree = "Get_First() Object";
                for (int stop = DSSObjs[ActiveActor].get_myNumList(), i = 1; i <= stop; i++)
                {
                    DSSObj = (TDSSObject*) DSSObjs[ActiveActor].Get(i);
                    TraceName = DSSObj->Get_myPName() + "." + DSSObj->get_Name();
                    delete DSSObj;
                    SuccessFree = TraceName;
                }
                TraceName = "(DSSObjs Class)";
                DSSObjs[ActiveActor].Clear();
            }
            catch (exception& E)
            {
                DoSimpleMsg(String("Exception disposing of DSS Obj \"") + TraceName + "\". " + CRLF 
                    + "Last Successful dispose was for object \"" + SuccessFree + "\" " + CRLF 
                    + (std::string) E.what(), 901);
            }
            try
            {
                for (int stop = DSSClassList[ActiveActor].get_myNumList(), i = 1; i <= stop; i++)
                {
                    DSSClass_idx = (TDSSClass*) DSSClassList[ActiveActor].Get(i);
                    TraceName = DSSClass_idx->get_myClass_name();
                    delete DSSClass_idx;
                }
                TraceName = "(DSS Class List)";
                DSSClassList[ActiveActor].Clear();
                TraceName = "(DSS Classes)";
                delete DSSClasses;
                DSSClasses = NULL;
                TraceName = "(ClassNames)";
                ClassNames[ActiveActor].Clear();
            }
            catch (exception& E)
            {
                DoSimpleMsg(String("Exception disposing of DSS Class\"") + TraceName + "\". " + CRLF 
                    + (std::string) E.what(), 902);
            }
        }
        else
        {
            temp = ActiveActor;
            for (int stop = NumOfActors, DSSCidx = 1; DSSCidx <= stop; DSSCidx++)
            {
                ActiveActor = DSSCidx;
                DisposeDSSClasses(false);
            }
            TraceName = "(DSS Classes)";
            delete DSSClasses;
            DSSClasses = NULL;
            ActiveActor = 1;
        }
    }


    /*--------------------------------------------------------------*/


    void AddUserClass()
    {
        // ***** ADD STUFF HERE ****

        /*Assumes DLL has been loaded by call to LoadLibrary and the Handle is stored
         in LastUserDLLHandle.  Also, assumes DSSRegisterProc has the address of
         the user.*/


         /* ***** Needs to be re-done ****** */
    }

    /*--------------------------------------------------------------*/

    void LoadUserClasses()
    {
        TSearchRec F;

        /*  Rework This !!!!*/

            // Check All DLLs in present directory
        if (FindFirst("*.dll", 0, F) == 0)
        {
            do
            {
                if (IsDSSDLL(F.Name))
                    AddUserClass(); // Attempt to add (ignored if classname already exists)
            } while (!(FindNext(F) != 0));
        }

        // Check All DLLs in DSS Directory   unless that is the directory we just checked
        if (CompareText(StartupDirectory, DSSDirectory) != 0)
            if (FindFirst(DSSDirectory + "*.dll", 0, F) == 0)
            {
                do
                {
                    if (IsDSSDLL(F.Name))
                        AddUserClass(); // Attempt to add (ignored if classname already exists)
                } while (!(FindNext(F) != 0));
            }
    }

    //----------------------------------------------------------------------------



    bool SetObjectClass(const String ObjType)

        // set LastClassReferenced variable by class name

    {
        bool result = false;
        int Classref = 0;
        Classref = ClassNames[ActiveActor].Find(ObjType);
        switch (Classref)
        {
        case 0:
        {
            DoSimpleMsg(String("Error! Object Class \"") + ObjType + "\" not found." + CRLF + Parser[ActiveActor]->get_CmdBuffer(), 903);
            result = false;
            return result;
        }
        break;/*Error*/
        default:
            LastClassReferenced[ActiveActor] = Classref;
        }
        result = true;
        return result;
    }

    //----------------------------------------------------------------------------



    void* GetDSSClassPtr(const String ClassName)
    {
        TDSSClass* result;
        result = (TDSSClass*) DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find(LowerCase(ClassName)));
        return result;
    }


}// namespace DSSClassDefs

