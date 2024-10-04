#ifndef DSSClassDefsH
#define DSSClassDefsH

/*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/


#include "System.h"
#include "Sysutils.h"

#include "DSSClass.h"
#include "PointerList.h"
#include "HashList.h"

namespace DSSClassDefs
{

    extern unsigned int BaseClassMask;
    extern unsigned int CLASSMASK;
    const int NON_PCPD_ELEM = 1;  // A circuit Element we don't want enumerated in PD and PC Elements

    const int PD_ELEMENT = 2;
    const int PC_ELEMENT = 3;
    const int CTRL_ELEMENT = 4;
    const int METER_ELEMENT = 5;
    const int HIDDEN_ELEMENT = 6;

    /*Specific element Types*/
    const int MON_ELEMENT = 1 * 8;
    const int DSS_OBJECT = 2 * 8;   // Just a general DSS object, accessible to all circuits

    const int SOURCE = 3 * 8;
    const int XFMR_ELEMENT = 4 * 8;
    const int SUBSTATION = 5 * 8;  // not used

    const int LINE_ELEMENT = 6 * 8;
    const int LOAD_ELEMENT = 7 * 8;
    const int FAULTOBJECT = 8 * 8;
    const int ENERGY_METER = 9 * 8;
    const int GEN_ELEMENT = 10 * 8;
    const int CAP_CONTROL = 11 * 8;
    const int REG_CONTROL = 12 * 8;
    const int CAP_ELEMENT = 13 * 8;
    const int RELAY_CONTROL = 14 * 8;
    const int RECLOSER_CONTROL = 15 * 8;
    const int FUSE_CONTROL = 16 * 8;
    const int REACTOR_ELEMENT = 17 * 8;
    const int FEEDER_ELEMENT = 18 * 8;
    const int GEN_CONTROL = 19 * 8;
    const int SENSOR_ELEMENT = 20 * 8;
    const int STORAGE_ELEMENT = 21 * 8;
    const int STORAGE_CONTROL = 22 * 8;
    const int SWT_CONTROL = 23 * 8;
    const int PVSYSTEM_ELEMENT = 24 * 8;
    //      PVSYSTEM2_ELEMENT = 25 * 8; // Using 25 (PR)
          // Deleted --- VV_CONTROL       = 25 * 8;

    const int GIC_Line = 26 * 8;
    const int GIC_Transformer = 27 * 8;
    const int INV_CONTROL = 28 * 8;
    const int VS_CONVERTER = 29 * 8;
    const int EXP_CONTROL = 30 * 8;
    const int UPFC_ELEMENT = 31 * 8;
    const int UPFC_CONTROL = 32 * 8;
    const int VCCS_ELEMENT = 33 * 8;
    const int ESPVL_CONTROL = 34 * 8;
    const int INDMACH012_ELEMENT = 35 * 8;
    const int GIC_SOURCE = 36 * 8;
    const int AUTOTRANS_ELEMENT = 37 * 8;
    const int FMON_ELEMENT = 38 * 8;                        /*BY Dahei UCF*/
    const int Generic5OrderMach_ELEMENT = 39 * 8;         /*BY Dahei UCF*/
    const int INV_CONTROL2 = 40 * 8;
    //  STORAGE2_ELEMENT  = 41 * 8;
    //  STORAGE2_CONTROL  = 42 * 8;

    const int WINDGEN_ELEMENT = 43 * 8;

    // BHSL
    const int GENMODELS_ELEMENT = 44 * 8;
    const int GENCLS_ELEMENT = 45 * 8;
    const int GENROU_ELEMENT = 46 * 8;
    
    const int EXCSEXS_ELEMENT = 50 * 8;
    
    const int TGOV_ELEMENT = 51 * 8;

    extern int NumIntrinsicClasses, NumUserClasses;
    void CreateDSSClasses();
    void DisposeDSSClasses(bool AllActors);
    void* GetDSSClassPtr(const String ClassName);
    bool SetObjectClass(const String ObjType);

}

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace DSSClassDefs;
#endif


#endif //  DSSClassDefsH








