#include "ExportCIMXML.h"

#include "Sysutils.h"
#include "Utilities.h"
#include "Circuit.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "CktElement.h"
#include "PDElement.h"
#include "PCElement.h"
#include "generator.h"
#include "Load.h"
#include "RegControl.h"
#include "VSource.h"
#include "Line.h"
#include "Transformer.h"
#include "Ucomplex.h"
#include "Ucmatrix.h"
#include "LineCode.h"
#include "fuse.h"
#include "Capacitor.h"
#include "CapControl.h"
#include "CapControlVars.h"
#include "Reactor.h"
#include "Feeder.h"
#include "ConductorData.h"
#include "LineUnits.h"
#include "LineGeometry.h"
#include <math.h>
#include "HashList.h"
#include "WireData.h"
#include "XfmrCode.h"
#include "LineSpacing.h"
#include "CableData.h"
#include "CNData.h"
#include "TSData.h"
#include "Storage.h"
#include "PVsystem.h"
#include "Relay.h"
#include "Recloser.h"
#include "AutoTrans.h"
#include "InvControl.h"
#include "ExpControl.h"
#include "DSSObject.h"
#include "DSSClass.h"

#include "System.h"
#include <algorithm>

/*
  ----------------------------------------------------------
  Copyright (c) 2009-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*Write a CIM XML file using RDF Schema for the Common Distribution
  Power System Model, IEC 61968-13.*/

enum UuidChoice {Bank, Wdg, XfCore, XfMesh, WdgInf, ScTest, OcTest,
        BaseV, LinePhase, LoadPhase, GenPhase, CapPhase, SolarPhase, BatteryPhase,
        XfLoc, LoadLoc, LineLoc, CapLoc, Topo, ReacLoc, SolarLoc, BatteryLoc,
        OpLimV, OpLimI, LoadResp, CIMVer, PosPt, CoordSys, TopoIsland, Station,
        GeoRgn, SubGeoRgn, ZData, OpLimT, XfInfo, FdrLoc, OpLimAHi, OpLimALo,
        OpLimBHi, OpLimBLo, MachLoc, PVPanels, Battery, SrcLoc, TankInfo,
        TapCtrl, PUZ, WirePos, NormAmps, EmergAmps,
        I1547NameplateData, I1547NameplateDataApplied, I1547Signal, I1547VoltVar,
        I1547WattVar, I1547ConstPF, I1547VoltWatt, I1547ConstQ, ECProfile};

enum ProfileChoice {FunPrf, EpPrf, GeoPrf, TopoPrf, CatPrf, SshPrf, DynPrf, NUM_PROFILE_CHOICES};

class TBankObject: public TNamedObject {
    typedef TNamedObject inherited;
public:
    String vectorGroup;
    int maxWindings;
    int nWindings;
    std::vector<int> connections;
    bool bAuto;
    std::vector<int> angles;
    std::vector<int> phaseA;
    std::vector<int> phaseB;
    std::vector<int> phaseC;
    std::vector<int> ground;
    TPDElement* pd_unit;  // save this for writing the bank coordinates
    TBankObject(int MaxWdg);
    virtual ~TBankObject();
    void AddTransformer(TTransfObj& pXf);
    void AddAutoTransformer(TAutoTransObj& pAuto);
    void BuildVectorGroup();
};

class TOpLimitObject: public TNamedObject {
    typedef TNamedObject inherited;
public:
    double NormAmps;
    double EmergAmps;
    TOpLimitObject(double norm, double emerg);
    virtual ~TOpLimitObject();
};

enum ECPChoice {LoadEcp, PvEcp, GenEcp, BatEcp};

class TECPObject: public TNamedObject {
    typedef TNamedObject inherited;
public:
    ECPChoice connType;
    std::vector<TUuid> connections;
    int nconn;

    String daily;
    String duty;
    String yearly;
    String spectrum;
    String cvr;
    String growth;
    String Tdaily;
    String Tduty;
    String Tyearly;

    TECPObject(ECPChoice choice);
    virtual ~TECPObject();
    void AddConnection(TNamedObject& pObj);
};

class TFileDealer: public TObject {
    typedef TObject inherited;
private:
    // the Combined XML can be broken into seven separate profiles
    TTextRec F_FUN;
    TTextRec F_EP;
    TTextRec F_SSH;
    TTextRec F_CAT;
    TTextRec F_GEO;
    TTextRec F_TOPO;
    TTextRec F_DYN;
    String roots[NUM_PROFILE_CHOICES];
    TUuid ids[NUM_PROFILE_CHOICES];
public:
    bool Separate;
    TFileDealer(bool Combined, const String& Filename);
    virtual ~TFileDealer();
    void WriteCimLn(ProfileChoice prf, const String& S);
    void StartInstance(ProfileChoice prf, const String& Root, TNamedObject& Obj);
    void StartInstance(ProfileChoice prf, const String& Root, TNamedObject* Obj);
    void StartFreeInstance( ProfileChoice prf, const String& Root, TUuid uuid);
    void EndInstance(ProfileChoice prf, const String& Root);
};

class TRemoteSignalObject: public TNamedObject {
    typedef TNamedObject inherited;
public:
    String busName;
    TDSSCktElement *pElem;
    int trm;
    String phase; // want A, B, C, s1 or s2
    TRemoteSignalObject(const String& aBusName, int seq, const String& invName);
    virtual ~TRemoteSignalObject();
};

class TIEEE1547Controller {
private:
    double ND_acVmax, ND_acVmin, AD_pMax, AD_pMaxOverPF, AD_overPF, AD_pMaxUnderPF;
    double AD_underPF, AD_sMax, AD_qMaxInj, AD_qMaxAbs, AD_pMaxCharge;
    double AD_apparentPowerChargeMax, AD_acVnom;
    double VV_vRef, VV_vRefOlrt, VV_curveV1, VV_curveV2, VV_curveV3, VV_curveV4;
    double VV_olrt, VV_curveQ1, VV_curveQ2, VV_curveQ3, VV_curveQ4;
    double Q_reactivePower, PF_powerFactor, VW_olrt, VW_curveV1, VW_curveV2;
    double VW_curveP1, VW_curveP2gen, VW_curveP2load;
    double WV_curveP1gen, WV_curveP2gen, WV_curveP3gen;
    double WV_curveP1load, WV_curveP2load, WV_curveP3load;
    double WV_curveQ1gen, WV_curveQ2gen, WV_curveQ3gen;
    double WV_curveQ1load, WV_curveQ2load, WV_curveQ3load;

    String ND_normalOPcatKind, PF_constPFexcitationKind;
    bool VV_enabled, WV_enabled, PF_enabled, Q_enabled, VW_enabled;
    bool VV_vRefAutoModeEnabled;

    TNamedObject* pInvName;
    TNamedObject* pPlateName;
    TNamedObject* pSetName;
    std::vector<String> pDERNames;
    std::vector<String> pMonBuses;
    std::vector<TRemoteSignalObject> Signals;

    bool bNameplateSet;

    void FinishNameplate();
    void SetStorageNameplate(TStorageObj& pBat);
    void SetPhotovoltaicNameplate(TPVsystemObj& pPV);
    void SetElementNameplate(TDSSCktElement& pElem);
    void SetDefaults(bool bCatB);
    void FindSignalTerminals();
    bool CheckSignalMatch(TRemoteSignalObject& sig, TDSSCktElement& pElm, int seq);
public:
    TIEEE1547Controller();
    virtual ~TIEEE1547Controller();

    void PullFromInvControl(TInvControlObj& pInv);
    void PullFromExpControl(TExpControlObj& pExp);
    void WriteCIM(ProfileChoice prf);
};

THashList uuidHash; // index is 1-based
std::vector<TUuid> UuidList; // index is 0-based
std::vector<String> UuidKeyList;
THashList BankHash;
std::vector<TBankObject*> BankList;
THashList ECPHash;
std::vector<TECPObject*> ECPList;
THashList OpLimitHash;
std::vector<TOpLimitObject*> OpLimitList;
TFileDealer* FD = nullptr;

const string CIM_NS = "http://iec.ch/TC57/CIM100";
const double CatBQmin = 0.43; // for IEEE 1547 Category B estimate

TECPObject::TECPObject(ECPChoice choice): inherited("ECProfile")
{
    connType = choice;
    nconn = 0;
    connections.resize(10);
}

TECPObject::~TECPObject()
{
}

void TECPObject::AddConnection(TNamedObject &pObj)
{
    int size = connections.size();
    if (nconn > size)
        connections.resize(2 * size);
    connections[nconn] = pObj.Get_UUID();
    nconn++;
}

void StartCIMFile(Textfile& F, const String &Filenm, ProfileChoice prf);

void TFileDealer::WriteCimLn(ProfileChoice prf, const String& s)
{
    if (Separate)
    {
        if (prf != FunPrf)
        {
            if (roots[prf].size() < 1)
            {
                StartFreeInstance(prf, roots[FunPrf], ids[FunPrf]);
            }
        }
        switch(prf)
        {
            case FunPrf:
                WriteLn(F_FUN, s);
                break;
            case EpPrf:
                WriteLn(F_EP, s);
                break;
            case GeoPrf:
                WriteLn(F_GEO, s);
                break;
            case TopoPrf:
                WriteLn(F_TOPO, s);
                break;
            case CatPrf:
                WriteLn(F_CAT, s);
                break;
            case SshPrf:
                WriteLn(F_SSH, s);
                break;
            case DynPrf:
                WriteLn(F_DYN, s);
                break;
        }
    }
    else
    {
        WriteLn(F_FUN, s);
    }
}

TFileDealer::TFileDealer(bool Combined, const String& FileName)
{
    int i;
    Separate = !Combined;
    if (Separate)
    {
        for (i = 0; i < NUM_PROFILE_CHOICES; ++i)
            roots[i] = "";
        StartCIMFile(F_FUN, FileName + "_FUN.XML", FunPrf);
        StartCIMFile(F_GEO, FileName + "_GEO.XML", GeoPrf);
        StartCIMFile(F_TOPO, FileName + "_TOPO.XML", TopoPrf);
        StartCIMFile(F_SSH, FileName + "_SSH.XML", SshPrf);
        StartCIMFile(F_CAT, FileName + "_CAT.XML", CatPrf);
        StartCIMFile(F_EP, FileName + "_EP.XML", EpPrf);
        StartCIMFile(F_DYN, FileName + "_DYN.XML", EpPrf);
    }
    else
    {
        StartCIMFile(F_FUN, FileName, FunPrf);
    }
}

void TFileDealer::StartInstance(ProfileChoice prf, const String& Root, TNamedObject& Obj)
{
    if (Separate)
    { // must be first to avoid stack overflow in WriteCimLn
        roots[prf] = Root;
        ids[prf] = Obj.Get_UUID();
    }
    WriteCimLn(prf, Format("<cim:%s rdf:about=\"urn:uuid:%s\">", Root.c_str(), Obj.Get_CIM_ID().c_str()));
    WriteCimLn(prf, Format("  <cim:IdentifiedObject.mRID>%s</cim:IdentifiedObject.mRID>", Obj.Get_CIM_ID().c_str()));
    WriteCimLn(prf, Format("  <cim:IdentifiedObject.name>%s</cim:IdentifiedObject.name>", Obj.LName.c_str()));
}

void TFileDealer::StartInstance(ProfileChoice prf, const String& Root, TNamedObject* Obj)
{
    StartInstance(prf, Root, *Obj);
}

void TFileDealer::StartFreeInstance(ProfileChoice prf, const String& Root, TUuid uuid)
{
    if (Separate)
    { // must be first to avoid stack overflow in WriteCimLn
        roots[prf] = Root;
        ids[prf] = uuid;
    }
    WriteCimLn(prf, Format("<cim:%s rdf:about=\"urn:uuid:%s\">", Root.c_str(), UUIDToCIMString(uuid).c_str()));
}

void TFileDealer::EndInstance(ProfileChoice prf, const String& Root)
{
    int i;
    if (Separate)
    {
        for (i = 0; i < NUM_PROFILE_CHOICES; ++i)
        {
            if (roots[i].size() > 0)
            {
                WriteCimLn((ProfileChoice) i, Format("</cim:%s>", Root.c_str()));
                roots[i] = "";
            }
        }
    }
    else
    {
        WriteCimLn(prf, Format("</cim:%s>", Root.c_str()));
    }
}

TFileDealer::~TFileDealer()
{
    WriteLn(F_FUN, "</rdf:RDF>");
    CloseFile(F_FUN);
    if (Separate)
    {
        WriteLn(F_GEO, "</rdf:RDF>");
        WriteLn(F_CAT, "</rdf:RDF>");
        WriteLn(F_SSH, "</rdf:RDF>");
        WriteLn(F_TOPO, "</rdf:RDF>");
        WriteLn(F_EP, "</rdf:RDF>");
        WriteLn(F_DYN, "</rdf:RDF>");
        CloseFile(F_GEO);
        CloseFile(F_CAT);
        CloseFile(F_SSH);
        CloseFile(F_TOPO);
        CloseFile(F_EP);
        CloseFile(F_DYN);
    }
}

void ParseSwitchClass(TLineObj& pLine, String& swtCls, double& ratedAmps, double& breakingAmps)
{
    TFuseObj* pFuse;
    TRelayObj* pRelay;
    TRecloserObj* pRecloser;

    swtCls = "LoadBreakSwitch";
    ratedAmps = pLine.NormAmps;
    breakingAmps = ratedAmps;
    pFuse = (TFuseObj*) ActiveCircuit[ActiveActor]->Fuses.Get_First();
    while ((pFuse != nullptr))
    {
        if (pFuse->FControlledElement == &pLine)
        {
            swtCls = "Fuse";
            ratedAmps = pFuse->RatedCurrent;
            breakingAmps = 0.0;
            return;
        }
        pFuse = (TFuseObj*) ActiveCircuit[ActiveActor]->Fuses.Get_Next();
    }
    pRelay = (TRelayObj*) ActiveCircuit[ActiveActor]->Relays.Get_First();
    while ((pRelay != nullptr))
    {
        if (pRelay->FControlledElement == &pLine)
        {
            swtCls = "Breaker";
            return;
        }
        pRelay = (TRelayObj*) ActiveCircuit[ActiveActor]->Relays.Get_Next();
    }
    pRecloser = (TRecloserObj*) ActiveCircuit[ActiveActor]->Reclosers.Get_First();
    while ((pRecloser != nullptr))
    {
        if (pRecloser->FControlledElement == &pLine)
        {
            swtCls = "Recloser";
            return;
        }
        pRecloser = (TRecloserObj*) ActiveCircuit[ActiveActor]->Reclosers.Get_Next();
    }
}

// this returns s1, s2, or a combination of ABCN
String PhaseString(TDSSCktElement &pElem, int bus, bool bAllowSec = true) // if order doesn't matter
{
    String val, phs;
    int dot;
    bool bSec;

    phs = pElem.Get_FirstBus();
    for (dot = 2; dot <= bus; ++dot)
        phs = pElem.Get_NextBus();
    bSec = false;
    if (bAllowSec)
    {
        if (pElem.Fnphases == 2)
            if (ActiveCircuit[ActiveActor]->Buses[pElem.Terminals[bus - 1].BusRef - 1]->kVBase < 0.25)
                bSec = true;
        if (pElem.Fnphases == 1)
            if (ActiveCircuit[ActiveActor]->Buses[pElem.Terminals[bus - 1].BusRef - 1]->kVBase < 0.13)
                bSec = true;
    }

    dot = Pos(".", phs);
    if (dot < 1)
    {
        val = "ABC";
    }
    else
    {
        phs = phs.substr(dot);
        if (Pos("3", phs) > 0)
            bSec = false; // i.e. it's a three-phase secondary, not split-phase
        if (bSec)
        {
            if (Pos("1", phs) > 0)
            {
                val = "s1";
                if (Pos("2", phs) > 0)
                    val = val + "2";
            }
            else if (Pos("2", phs) > 0)
                val = "s2";
        }
        else
        {
            val = "";
            if (Pos("1", phs) > 0)
                val = val + "A";
            if (Pos("2", phs) > 0)
                val = val + "B";
            if (Pos("3", phs) > 0)
                val = val + "C";
            if (Pos("4", phs) > 0)
                val = val + "N";
        }
    }
    return val;
}

// returns s1, s12, s2, or an ordered combination of ABC
String PhaseOrderString(TDSSCktElement& pElem, int bus, bool bAllowSec = true) // for transposition
{
    String phs, Result;
    int dot;
    bool bSec;

    phs = pElem.Get_FirstBus();
    for (dot = 2; dot <= bus; ++dot)
        phs = pElem.Get_NextBus();

    bSec = false;
    if (bAllowSec)
    {
        if (pElem.Fnphases == 2)
            if (ActiveCircuit[ActiveActor]->Buses[pElem.Terminals[bus - 1].BusRef - 1]->kVBase < 0.25)
                bSec = true;
        if (pElem.Fnphases == 1)
            if (ActiveCircuit[ActiveActor]->Buses[pElem.Terminals[bus - 1].BusRef - 1]->kVBase < 0.13)
                bSec = true;
    }

    dot = Pos(".", phs);
    if (dot < 1)
    {
        Result = "ABC";
    }
    else
    {
        phs = phs.substr(dot);
        if (Pos("3", phs) > 0)
            bSec = false; // i.e. it's a three-phase secondary, not split-phase
        if (bSec)
        {
            if (Pos("1", phs) > 0)
            {
                Result = "s1";
                if (Pos("2", phs) > 0)
                    Result = Result + "2";
            }
            else if (Pos("2", phs) > 0)
                Result = "s2";
        }
        else if (Pos("1.2.3", phs) > 0)
            Result = "ABC";
        else if (Pos("1.3.2", phs) > 0)
            Result = "ACB";
        else if (Pos("2.3.1", phs) > 0)
            Result = "BCA";
        else if (Pos("2.1.3", phs) > 0)
            Result = "BAC";
        else if (Pos("3.2.1", phs) > 0)
            Result = "CBA";
        else if (Pos("3.1.2", phs) > 0)
            Result = "CAB";
        else if (Pos("1.2", phs) > 0)
            Result = "AB";
        else if (Pos("1.3", phs) > 0)
            Result = "AC";
        else if (Pos("2.3", phs) > 0)
            Result = "BC";
        else if (Pos("2.1", phs) > 0)
            Result = "BA";
        else if (Pos("3.2", phs) > 0)
            Result = "CB";
        else if (Pos("3.1", phs) > 0)
            Result = "CA";
        else if (Pos("1", phs) > 0)
            Result = "A";
        else if (Pos("2", phs) > 0)
            Result = "B";
        else
            Result = "C";
    }

    return Result;
}

String DeltaPhaseString(TDSSCktElement& pElem)
{
    String phs, Result;
    int dot;

    phs = pElem.Get_FirstBus();

    dot = Pos(".", phs);
    if ((dot < 1) || (pElem.Fnphases == 3))
    {
        Result = "ABC"; // if Nphases < 3 this would be a user input error
    }
    else
    {
        phs = phs.substr(dot);
        if (pElem.Fnphases == 1)
        {
            if (Pos("1.2", phs) > 0)
                Result = "A";
            else if (Pos("2.1", phs) > 0)
                Result = "A";
            else if (Pos("2.3", phs) > 0)
                Result = "B";
            else if (Pos("3.2", phs) > 0)
                Result = "B";
            else if (Pos("1.3", phs) > 0)
                Result = "C";
            else if (Pos("3.1", phs) > 0)
                Result = "C";
        }
        else
        {
            if (Pos("1.2.3", phs) > 0)
                Result = "AB";
            else if (Pos("1.3.2", phs) > 0)
                Result = "CB";
            else if (Pos("2.1.3", phs) > 0)
                Result = "AC";
            else if (Pos("2.3.1", phs) > 0)
                Result = "BC";
            else if (Pos("3.1.2", phs) > 0)
                Result = "CA";
            else if (Pos("3.2.1", phs) > 0)
                Result = "BA";
        }
    }

    return Result;
}

TBankObject::TBankObject(int MaxWdg): inherited("Bank")
{
    maxWindings = MaxWdg;
    nWindings = 0;
    bAuto = false;
    connections.resize(MaxWdg);
    angles.resize(MaxWdg);
    phaseA.resize(MaxWdg);
    phaseB.resize(MaxWdg);
    phaseC.resize(MaxWdg);
    ground.resize(MaxWdg);
}

TBankObject::~TBankObject()
{
}

void TBankObject::BuildVectorGroup()
{
    int i;
    if (bAuto)
    {
        if (nWindings < 3)
            vectorGroup = "YNa";
        else
            vectorGroup = "YNad1";
        return;
    }
    vectorGroup = "";
    i = 0; // dynamic arrays are zero-based
    while (i < nWindings)
    {
        if ((phaseA[i] > 0) && (phaseB[i] > 0) && (phaseC[i] > 0))
        {
            if (connections[i] > 0)
                vectorGroup = vectorGroup + "d";
            else
                vectorGroup = vectorGroup + "y";
            if (ground[i] > 0)
                vectorGroup = vectorGroup + "n";
            if (angles[i] > 0)
                vectorGroup = vectorGroup + IntToStr(angles[i]);
        }
        else
            vectorGroup = vectorGroup + "i";
        i++;
    }
    if (vectorGroup.size() > 0)
        vectorGroup = UpperCase(vectorGroup.substr(0, 1)) + vectorGroup.substr(1);
}

void TBankObject::AddTransformer(TTransfObj& pXf)
{
    int i;
    String phs;

    if (pXf.get_NumWindings() > nWindings)
        nWindings = pXf.get_NumWindings();

    pd_unit = &pXf;
    for (i = 1; i <= pXf.get_NumWindings(); ++i)
    {
        phs = PhaseString(pXf, i);
        if (Pos("A", phs) > 0)
            phaseA[i - 1] = 1;
        if (Pos("B", phs) > 0)
            phaseB[i - 1] = 1;
        if (Pos("C", phs) > 0)
            phaseC[i - 1] = 1;
        connections[i - 1] = pXf.Get_WdgConnection(i);
        if (connections[i - 1] != connections[0])
            angles[i - 1] = 1;
        if ((pXf.Get_WdgRneutral(i) >= 0.0) || (pXf.Get_WdgXneutral(i) > 0.0))
            if (connections[i - 1] < 1)
                ground[i - 1] = 1;
    }
}

void TBankObject::AddAutoTransformer(TAutoTransObj& pAuto) // 3-phase, 2 or 3 windings
{
    int i;
    if (pAuto.get_NumWindings() > nWindings)
        nWindings = pAuto.get_NumWindings();
    bAuto = true;
    pd_unit = &pAuto;
    for (i = 1; i <= pAuto.get_NumWindings(); ++i)
    {
        phaseA[i - 1] = 1;
        phaseB[i - 1] = 1;
        phaseC[i - 1] = 1;
        connections[i - 1] = pAuto.Get_WdgConnection(i);
        if (i == 2)
            ground[i - 1] = 1;
    }
}

TOpLimitObject::TOpLimitObject(double norm, double emerg): inherited("OpLimI")
{
    NormAmps = norm;
    EmergAmps = emerg;
}

TOpLimitObject::~TOpLimitObject()
{
}

// the CIM transformer model requires some identified objects that don't have
// a counterpart in the DSS named objects.  These include banks, windings, and
// winding info.  So we create temporary UUIDs on the fly, and use a hash list when we
// need the UUIDs for later reference
void StartUuidList(int size)
{
    if (!UuidList.empty())
        FreeUuidList();
    uuidHash = THashList(size);
    UuidList.resize(size);
    UuidKeyList.resize(size);
}

void StartBankList(int size)
{
    BankHash = THashList(size);
    BankList.clear();
}

void StartECPList(int size)
{
    ECPHash = THashList(size);
    ECPList.clear();
}

void StartOpLimitList(int size)
{
    OpLimitHash = THashList(size);
    OpLimitList.clear();
}

void FreeUuidList()
{
    // uuidHash.Free;
    UuidList.clear();
    UuidKeyList.clear();
}

void FreeBankList()
{
    int i;
    // BankHash.Free;
    for (i = 0; i < BankList.size(); ++i)
        if (BankList[i] != nullptr)
            delete BankList[i];
    BankList.clear();
}

void FreeECPList()
{
    // ECPHash.Free;
    for (int i = 0; i < ECPList.size(); ++i)
        if (ECPList[i] != nullptr)
            delete ECPList[i];
    ECPList.clear();;
}

void FreeOpLimitList()
{
    // OpLimitHash.Free;
    for (int i = 0; i < OpLimitList.size(); ++i)
        if (OpLimitList[i] != nullptr)
            delete OpLimitList[i];
    OpLimitList.clear();
}

void AddBank(TBankObject* pBank)
{
    BankHash.Add(pBank->LName);
    BankList.push_back(pBank);
}

TBankObject* GetBank(const String& sBank)
{
    int ref;
    TBankObject* Result = nullptr;
    ref = BankHash.Find(sBank);
    if (ref > 0)
        Result = BankList[ref - 1];

    return Result;
}

void AddECP(TECPObject* pECP)
{
    ECPHash.Add(pECP->LName);
    ECPList.push_back(pECP);
}

TECPObject* GetECP(const String& key)
{
    int ref;

    TECPObject* Result = nullptr;
    ref = ECPHash.Find(key);
    if (ref > 0)
        Result = ECPList[ref - 1];

    return Result;
}

void AddOpLimit(TOpLimitObject* pLimit)
{
    OpLimitHash.Add(pLimit->LName);
    OpLimitList.push_back(pLimit);
}

TOpLimitObject* GetOpLimit(const String& sLimit)
{
    TOpLimitObject* Result = nullptr;
    int ref;
    ref = OpLimitHash.Find(sLimit);
    if (ref > 0)
        Result = OpLimitList[ref - 1];

    return Result;
}

TUuid GetHashedUuid(const String& key)
{
    TUuid Result;
    int ref, size;
    ref = uuidHash.Find(key);
    if (ref == 0)
    {
        ref = uuidHash.Add(key);
        CreateUUID4(Result);  // this should be the ONLY place to call CreateUUID4
        size = UuidList.size();
        if (ref > size)
        {
            UuidList.resize(2 * (size + 1));
            UuidKeyList.resize(2 * (size + 1));
        }
        UuidList[ref - 1] = Result;
        UuidKeyList[ref - 1] = key;
    }
    else
    {
        Result = UuidList[ref - 1];
    }
    return Result;
}

void AddHashedUUID(const String& key, const String& UuidVal)
{
    int ref, size;
    ref = uuidHash.Find(key);
    if (ref == 0)
    {
        ref = uuidHash.Add(key);
        size = UuidList.size();
        if (ref > size)
        {
            UuidList.resize(2 * (size + 1));
            UuidKeyList.resize(2 * (size + 1));
        }
        UuidList[ref - 1] = StringToUUID(UuidVal);
        UuidKeyList[ref - 1] = key;
    }
    else
    {
        UuidList[ref - 1] = StringToUUID(UuidVal);
    }
}

// any temporary object (not managed by DSS) should have "=" prepended to the Name
TUuid GetDevUuid(UuidChoice which, const String& Name, int Seq)
{
    String key;
    switch(which)
    {
        case Bank:
            key = "Bank=";
            break;
        case Wdg:
            key = "Wdg=";
            break;
        case XfCore:
            key = "XfCore=";
            break;
        case XfMesh:
            key = "XfMesh=";
            break;
        case WdgInf:
            key = "WdgInf=";
            break;
        case ScTest:
            key = "ScTest=";
            break;
        case OcTest:
            key = "OcTest=";
            break;
        case BaseV:
            key = "BaseV=";
            break;
        case OpLimV:
            key = "OpLimV=";
            break;
        case OpLimI:
            key = "OpLimI=";
            break;
        case LinePhase:
            key = "LinePhase=";
            break;
        case LoadPhase:
            key = "LoadPhase=";
            break;
        case GenPhase:
            key = "GenPhase=";
            break;
        case SolarPhase:
            key = "PVPhase=";
            break;
        case BatteryPhase:
            key = "BattPhase=";
            break;
        case CapPhase:
            key = "CapPhase=";
            break;
        case XfLoc:
            key = "XfLoc=";
            break;
        case LoadLoc:
            key = "LoadLoc=";
            break;
        case LineLoc:
            key = "LineLoc=";
            break;
        case ReacLoc:
            key = "ReacLoc=";
            break;
        case CapLoc:
            key = "CapLoc=";
            break;
        case Topo:
            key = "Topo=";
            break;
        case SolarLoc:
            key = "SolarLoc=";
            break;
        case BatteryLoc:
            key = "BatteryLoc=";
            break;
        case LoadResp:
            key = "LoadResp=";
            break;
        case CIMVer:
            key = "CIMVer=";
            break;
        case ZData:
            key = "ZData=";
            break;
        case PosPt:
            key = "PosPt=";
            break;
        case CoordSys:
            key = "CoordSys=";
            break;
        case TopoIsland:
            key = "TopoIsland=";
            break;
        case OpLimT:
            key = "OpLimT=";
            break;
        case Station:
            key = "Station=";
            break;
        case GeoRgn:
            key = "GeoRgn=";
            break;
        case SubGeoRgn:
            key = "SubGeoRgn=";
            break;
        case FdrLoc:
            key = "FdrLoc=";
            break;
        case XfInfo:
            key = "XfInfo=";
            break;
        case OpLimAHi:
            key = "OpLimAHi=";
            break;
        case OpLimALo:
            key = "OpLimALo=";
            break;
        case OpLimBHi:
            key = "OpLimBHi=";
            break;
        case OpLimBLo:
            key = "OpLimBLo=";
            break;
        case MachLoc:
            key = "MachLoc=";
            break;
        case SrcLoc:
            key = "SrcLoc=";
            break;
        case PVPanels:
            key = "PVPanels=";
            break;
        case Battery:
            key = "Battery=";
            break;
        case TankInfo:
            key = "TankInfo=";
            break;
        case TapCtrl:
            key = "TapCtrl=";
            break;
        case PUZ:
            key = "PUZ=";
            break;
        case WirePos:
            key = "WirePos=";
            break;
        case NormAmps:
            key = "NormAmps=";
            break;
        case EmergAmps:
            key = "EmergAmps=";
            break;
        case I1547NameplateData:
            key = "INameplate=";
            break;
        case I1547NameplateDataApplied:
            key = "IApplied=";
            break;
        case I1547Signal:
            key = "ISignal=";
            break;
        case I1547VoltVar:
            key = "IVVar=";
            break;
        case I1547WattVar:
            key = "IWVar=";
            break;
        case I1547ConstPF:
            key = "IPF=";
            break;
        case I1547VoltWatt:
            key = "IVWatt=";
            break;
        case I1547ConstQ:
            key = "IQ=";
            break;
        case ECProfile:
            key = "ECP=";
            break;
    }
    key = key + Name + "=" + IntToStr(Seq);
    return GetHashedUuid(key);
}

void AddLoadECP(TLoadObj& pLoad)
{
    String key;
    TECPObject* pECP;

    if (((pLoad.DailyShape != "") || (pLoad.DutyShape != "") || (pLoad.GrowthShape != "") || (pLoad.YearlyShape != "") || (pLoad.CVRshape != "") || (pLoad.Spectrum != "defaultload")))
    {
        key = Format("Load:%s:%s:%s:%s:%s:%s", pLoad.DailyShape.c_str(), pLoad.DutyShape.c_str(), pLoad.GrowthShape.c_str(), pLoad.YearlyShape.c_str(), pLoad.CVRshape.c_str(), pLoad.Spectrum.c_str());
        pECP = GetECP(key);
        if (pECP == nullptr)
        {
            pECP = new TECPObject(LoadEcp);
            pECP->LName = key;
            pECP->Set_UUID(GetDevUuid(ECProfile, key, 0));
            pECP->daily = pLoad.DailyShape;
            pECP->duty = pLoad.DutyShape;
            pECP->growth = pLoad.GrowthShape;
            pECP->cvr = pLoad.CVRshape;
            pECP->yearly = pLoad.YearlyShape;
            if (pLoad.Spectrum != "defaultload")
                pECP->spectrum = pLoad.Spectrum;
            AddECP(pECP);
        }
        pECP->AddConnection(pLoad);
    }
}

void AddSolarECP(TPVsystemObj& pPV)
{
    String key;
    TECPObject* pECP;
    if (((pPV.DailyShape != "") || (pPV.DutyShape != "") || (pPV.YearlyShape != "") || (pPV.DailyTShape != "") || (pPV.DutyTShape != "") || (pPV.YearlyTShape != "") || (pPV.Spectrum != "")))
    {
        key = Format("PV:%s:%s:%s:%s:%s:%s:%s", pPV.DailyShape.c_str(), pPV.DutyShape.c_str(), pPV.YearlyShape.c_str(), pPV.DailyTShape.c_str(), pPV.DutyTShape.c_str(), pPV.YearlyTShape.c_str(), pPV.Spectrum.c_str());
        pECP = GetECP(key);
        if (pECP == nullptr)
        {
            pECP = new TECPObject(PvEcp);
            pECP->LName = key;
            pECP->Set_UUID(GetDevUuid(ECProfile, key, 0));
            pECP->daily = pPV.DailyShape;
            pECP->duty = pPV.DutyShape;
            pECP->yearly = pPV.YearlyShape;
            pECP->Tdaily = pPV.DailyTShape;
            pECP->Tduty = pPV.DutyTShape;
            pECP->Tyearly = pPV.YearlyTShape;
            pECP->spectrum = pPV.Spectrum;
            AddECP(pECP);
        }
        pECP->AddConnection(pPV);
    }
}

void AddStorageECP(TStorageObj& pBat)
{
    String key;
    TECPObject* pECP;

    if (((pBat.DailyShape != "") || (pBat.DutyShape != "") || (pBat.YearlyShape != "") || (pBat.Spectrum != "")))
    {
        key = Format("Bat:%s:%s:%s:%s", pBat.DailyShape.c_str(), pBat.DutyShape.c_str(), pBat.YearlyShape.c_str(), pBat.Spectrum.c_str());
        pECP = GetECP(key);
        if (pECP == nullptr)
        {
            pECP = new TECPObject(BatEcp);
            pECP->LName = key;
            pECP->Set_UUID(GetDevUuid(ECProfile, key, 0));
            pECP->daily = pBat.DailyShape;
            pECP->duty = pBat.DutyShape;
            pECP->yearly = pBat.YearlyShape;
            pECP->spectrum = pBat.Spectrum;
            AddECP(pECP);
        }
        pECP->AddConnection(pBat);
    }
}

void AddGeneratorECP(TGeneratorObj& pGen)
{
    String key;
    TECPObject* pECP;

    if (((pGen.DailyDispShape != "") || (pGen.DutyShape != "") || (pGen.YearlyShape != "") || (pGen.Spectrum != "defaultgen")))
    {
        key = Format("Gen:%s:%s:%s:%s", pGen.DailyDispShape.c_str(), pGen.DutyShape.c_str(), pGen.YearlyShape.c_str(), pGen.Spectrum.c_str());
        pECP = GetECP(key);
        if (pECP == nullptr)
        {
            pECP = new TECPObject(GenEcp);
            pECP->LName = key;
            pECP->Set_UUID(GetDevUuid(ECProfile, key, 0));
            pECP->daily = pGen.DailyDispShape;
            pECP->duty = pGen.DutyShape;
            pECP->yearly = pGen.YearlyShape;
            if (pGen.Spectrum != "defaultgen")
                pECP->spectrum = pGen.Spectrum;
            AddECP(pECP);
        }
        pECP->AddConnection(pGen);
    }
}

void DefaultCircuitUUIDs(TUuid& fdrID, TUuid& subID, TUuid& rgnID, TUuid&subGeoID)
{
    if (UuidList.empty())
        StartUuidList(ActiveCircuit[ActiveActor]->NumBuses + 2 * ActiveCircuit[ActiveActor]->NumDevices);
    fdrID = ActiveCircuit[ActiveActor]->Get_UUID();
    subID = GetDevUuid(Station, "Station", 1);
    rgnID = GetDevUuid(GeoRgn, "GeoRgn", 1);
    subGeoID = GetDevUuid(SubGeoRgn, "SubGeoRgn", 1);
}

void WriteHashedUUIDs(Textfile& F)
{
    for (int i = 0; i < UuidList.size(); ++i)
    {
        if (UuidKeyList[i].size() < 1)
            break;
        WriteLn(F, Format("%s %s", UuidKeyList[i].c_str(), UUIDToString(UuidList[i]).c_str()));
    }
}

// terminals are uniquely identified by class (DSSObjType), plus name and sequence
TUuid GetTermUuid(TDSSCktElement& pElem, int Seq)
{
    String key = IntToStr(pElem.DSSObjType) + "=" + pElem.get_Name() + "=" + IntToStr(Seq);
    return GetHashedUuid(key);
}

String GetBaseVName(double val)
{
//  Result = Format("BaseV_%.3f", [val]);
    return "BaseV_" + FloatToStrF(val, ffFixed, 6, 4);
}

TUuid GetBaseVUuid(double val)
{
    return GetDevUuid(BaseV, GetBaseVName(val), 1);
}

String GetOpLimVName(double val)
{
    return "OpLimV_" + FloatToStrF(val, ffFixed, 6, 4);
}

TUuid GetOpLimVUuid(double val)
{
    return GetDevUuid(OpLimV, GetOpLimVName(val), 1);
}

String GetOpLimIName(double norm, double emerg)
{
    return "OpLimI_" + FloatToStrF(norm, ffFixed, 6, 1) + "_" + FloatToStrF(emerg, ffFixed, 6, 1);
}

TUuid GetOpLimIUuid(double norm, double emerg)
{
    return GetDevUuid(OpLimI, GetOpLimIName(norm, emerg), 1);
}

void DoubleNode(ProfileChoice prf, const String& Node, double val)
{
    FD->WriteCimLn(prf, Format("  <cim:%s>%.8g</cim:%s>", Node.c_str(), val, Node.c_str()));
}

void IntegerNode(ProfileChoice prf, const String& Node, int val)
{
    FD->WriteCimLn(prf, Format("  <cim:%s>%d</cim:%s>", Node.c_str(), val, Node.c_str()));
}

void BooleanNode(ProfileChoice prf, const String& Node, bool val)
{
    String i;

    if (val)
        i = "true";
    else
        i = "false";
    FD->WriteCimLn(prf, Format("  <cim:%s>%s</cim:%s>", Node.c_str(), i.c_str(), Node.c_str()));
}

void RefNode(ProfileChoice prf, const String& Node, TNamedObject& Obj)
{
    FD->WriteCimLn(prf, Format("  <cim:%s rdf:resource=\"urn:uuid:%s\"/>", Node.c_str(), Obj.Get_CIM_ID().c_str()));
}

void RefNode(ProfileChoice prf, const String& Node, TNamedObject* Obj)
{
    RefNode(prf, Node, *Obj);
}

void UuidNode(ProfileChoice prf, const String& Node, TUuid ID)
{
    FD->WriteCimLn(prf, Format("  <cim:%s rdf:resource=\"urn:uuid:%s\"/>", Node.c_str(), UUIDToCIMString(ID).c_str()));
}

void LineCodeRefNode(ProfileChoice prf, TLineCode& List, const String& Name)
{
    if (List.SetActive(Name))
    {
        TLineCodeObj *Obj = (TLineCodeObj *) List.GetActiveObj();
        FD->WriteCimLn(prf, Format("  <cim:ACLineSegment.PerLengthImpedance rdf:resource=\"urn:uuid:%s\"/>", Obj->Get_CIM_ID().c_str()));
    }
}

void LineSpacingRefNode(ProfileChoice prf, TDSSClass& List, const String &Name)
{
    if (List.SetActive(Name))
    {
        TDSSObject* Obj = (TDSSObject*) List.GetActiveObj(); // should be a TLineGeometryObj or TLineSpacingObj
        FD->WriteCimLn(prf, Format("  <cim:ACLineSegment.WireSpacingInfo rdf:resource=\"urn:uuid:%s\"/>", Obj->Get_CIM_ID().c_str()));
    }
}

void PhaseWireRefNode(ProfileChoice prf, TConductorDataObj& Obj)
{
    FD->WriteCimLn(prf, Format("  <cim:ACLineSegmentPhase.WireInfo rdf:resource=\"urn:uuid:%s\"/>", Obj.Get_CIM_ID().c_str()));
}

void CircuitNode(ProfileChoice prf, TNamedObject& Obj)
{
    FD->WriteCimLn(prf, Format("  <cim:Equipment.EquipmentContainer rdf:resource=\"urn:uuid:%s\"/>", Obj.Get_CIM_ID().c_str()));
}

void CircuitNode(ProfileChoice prf, TNamedObject* Obj)
{
    CircuitNode(prf, *Obj);
}

String FirstPhaseString(TDSSCktElement& pElem, int bus)
{
    String val = PhaseString(pElem, bus);
    if (val != "")
        return val.substr(0, 1);
    else
        return "A";
}

void GeneratorControlEnum(ProfileChoice prf, const String& val)
{
    FD->WriteCimLn(prf, Format("  <cim:GeneratingUnit.genControlSource rdf:resource=\"%s#GeneratorControlSource.%s\"/>", CIM_NS.c_str(), val.c_str()));
}

void BatteryStateEnum(ProfileChoice prf, int val)
{
    String str = "waiting";
    if (val == STORE_CHARGING)
        str = "charging";
    else if (val == STORE_DISCHARGING)
        str = "discharging";
    FD->WriteCimLn(prf, Format("  <cim:BatteryUnit.batteryState rdf:resource=\"%s#BatteryStateKind.%s\"/>", CIM_NS.c_str(), str.c_str()));
}

void ConverterControlEnum(ProfileChoice prf, int varMode, bool CIMdynamics)
{
    String str = "constantPowerFactor"; // VARMODEPF
    if (CIMdynamics)
        str = "dynamic";
    else if (varMode == VARMODEKVAR)
        str = "constantReactivePower";
    FD->WriteCimLn(prf, Format("  <cim:PowerElectronicsConnection.controlMode rdf:resource=\"%s#ConverterControlModeKind.%s\"/>", CIM_NS.c_str(), str.c_str()));
}

void SynchMachTypeEnum(ProfileChoice prf, const String& val)
{
    FD->WriteCimLn(prf, Format("  <cim:SynchronousMachine.type rdf:resource=\"%s#SynchronousMachineType.%s\"/>", CIM_NS.c_str(), val.c_str()));
}

void SynchMachModeEnum(ProfileChoice prf, const String& val)
{
    FD->WriteCimLn(prf, Format("  <cim:SynchronousMachine.operatingMode rdf:resource=\"%s#SynchronousMachineOperatingMode.%s\"/>", CIM_NS.c_str(), val.c_str()));
}

void RegulatingControlEnum(ProfileChoice prf, const String& val)
{
    FD->WriteCimLn(prf, Format("  <cim:RegulatingControl.mode rdf:resource=\"%s#RegulatingControlModeKind.%s\"/>", CIM_NS.c_str(), val.c_str()));
}

void WindingConnectionEnum(ProfileChoice prf, const String& val)
{
    FD->WriteCimLn(prf, Format("  <cim:TransformerEndInfo.connectionKind rdf:resource=\"%s#WindingConnection.%s\"/>", CIM_NS.c_str(), val.c_str()));
}

void ConductorInsulationEnum(ProfileChoice prf, const String& val)
{
    FD->WriteCimLn(prf, Format("  <cim:WireInfo.insulationMaterial rdf:resource=\"%s#WireInsulationKind.%s\"/>", CIM_NS.c_str(), val.c_str()));
}

void ConductorUsageEnum(ProfileChoice prf, const String& val)
{
    FD->WriteCimLn(prf, Format("  <cim:WireSpacingInfo.usage rdf:resource=\"%s#WireUsageKind.%s\"/>", CIM_NS.c_str(), val.c_str()));
}

void CableShieldMaterialEnum(ProfileChoice prf, const String& val)
{
//  FD->WriteCimLn (prf, Format ("  <cim:CableInfo.shieldMaterial rdf:resource=\"%s#CableShieldMaterialKind.%s\"/>",
//    [CIM_NS, val]));
}

void ConductorMaterialEnum(ProfileChoice prf, const String& val)
{
//  FD->WriteCimLn (prf, Format ("  <cim:WireInfo.material rdf:resource=\"%s#WireMaterialKind.%s\"/>",
//    [CIM_NS, val]));
}

void CableOuterJacketEnum(ProfileChoice prf, const String& val)
{
//  FD->WriteCimLn (prf, Format ("  <cim:CableInfo.outerJacketKind rdf:resource=\"%s#CableOuterJacketKind.%s\"/>",
//    [CIM_NS, val]));
}

void CableConstructionEnum(ProfileChoice prf, const String& val)
{
//  FD->WriteCimLn (prf, Format ("  <cim:CableInfo.constructionKind rdf:resource=\"%s#CableConstructionKind.%s\"/>",
//    [CIM_NS, val]));
}

void TransformerControlEnum(ProfileChoice prf, const String& val)
{
//  FD->WriteCimLn (prf, Format ("  <cim:RatioTapChanger.tculControlMode rdf:resource=\"%s#TransformerControlMode.%s\"/>",
//    [CIM_NS, val]));
}

void MonitoredPhaseNode(ProfileChoice prf, const String& val)
{
    FD->WriteCimLn(prf, Format("  <cim:RegulatingControl.monitoredPhase rdf:resource=\"%s#PhaseCode.%s\"/>", CIM_NS.c_str(), val.c_str()));
}

void OpLimitDirectionEnum(ProfileChoice prf, const String& val)
{
    FD->WriteCimLn(prf, Format("  <cim:OperationalLimitType.direction rdf:resource=\"%s#OperationalLimitDirectionKind.%s\"/>", CIM_NS.c_str(), val.c_str()));
}

// next several for DERIEEEType1 CIM dynamics
void NormalOpCatEnum(ProfileChoice prf, const String& val)
{
    FD->WriteCimLn(prf, Format("  <cim:DERNameplateData.normalOPcatKind rdf:resource=\"%s#NormalOPcatKind.%s\"/>", CIM_NS.c_str(), val.c_str()));
}

/*
void SupportedModesEnum (ProfileChoice prf, const String& val)
{
  FD->WriteCimLn (prf, Format ("  <cim:DERNameplateData.supportedModesKind rdf:resource=\"%s#SupportedModesKind.%s\"/>",
    [CIM_NS, val]));
}
*/
void PowerFactorExcitationEnum(ProfileChoice prf, const String& val)
{
    FD->WriteCimLn(prf, Format("  <cim:ConstantPowerFactorSettings.constantPowerFactorExcitationKind rdf:resource=\"%s#ConstantPowerFactorSettingKind.%s\"/>", CIM_NS.c_str(), val.c_str()));
}

void RemoteInputSignalEnum(ProfileChoice prf, const String& val)
{
    FD->WriteCimLn(prf, Format("  <cim:RemoteInputSignal.remoteSignalType rdf:resource=\"%s#RemoteSignalKind.%s\"/>", CIM_NS.c_str(), val.c_str()));
}
// end of Enums for DERIEEEType1 CIM dynamics

void StringNode(ProfileChoice prf, const String& Node, const String& val)
{
    FD->WriteCimLn(prf, Format("  <cim:%s>%s</cim:%s>", Node.c_str(), val.c_str(), Node.c_str()));
}

void StartInstance(ProfileChoice prf, const String& Root, TNamedObject& Obj)
{
    FD->StartInstance(prf, Root, Obj);
}

void StartInstance(ProfileChoice prf, const String& Root, TNamedObject* Obj)
{
    FD->StartInstance(prf, Root, *Obj);
}

void StartFreeInstance(ProfileChoice prf, const String& Root, TUuid uuid)
{
    FD->StartFreeInstance(prf, Root, uuid);
}

void EndInstance(ProfileChoice prf, const String& Root)
{
    FD->EndInstance(prf, Root);
}

void XfmrTankPhasesAndGround(ProfileChoice fprf, ProfileChoice eprf, TTransfObj& pXf, int bus)
{
    String ordered_phs;
    int j1, j2;
    bool reverse_ground, wye_ground, wye_unground;

    j1 = (bus - 1) * pXf.Fnconds + 1;
    j2 = j1 + pXf.Fnphases;
    reverse_ground = false;
    wye_ground = false;
    wye_unground = false;
    //  WriteLn(Format("  Testing %d and %d", [j1, j2]));
    if ((pXf.WINDING_[bus - 1].Connection == 1))
    { // delta
        BooleanNode(fprf, "TransformerEnd.grounded", false);
    }
    else if ((pXf.NodeRef[j2 - 1] == 0))
    { // last conductor is grounded solidly
        BooleanNode(FunPrf, "TransformerEnd.grounded", true);
        DoubleNode(EpPrf, "TransformerEnd.rground", 0.0);
        DoubleNode(EpPrf, "TransformerEnd.xground", 0.0);
        wye_ground = true;
    }
    else if ((pXf.NodeRef[j1 - 1] == 0))
    { // first conductor is grounded solidly, but should be reversed
        BooleanNode(FunPrf, "TransformerEnd.grounded", true);
        DoubleNode(EpPrf, "TransformerEnd.rground", 0.0);
        DoubleNode(EpPrf, "TransformerEnd.xground", 0.0);
        reverse_ground = true;
    }
    else if ((pXf.WINDING_[bus - 1].Rneut < 0.0))
    { // probably wye ungrounded
        BooleanNode(FunPrf, "TransformerEnd.grounded", false);
        wye_unground = true;
    }
    else
    { // not delta, not wye solidly grounded or ungrounded
        BooleanNode(FunPrf, "TransformerEnd.grounded", true);
        DoubleNode(EpPrf, "TransformerEnd.rground", pXf.WINDING_[bus - 1].Rneut);
        DoubleNode(EpPrf, "TransformerEnd.xground", pXf.WINDING_[bus - 1].Xneut);
    }
    ordered_phs = PhaseOrderString(pXf, bus);
    if ((ordered_phs == "s1"))
        ordered_phs = "s1N";
    else if ((ordered_phs == "s2"))
        ordered_phs = "Ns2";
    else if (reverse_ground)
        ordered_phs = "N" + ordered_phs;
    else if (wye_ground)
        ordered_phs = ordered_phs + "N";
    else if (wye_unground)
        ordered_phs = ordered_phs + "N";

    FD->WriteCimLn(fprf, Format("  <cim:TransformerTankEnd.orderedPhases rdf:resource=\"%s#OrderedPhaseCodeKind.%s\"/>", CIM_NS.c_str(), ordered_phs.c_str()));
}

void PhaseNode(ProfileChoice prf, const String& Root, const String& val)
{
    FD->WriteCimLn(prf, Format("  <cim:%s.phase rdf:resource=\"%s#PhaseCode.%s\"/>", Root.c_str(), CIM_NS.c_str(), val.c_str()));
}

void PhaseKindNode(ProfileChoice prf, const String& Root, const String& val)
{
    FD->WriteCimLn(prf, Format("  <cim:%s.phase rdf:resource=\"%s#SinglePhaseKind.%s\"/>", Root.c_str(), CIM_NS.c_str(), val.c_str()));
}

void PhaseSideNode(ProfileChoice prf, const String& Root, int Side, const String& val)
{
    FD->WriteCimLn(prf, Format("  <cim:%s.phaseSide%d rdf:resource=\"%s#SinglePhaseKind.%s\"/>", Root.c_str(), Side, CIM_NS.c_str(), val.c_str()));
}

void ShuntConnectionKindNode(ProfileChoice prf, const String& Root, const String& val) // D, Y, Yn, I
{
    FD->WriteCimLn(prf, Format("  <cim:%s.phaseConnection rdf:resource=\"%s#PhaseShuntConnectionKind.%s\"/>", Root.c_str(), CIM_NS.c_str(), val.c_str()));
}

void WindingConnectionKindNode(ProfileChoice prf, const String& val) // D, Y, Z, Yn, Zn, A, I
{
    FD->WriteCimLn(prf, Format("  <cim:PowerTransformerEnd.connectionKind rdf:resource=\"%s#WindingConnection.%s\"/>", CIM_NS.c_str(), val.c_str()));
}

// we specify phases except for balanced three-phase
void AttachLinePhases(TLineObj& pLine)
{
    String s, phs;
    int i;
    TNamedObject* pPhase;

    pPhase = new TNamedObject("dummy");
    s = PhaseOrderString(pLine, 1);
    if (pLine.NumConductorData() > s.size())
        s = s + "N"; // so we can specify the neutral conductor
    for (i = 1; i <= s.size(); ++i)
    {
        phs = s[i - 1];
        if (phs == "s")
            continue;
        if (phs == "1")
            phs = "s1";
        if (phs == "2")
            phs = "s2";
        pPhase->LName = pLine.get_Name() + "_" + phs;
        pPhase->Set_UUID(GetDevUuid(LinePhase, pPhase->LName, 1));
        StartInstance(FunPrf, "ACLineSegmentPhase", *pPhase);
        PhaseKindNode(FunPrf, "ACLineSegmentPhase", phs);
        IntegerNode(FunPrf, "ACLineSegmentPhase.sequenceNumber", i);
        if (i <= pLine.NumConductorData())
            PhaseWireRefNode(CatPrf, *pLine.FetchConductorData(i));
        RefNode(FunPrf, "ACLineSegmentPhase.ACLineSegment", pLine);
        UuidNode(GeoPrf, "PowerSystemResource.Location",
            GetDevUuid(LineLoc, pLine.get_Name(), 1));
        EndInstance(FunPrf, "ACLineSegmentPhase");
    }
    delete pPhase;
}

void AttachSwitchPhases(TLineObj& pLine)
{
    String s1, s2, phs1, phs2;
    int i;
    TNamedObject* pPhase;

  // also write the switch phases if needed to support transpositions
    s1 = PhaseOrderString(pLine, 1);
    s2 = PhaseOrderString(pLine, 2);
    if ((pLine.Fnphases == 3) && (s1.size() == 3) && (s1 == s2))
        return;
    pPhase = new TNamedObject("dummy");
    for (i = 0; i < s1.size(); ++i)
    {
        phs1 = s1[i];
        phs2 = s2[i];
        if (phs1 == "s")
            continue;
        if (phs2 == "s")
            continue;
        if (phs1 == "1")
            phs1 = "s1";
        if (phs1 == "2")
            phs1 = "s2";
        if (phs2 == "1")
            phs2 = "s1";
        if (phs2 == "2")
            phs2 = "s2";
        pPhase->LName = pLine.get_Name() + "_" + phs1;
        pPhase->Set_UUID(GetDevUuid(LinePhase, pPhase->LName, 1));
        StartInstance(FunPrf, "SwitchPhase", pPhase);
        BooleanNode(SshPrf, "SwitchPhase.closed", pLine.Get_ConductorClosed(0, ActiveActor));
        BooleanNode(FunPrf, "SwitchPhase.normalOpen", !pLine.Get_ConductorClosed(0, ActiveActor));
        PhaseSideNode(FunPrf, "SwitchPhase", 1, phs1);
        PhaseSideNode(FunPrf, "SwitchPhase", 2, phs2);
        RefNode(FunPrf, "SwitchPhase.Switch", pLine);
        UuidNode(GeoPrf, "PowerSystemResource.Location", GetDevUuid(LineLoc, pLine.get_Name(), 1));
        EndInstance(FunPrf, "SwitchPhase");
    }
    delete pPhase;
}

void AttachCapPhases(TCapacitorObj& pCap, const TUuid& geoUUID, double sections)
{
    String s, phs;
    int i;
    TNamedObject* pPhase;
    double bph;

    if (pCap.Fnphases == 3)
        return;
    pPhase = new TNamedObject("dummy");
    s = PhaseString(pCap, 1);
    // with pCap do
    {
        bph = 0.001 * pCap.Ftotalkvar / pCap.kvrating / pCap.kvrating / pCap.FNumSteps / pCap.Fnphases;
        if (pCap.Connection == 1)
            s = DeltaPhaseString(pCap);
    }
    for (i = 0; i < s.size(); ++i)
    {
        phs = s[i];
        pPhase->LName = pCap.get_Name() + "_" + phs;
        pPhase->Set_UUID(GetDevUuid(CapPhase, pPhase->LName, 1));
        StartInstance(FunPrf, "LinearShuntCompensatorPhase", pPhase);
        PhaseKindNode(FunPrf, "ShuntCompensatorPhase", phs);
        DoubleNode(EpPrf, "LinearShuntCompensatorPhase.bPerSection", bph);
        DoubleNode(EpPrf, "LinearShuntCompensatorPhase.gPerSection", 0.0);
        IntegerNode(EpPrf, "ShuntCompensatorPhase.normalSections", pCap.FNumSteps);
        IntegerNode(EpPrf, "ShuntCompensatorPhase.maximumSections", pCap.FNumSteps);
        DoubleNode(SshPrf, "ShuntCompensatorPhase.sections", sections);
        RefNode(FunPrf, "ShuntCompensatorPhase.ShuntCompensator", pCap);
        UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
        EndInstance(FunPrf, "LinearShuntCompensatorPhase");
    }
    delete pPhase;
}

void AttachSecondaryPhases(TLoadObj &pLoad, const TUuid& geoUUID, TNamedObject& pPhase, double p, double q, const String& phs)
{
    pPhase.LName = pLoad.get_Name() + "_" + phs;
    pPhase.Set_UUID(GetDevUuid(LoadPhase, pPhase.LName, 1));
    StartInstance(FunPrf, "EnergyConsumerPhase", pPhase);
    PhaseKindNode(FunPrf, "EnergyConsumerPhase", phs);
    DoubleNode(SshPrf, "EnergyConsumerPhase.p", p);
    DoubleNode(SshPrf, "EnergyConsumerPhase.q", q);
    RefNode(FunPrf, "EnergyConsumerPhase.EnergyConsumer", pLoad);
    UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
    EndInstance(FunPrf, "EnergyConsumerPhase");
}

void AttachLoadPhases(TLoadObj& pLoad, const TUuid& geoUUID)
{
    String s, phs;
    int i;
    TNamedObject* pPhase;
    double p, q;
    bool bAllowSec;

    if (pLoad.Fnphases == 3)
        return;
    // TODO - use a more robust filter than pLoad.LoadClass, which is > 1 only for PNNL taxonomy imports
    if (pLoad.LoadClass <= 1)
        bAllowSec = true;
    else
        bAllowSec = false;
    p = 1000.0 * pLoad.kWBase / pLoad.Fnphases;
    q = 1000.0 * pLoad.kvarBase / pLoad.Fnphases;
    if (pLoad.Connection == 1)
        s = DeltaPhaseString(pLoad);
    else
        s = PhaseString(pLoad, 1, bAllowSec);

    pPhase = new TNamedObject("dummy");
    // first, filter out what appear to be split secondary loads
    // these can be 2-phase loads (balanced) nominally 0.208 kV, or
    //  1-phase loads (possibly unbalanced) nominally 0.12 kV
    //  TODO - handle s1 to s2 240-volt loads; these would be s12, which is not a valid SinglePhaseKind
    if ((pLoad.kVLoadBase < 0.25) && bAllowSec)
    {
        if (pLoad.Fnphases == 2)
        {
            AttachSecondaryPhases(pLoad, geoUUID, *pPhase, p, q, "s1");
            AttachSecondaryPhases(pLoad, geoUUID, *pPhase, p, q, "s2");
            delete pPhase;
            return;
        }
        else
        {
            AttachSecondaryPhases(pLoad, geoUUID, *pPhase, p, q, s);
            delete pPhase;
            return;
        }
    }

    for (i = 0; i < s.size(); ++i)
    {
        phs = s[i];
        pPhase->LName = pLoad.get_Name() + "_" + phs;
        pPhase->Set_UUID(GetDevUuid(LoadPhase, pPhase->LName, 1));
        StartInstance(FunPrf, "EnergyConsumerPhase", pPhase);
        PhaseKindNode(FunPrf, "EnergyConsumerPhase", phs);
        DoubleNode(SshPrf, "EnergyConsumerPhase.p", p);
        DoubleNode(SshPrf, "EnergyConsumerPhase.q", q);
        RefNode(FunPrf, "EnergyConsumerPhase.EnergyConsumer", pLoad);
        UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
        EndInstance(FunPrf, "EnergyConsumerPhase");
    }
    delete pPhase;
}

void AttachSecondaryGenPhases(TGeneratorObj& pGen, const TUuid& geoUUID, TNamedObject& pPhase, double p, double q, const String& phs)
{
    pPhase.LName = pGen.get_Name() + "_" + phs;
    pPhase.Set_UUID(GetDevUuid(GenPhase, pPhase.LName, 1));
    StartInstance(FunPrf, "SynchronousMachinePhase", pPhase);
    PhaseKindNode(FunPrf, "SynchronousMachinePhase", phs);
    DoubleNode(SshPrf, "SynchronousMachinePhase.p", p);
    DoubleNode(SshPrf, "SynchronousMachinePhase.q", q);
    RefNode(FunPrf, "SynchronousMachinePhase.SynchronousMachine", pGen);
    UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
    EndInstance(FunPrf, "SynchronousMachinePhase");
}

void AttachGeneratorPhases(TGeneratorObj& pGen, const TUuid& geoUUID)
{
    String s, phs;
    int i;
    TNamedObject* pPhase;
    double p, q;

    if (pGen.Fnphases == 3)
        return;

    p = 1000.0 * pGen.Get_PresentkW() / pGen.Fnphases;
    q = 1000.0 * pGen.Get_Presentkvar() / pGen.Fnphases;
    if (pGen.Connection == 1)
        s = DeltaPhaseString(pGen);
    else
        s = PhaseString(pGen, 1);

    pPhase = new TNamedObject("dummy");
    //  TODO - handle s1 to s2 240-volt loads; these would be s12, which is not a valid SinglePhaseKind
    if (pGen.Get_PresentkV() < 0.25)
    {
        if (pGen.Fnphases == 2)
        {
            AttachSecondaryGenPhases(pGen, geoUUID, *pPhase, p, q, "s1");
            AttachSecondaryGenPhases(pGen, geoUUID, *pPhase, p, q, "s2");
            delete pPhase;
            return;
        }
        else
        {
            AttachSecondaryGenPhases(pGen, geoUUID, *pPhase, p, q, s);
            delete pPhase;
            return;
        }
    }

    for (int i = 0; i < s.size(); ++i)
    {
        phs = s[i];
        pPhase->LName = pGen.get_Name() + "_" + phs;
        pPhase->Set_UUID(GetDevUuid(GenPhase, pPhase->LName, 1));
        StartInstance(FunPrf, "SynchronousMachinePhase", pPhase);
        PhaseKindNode(FunPrf, "SynchronousMachinePhase", phs);
        DoubleNode(SshPrf, "SynchronousMachinePhase.p", p);
        DoubleNode(SshPrf, "SynchronousMachinePhase.q", q);
        RefNode(FunPrf, "SynchronousMachinePhase.SynchronousMachine", pGen);
        UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
        EndInstance(FunPrf, "SynchronousMachinePhase");
    }
    delete pPhase;
}

void AttachSecondarySolarPhases(TPVsystemObj& pPV, const TUuid& geoUUID, TNamedObject& pPhase, double p, double q, const String &phs)
{
    pPhase.LName = pPV.get_Name() + "_" + phs;
    pPhase.Set_UUID(GetDevUuid(SolarPhase, pPhase.LName, 1));
    StartInstance(FunPrf, "PowerElectronicsConnectionPhase", pPhase);
    PhaseKindNode(FunPrf, "PowerElectronicsConnectionPhase", phs);
    DoubleNode(SshPrf, "PowerElectronicsConnectionPhase.p", p);
    DoubleNode(SshPrf, "PowerElectronicsConnectionPhase.q", q);
    RefNode(FunPrf, "PowerElectronicsConnectionPhase.PowerElectronicsConnection", pPV);
    UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
    EndInstance(FunPrf, "PowerElectronicsConnectionPhase");
}

void AttachSolarPhases(TPVsystemObj& pPV, const TUuid& geoUUID)
{
    String s, phs;
    int i;
    TNamedObject* pPhase;
    double p, q;

    if (pPV.Fnphases == 3)
        return;
    p = 1000.0 * pPV.Get_PresentkW() / pPV.Fnphases;
    q = 1000.0 * pPV.Get_Presentkvar() / pPV.Fnphases;
    if (pPV.Connection == 1)
        s = DeltaPhaseString(pPV);
    else
        s = PhaseString(pPV, 1);

    pPhase = new TNamedObject("dummy");
    //  TODO - handle s1 to s2 240-volt loads; these would be s12, which is not a valid SinglePhaseKind
    if (pPV.Get_PresentkV() < 0.25)
    {
        if (pPV.Fnphases == 2)
        {
            AttachSecondarySolarPhases(pPV, geoUUID, *pPhase, p, q, "s1");
            AttachSecondarySolarPhases(pPV, geoUUID, *pPhase, p, q, "s2");
            delete pPhase;
            return;
        }
        else
        {
            AttachSecondarySolarPhases(pPV, geoUUID, *pPhase, p, q, s);
            delete pPhase;
            return;
        }
    }

    for (i = 0; i < s.size(); ++i)
    {
        phs = s[i];
        pPhase->LName = pPV.get_Name() + "_" + phs;
        pPhase->Set_UUID(GetDevUuid(SolarPhase, pPhase->LName, 1));
        StartInstance(FunPrf, "PowerElectronicsConnectionPhase", pPhase);
        PhaseKindNode(FunPrf, "PowerElectronicsConnectionPhase", phs);
        DoubleNode(SshPrf, "PowerElectronicsConnectionPhase.p", p);
        DoubleNode(SshPrf, "PowerElectronicsConnectionPhase.q", q);
        RefNode(FunPrf, "PowerElectronicsConnectionPhase.PowerElectronicsConnection", pPV);
        UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
        EndInstance(FunPrf, "PowerElectronicsConnectionPhase");
    }
    delete pPhase;
}

void AttachSecondaryStoragePhases(TStorageObj& pBat, const TUuid& geoUUID, TNamedObject& pPhase, double p, double q, const String& phs)
{
    pPhase.LName = pBat.get_Name() + "_" + phs;
    pPhase.Set_UUID(GetDevUuid(BatteryPhase, pPhase.LName, 1));
    StartInstance(FunPrf, "PowerElectronicsConnectionPhase", pPhase);
    PhaseKindNode(FunPrf, "PowerElectronicsConnectionPhase", phs);
    DoubleNode(SshPrf, "PowerElectronicsConnectionPhase.p", p);
    DoubleNode(SshPrf, "PowerElectronicsConnectionPhase.q", q);
    RefNode(FunPrf, "PowerElectronicsConnectionPhase.PowerElectronicsConnection", pBat);
    UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
    EndInstance(FunPrf, "PowerElectronicsConnectionPhase");
}

void AttachStoragePhases(TStorageObj& pBat, const TUuid& geoUUID)
{
    String s, phs;
    int i;
    TNamedObject* pPhase;
    double p, q;

    if (pBat.Fnphases == 3)
        return;
    p = 1000.0 * pBat.Get_PresentkW() / pBat.Fnphases;
    q = 1000.0 * pBat.Get_Presentkvar() / pBat.Fnphases;
    if (pBat.Connection == 1)
        s = DeltaPhaseString(pBat);
    else
        s = PhaseString(pBat, 1);

    pPhase = new TNamedObject("dummy");
    //  TODO - handle s1 to s2 240-volt loads; these would be s12, which is not a valid SinglePhaseKind
    if (pBat.Get_PresentkV() < 0.25)
    {
        if (pBat.Fnphases == 2)
        {
            AttachSecondaryStoragePhases(pBat, geoUUID, *pPhase, p, q, "s1");
            AttachSecondaryStoragePhases(pBat, geoUUID, *pPhase, p, q, "s2");
            delete pPhase;
            return;
        }
        else
        {
            AttachSecondaryStoragePhases(pBat, geoUUID, *pPhase, p, q, s);
            delete pPhase;
            return;
        }
    }

    for (i = 0; i < s.size(); ++i)
    {
        phs = s[i];
        pPhase->LName = pBat.get_Name() + "_" + phs;
        pPhase->Set_UUID(GetDevUuid(BatteryPhase, pPhase->LName, 1));
        StartInstance(FunPrf, "PowerElectronicsConnectionPhase", *pPhase);
        PhaseKindNode(FunPrf, "PowerElectronicsConnectionPhase", phs);
        DoubleNode(SshPrf, "PowerElectronicsConnectionPhase.p", p);
        DoubleNode(SshPrf, "PowerElectronicsConnectionPhase.q", q);
        RefNode(FunPrf, "PowerElectronicsConnectionPhase.PowerElectronicsConnection", pBat);
        UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
        EndInstance(FunPrf, "PowerElectronicsConnectionPhase");
    }
    delete pPhase;
}

void WriteLoadModel(const String& Name, TUuid ID, double zP, double iP, double pP, double zQ, double iQ, double pQ, double eP, double eQ)
{
    FD->WriteCimLn(FunPrf, Format("<cim:LoadResponseCharacteristic rdf:about=\"urn:uuid:%s\">", UUIDToCIMString(ID).c_str()));
    StringNode(FunPrf, "IdentifiedObject.mRID", UUIDToCIMString(ID));
    StringNode(FunPrf, "IdentifiedObject.name", Name);
    if ((eP > 0.0) || (eQ > 0.0))
        BooleanNode(FunPrf, "LoadResponseCharacteristic.exponentModel", true);
    else
        BooleanNode(FunPrf, "LoadResponseCharacteristic.exponentModel", false);

    DoubleNode(FunPrf, "LoadResponseCharacteristic.pConstantImpedance", zP);
    DoubleNode(FunPrf, "LoadResponseCharacteristic.pConstantCurrent", iP);
    DoubleNode(FunPrf, "LoadResponseCharacteristic.pConstantPower", pP);

    DoubleNode(FunPrf, "LoadResponseCharacteristic.qConstantImpedance", zQ);
    DoubleNode(FunPrf, "LoadResponseCharacteristic.qConstantCurrent", iQ);
    DoubleNode(FunPrf, "LoadResponseCharacteristic.qConstantPower", pQ);

    DoubleNode(FunPrf, "LoadResponseCharacteristic.pVoltageExponent", eP);
    DoubleNode(FunPrf, "LoadResponseCharacteristic.qVoltageExponent", eQ);
    DoubleNode(FunPrf, "LoadResponseCharacteristic.pFrequencyExponent", 0.0);
    DoubleNode(FunPrf, "LoadResponseCharacteristic.qFrequencyExponent", 0.0);
    FD->WriteCimLn(FunPrf, "</cim:LoadResponseCharacteristic>");
}

bool IsGroundBus(const String& S)
{
    int i;
    bool Result = true;
    i = Pos(".1", S);
    if (i > 0)
        Result = false;
    i = Pos(".2", S);
    if (i > 0)
        Result = false;
    i = Pos(".3", S);
    if (i > 0)
        Result = false;
    i = Pos(".", S);
    if (i == 0)
        Result = false;

    return Result;
}

void WritePositions(TDSSCktElement& pElem, const TUuid& geoUUID, TUuid crsUUID)
{
    int Nterm, j, ref;
    String BusName;

    Nterm = pElem.Fnterms;
    BusName = pElem.Get_FirstBus();
    StartFreeInstance(GeoPrf, "Location", geoUUID);
    StringNode(GeoPrf, "IdentifiedObject.mRID", UUIDToCIMString(geoUUID));
    StringNode(GeoPrf, "IdentifiedObject.name", pElem.LName + "_Loc");
    UuidNode(GeoPrf, "Location.CoordinateSystem", crsUUID);
    EndInstance(GeoPrf, "Location");

    for (j = 1; j <= Nterm; ++j)
    {
        if (!IsGroundBus(BusName))
        {
            ref = pElem.Terminals[j - 1].BusRef - 1;
            StartFreeInstance(GeoPrf, "PositionPoint", GetDevUuid(PosPt, pElem.ParentClass->get_myClass_name() + "." + pElem.LName, j));
            UuidNode(GeoPrf, "PositionPoint.Location", geoUUID);
            IntegerNode(GeoPrf, "PositionPoint.sequenceNumber", j);
            StringNode(GeoPrf, "PositionPoint.xPosition", FloatToStr(ActiveCircuit[ActiveActor]->Buses[ref]->x));
            StringNode(GeoPrf, "PositionPoint.yPosition", FloatToStr(ActiveCircuit[ActiveActor]->Buses[ref]->y));
            EndInstance(GeoPrf, "PositionPoint");
        }
        BusName = pElem.Get_NextBus();
    }
}

void WriteReferenceTerminals(TDSSCktElement& pElem, TUuid RefUuid, double norm = 0.0, double emerg = 0.0)
{
    int Nterm, j, ref;
    String BusName, TermName, LimitName;
    TUuid TermUuid, LimiTUuid;
    TOpLimitObject* pLimit;

    Nterm = pElem.Fnterms;
    BusName = pElem.Get_FirstBus();
    for (j = 1; j <= Nterm; ++j)
    {
        if (!IsGroundBus(BusName))
        {
            ref = pElem.Terminals[j - 1].BusRef - 1;
            TermName = pElem.get_Name() + "_T" + IntToStr(j);
            TermUuid = GetTermUuid(pElem, j);
            StartFreeInstance(FunPrf, "Terminal", TermUuid);
            StringNode(FunPrf, "IdentifiedObject.mRID", UUIDToCIMString(TermUuid));
            StringNode(FunPrf, "IdentifiedObject.name", TermName);
            UuidNode(FunPrf, "Terminal.ConductingEquipment", RefUuid);
            IntegerNode(FunPrf, "ACDCTerminal.sequenceNumber", j);
            FD->WriteCimLn(TopoPrf, Format("  <cim:Terminal.ConnectivityNode rdf:resource=\"urn:uuid:%s\"/>", ActiveCircuit[ActiveActor]->Buses[ref]->Get_CIM_ID().c_str()));
            if ((j == 1) && (norm > 0.0))
            {
                if (emerg < norm)
                    emerg = norm;
                LimitName = GetOpLimIName(norm, emerg);
                pLimit = GetOpLimit(LimitName);
                if (pLimit == nullptr)
                {
                    pLimit = new TOpLimitObject(norm, emerg);
                    pLimit->LName = LimitName;
                    pLimit->Set_UUID(GetDevUuid(OpLimI, LimitName, 0));
                    AddOpLimit(pLimit);
                }
                LimiTUuid = GetDevUuid(OpLimI, LimitName, 0);
                UuidNode(FunPrf, "ACDCTerminal.OperationalLimitSet", LimiTUuid);
            }
            EndInstance(FunPrf, "Terminal");
        }
        BusName = pElem.Get_NextBus();
    }
}

void WriteTerminals(TDSSCktElement &pElem, const TUuid& geoUUID, TUuid crsUUID, double norm = 0.0, double emerg = 0.0)
{
    WriteReferenceTerminals(pElem, pElem.Get_UUID(), norm, emerg);
    WritePositions(pElem, geoUUID, crsUUID);
}

void VbaseNode(ProfileChoice prf, TDSSCktElement& pElem)
{
    int j = pElem.Terminals[0].BusRef - 1;
    UuidNode(prf, "ConductingEquipment.BaseVoltage", GetBaseVUuid(sqrt(3.0) * ActiveCircuit[ActiveActor]->Buses[j]->kVBase));
}

void WriteXfmrCode(TXfmrCodeObj& pXfCd)
{
    TNamedObject* pName;
    double ratShort, ratEmerg, val, r, x, Zbase, TestKVA, pctIexc;
    int i, j, seq;

    pName = new TNamedObject("dummy");
    // with pXfCd do
    {
        StartInstance(CatPrf, "TransformerTankInfo", pXfCd);
        EndInstance(CatPrf, "TransformerTankInfo");
        ratShort = pXfCd.NormMaxHkVA / pXfCd.WINDING_[0].kVA;
        ratEmerg = pXfCd.EmergMaxHkVA / pXfCd.WINDING_[0].kVA;
        for (i = 1; i <= pXfCd.NumWindings; ++i)
        {
            Zbase = pXfCd.WINDING_[i - 1].kVLL;
            Zbase = 1000.0 * Zbase * Zbase / pXfCd.WINDING_[0].kVA;
            pName->LName = pXfCd.get_Name() + "_" + IntToStr(i);
            pName->Set_UUID(GetDevUuid(WdgInf, pXfCd.get_Name(), i));
            StartInstance(CatPrf, "TransformerEndInfo", pName);
            RefNode(CatPrf, "TransformerEndInfo.TransformerTankInfo", pXfCd);
            IntegerNode(CatPrf, "TransformerEndInfo.endNumber", i);
            if (pXfCd.Fnphases < 3)
            {
                WindingConnectionEnum(CatPrf, "I");
                if ((i == 3) && (pXfCd.WINDING_[i - 1].kVLL < 0.3)) // for center-tap secondary
                    IntegerNode(CatPrf, "TransformerEndInfo.phaseAngleClock", 6);
                else
                    IntegerNode(CatPrf, "TransformerEndInfo.phaseAngleClock", 0);
            }
            else
            {
                if (pXfCd.WINDING_[i - 1].Connection == 1)
                    WindingConnectionEnum(CatPrf, "D");
                else if ((pXfCd.WINDING_[i - 1].Rneut > 0.0) || (pXfCd.WINDING_[i - 1].Xneut > 0.0))
                    WindingConnectionEnum(CatPrf, "Yn");
                else
                    WindingConnectionEnum(CatPrf, "Y");
                if (pXfCd.WINDING_[i - 1].Connection != pXfCd.WINDING_[0].Connection)
                    IntegerNode(CatPrf, "TransformerEndInfo.phaseAngleClock", 1);
                else
                    IntegerNode(CatPrf, "TransformerEndInfo.phaseAngleClock", 0);
            }
            DoubleNode(CatPrf, "TransformerEndInfo.ratedU", 1000 * pXfCd.WINDING_[i - 1].kVLL);
            DoubleNode(CatPrf, "TransformerEndInfo.ratedS", 1000 * pXfCd.WINDING_[i - 1].kVA);
            DoubleNode(CatPrf, "TransformerEndInfo.shortTermS", 1000 * pXfCd.WINDING_[i - 1].kVA * ratShort);
            DoubleNode(CatPrf, "TransformerEndInfo.emergencyS", 1000 * pXfCd.WINDING_[i - 1].kVA * ratEmerg);
            DoubleNode(CatPrf, "TransformerEndInfo.r", pXfCd.WINDING_[i - 1].Rpu * Zbase);
            DoubleNode(CatPrf, "TransformerEndInfo.insulationU", 0.0);
            EndInstance(CatPrf, "TransformerEndInfo");
        }
        pName->LName = pXfCd.get_Name() + "_" + IntToStr(1);
        pName->Set_UUID(GetDevUuid(OcTest, pXfCd.get_Name(), 1));
        StartInstance(CatPrf, "NoLoadTest", pName);
        UuidNode(CatPrf, "NoLoadTest.EnergisedEnd", GetDevUuid(WdgInf, pXfCd.get_Name(), 1));
        DoubleNode(CatPrf, "NoLoadTest.energisedEndVoltage", 1000.0 * pXfCd.WINDING_[0].kVLL);
        pctIexc = sqrt(pXfCd.pctImag * pXfCd.pctImag + pXfCd.pctNoLoadLoss * pXfCd.pctNoLoadLoss);
        DoubleNode(CatPrf, "NoLoadTest.excitingCurrent", pctIexc);
        DoubleNode(CatPrf, "NoLoadTest.excitingCurrentZero", pctIexc);
        val = 0.01 * pXfCd.pctNoLoadLoss * pXfCd.WINDING_[0].kVA; // losses to be in kW
        DoubleNode(CatPrf, "NoLoadTest.loss", val);
        DoubleNode(CatPrf, "NoLoadTest.lossZero", val);
        DoubleNode(CatPrf, "TransformerTest.basePower", 1000.0 * pXfCd.WINDING_[0].kVA);
        DoubleNode(CatPrf, "TransformerTest.temperature", 50.0);
        EndInstance(CatPrf, "NoLoadTest");
        seq = 0;
        for (i = 1; i <= pXfCd.NumWindings; ++i)
        {
            for (j = (i + 1); j <= pXfCd.NumWindings; ++j)
            {
                seq++;
                pName->LName = pXfCd.get_Name() + "_" + IntToStr(seq);
                pName->Set_UUID(GetDevUuid(ScTest, pXfCd.get_Name(), seq));
                StartInstance(CatPrf, "ShortCircuitTest", pName);
                UuidNode(CatPrf, "ShortCircuitTest.EnergisedEnd", GetDevUuid(WdgInf, pXfCd.get_Name(), i));
                // NOTE: can insert more than one GroundedEnds for three-winding short-circuit tests
                UuidNode(CatPrf, "ShortCircuitTest.GroundedEnds", GetDevUuid(WdgInf, pXfCd.get_Name(), j));
                IntegerNode(CatPrf, "ShortCircuitTest.energisedEndStep", pXfCd.WINDING_[i - 1].NumTaps / 2);
                IntegerNode(CatPrf, "ShortCircuitTest.groundedEndStep", pXfCd.WINDING_[j - 1].NumTaps / 2);
                // TestKVA = min(Winding[i - 1].kVA, Winding[j - 1].kva);
                TestKVA = pXfCd.WINDING_[0].kVA;
                Zbase = pXfCd.WINDING_[i - 1].kVLL;
                Zbase = 1000.0 * Zbase * Zbase / TestKVA;  // all DSS impedances are on winding 1 kva base
                // windings are not overloaded during short-circuit tests, but in OpenDSS Sbase is on Winding 1 always
                x = pXfCd.XSC[seq - 1];
                r = pXfCd.WINDING_[i - 1].Rpu + pXfCd.WINDING_[j - 1].Rpu;
                val = sqrt(r * r + x * x) * Zbase;
                DoubleNode(CatPrf, "ShortCircuitTest.leakageImpedance", val);
                DoubleNode(CatPrf, "ShortCircuitTest.leakageImpedanceZero", val);
                val = r * TestKVA;
                DoubleNode(CatPrf, "ShortCircuitTest.loss", val);
                DoubleNode(CatPrf, "ShortCircuitTest.lossZero", val);
                DoubleNode(CatPrf, "TransformerTest.basePower", 1000.0 * TestKVA);
                DoubleNode(CatPrf, "TransformerTest.temperature", 50.0);
                EndInstance(CatPrf, "ShortCircuitTest");
            }
        }
    }
    delete pName;
}

void WriteCableData(TCableDataObj& pCab)
{
    double v1;
    // with pCab do {
    v1 = To_Meters(pCab.FRadiusUnits);
    BooleanNode(CatPrf, "WireInfo.insulated", true);
    DoubleNode(CatPrf, "WireInfo.insulationThickness", v1 * pCab.FInsLayer);
    ConductorInsulationEnum(CatPrf, "crosslinkedPolyethylene"); // TODO -  code EpsR
    CableOuterJacketEnum(CatPrf, "none");
    CableConstructionEnum(CatPrf, "stranded");
    BooleanNode(CatPrf, "CableInfo.isStrandFill", false); // we don't really know this
    DoubleNode(CatPrf, "CableInfo.diameterOverCore", v1 * (pCab.FDiaIns - 2.0 * pCab.FInsLayer));
    DoubleNode(CatPrf, "CableInfo.diameterOverInsulation", v1 * pCab.FDiaIns);
    DoubleNode(CatPrf, "CableInfo.diameterOverJacket", v1 * pCab.FDiaCable);
    DoubleNode(CatPrf, "CableInfo.nominalTemperature", 90.0);  // we don't really know this
    DoubleNode(CatPrf, "CableInfo.relativePermittivity", pCab.FEpsR);
    // }
}

void WriteTapeData(TTSDataObj& pCab)
{
    double v1;
    // with pCab do {
    v1 = To_Meters(pCab.FRadiusUnits);
    DoubleNode(CatPrf, "CableInfo.diameterOverScreen", v1 * (pCab.FDiaShield - 2.0 * pCab.FTapeLayer));
    DoubleNode(CatPrf, "TapeShieldCableInfo.tapeLap", pCab.FTapeLap);
    DoubleNode(CatPrf, "TapeShieldCableInfo.tapeThickness", v1 * pCab.FTapeLayer);
    CableShieldMaterialEnum(CatPrf, "copper");
    BooleanNode(CatPrf, "CableInfo.sheathAsNeutral", true);
    // }
}

void WriteConcData(TCNDataObj& pCab)
{
    double v1;
    // with pCab do {
    v1 = To_Meters(pCab.FRadiusUnits);
    DoubleNode(CatPrf, "CableInfo.diameterOverScreen", v1 * (pCab.FDiaCable - 2.0 * pCab.FDiaStrand));
    DoubleNode(CatPrf, "ConcentricNeutralCableInfo.diameterOverNeutral", v1 * pCab.FDiaCable);
    DoubleNode(CatPrf, "ConcentricNeutralCableInfo.neutralStrandRadius", v1 * 0.5 * pCab.FDiaStrand);
    DoubleNode(CatPrf, "ConcentricNeutralCableInfo.neutralStrandGmr", v1 * pCab.FGmrStrand);
    v1 = To_per_Meter(pCab.FResistanceUnits); 
    DoubleNode(CatPrf, "ConcentricNeutralCableInfo.neutralStrandRDC20", v1 * pCab.FRStrand);
    IntegerNode(CatPrf, "ConcentricNeutralCableInfo.neutralStrandCount", pCab.FkStrand);
    BooleanNode(CatPrf, "CableInfo.sheathAsNeutral", false);
    // }
}

void WriteWireData(TConductorDataObj& pWire)
{
    double v1;
    // with pWire do {
    StringNode(CatPrf, "WireInfo.sizeDescription", pWire.Get_DisplayName());
    if (CompareText(pWire.get_Name().substr(0, 2), "AA") == 0)
        ConductorMaterialEnum(CatPrf, "aluminum");
    else if (CompareText(pWire.get_Name().substr(0, 4), "ACSR") == 0)
        ConductorMaterialEnum(CatPrf, "acsr");
    else if (CompareText(pWire.get_Name().substr(0, 2), "CU") == 0)
        ConductorMaterialEnum(CatPrf, "copper");
    else if (CompareText(pWire.get_Name().substr(0, 3), "EHS") == 0)
        ConductorMaterialEnum(CatPrf, "steel");
    else
        ConductorMaterialEnum(CatPrf, "other");
    v1 = To_Meters(pWire.FGMRUnits);
    DoubleNode(CatPrf, "WireInfo.gmr", pWire.FGMR60 * v1);
    v1 = To_Meters(pWire.FRadiusUnits);
    DoubleNode(CatPrf, "WireInfo.radius", pWire.Fradius * v1);
    v1 = To_per_Meter(pWire.FResistanceUnits);
    DoubleNode(CatPrf, "WireInfo.rDC20", pWire.FRDC * v1);
    DoubleNode(CatPrf, "WireInfo.rAC25", pWire.FR60 * v1);
    DoubleNode(CatPrf, "WireInfo.rAC50", pWire.FR60 * v1);
    DoubleNode(CatPrf, "WireInfo.rAC75", pWire.FR60 * v1);
    DoubleNode(CatPrf, "WireInfo.ratedCurrent", std::max(pWire.NormAmps, 0.0));
    IntegerNode(CatPrf, "WireInfo.strandCount", 0);
    IntegerNode(CatPrf, "WireInfo.coreStrandCount", 0);
    DoubleNode(CatPrf, "WireInfo.coreRadius", 0.0);
    // }
}

void StartCIMFile(Textfile& F, const String &Filenm, ProfileChoice prf)
{
    AssignFile(F, Filenm);
    Rewrite(F);
    IOResultToException();
    WriteLn(F, "<?xml version=\"1.0\" encoding=\"utf-8\"?>");
    WriteLn(F, "<!-- un-comment this line to enable validation");
    WriteLn(F, "-->");
    WriteLn(F, "<rdf:RDF xmlns:cim=\"" + CIM_NS + "#\" xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">");
    WriteLn(F, "<!--");
    WriteLn(F, "-->");
    WriteLn(F, Format("<cim:IEC61970CIMVersion rdf:about=\"urn:uuid:%s\">", UUIDToCIMString(GetDevUuid(CIMVer, "IEC", 1)).c_str()));
    WriteLn(F, Format("  <cim:IEC61970CIMVersion.version>%s</cim:IEC61970CIMVersion.version>", "IEC61970CIM100"));
    WriteLn(F, Format("  <cim:IEC61970CIMVersion.date>%s</cim:IEC61970CIMVersion.date>", "2019-04-01"));
    WriteLn(F, "</cim:IEC61970CIMVersion>");
}

void ListXfmrCodes(TXfmrCode &clsXfCd, String lbl) // for debugging
{
    TXfmrCodeObj* pXfCd;
    WriteLn("xfmrcodes at " + lbl);
    pXfCd = (TXfmrCodeObj*) clsXfCd.ElementList.Get_First();
    while (pXfCd != nullptr)
    {
        WriteLn("  " + pXfCd->LName + " " + pXfCd->get_Name() + " " + UUIDToString(pXfCd->Get_UUID()));
        pXfCd = (TXfmrCodeObj*) clsXfCd.ElementList.Get_Next();
    }
}

///////// { helper class for exporting IEEE 1547 model parameters /////////////

TRemoteSignalObject::TRemoteSignalObject(const String& aBusName, int seq, const String& invName): inherited("ISignal")
{
    busName = aBusName;
    pElem = nullptr;
    trm = -1;
    phase = "A";
    LName = invName + "_" + IntToStr(seq);
    Set_UUID(GetDevUuid(I1547Signal, LName, seq));
}

TRemoteSignalObject::~TRemoteSignalObject()
{
}

bool TIEEE1547Controller::CheckSignalMatch(TRemoteSignalObject& sig, TDSSCktElement& pElm, int seq)
{
    String elmPhases, trmBus;
    int dotpos;

    bool Result = false;
    trmBus = pElm.GetBus(seq);
    dotpos = AnsiPos(".", trmBus);
    if (dotpos > 0)
    {
        trmBus = trmBus.substr(0, dotpos - 1);
    }

    if (CompareText(sig.busName, trmBus) == 0)
    {
        elmPhases = PhaseString(pElm, seq, true);
        if (Pos(sig.phase, elmPhases) > 0)
        {
            sig.trm = seq;
            sig.pElem = &pElm;
            Result = true;
        }
        else if ((Pos("1", elmPhases) > 0) && (sig.phase == "A"))
        {  // switch to secondary phasing
            sig.trm = seq;
            sig.pElem = &pElm;
            sig.phase = "s1";
            Result = true;
        }
        else if ((Pos("2", elmPhases) > 0) && (sig.phase == "B"))
        {  // switch to secondary phasing
            sig.trm = seq;
            sig.pElem = &pElm;
            sig.phase = "s2";
            Result = true;
        }
    }

    return Result;
}

void TIEEE1547Controller::FindSignalTerminals()
{
    int i, j, k, dotpos;
    String bus, phase;
    std::vector<String> elements;
    bool found;
    TDSSCktElement* pElem;

    if (pMonBuses.size() < 1)
    {
        Signals.clear();
        return;
    }

    // create just one remote signal for the main bus, based on the first MonBus
    //  IEEE 1547 doesn't allow different main buses
    //  IEEE 1547 also specifies that the average (pos seq) of all applicable voltages be used
    // Signals.resize(1); // pMonBuses.size());

    for (i = 0; i < 1; ++i)
    {
        bus = pMonBuses[i];
        Signals.emplace_back(bus, i + 1, pInvName->LName);
        dotpos = AnsiPos(".", bus); // removes the dot
        if (dotpos > 0)
        {
            phase = bus.substr(0, dotpos - 1);
            if (Pos("3", phase) > 0)
                Signals[i].phase = "C";
            else if (Pos("2", phase) > 0)
                Signals[i].phase = "B";
            else
                Signals[i].phase = "A";
            Signals[i].busName = bus.substr(0, dotpos - 1);
        }
        else
        { // this is a three-phase bus, which must be ABC, not s1 and/or s2
            Signals[i].phase = "A"; // if user wants B and/or C as well, the MonBus input should have specified
        }

        found = false;
        // with ActiveCircuit[ActiveActor] do
        {
            elements = ActiveCircuit[ActiveActor]->getPDEatBus(Signals[i].busName);
            for (j = 0; j < elements.size(); ++j)
            {
                if (found)
                    break;
                if (ActiveCircuit[ActiveActor]->SetElementActive(elements[j]) > 0)
                {
                    pElem = ActiveCircuit[ActiveActor]->get_FActiveCktElement();
                    for (k = 1; k <= pElem->Fnterms; ++k)
                    {
                        if (CheckSignalMatch(Signals[i], *pElem, k))
                        {
                            found = true;
                            break;
                        }
                    }
                }
            }
            if (!found)
            {
                elements = ActiveCircuit[ActiveActor]->getPCEatBus(bus);
                for (j = 0; j < elements.size(); ++j)
                {
                    if (found)
                        break;
                    if (ActiveCircuit[ActiveActor]->SetElementActive(elements[j]) > 0)
                    {
                        pElem = ActiveCircuit[ActiveActor]->get_FActiveCktElement();
                        for (k = 1; k <= pElem->Fnterms; ++k)
                        {
                            if (CheckSignalMatch(Signals[i], *pElem, k))
                            {
                                found = true;
                                break;
                            }
                        }
                    }
                }
            }
        }
    }
}

TIEEE1547Controller::TIEEE1547Controller()
{
    SetDefaults(false);
    pInvName = new TNamedObject("Inv");
    pPlateName = new TNamedObject("Nameplate");
    pSetName = new TNamedObject("Settings");
}

TIEEE1547Controller::~TIEEE1547Controller()
{
    delete pInvName;
    delete pPlateName;
    delete pSetName;
}

void TIEEE1547Controller::PullFromInvControl(TInvControlObj& pInv)
{
    TXYcurveObj* xy;
    bool bCatB, bValid, bSet1, bSet2, bSet3, bSet4, bSet5, bSet6;
    int mode, combi, i;
    double v, p, q, qvslope;

    pInvName->LName = pInv.get_Name();
    pInvName->Set_UUID(pInv.Get_UUID());
    pDERNames = pInv.FDERNameList;
    if (pInv.FMonBusesNameList.size() > 0)
        pMonBuses = pInv.FMonBusesNameList;
    else
        pMonBuses.clear();

    bCatB = false;
    xy = pInv.Fvvc_curve;
    if (xy != nullptr)
    {
        for (i = 1; i <= xy->FNumPoints; ++i)
        {
            if (xy->Get_YValue(i) < -CatBQmin)
            {
                bCatB = true;
                break;
            }
        }
    }
    SetDefaults(bCatB);

    VV_olrt = pInv.get_FLPFTau() * 2.3026;
    VW_olrt = VV_olrt;

    if (xy != nullptr)
    {
        i = 1;
        bValid = false;
        bSet1 = false;
        bSet2 = false;
        bSet3 = false;
        bSet4 = false;
        while (i <= xy->FNumPoints)
        {
            v = xy->Get_XValue(i);
            if ((v >= 0.77) && (v <= 1.25))
                bValid = true;
            if (bValid)
            {
                if (!bSet1)
                {
                    VV_curveV1 = v;
                    VV_curveQ1 = xy->Get_YValue(i);
                    bSet1 = true;
                }
                else if (!bSet2)
                {
                    if (v > 1.05)
                    {
                        VV_curveV2 = 1.0;
                        VV_curveQ2 = 0.0;
                        if (v > 1.08)
                        {
                            VV_curveV3 = 1.0;
                            VV_curveQ3 = 0.0;
                            bSet3 = true;
                            VV_curveV4 = v;
                            VV_curveQ4 = xy->Get_YValue(i);
                            bSet4 = true;
                        }
                    }
                    else
                    {
                        VV_curveV2 = v;
                        VV_curveQ2 = xy->Get_YValue(i);
                    }
                    bSet2 = true;
                }
                else if (!bSet3)
                {
                    VV_curveV3 = v;
                    VV_curveQ3 = xy->Get_YValue(i);
                    bSet3 = true;
                }
                else if (!bSet4)
                {
                    VV_curveV4 = v;
                    VV_curveQ4 = xy->Get_YValue(i);
                    bSet4 = true;
                }
            }
            i++;
        }
    }

    xy = pInv.Fvoltwatt_curve;
    if ((xy != nullptr))
    {
        i = 1;
        bValid = false;
        bSet1 = false;
        bSet2 = false;
        while (i <= xy->FNumPoints)
        {
            v = xy->Get_XValue(i);
            p = xy->Get_YValue(i);
            if ((v >= 1.00) && (v <= 1.10))
                bValid = true; // TODO: per standard, v should be >= 1.05 but we loosen that criteria for testing
            if (bValid)
            {
                if (!bSet1)
                {
                    VW_curveV1 = v;
                    VW_curveP1 = p; // this is actually supposed to be 1.0 always
                    bSet1 = true;
                }
                else if (!bSet2)
                {
                    VW_curveV2 = v;
                    if (p < 0.0)
                    {
                        VW_curveP2gen = 0.2; // TODO: should have a pMin
                        VW_curveP2load = p;
                    }
                    else
                    {
                        VW_curveP2gen = p;
                        VW_curveP2load = 0.0;
                    }
                    bSet2 = true;
                }
            }
            i++;
        }
    }

    xy = pInv.FvoltwattCH_curve;
    if ((xy != nullptr))
    {
        p = 0.0;
        i = 1;
        while (i <= xy->FNumPoints)
        {
            if (xy->Get_YValue(i) > p)
                p = xy->Get_YValue(i);
            i++;
        }
        if ((-p < VW_curveP2load))
            VW_curveP2load = -p;
    }

    xy = pInv.Fwattvar_curve;
    if ((xy != nullptr))
    {
        i = 1;
        bValid = false;
        bSet1 = false;
        bSet2 = false;
        bSet3 = false;
        bSet4 = false;
        bSet5 = false;
        bSet6 = false;
        while (i <= xy->FNumPoints)
        {
            p = xy->Get_XValue(i);
            q = xy->Get_YValue(i);
            if ((p >= -1.0) && (p <= 1.0))
                bValid = true;
            if (bValid)
            {
                if (!bSet1)
                {
                    if (p <= -0.5)
                    {
                        WV_curveP3load = p;
                        WV_curveQ3load = q;
                    }
                    else
                    {
                        WV_curveP3load = -1.0;
                        WV_curveQ3load = 0.0;
                        i--; // re-scan
                    }
                    bSet1 = true;
                }
                else if (!bSet2)
                {
                    if (p <= -0.4)
                    {
                        WV_curveP2load = p;
                        WV_curveQ2load = q;
                    }
                    else
                    {
                        WV_curveP2load = -0.5;
                        WV_curveQ2load = 0.0;
                        i--; // re-scan
                    }
                    bSet2 = true;
                }
                else if (!bSet3)
                {
                    if (p <= 0.0)
                    {
                        WV_curveP1load = p;
                        WV_curveQ1load = q;
                    }
                    else
                    {
                        WV_curveP1load = -0.2;
                        WV_curveQ1load = 0.0;
                        i--; // re-scan
                    }
                    bSet3 = true;
                }
                else if (!bSet4)
                {
                    if (p <= 0.7)
                    {
                        WV_curveP1gen = p;
                        WV_curveQ1gen = q;
                    }
                    else
                    {
                        WV_curveP1gen = 0.2;
                        WV_curveQ1gen = 0.0;
                        i--; // re-scan
                    }
                    bSet4 = true;
                }
                else if (!bSet5)
                {
                    if (p <= 0.8)
                    {
                        WV_curveP2gen = p;
                        WV_curveQ2gen = q;
                    }
                    else
                    {
                        WV_curveP2gen = 0.5;
                        WV_curveQ2gen = 0.0;
                        i--; // re-scan
                    }
                    bSet5 = true;
                }
                else if (!bSet6)
                {
                    if (p <= 1.0)
                    {
                        WV_curveP3gen = p;
                        WV_curveQ3gen = q;
                    }
                    else
                    {
                        WV_curveP3gen = 1.0;
                        WV_curveQ3gen = 0.0;
                        i--; // re-scan
                    }
                    bSet6 = true;
                }
            }
            i++;
        }
        // handle the edge cases when default zero watt-var points were not input
        if (WV_curveP1gen >= WV_curveP2gen)
            WV_curveP1gen = WV_curveP2gen - 0.1;
        if (WV_curveP1load <= WV_curveP2load)
            WV_curveP1load = WV_curveP2load + 0.1;
    }

    /* copied from InvControl!!
        // Modes
        NONE_MODE = 0;
        VOLTWATT  = 2;
        DRC       = 3;
        WATTPF    = 4;
        WATTVAR   = 5;
        AVR       = 6;

        // Combi Modes
        NONE_COMBMODE = 0;
        VV_VW         = 1;
        VV_DRC        = 2;
    */
    mode = pInv.ControlMode;
    combi = pInv.CombiControlMode;
    if (combi == 1)
    {
        PF_enabled = false;
        VV_enabled = true;
        VW_enabled = true;
    }
    else if (combi == 2)
    {
        PF_enabled = false;
        VV_enabled = true;
    }
    else if (mode == 1)
    {
        PF_enabled = false;
        VV_enabled = true;
    }
    else if (mode == 2)
    {
        PF_enabled = false;
        VW_enabled = true;
    }
    else if (mode == 3)
    { // approximating AVR with DRC
        PF_enabled = false;
        VV_enabled = true;
        VV_vRefAutoModeEnabled = true;
        VV_vRefOlrt = pInv.get_FDRCRollAvgWindowLength();
        qvslope = 0.5 * (pInv.get_FArGraLowV() + pInv.get_FArGraHiV());
        if (qvslope > 12.5)
            bCatB = true;  // for catA, maximum slope would be 12.5    
        if (bCatB)
            q = 0.44;
        else
            q = 0.25;
        VV_curveQ1 = q;
        VV_curveQ2 = VV_curveQ1;
        VV_curveQ3 = -VV_curveQ1;
        VV_curveQ4 = VV_curveQ3;
        VV_curveV1 = 0.50;
        VV_curveV2 = 1.0 - VV_curveQ2 / qvslope;
        VV_curveV3 = 1.0 - VV_curveQ3 / qvslope;  // - because Q3 should be negative
        VV_curveV4 = 1.50;
    }
    else if (mode == 5)
    {
        PF_enabled = false;
        WV_enabled = true;
    }
}

void TIEEE1547Controller::PullFromExpControl(TExpControlObj& pExp)
{
    int i;
    pInvName->LName = pExp.get_Name();
    pInvName->Set_UUID(pExp.Get_UUID());
    i = 0;
    while (i < pExp.FDERNameList->size())
    {
        pDERNames.push_back((*pExp.FDERNameList)[i]);
        i++;
    }
    pMonBuses.clear();

    if (pExp.FQmaxLead > CatBQmin) // catB estimate
        SetDefaults(true);
    else
        SetDefaults(false);

    PF_enabled = false;
    VV_enabled = true;
    VV_vRefAutoModeEnabled = true;
    VV_vRefOlrt = pExp.FVregTau;
    VV_olrt = pExp.FTresponse;
    VV_curveQ1 = pExp.FQmaxLead;
    VV_curveQ2 = VV_curveQ1;
    VV_curveQ3 = -pExp.FQmaxLag;
    VV_curveQ4 = VV_curveQ3;
    VV_curveV1 = 0.50;
    VV_curveV2 = 1.0 - VV_curveQ2 / pExp.FSlope;
    VV_curveV3 = 1.0 - VV_curveQ3 / pExp.FSlope;  // - because Q3 should be negative
    VV_curveV4 = 1.50;
}

void TIEEE1547Controller::SetDefaults(bool bCatB)
{
    bNameplateSet = false;
    ND_acVmax = 1.05;
    ND_acVmin = 0.95;
    AD_pMax = 0.0;
    AD_pMaxOverPF = 0.0;
    AD_overPF = 0.0;
    AD_pMaxUnderPF = 0.0;
    AD_underPF = 0.0;
    AD_sMax = 0.0;
    AD_pMaxCharge = 0.0;
    AD_apparentPowerChargeMax = 0.0;
    AD_acVnom = 0.0;
    AD_qMaxInj = 0.44;
    if (bCatB)
    {
        ND_normalOPcatKind = "catB";
        AD_qMaxAbs = 0.44;
        VV_curveV1 = 0.92;
        VV_curveV2 = 0.98;
        VV_curveV3 = 1.02;
        VV_curveV4 = 1.08;
        VV_curveQ1 = 0.44;
        VV_curveQ2 = 0.0;
        VV_curveQ3 = 0.0;
        VV_curveQ4 = -0.44;
        VV_olrt = 5.0;
        WV_curveQ3load = 0.44;
    }
    else
    {
        ND_normalOPcatKind = "catA";
        AD_qMaxAbs = 0.25;
        VV_curveV1 = 0.90;
        VV_curveV2 = 1.00;
        VV_curveV3 = 1.00;
        VV_curveV4 = 1.10;
        VV_curveQ1 = 0.25;
        VV_curveQ2 = 0.0;
        VV_curveQ3 = 0.0;
        VV_curveQ4 = -0.25;
        VV_olrt = 10.0;
        WV_curveQ3load = 0.25;
    }
    VV_vRef = 1.0;
    VV_vRefOlrt = 300.0;
    Q_reactivePower = 0.0;
    PF_powerFactor = 1.0;
    VW_olrt = 10.0;
    VW_curveV1 = 1.06;
    VW_curveV2 = 1.10;
    VW_curveP1 = 1.0;
    VW_curveP2gen = 0.2;
    VW_curveP2load = 0.0; // for storage, -1.0

    WV_curveP1gen = 0.2;
    WV_curveP2gen = 0.5;
    WV_curveP3gen = 1.0;
    WV_curveP1load = 0.0; // for storage, 0.2, 0.5, 1.0
    WV_curveP2load = 0.0;
    WV_curveP3load = 0.0;
    WV_curveQ1gen = 0.0;
    WV_curveQ2gen = 0.0;
    WV_curveQ3gen = 0.44;
    WV_curveQ1load = 0.0;
    WV_curveQ2load = 0.0;

    PF_constPFexcitationKind = "inj";
    VV_enabled = false;
    WV_enabled = false;
    PF_enabled = true;
    Q_enabled = false;
    VW_enabled = false;
    VV_vRefAutoModeEnabled = false;
}

void TIEEE1547Controller::FinishNameplate()
{
    AD_overPF = AD_pMaxOverPF / AD_sMax;
    AD_underPF = AD_pMaxUnderPF / AD_sMax;
    bNameplateSet = true;
}

void TIEEE1547Controller::SetStorageNameplate(TStorageObj& pBat)
{
    AD_acVnom = pBat.Get_acVnom() * 1000.0;
    ND_acVmax = pBat.Get_acVmax() * 1000.0;
    ND_acVmin = pBat.Get_acVmin() * 1000.0;
    AD_sMax = pBat.Get_FkVARating() * 1000.0;
    AD_pMax = pBat.Get_Pmax() * 1000.0;
    AD_pMaxOverPF = pBat.Get_pMaxOverPF() * 1000.0;
    AD_pMaxUnderPF = pBat.Get_pMaxUnderPF() * 1000.0;
    AD_pMaxCharge = pBat.Get_pMaxCharge() * 1000.0;
    AD_apparentPowerChargeMax = pBat.Get_sMaxCharge() * 1000.0;
    AD_qMaxInj = pBat.Get_qMaxInj() * 1000.0;
    AD_qMaxAbs = pBat.Get_qMaxAbs() * 1000.0;
    FinishNameplate();
}

void TIEEE1547Controller::SetPhotovoltaicNameplate(TPVsystemObj& pPV)
{
    AD_acVnom = pPV.Get_acVnom() * 1000.0;
    ND_acVmax = pPV.Get_acVmax() * 1000.0;
    ND_acVmin = pPV.Get_acVmin() * 1000.0;
    AD_sMax = pPV.Get_FkVArating() * 1000.0;
    AD_pMax = pPV.Get_Pmax() * 1000.0;
    AD_pMaxOverPF = pPV.Get_pMaxOverPF() * 1000.0;
    AD_pMaxUnderPF = pPV.Get_pMaxUnderPF() * 1000.0;
    AD_pMaxCharge = pPV.Get_pMaxCharge() * 1000.0;
    AD_apparentPowerChargeMax = pPV.Get_sMaxCharge() * 1000.0;
    AD_qMaxInj = pPV.Get_qMaxInj() * 1000.0;
    AD_qMaxAbs = pPV.Get_qMaxAbs() * 1000.0;
    FinishNameplate();
}

void TIEEE1547Controller::SetElementNameplate(TDSSCktElement& pElem)
{
    if (bNameplateSet)
        return;
    if (pElem.DSSObjType == (PC_ELEMENT + PVSYSTEM_ELEMENT))
        SetPhotovoltaicNameplate(*((TPVsystemObj*)(&pElem)));
    if (pElem.DSSObjType == (PC_ELEMENT + STORAGE_ELEMENT))
        SetStorageNameplate(*((TStorageObj*)(&pElem)));
    FinishNameplate();
}

void TIEEE1547Controller::WriteCIM(ProfileChoice prf)
{
    int i;
    TPVsystemObj* pPV;
    TStorageObj* pBat;

    FindSignalTerminals();
    StartInstance(prf, "DERIEEEType1", pInvName);
    BooleanNode(prf, "DynamicsFunctionBlock.enabled", true);
    BooleanNode(prf, "DERIEEEType1.phaseToGroundApplicable", true); // seems to be the only OpenDSS option
    BooleanNode(prf, "DERIEEEType1.phaseToNeutralApplicable", false);
    BooleanNode(prf, "DERIEEEType1.phaseToPhaseApplicable", false);
    // with ActiveCircuit[ActiveActor] do {
    if (pDERNames.size() < 1)
    {
        pBat = (TStorageObj*) ActiveCircuit[ActiveActor]->StorageElements.Get_First();
        while (pBat != nullptr)
        {
            if (pBat->FEnabled)
            {
                RefNode(prf, "DERDynamics.PowerElectronicsConnection", pBat);
                SetStorageNameplate(*pBat);
            }
            pBat = (TStorageObj*) ActiveCircuit[ActiveActor]->StorageElements.Get_Next();
        }
        pPV = (TPVsystemObj*) ActiveCircuit[ActiveActor]->PVSystems.Get_First();
        while (pPV != nullptr)
        {
            if (pPV->FEnabled)
            {
                RefNode(prf, "DERDynamics.PowerElectronicsConnection", pPV);
                SetPhotovoltaicNameplate(*pPV);
            }
            pPV = (TPVsystemObj*) ActiveCircuit[ActiveActor]->PVSystems.Get_Next();
        }
    }
    else
    {
        for (i = 1; i <= pDERNames.size(); ++i)
        {
            ActiveCircuit[ActiveActor]->SetElementActive(pDERNames[i - 1]);
            RefNode(prf, "DERDynamics.PowerElectronicsConnection", ActiveCircuit[ActiveActor]->FActiveCktElement);
            SetElementNameplate(*ActiveCircuit[ActiveActor]->FActiveCktElement);
        }
    }
    // }

    for (i = 0; i < Signals.size(); ++i)
        RefNode(prf, "DERDynamics.RemoteInputSignal", Signals[i]);
    EndInstance(prf, "DERIEEEType1");

    for (i = 0; i < Signals.size(); ++i)
    {
        StartInstance(prf, "RemoteInputSignal", Signals[i]);
        RemoteInputSignalEnum(prf, "remoteBusVoltageAmplitude");
        UuidNode(prf, "RemoteInputSignal.Terminal", GetTermUuid(*Signals[i].pElem, Signals[i].trm));
        EndInstance(prf, "RemoteInputSignal");
    }

    pPlateName->LName = pInvName->LName;
    pPlateName->Set_UUID(GetDevUuid(I1547NameplateData, pInvName->LName, 1));
    StartInstance(prf, "DERNameplateData", pPlateName);
    RefNode(prf, "DERNameplateData.DERIEEEType1", pInvName);
    NormalOpCatEnum(prf, ND_normalOPcatKind);
    BooleanNode(prf, "DERNameplateData.supportsConstPFmode", true);
    BooleanNode(prf, "DERNameplateData.supportsConstQmode", true);
    BooleanNode(prf, "DERNameplateData.supportsQVmode", true);
    if (ND_normalOPcatKind == "catB")
    {
        BooleanNode(prf, "DERNameplateData.supportsPVmode", true);
        BooleanNode(prf, "DERNameplateData.supportsQPmode", true);
    }
    else
    {
        BooleanNode(prf, "DERNameplateData.supportsPVmode", false);
        BooleanNode(prf, "DERNameplateData.supportsQPmode", false);
    }
    BooleanNode(prf, "DERNameplateData.supportsPFmode", false); // no frequency response in GridAPPS-D
    DoubleNode(prf, "DERNameplateData.acVmax", ND_acVmax);
    DoubleNode(prf, "DERNameplateData.acVmin", ND_acVmin);
    EndInstance(prf, "DERNameplateData");

    pSetName->LName = pInvName->LName;
    pSetName->Set_UUID(GetDevUuid(I1547NameplateDataApplied, pSetName->LName, 1));
    StartInstance(prf, "DERNameplateDataApplied", pSetName);
    RefNode(prf, "DERNameplateDataApplied.DERNameplateData", pPlateName);
    DoubleNode(prf, "DERNameplateDataApplied.pMax", AD_pMax);
    DoubleNode(prf, "DERNameplateDataApplied.pMaxOverPF", AD_pMaxOverPF);
    DoubleNode(prf, "DERNameplateDataApplied.overPF", AD_overPF);
    DoubleNode(prf, "DERNameplateDataApplied.pMaxUnderPF", AD_pMaxUnderPF);
    DoubleNode(prf, "DERNameplateDataApplied.underPF", AD_underPF);
    DoubleNode(prf, "DERNameplateDataApplied.sMax", AD_sMax);
    DoubleNode(prf, "DERNameplateDataApplied.qMaxInj", AD_qMaxInj);
    DoubleNode(prf, "DERNameplateDataApplied.qMaxAbs", AD_qMaxAbs);
    DoubleNode(prf, "DERNameplateDataApplied.pMaxCharge", AD_pMaxCharge);
    DoubleNode(prf, "DERNameplateDataApplied.apparentPowerChargeMax", AD_apparentPowerChargeMax);
    DoubleNode(prf, "DERNameplateDataApplied.acVnom", AD_acVnom);
    EndInstance(prf, "DERNameplateDataApplied");

    pSetName->Set_UUID(GetDevUuid(I1547VoltVar, pSetName->LName, 1));
    StartInstance(prf, "VoltVarSettings", pSetName);
    RefNode(prf, "VoltVarSettings.DERIEEEType1", pInvName);
    BooleanNode(prf, "VoltVarSettings.enabled", VV_enabled);
    BooleanNode(prf, "VoltVarSettings.vRefAutoModeEnabled", VV_vRefAutoModeEnabled);
    DoubleNode(prf, "VoltVarSettings.vRef", VV_vRef);
    DoubleNode(prf, "VoltVarSettings.vRefOlrt", VV_vRefOlrt);
    DoubleNode(prf, "VoltVarSettings.curveV1", VV_curveV1);
    DoubleNode(prf, "VoltVarSettings.curveV2", VV_curveV2);
    DoubleNode(prf, "VoltVarSettings.curveV3", VV_curveV3);
    DoubleNode(prf, "VoltVarSettings.curveV4", VV_curveV4);
    DoubleNode(prf, "VoltVarSettings.curveQ1", VV_curveQ1);
    DoubleNode(prf, "VoltVarSettings.curveQ2", VV_curveQ2);
    DoubleNode(prf, "VoltVarSettings.curveQ3", VV_curveQ3);
    DoubleNode(prf, "VoltVarSettings.curveQ4", VV_curveQ4);
    DoubleNode(prf, "VoltVarSettings.olrt", VV_olrt);
    EndInstance(prf, "VoltVarSettings");

    pSetName->Set_UUID(GetDevUuid(I1547WattVar, pSetName->LName, 1));
    StartInstance(prf, "WattVarSettings", pSetName);
    RefNode(prf, "WattVarSettings.DERIEEEType1", pInvName);
    BooleanNode(prf, "WattVarSettings.enabled", WV_enabled);
    DoubleNode(prf, "WattVarSettings.curveP1gen", WV_curveP1gen);
    DoubleNode(prf, "WattVarSettings.curveP2gen", WV_curveP2gen);
    DoubleNode(prf, "WattVarSettings.curveP3gen", WV_curveP3gen);
    DoubleNode(prf, "WattVarSettings.curveQ1gen", WV_curveQ1gen);
    DoubleNode(prf, "WattVarSettings.curveQ2gen", WV_curveQ2gen);
    DoubleNode(prf, "WattVarSettings.curveQ3gen", WV_curveQ3gen);
    DoubleNode(prf, "WattVarSettings.curveP1load", WV_curveP1load);
    DoubleNode(prf, "WattVarSettings.curveP2load", WV_curveP2load);
    DoubleNode(prf, "WattVarSettings.curveP3load", WV_curveP3load);
    DoubleNode(prf, "WattVarSettings.curveQ1load", WV_curveQ1load);
    DoubleNode(prf, "WattVarSettings.curveQ2load", WV_curveQ2load);
    DoubleNode(prf, "WattVarSettings.curveQ3load", WV_curveQ3load);
    EndInstance(prf, "WattVarSettings");

    pSetName->Set_UUID(GetDevUuid(I1547ConstPF, pSetName->LName, 1));
    StartInstance(prf, "ConstantPowerFactorSettings", pSetName);
    RefNode(prf, "ConstantPowerFactorSettings.DERIEEEType1", pInvName);
    BooleanNode(prf, "ConstantPowerFactorSettings.enabled", PF_enabled);
    PowerFactorExcitationEnum(prf, PF_constPFexcitationKind);
    DoubleNode(prf, "ConstantPowerFactorSettings.powerFactor", PF_powerFactor);
    EndInstance(prf, "ConstantPowerFactorSettings");

    pSetName->Set_UUID(GetDevUuid(I1547ConstQ, pSetName->LName, 1));
    StartInstance(prf, "ConstantReactivePowerSettings", pSetName);
    RefNode(prf, "ConstantReactivePowerSettings.DERIEEEType1", pInvName);
    BooleanNode(prf, "ConstantReactivePowerSettings.enabled", Q_enabled);
    DoubleNode(prf, "ConstantReactivePowerSettings.reactivePower", Q_reactivePower);
    EndInstance(prf, "ConstantReactivePowerSettings");

    pSetName->Set_UUID(GetDevUuid(I1547VoltWatt, pSetName->LName, 1));
    StartInstance(prf, "VoltWattSettings", pSetName);
    RefNode(prf, "VoltWattSettings.DERIEEEType1", pInvName);
    BooleanNode(prf, "VoltWattSettings.enabled", VW_enabled);
    DoubleNode(prf, "VoltWattSettings.curveV1", VW_curveV1);
    DoubleNode(prf, "VoltWattSettings.curveV2", VW_curveV2);
    DoubleNode(prf, "VoltWattSettings.curveP1", VW_curveP1);
    DoubleNode(prf, "VoltWattSettings.curveP2gen", VW_curveP2gen);
    DoubleNode(prf, "VoltWattSettings.curveP2load", VW_curveP2load);
    DoubleNode(prf, "VoltWattSettings.olrt", VW_olrt);
    EndInstance(prf, "VoltWattSettings");
}

///////// } helper class for exporting IEEE 1547 model parameters /////////////

void ExportCDPSM(String Filenm, String Substation, String SubGeographicRegion, String GeographicRegion, TUuid FdrUUID, TUuid SubUUID, TUuid SubGeoUUID, TUuid RgnUUID, bool Combined)
{
    int i, j, k;
    int seq;
    double val;
    bool bval;
    double v1, v2;
    int i1, i2;
    complex Zs, Zm;
    double Rs, Rm, Xs, Xm, R1, R0, X1, X0;
    TNamedObject *pName1, *pName2;
    TNamedObject *pIsland, *pSwing;  // island and ref node
    TNamedObject *pRegion, *pSubRegion, *pLocation, *pSubstation, *pCRS;

    TOpLimitObject *pILimit;
    TNamedObject *pNormLimit, *pEmergLimit, *pRangeAHiLimit, *pRangeALoLimit, *pRangeBHiLimit, *pRangeBLoLimit; // OperationalLimitType
    String LimitName;
    TUuid LimiTUuid;

    double zbase;
    String s;
    String swtCls;  // based on controls, if any, attached to a line having switch=yes
    double ratedAmps, breakingAmps;

    TBankObject* pBank;
    int maxWdg;
    std::vector<TNamedObject*> WdgList;
    std::vector<TNamedObject*> CoreList;
    std::vector<TNamedObject*> MeshList;
    String sBank;
    bool bTanks;

    TLoadObj* pLoad;
    TVsourceObj* pVsrc;
    TGeneratorObj* pGen;
    TPVsystemObj* pPV;
    TStorageObj* pBat;
    TECPObject *pECP;

    TCapacitorObj* pCap;
    TCapControlObj* pCapC;
    TTransfObj* pXf;
    TAutoTransObj* pAuto;
    TRegControlObj* pReg;
    TLineObj* pLine;
    TReactorObj* pReac;
    TInvControlObj* pInv;
    TExpControlObj* pExp;
    TIEEE1547Controller* pI1547;

    TLineCode* clsLnCd;
    TLineGeometry* clsGeom;
    TWireData* clsWire;
    TXfmrCode* clsXfCd;
    TLineSpacing* clsSpac;
    TTSData* clsTape;
    TCNData* clsConc;

    TLineCodeObj* pLnCd;
    TLineGeometryObj* pGeom;
    TWireDataObj* pWire;
    TXfmrCodeObj* pXfCd;
    TLineSpacingObj* pSpac;
    TTSDataObj* pTape;
    TCNDataObj* pConc;

  // DSS-like load models
    TUuid id1_ConstkVA;
    TUuid id2_ConstZ;
    TUuid id3_ConstPQuadQ;
    TUuid id4_LinPQuadQ;
    TUuid id5_ConstI;
    TUuid id6_ConstPConstQ;  // P can vary, Q not
    TUuid id7_ConstPConstX;

  // for CIM Locations
    TUuid geoUUID;
    TUuid crsUUID;

    try
    {
        clsLnCd = (TLineCode*) DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find("linecode"));
        clsWire = (TWireData*) DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find("wiredata"));
        clsGeom = (TLineGeometry*) DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find("linegeometry"));
        clsXfCd = (TXfmrCode*) DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find("xfmrcode"));
        clsSpac = (TLineSpacing*) DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find("linespacing"));
        clsTape = (TTSData*) DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find("TSData"));
        clsConc = (TCNData*) DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find("CNData"));
        pName1 = new TNamedObject("Temp1");
        pName2 = new TNamedObject("Temp2");
        if (UuidList.empty())
        {  // this may have been done already from the uuids command
            i1 = clsXfCd->Get_ElementCount() * 6; // 3 wdg info, 3 sctest
            i2 = ActiveCircuit[ActiveActor]->Transformers.NumInList * 11; // bank, info, 3 wdg, 3 wdg info, 3sctest
            StartUuidList(i1 + i2);
        }
        StartBankList(ActiveCircuit[ActiveActor]->Transformers.NumInList + ActiveCircuit[ActiveActor]->AutoTransformers.NumInList);
        StartECPList(ActiveCircuit[ActiveActor]->Loads.NumInList + ActiveCircuit[ActiveActor]->Generators.NumInList + ActiveCircuit[ActiveActor]->StorageElements.NumInList + ActiveCircuit[ActiveActor]->PVSystems.NumInList);
        StartOpLimitList(ActiveCircuit[ActiveActor]->Lines.NumInList + ActiveCircuit[ActiveActor]->Transformers.NumInList + ActiveCircuit[ActiveActor]->AutoTransformers.NumInList + 1);


        FD = new TFileDealer(Combined, Filenm);

        pCRS = new TNamedObject("CoordinateSystem");
        crsUUID = GetDevUuid(CoordSys, "Local", 1);
        pCRS->Set_UUID(crsUUID);
        pCRS->LName = ActiveCircuit[ActiveActor]->Get_Name() + "_CrsUrn";
        StartInstance(GeoPrf, "CoordinateSystem", pCRS);
        StringNode(GeoPrf, "CoordinateSystem.crsUrn", "OpenDSSLocalBusCoordinates");
        EndInstance(GeoPrf, "CoordinateSystem");

        pRegion = new TNamedObject("GeographicalRegion");
        pRegion->Set_UUID(RgnUUID);
        pRegion->LName = GeographicRegion;
        StartInstance(FunPrf, "GeographicalRegion", pRegion);
        EndInstance(FunPrf, "GeographicalRegion");

        pSubRegion = new TNamedObject("SubGeographicalRegion");
        pSubRegion->Set_UUID(SubGeoUUID);
        pSubRegion->LName = SubGeographicRegion;
        StartInstance(FunPrf, "SubGeographicalRegion", pSubRegion);
        RefNode(FunPrf, "SubGeographicalRegion.Region", pRegion);
        EndInstance(FunPrf, "SubGeographicalRegion");

        pSubstation = new TNamedObject("Substation");
        pSubstation->Set_UUID(SubUUID);
        pSubstation->LName = Substation;
        StartInstance(FunPrf, "Substation", pSubstation);
        RefNode(FunPrf, "Substation.Region", pSubRegion);
        EndInstance(FunPrf, "Substation");

        pLocation = new TNamedObject("Location");
        pLocation->Set_UUID(GetDevUuid(FdrLoc, ActiveCircuit[ActiveActor]->Get_Name(), 1));
        pLocation->LName = ActiveCircuit[ActiveActor]->Get_Name() + "_Location";
        StartInstance(GeoPrf, "Location", pLocation);
        UuidNode(GeoPrf, "Location.CoordinateSystem", crsUUID);
        EndInstance(GeoPrf, "Location");

        ActiveCircuit[ActiveActor]->Set_UUID(FdrUUID);
        StartInstance(FunPrf, "Feeder", ActiveCircuit[ActiveActor]);
        RefNode(FunPrf, "Feeder.NormalEnergizingSubstation", pSubstation);
        RefNode(FunPrf, "PowerSystemResource.Location", pLocation);
        EndInstance(FunPrf, "Feeder");

        // the whole system will be a topo island
        pIsland = new TNamedObject("Island");
        pIsland->LName = ActiveCircuit[ActiveActor]->Get_Name() + "_Island";
        pIsland->Set_UUID(GetDevUuid(TopoIsland, "Island", 1));
        pSwing = new TNamedObject("SwingBus");
        pSwing->LName = ActiveCircuit[ActiveActor]->Get_Name() + "_SwingBus";

        pNormLimit = new TNamedObject("NormalAmpsType");
        pNormLimit->LName = ActiveCircuit[ActiveActor]->Get_Name() + "_NormAmpsType";
        pNormLimit->Set_UUID(GetDevUuid(OpLimT, "NormalAmps", 1));
        StartInstance(FunPrf, "OperationalLimitType", pNormLimit);
        DoubleNode(FunPrf, "OperationalLimitType.acceptableDuration", 5.0e9);  // more than 100 years
        OpLimitDirectionEnum(FunPrf, "absoluteValue");
        EndInstance(FunPrf, "OperationalLimitType");

        pEmergLimit = new TNamedObject("EmergencyAmpsType");
        pEmergLimit->LName = ActiveCircuit[ActiveActor]->Get_Name() + "_EmergencyAmpsType";
        pEmergLimit->Set_UUID(GetDevUuid(OpLimT, "EmergencyAmps", 1));
        StartInstance(FunPrf, "OperationalLimitType", pEmergLimit);
        DoubleNode(FunPrf, "OperationalLimitType.acceptableDuration", 2.0 * 3600.0); // 2 hours
        OpLimitDirectionEnum(FunPrf, "absoluteValue");
        EndInstance(FunPrf, "OperationalLimitType");

        pRangeAHiLimit = new TNamedObject("RangeAHiType");
        pRangeAHiLimit->LName = ActiveCircuit[ActiveActor]->Get_Name() + "_RangeAHiType";
        pRangeAHiLimit->Set_UUID(GetDevUuid(OpLimT, "AHi", 1));
        StartInstance(FunPrf, "OperationalLimitType", pRangeAHiLimit);
        DoubleNode(FunPrf, "OperationalLimitType.acceptableDuration", 5.0e9);
        OpLimitDirectionEnum(FunPrf, "high");
        EndInstance(FunPrf, "OperationalLimitType");

        pRangeALoLimit = new TNamedObject("RangeALoType");
        pRangeALoLimit->LName = ActiveCircuit[ActiveActor]->Get_Name() + "_RangeALoType";
        pRangeALoLimit->Set_UUID(GetDevUuid(OpLimT, "ALo", 1));
        StartInstance(FunPrf, "OperationalLimitType", pRangeALoLimit);
        DoubleNode(FunPrf, "OperationalLimitType.acceptableDuration", 5.0e9);
        OpLimitDirectionEnum(FunPrf, "low");
        EndInstance(FunPrf, "OperationalLimitType");

        pRangeBHiLimit = new TNamedObject("RangeBHiType");
        pRangeBHiLimit->LName = ActiveCircuit[ActiveActor]->Get_Name() + "_RangeBHiType";
        pRangeBHiLimit->Set_UUID(GetDevUuid(OpLimT, "BHi", 1));
        StartInstance(FunPrf, "OperationalLimitType", pRangeBHiLimit);
        DoubleNode(FunPrf, "OperationalLimitType.acceptableDuration", 24.0 * 3600.0);
        OpLimitDirectionEnum(FunPrf, "high");
        EndInstance(FunPrf, "OperationalLimitType");

        pRangeBLoLimit = new TNamedObject("RangeBLoType");
        pRangeBLoLimit->LName = ActiveCircuit[ActiveActor]->Get_Name() + "_RangeBLoType";
        pRangeBLoLimit->Set_UUID(GetDevUuid(OpLimT, "BLo", 1));
        StartInstance(FunPrf, "OperationalLimitType", pRangeBLoLimit);
        DoubleNode(FunPrf, "OperationalLimitType.acceptableDuration", 24.0 * 3600.0);
        OpLimitDirectionEnum(FunPrf, "low");
        EndInstance(FunPrf, "OperationalLimitType");

        // with ActiveCircuit[ActiveActor] do
        // build the lists of base voltages and operational voltage limits
        i = 0;
        while (ActiveCircuit[ActiveActor]->LegalVoltageBases[i] > 0.0)
        {
            s = GetBaseVName(ActiveCircuit[ActiveActor]->LegalVoltageBases[i]);
            pName1->LName = s;
            pName1->Set_UUID(GetBaseVUuid(ActiveCircuit[ActiveActor]->LegalVoltageBases[i]));
            StartInstance(FunPrf, "BaseVoltage", pName1);
            DoubleNode(FunPrf, "BaseVoltage.nominalVoltage", 1000.0 * ActiveCircuit[ActiveActor]->LegalVoltageBases[i]);
            EndInstance(FunPrf, "BaseVoltage");

            pName1->LName = GetOpLimVName(ActiveCircuit[ActiveActor]->LegalVoltageBases[i]);
            pName1->Set_UUID(GetOpLimVUuid(ActiveCircuit[ActiveActor]->LegalVoltageBases[i]));
            StartInstance(FunPrf, "OperationalLimitSet", pName1);
            EndInstance(FunPrf, "OperationalLimitSet");

            pName2->LName = pName1->LName + "_RangeAHi";
            pName2->Set_UUID(GetDevUuid(OpLimAHi, s, 1));
            StartInstance(FunPrf, "VoltageLimit", pName2);
            RefNode(FunPrf, "OperationalLimit.OperationalLimitSet", pName1);
            RefNode(FunPrf, "OperationalLimit.OperationalLimitType", pRangeAHiLimit);
            DoubleNode(FunPrf, "VoltageLimit.value", 1.05 * 1000.0 * ActiveCircuit[ActiveActor]->LegalVoltageBases[i]);
            EndInstance(FunPrf, "VoltageLimit");

            pName2->LName = pName1->LName + "_RangeALo";
            pName2->Set_UUID(GetDevUuid(OpLimALo, s, 1));
            StartInstance(FunPrf, "VoltageLimit", pName2);
            RefNode(FunPrf, "OperationalLimit.OperationalLimitSet", pName1);
            RefNode(FunPrf, "OperationalLimit.OperationalLimitType", pRangeALoLimit);
            DoubleNode(FunPrf, "VoltageLimit.value", 0.95 * 1000.0 * ActiveCircuit[ActiveActor]->LegalVoltageBases[i]);
            EndInstance(FunPrf, "VoltageLimit");

            pName2->LName = pName1->LName + "_RangeBHi";
            pName2->Set_UUID(GetDevUuid(OpLimBHi, s, 1));
            StartInstance(FunPrf, "VoltageLimit", pName2);
            RefNode(FunPrf, "OperationalLimit.OperationalLimitSet", pName1);
            RefNode(FunPrf, "OperationalLimit.OperationalLimitType", pRangeBHiLimit);
            DoubleNode(FunPrf, "VoltageLimit.value", 1.0583333 * 1000.0 * ActiveCircuit[ActiveActor]->LegalVoltageBases[i]);
            EndInstance(FunPrf, "VoltageLimit");

            pName2->LName = pName1->LName + "_RangeBLo";
            pName2->Set_UUID(GetDevUuid(OpLimBLo, s, 1));
            StartInstance(FunPrf, "VoltageLimit", pName2);
            RefNode(FunPrf, "OperationalLimit.OperationalLimitSet", pName1);
            RefNode(FunPrf, "OperationalLimit.OperationalLimitType", pRangeBLoLimit);
            DoubleNode(FunPrf, "VoltageLimit.value", 0.9166667 * 1000.0 * ActiveCircuit[ActiveActor]->LegalVoltageBases[i]);
            EndInstance(FunPrf, "VoltageLimit");

            i++;
        }

        for (i = 1; i <= ActiveCircuit[ActiveActor]->NumBuses; ++i)
        {
            ActiveCircuit[ActiveActor]->Buses[i - 1]->LName = ActiveCircuit[ActiveActor]->BusList.Get(i);
        }

        // each bus corresponds to a topo node (TODO, do we need topo nodes anymore?) and connectivity node
        for (i = 1; i <= ActiveCircuit[ActiveActor]->NumBuses; ++i)
        {
            geoUUID = GetDevUuid(Topo, ActiveCircuit[ActiveActor]->Buses[i - 1]->LName, 1);
            StartFreeInstance(TopoPrf, "TopologicalNode", geoUUID);
            StringNode(TopoPrf, "IdentifiedObject.mRID", UUIDToCIMString(geoUUID));
            StringNode(TopoPrf, "IdentifiedObject.name", ActiveCircuit[ActiveActor]->Buses[i - 1]->LName);
            UuidNode(TopoPrf, "TopologicalNode.TopologicalIsland", pIsland->Get_UUID());
            EndInstance(TopoPrf, "TopologicalNode");

            StartFreeInstance(TopoPrf, "ConnectivityNode", ActiveCircuit[ActiveActor]->Buses[i - 1]->Get_UUID());
            StringNode(TopoPrf, "IdentifiedObject.mRID", UUIDToCIMString(ActiveCircuit[ActiveActor]->Buses[i - 1]->Get_UUID()));
            StringNode(TopoPrf, "IdentifiedObject.name", ActiveCircuit[ActiveActor]->Buses[i - 1]->LName);
            UuidNode(TopoPrf, "ConnectivityNode.TopologicalNode", geoUUID);
            UuidNode(TopoPrf, "ConnectivityNode.OperationalLimitSet", GetOpLimVUuid(sqrt(3.0) * ActiveCircuit[ActiveActor]->Buses[i - 1]->kVBase));
            FD->WriteCimLn(TopoPrf, Format("  <cim:ConnectivityNode.ConnectivityNodeContainer rdf:resource=\"urn:uuid:%s\"/>", ActiveCircuit[ActiveActor]->Get_CIM_ID().c_str()));
            EndInstance(TopoPrf, "ConnectivityNode");
        }

        // find the swing bus ==> first voltage source
        pVsrc = (TVsourceObj*) ActiveCircuit[ActiveActor]->Sources.Get_First(); // pIsrc are in the same list
        while (pVsrc != nullptr)
        {
            // if (pVsrc->ClassNameIs("TVSourceObj"))
            {
                if (pVsrc->FEnabled)
                {
                    i = pVsrc->Terminals[0].BusRef - 1;
                    geoUUID = GetDevUuid(Topo, ActiveCircuit[ActiveActor]->Buses[i]->LName, 1);
                    pSwing->Set_UUID(geoUUID);
                    StartInstance(TopoPrf, "TopologicalIsland", pIsland);
                    RefNode(TopoPrf, "TopologicalIsland.AngleRefTopologicalNode", pSwing);
                    EndInstance(TopoPrf, "TopologicalIsland");
                    break;
                }
            }
            pVsrc = (TVsourceObj*) ActiveCircuit[ActiveActor]->Sources.Get_Next();
        }


        pGen = (TGeneratorObj*) ActiveCircuit[ActiveActor]->Generators.Get_First();
        while (pGen != nullptr)
        {
            if (pGen->FEnabled)
            {
                StartInstance(FunPrf, "SynchronousMachine", pGen);
                CircuitNode(FunPrf, ActiveCircuit[ActiveActor]);
                DoubleNode(SshPrf, "RotatingMachine.p", pGen->Get_PresentkW() * 1000.0);
                DoubleNode(SshPrf, "RotatingMachine.q", pGen->Get_Presentkvar() * 1000.0);
                DoubleNode(EpPrf, "RotatingMachine.ratedS", pGen->GenVars.kVArating * 1000.0);
                DoubleNode(EpPrf, "RotatingMachine.ratedU", pGen->Get_PresentkV() * 1000.0);
                // SynchMachTypeEnum (F, "generator");
                // SynchMachModeEnum (F, "generator");
                geoUUID = GetDevUuid(MachLoc, pGen->LName, 1);
                UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
                EndInstance(FunPrf, "SynchronousMachine");
                AttachGeneratorPhases(*pGen, geoUUID);
                WriteTerminals(*pGen, geoUUID, crsUUID);
                AddGeneratorECP(*pGen);
            }
            pGen = (TGeneratorObj*) ActiveCircuit[ActiveActor]->Generators.Get_Next();
        }

        pPV = (TPVsystemObj*) ActiveCircuit[ActiveActor]->PVSystems.Get_First();
        while (pPV != nullptr)
        {
            if (pPV->FEnabled)
            {
                pName1->LName = pPV->get_Name(); // + "_PVPanels";
                pName1->Set_UUID(GetDevUuid(PVPanels, pPV->LName, 1));
                StartInstance(FunPrf, "PhotovoltaicUnit", pName1);
                geoUUID = GetDevUuid(SolarLoc, pPV->LName, 1);
                UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
                DoubleNode(EpPrf, "PowerElectronicsUnit.maxP", pPV->Get_Pmax() * 1000.0);
                DoubleNode(EpPrf, "PowerElectronicsUnit.minP", pPV->Get_Pmin() * 1000.0);
                EndInstance(FunPrf, "PhotovoltaicUnit");
                StartInstance(FunPrf, "PowerElectronicsConnection", pPV);
                CircuitNode(FunPrf, ActiveCircuit[ActiveActor]);
                RefNode(FunPrf, "PowerElectronicsConnection.PowerElectronicsUnit", pName1);
                DoubleNode(EpPrf, "PowerElectronicsConnection.maxIFault", 1.0 / pPV->Vminpu);
                // if (FD->Separate) StartFreeInstance (SshPrf, "PowerElectronicsConnection", pPV->Get_UUID());
                DoubleNode(SshPrf, "PowerElectronicsConnection.p", pPV->Get_PresentkW() * 1000.0);
                DoubleNode(SshPrf, "PowerElectronicsConnection.q", pPV->Get_Presentkvar() * 1000.0);
                ConverterControlEnum(SshPrf, pPV->FvarMode, pPV->Get_CIMDynamicMode());
                // if (FD->Separate) EndInstance (SshPrf, "PowerElectronicsConnection");
                DoubleNode(EpPrf, "PowerElectronicsConnection.ratedS", pPV->PVSystemVars.FkVArating * 1000.0);
                if (pPV->Fnphases == 1)
                    DoubleNode(EpPrf, "PowerElectronicsConnection.ratedU", pPV->Get_PresentkV() * 1000.0 * sqrt(3.0));
                else
                    DoubleNode(EpPrf, "PowerElectronicsConnection.ratedU", pPV->Get_PresentkV() * 1000.0);
                DoubleNode(EpPrf, "PowerElectronicsConnection.maxQ", pPV->Get_qMaxInj() * 1000.0);
                DoubleNode(EpPrf, "PowerElectronicsConnection.minQ", -pPV->Get_qMaxAbs() * 1000.0);
                UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
                EndInstance(FunPrf, "PowerElectronicsConnection");
                AttachSolarPhases(*pPV, geoUUID);
                // we want the location using PV unit name
                WriteReferenceTerminals(*pPV, pPV->Get_UUID());
                s = pPV->LName;
                pPV->LName = pName1->LName;
                WritePositions(*pPV, geoUUID, crsUUID);
                pPV->LName = s;
                AddSolarECP(*pPV);
            }
            pPV = (TPVsystemObj*) ActiveCircuit[ActiveActor]->PVSystems.Get_Next();
        }

        pBat = (TStorageObj*) ActiveCircuit[ActiveActor]->StorageElements.Get_First();
        while (pBat != nullptr)
        {
            if (pBat->FEnabled)
            {
                pName1->LName = pBat->get_Name(); // + "_Cells";
                pName1->Set_UUID(GetDevUuid(Battery, pBat->LName, 1));
                StartInstance(FunPrf, "BatteryUnit", pName1);
                DoubleNode(EpPrf, "PowerElectronicsUnit.maxP", pBat->Get_Pmax() * 1000.0);
                DoubleNode(EpPrf, "PowerElectronicsUnit.minP", pBat->Get_Pmin() * 1000.0);
                DoubleNode(SshPrf, "BatteryUnit.ratedE", pBat->StorageVars.kWhRating * 1000.0);
                DoubleNode(SshPrf, "BatteryUnit.storedE", pBat->StorageVars.kWhStored * 1000.0);
                BatteryStateEnum(SshPrf, pBat->fState);
                geoUUID = GetDevUuid(BatteryLoc, pBat->LName, 1);
                UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
                EndInstance(FunPrf, "BatteryUnit");
                StartInstance(FunPrf, "PowerElectronicsConnection", pBat);
                CircuitNode(FunPrf, ActiveCircuit[ActiveActor]);
                RefNode(FunPrf, "PowerElectronicsConnection.PowerElectronicsUnit", pName1);
                DoubleNode(EpPrf, "PowerElectronicsConnection.maxIFault", 1.0 / pBat->Vminpu);
                DoubleNode(SshPrf, "PowerElectronicsConnection.p", pBat->Get_PresentkW() * 1000.0);
                DoubleNode(SshPrf, "PowerElectronicsConnection.q", pBat->Get_Presentkvar() * 1000.0);
                ConverterControlEnum(SshPrf, pBat->FvarMode, pBat->Get_CIMDynamicMode());
                DoubleNode(EpPrf, "PowerElectronicsConnection.ratedS", pBat->Get_FkVARating() * 1000.0);
                if (pBat->Fnphases == 1)
                    DoubleNode(EpPrf, "PowerElectronicsConnection.ratedU", pBat->Get_PresentkV() * 1000.0 * sqrt(3.0));
                else
                    DoubleNode(EpPrf, "PowerElectronicsConnection.ratedU", pBat->Get_PresentkV() * 1000.0);
                DoubleNode(EpPrf, "PowerElectronicsConnection.maxQ", pBat->Get_qMaxInj() * 1000.0);
                DoubleNode(EpPrf, "PowerElectronicsConnection.minQ", -pBat->Get_qMaxAbs() * 1000.0);
                UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
                EndInstance(FunPrf, "PowerElectronicsConnection");
                AttachStoragePhases(*pBat, geoUUID);
                // we want the location using battery unit name
                WriteReferenceTerminals(*pBat, pBat->Get_UUID());
                s = pBat->LName;
                pBat->LName = pName1->LName;
                WritePositions(*pBat, geoUUID, crsUUID);
                pBat->LName = s;
                AddStorageECP(*pBat);
            }
            pBat = (TStorageObj*) ActiveCircuit[ActiveActor]->StorageElements.Get_Next();
        }

        // with ActiveCircuit[ActiveActor] do {
        if ((ActiveCircuit[ActiveActor]->InvControls2.NumInList > 0) || (ActiveCircuit[ActiveActor]->ExpControls.NumInList > 0))
        {
            pI1547 = new TIEEE1547Controller();
            pInv = (TInvControlObj*) ActiveCircuit[ActiveActor]->InvControls2.Get_First();
            while (pInv != nullptr)
            {
                if (pInv->FEnabled)
                {
                    pI1547->PullFromInvControl(*pInv);
                    pI1547->WriteCIM(DynPrf);
                }
                pInv = (TInvControlObj*) ActiveCircuit[ActiveActor]->InvControls2.Get_Next();
            }
            pExp = (TExpControlObj*) ActiveCircuit[ActiveActor]->ExpControls.Get_First();
            while (pExp != nullptr)
            {
                if (pExp->FEnabled)
                {
                    pI1547->PullFromExpControl(*pExp);
                    pI1547->WriteCIM(DynPrf);
                }
                pExp = (TExpControlObj*) ActiveCircuit[ActiveActor]->ExpControls.Get_Next();
            }
            delete pI1547;
        }
        // }

        pVsrc = (TVsourceObj*) ActiveCircuit[ActiveActor]->Sources.Get_First(); // pIsrc are in the same list
        while (pVsrc != nullptr)
        {
            // if (pVsrc->ClassNameIs("TVSourceObj"))
            {
                if (pVsrc->FEnabled) // with pVsrc do
                {
                    Zs = pVsrc->Z->AvgDiagonal();
                    Zm = pVsrc->Z->AvgOffDiagonal();
                    Rs = Zs.re;
                    Rm = Zm.re;
                    Xs = Zs.im;
                    Xm = Zm.im;
                    v1 = pVsrc->Fnphases;
                    if (v1 > 1.0)
                    {
                        R1 = Rs - Rm;
                        X1 = Xs - Xm;
                        R0 = Rs + (v1 - 1.0) * Rm;
                        X0 = Xs + (v1 - 1.0) * Xm;
                    }
                    else
                    {
                        R1 = Rs;
                        X1 = Xs;
                        R0 = Rs;
                        X0 = Xs;
                    }

                    StartInstance(FunPrf, "EnergySource", pVsrc);
                    CircuitNode(FunPrf, ActiveCircuit[ActiveActor]);
                    VbaseNode(FunPrf, *pVsrc);
                    DoubleNode(EpPrf, "EnergySource.nominalVoltage", 1000 * pVsrc->kVBase);
                    DoubleNode(SshPrf, "EnergySource.voltageMagnitude", 1000 * pVsrc->kVBase * pVsrc->PerUnit);
                    DoubleNode(SshPrf, "EnergySource.voltageAngle", TwoPi * pVsrc->Angle / 360.0);
                    DoubleNode(EpPrf, "EnergySource.r", R1);
                    DoubleNode(EpPrf, "EnergySource.x", X1);
                    DoubleNode(EpPrf, "EnergySource.r0", R0);
                    DoubleNode(EpPrf, "EnergySource.x0", X0);
                    geoUUID = GetDevUuid(SrcLoc, pVsrc->LName, 1);
                    UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
                    EndInstance(FunPrf, "EnergySource");
                    // AttachPhases (F, pVsrc, 1, "EnergySource");
                    WriteTerminals(*pVsrc, geoUUID, crsUUID);
                }
            }
            pVsrc = (TVsourceObj*) ActiveCircuit[ActiveActor]->Sources.Get_Next();
        }

        pCap = (TCapacitorObj*) ActiveCircuit[ActiveActor]->ShuntCapacitors.Get_First();
        while (pCap != nullptr)
        {
            if (pCap->FEnabled)
            {
                StartInstance(FunPrf, "LinearShuntCompensator", pCap);
                CircuitNode(FunPrf, ActiveCircuit[ActiveActor]);
                VbaseNode(FunPrf, *pCap);
                // with pCap do {
                val = 0.001 * pCap->Ftotalkvar / pCap->kvrating / pCap->kvrating / pCap->FNumSteps;
                DoubleNode(EpPrf, "ShuntCompensator.nomU", 1000.0 * pCap->kvrating);
                DoubleNode(EpPrf, "LinearShuntCompensator.bPerSection", val);
                DoubleNode(EpPrf, "LinearShuntCompensator.gPerSection", 0.0);
                if (pCap->Connection == 0)
                {
                    ShuntConnectionKindNode(FunPrf, "ShuntCompensator", "Y");
                    BooleanNode(FunPrf, "ShuntCompensator.grounded", true);  // TODO - check bus 2
                    DoubleNode(EpPrf, "LinearShuntCompensator.b0PerSection", val);
                }
                else
                {
                    ShuntConnectionKindNode(FunPrf, "ShuntCompensator", "D");
                    BooleanNode(FunPrf, "LinearShuntCompensator.grounded", false);
                    DoubleNode(EpPrf, "LinearShuntCompensator.b0PerSection", 0.0);
                }
                DoubleNode(EpPrf, "LinearShuntCompensator.g0PerSection", 0.0);
                IntegerNode(EpPrf, "ShuntCompensator.normalSections", pCap->FNumSteps);
                IntegerNode(EpPrf, "ShuntCompensator.maximumSections", pCap->FNumSteps);

                val = 0.0;
                pCapC = (TCapControlObj*) ActiveCircuit[ActiveActor]->CapControls.Get_First();
                while ((pCapC != nullptr))
                {
                    if (pCapC->Get_Capacitor() == pCap)
                        val = pCapC->ControlVars.OnDelay;
                    pCapC = (TCapControlObj*) ActiveCircuit[ActiveActor]->CapControls.Get_Next();
                }
                DoubleNode(EpPrf, "ShuntCompensator.aVRDelay", val);

                val = 0;
                for (i = 1; i <= pCap->FNumSteps; ++i)
                    if (pCap->get_States(i, ActiveActor) > 0)
                        val = val + 1.0;
                DoubleNode(SshPrf, "ShuntCompensator.sections", val);
                geoUUID = GetDevUuid(CapLoc, pCap->LName, 1);
                UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
                EndInstance(FunPrf, "LinearShuntCompensator");
                AttachCapPhases(*pCap, geoUUID, val);
                WriteTerminals(*pCap, geoUUID, crsUUID, pCap->NormAmps, pCap->EmergAmps);
                // }
            }
            pCap = (TCapacitorObj*) ActiveCircuit[ActiveActor]->ShuntCapacitors.Get_Next();
        }

        pCapC = (TCapControlObj*) ActiveCircuit[ActiveActor]->CapControls.Get_First();
        while ((pCapC != nullptr))
        {   
            // with pCapC do {
            StartInstance(FunPrf, "RegulatingControl", pCapC);
            UuidNode(GeoPrf, "PowerSystemResource.Location", GetDevUuid(CapLoc, pCapC->Get_Capacitor()->get_Name(), 1));
            RefNode(FunPrf, "RegulatingControl.RegulatingCondEq", pCapC->Get_Capacitor());
            i1 = GetCktElementIndex(pCapC->ElementName); // Global function
            UuidNode(FunPrf, "RegulatingControl.Terminal", GetTermUuid(*(TDSSCktElement*) ActiveCircuit[ActiveActor]->CktElements.Get(i1), pCapC->ElementTerminal));
            s = FirstPhaseString(*(TDSSCktElement*)ActiveCircuit[ActiveActor]->CktElements.Get(i1), 1);
            if (pCapC->ControlVars.FPTPhase > 0)
            {
                s.resize(1);
                s[0] += pCapC->ControlVars.FPTPhase - 1;
                MonitoredPhaseNode(FunPrf, s);
            }
            else
            {
                s.resize(1);
                MonitoredPhaseNode(FunPrf, s); // TODO - average, min and max unsupported in CIM
            }
            val = 1.0;
            if (pCapC->get_ControlType() == PFCONTROL)
            {
                v1 = pCapC->ControlVars.PFON_Value;
                v2 = pCapC->ControlVars.PFOFF_Value;
            }
            else
            {
                v1 = pCapC->ControlVars.ON_Value;
                v2 = pCapC->ControlVars.OFF_Value;
                if (pCapC->get_ControlType() == KVARCONTROL)
                    val = 1000.0;
                if (pCapC->get_ControlType() == CURRENTCONTROL)
                    val = pCapC->ControlVars.CTRatio;
                if (pCapC->get_ControlType() == VOLTAGECONTROL)
                    val = pCapC->ControlVars.PTRatio;
            }
            switch (pCapC->get_ControlType())
            {
                case CURRENTCONTROL:
                    RegulatingControlEnum(EpPrf, "currentFlow");
                    break;
                case VOLTAGECONTROL:
                    RegulatingControlEnum(EpPrf, "voltage");
                    break;
                case KVARCONTROL:
                    RegulatingControlEnum(EpPrf, "reactivePower");
                    break;
                case TIMECONTROL:
                    RegulatingControlEnum(EpPrf, "timeScheduled");
                    break;
                case PFCONTROL:
                    RegulatingControlEnum(EpPrf, "powerFactor");
                    break;
                case USERCONTROL:
                    RegulatingControlEnum(EpPrf, "userDefined"); // i.e. unsupported in CIM
                    break;
            }
            BooleanNode(EpPrf, "RegulatingControl.discrete", true);
            BooleanNode(EpPrf, "RegulatingControl.enabled", pCapC->FEnabled);
            DoubleNode(EpPrf, "RegulatingControl.targetValue", val * 0.5 * (v1 + v2));
            DoubleNode(EpPrf, "RegulatingControl.targetDeadband", val * (v2 - v1));
            EndInstance(FunPrf, "RegulatingControl");
            // }
            pCapC = (TCapControlObj*) ActiveCircuit[ActiveActor]->CapControls.Get_Next();
        }

        // size the auxiliary winding, mesh, and core lists for transformer export
        maxWdg = 3; // start with the size of autos
        pXf = (TTransfObj*) ActiveCircuit[ActiveActor]->Transformers.Get_First();
        while (pXf != nullptr)
        {
            if (pXf->FEnabled)
                if (pXf->get_NumWindings() > maxWdg)
                    maxWdg = pXf->get_NumWindings();
            pXf = (TTransfObj*) ActiveCircuit[ActiveActor]->Transformers.Get_Next();
        }

        if (maxWdg > 0)
        {
            WdgList.resize(maxWdg);
            CoreList.resize(maxWdg);
            MeshList.resize(XSCSize(maxWdg));
            for (i = 1; i <= maxWdg; ++i)
                WdgList[i - 1] = new TNamedObject("dummy");
            CoreList[0] = new TNamedObject("dummy");
            for (i = 1; i <= XSCSize(maxWdg); ++i)
                MeshList[i - 1] = new TNamedObject("dummy");
        }

        // do the autotransformers as balanced, three-phase autos, PowerTransformerEnd(s), mesh impedances and core admittances
        // only considering 2 windings, vector group YNa, or 3 windings, vector group YNad1
        pAuto = (TAutoTransObj*) ActiveCircuit[ActiveActor]->AutoTransformers.Get_First();
        while (pAuto != nullptr)
        {
            if (pAuto->FEnabled) // with pAuto do
            {
                if (pAuto->XfmrBank == "")
                    sBank = "=" + pAuto->get_Name();
                else
                    sBank = pAuto->XfmrBank;
                pBank = GetBank(sBank);
                if (pBank == nullptr)
                {
                    pBank = new TBankObject(maxWdg);
                    pBank->LName = sBank;
                    pBank->Set_UUID(GetDevUuid(Bank, sBank, 0));
                    AddBank(pBank);
                }
                pBank->AddAutoTransformer(*pAuto);
                geoUUID = GetDevUuid(XfLoc, pAuto->get_Name(), 1);
                WritePositions(*pAuto, geoUUID, crsUUID);
                // pre-make the winding, mesh and core name objects for easy reference
                for (i = 1; i <= pAuto->get_NumWindings(); ++i)
                {
                    WdgList[i - 1]->LName = pAuto->get_Name() + "_End_" + IntToStr(i);
                    WdgList[i - 1]->Set_UUID(GetDevUuid(Wdg, pAuto->get_Name(), i));
                }
                CoreList[0]->LName = pAuto->get_Name() + "_Yc";
                CoreList[0]->Set_UUID(GetDevUuid(XfCore, pAuto->get_Name(), 1));
                for (i = 1; i <= XSCSize(maxWdg); ++i)
                {
                    MeshList[i - 1]->LName = pAuto->get_Name() + "_Zsc_" + IntToStr(i);
                    MeshList[i - 1]->Set_UUID(GetDevUuid(XfMesh, pAuto->get_Name(), i));
                }
                val = pAuto->Get_BasekVLL(1); // write core Y
                zbase = 1000.0 * val * val / pAuto->Get_WdgkVA(1);
                StartInstance(EpPrf, "TransformerCoreAdmittance", CoreList[0]);
                val = pAuto->pctNoLoadLoss / 100.0 / zbase;
                DoubleNode(EpPrf, "TransformerCoreAdmittance.g", val);
                DoubleNode(EpPrf, "TransformerCoreAdmittance.g0", val);
                val = -pAuto->pctImag / 100.0 / zbase; // inductive B < 0
                DoubleNode(EpPrf, "TransformerCoreAdmittance.b", val);
                DoubleNode(EpPrf, "TransformerCoreAdmittance.b0", val);
                RefNode(EpPrf, "TransformerCoreAdmittance.TransformerEnd", WdgList[0]);
                EndInstance(EpPrf, "TransformerCoreAdmittance");
                seq = 1; // write mesh Z
                for (i = 1; i <= pAuto->get_NumWindings(); ++i)
                {
                    for (k = i + 1; k <= pAuto->get_NumWindings(); ++k)
                    {
                        val = pAuto->Get_BasekVLL(i);
                        zbase = 1000.0 * val * val / pAuto->Get_WdgkVA(1); // always based on Winding 1 kVA
                        StartInstance(EpPrf, "TransformerMeshImpedance", MeshList[seq - 1]);
                        val = zbase * (pAuto->Get_WdgResistance(i) + pAuto->Get_WdgResistance(k));
                        DoubleNode(EpPrf, "TransformerMeshImpedance.r", val);
                        DoubleNode(EpPrf, "TransformerMeshImpedance.r0", val);
                        val = zbase * pAuto->Get_Xsc(seq);
                        seq++;
                        DoubleNode(EpPrf, "TransformerMeshImpedance.x", val);
                        DoubleNode(EpPrf, "TransformerMeshImpedance.x0", val);
                        RefNode(EpPrf, "TransformerMeshImpedance.FromTransformerEnd", WdgList[i - 1]);
                        RefNode(EpPrf, "TransformerMeshImpedance.ToTransformerEnd", WdgList[k - 1]);
                        EndInstance(EpPrf, "TransformerMeshImpedance");
                    }
                }
                // write the Ends, and a Terminal with operational limit for each End
                for (i = 1; i <= pAuto->get_NumWindings(); ++i)
                {
                    StartInstance(FunPrf, "PowerTransformerEnd", WdgList[i - 1]);
                    RefNode(FunPrf, "PowerTransformerEnd.PowerTransformer", pBank);
                    DoubleNode(EpPrf, "PowerTransformerEnd.ratedS", 1000 * pAuto->Get_WdgkVA(i));
                    DoubleNode(EpPrf, "PowerTransformerEnd.ratedU", 1000 * pAuto->WINDING_[i - 1].kVLL);
                    zbase = 1000.0 * pAuto->Get_BasekVLL(i) * pAuto->Get_BasekVLL(i) / pAuto->Get_WdgkVA(i);
                    DoubleNode(EpPrf, "PowerTransformerEnd.r", zbase * pAuto->Get_WdgResistance(i));
                    if (i == 1)
                    {
                        WindingConnectionKindNode(FunPrf, "Y");
                        IntegerNode(FunPrf, "PowerTransformerEnd.phaseAngleClock", 0);
                        BooleanNode(FunPrf, "TransformerEnd.grounded", false);
                    }
                    else if (i == 2)
                    {
                        WindingConnectionKindNode(FunPrf, "A");
                        IntegerNode(FunPrf, "PowerTransformerEnd.phaseAngleClock", 0);
                        BooleanNode(FunPrf, "TransformerEnd.grounded", true);
                        DoubleNode(EpPrf, "TransformerEnd.rground", 0.0); // no rneut or xneut for autotrans
                        DoubleNode(EpPrf, "TransformerEnd.xground", 0.0);
                    }
                    else
                    {
                        WindingConnectionKindNode(FunPrf, "D");
                        IntegerNode(FunPrf, "PowerTransformerEnd.phaseAngleClock", 1);
                        BooleanNode(FunPrf, "TransformerEnd.grounded", false);
                    }
                    IntegerNode(FunPrf, "TransformerEnd.endNumber", i);
                    j = pAuto->Terminals[i - 1].BusRef - 1;
                    pName2->LName = pAuto->get_Name() + "_T" + IntToStr(i);
                    pName2->Set_UUID(GetTermUuid(*pAuto, i));
                    RefNode(FunPrf, "TransformerEnd.Terminal", pName2);
                    UuidNode(FunPrf, "TransformerEnd.BaseVoltage", GetBaseVUuid(sqrt(3.0) * ActiveCircuit[ActiveActor]->Buses[j]->kVBase));
                    EndInstance(FunPrf, "PowerTransformerEnd");
                    // write the Terminal for this End
                    StartInstance(FunPrf, "Terminal", pName2);
                    RefNode(FunPrf, "Terminal.ConductingEquipment", pBank);
                    IntegerNode(FunPrf, "ACDCTerminal.sequenceNumber", i);
                    FD->WriteCimLn(TopoPrf, Format("  <cim:Terminal.ConnectivityNode rdf:resource=\"urn:uuid:%s\"/>", ActiveCircuit[ActiveActor]->Buses[j]->Get_CIM_ID().c_str()));
                    if (i == 1)
                    {   // write the current limit on HV winding, assuming that's winding 1
                        LimitName = GetOpLimIName(pAuto->NormAmps, pAuto->EmergAmps);
                        pILimit = GetOpLimit(LimitName);
                        if (pILimit == nullptr)
                        {
                            pILimit = new TOpLimitObject(pAuto->NormAmps, pAuto->EmergAmps);
                            pILimit->LName = LimitName;
                            pILimit->Set_UUID(GetDevUuid(OpLimI, LimitName, 0));
                            AddOpLimit(pILimit);
                        }
                        LimiTUuid = GetDevUuid(OpLimI, LimitName, 0);
                        UuidNode(FunPrf, "ACDCTerminal.OperationalLimitSet", LimiTUuid);
                    }
                    EndInstance(FunPrf, "Terminal");
                }
            }
            pAuto = (TAutoTransObj*) ActiveCircuit[ActiveActor]->AutoTransformers.Get_Next();
        }

        // { the transformers; 
        //   1. if balanced three-phase and no XfmrCode, use PowerTransformerEnd(s), mesh impedances and core admittances with no tanks
        //   2. with XfmrCode, write TransformerTank, TransformerTankEnd(s) and references to TransformerTankInfoInfo
        //   3. otherwise, write TransformerTank, then create and reference TransformerTankInfo classes

        // for case 3, it's better to identify and create the info classes first
        //    TODO: side effect is that these transformers will reference XfmrCode until the text file is reloaded. Solution results should be the same.
        pXf = (TTransfObj*) ActiveCircuit[ActiveActor]->Transformers.Get_First();
        while (pXf != nullptr)
        {
            if (pXf->FEnabled)
            {
                if ((pXf->XfmrCode.size() < 1) && (pXf->Fnphases != 3))
                {
                    sBank = "CIMXfmrCode_" + pXf->get_Name();
                    clsXfCd->NewObject(sBank);
                    clsXfCd->Set_Code(sBank);
                    pXfCd = ActiveXfmrCodeObj;
                    DSSObjs[ActiveActor].Add(pXfCd); // this is how ExecHelper.pas keeps track of \"General Objects\" for cleanup
                    pXfCd->Set_UUID(GetDevUuid(TankInfo, pXfCd->get_Name(), 1));
                    pXfCd->PullFromTransformer(pXf);
                    pXf->XfmrCode = pXfCd->get_Name();
                }
            }
            pXf = (TTransfObj*) ActiveCircuit[ActiveActor]->Transformers.Get_Next();
        }

        // write all the XfmrCodes first (CIM TransformerTankInfo)
        pXfCd = (TXfmrCodeObj*) clsXfCd->ElementList.Get_First();
        while (pXfCd != nullptr)
        {
            WriteXfmrCode(*pXfCd);
            pXfCd = (TXfmrCodeObj*) clsXfCd->ElementList.Get_Next();
        }

        // create all the banks (CIM PowerTransformer) for regular transformers
        pXf = (TTransfObj*) ActiveCircuit[ActiveActor]->Transformers.Get_First();
        while (pXf != nullptr)
        {
            if (pXf->FEnabled)
            {
                if (pXf->XfmrBank == "")
                    sBank = "=" + pXf->get_Name();
                else
                    sBank = pXf->XfmrBank;
                pBank = GetBank(sBank);
                if (pBank == nullptr)
                {
                    pBank = new TBankObject(maxWdg);
                    pBank->LName = sBank;
                    pBank->Set_UUID(GetDevUuid(Bank, sBank, 0));
                    AddBank(pBank);
                }
            }
            pXf = (TTransfObj*) ActiveCircuit[ActiveActor]->Transformers.Get_Next();
        }

        // write all the transformers, according to the three cases
        pXf = (TTransfObj*) ActiveCircuit[ActiveActor]->Transformers.Get_First();
        while (pXf != nullptr)
        {
            if (pXf->FEnabled) // with pXf do
            {
                // collect this transformer into tanks and banks, and make a location
                if (pXf->XfmrBank == "")
                    sBank = "=" + pXf->get_Name();
                else
                    sBank = pXf->XfmrBank;
                bTanks = true;  // defaults to case 2 or 3 if XfmrCode exists
                if ((pXf->XfmrCode.size() < 1) && (pXf->Fnphases == 3))
                    bTanks = false; // case 1, balanced three-phase

                pBank = GetBank(sBank);
                pBank->AddTransformer(*pXf);
                geoUUID = GetDevUuid(XfLoc, pXf->get_Name(), 1);

                if (bTanks)
                {
                    StartInstance(FunPrf, "TransformerTank", pXf);
                    CircuitNode(FunPrf, ActiveCircuit[ActiveActor]);
                    pXfCd = (TXfmrCodeObj*) clsXfCd->Find(pXf->XfmrCode);
                    RefNode(FunPrf, "TransformerTank.TransformerTankInfo", pXfCd);
                    RefNode(FunPrf, "TransformerTank.PowerTransformer", pBank);
                    UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
                    EndInstance(FunPrf, "TransformerTank");
                    WritePositions(*pXf, geoUUID, crsUUID);
                }
                else
                {
                    WritePositions(*pXf, geoUUID, crsUUID);
                }

                // make the winding, mesh and core name objects for easy reference
                for (i = 1; i <= pXf->get_NumWindings(); ++i)
                {
                    WdgList[i - 1]->LName = pXf->get_Name() + "_End_" + IntToStr(i);
                    WdgList[i - 1]->Set_UUID(GetDevUuid(Wdg, pXf->get_Name(), i));
                }
                CoreList[0]->LName = pXf->get_Name() + "_Yc";
                CoreList[0]->Set_UUID(GetDevUuid(XfCore, pXf->get_Name(), 1));
                for (i = 1; i <= XSCSize(maxWdg); ++i)
                {
                    MeshList[i - 1]->LName = pXf->get_Name() + "_Zsc_" + IntToStr(i);
                    MeshList[i - 1]->Set_UUID(GetDevUuid(XfMesh, pXf->get_Name(), i));
                }

                if (!bTanks)
                { // write the mesh impedances and core admittances
                    val = pXf->Get_BasekVLL(1);
                    zbase = 1000.0 * val * val / pXf->Get_WdgkVA(1);
                    StartInstance(EpPrf, "TransformerCoreAdmittance", CoreList[0]);
                    val = pXf->pctNoLoadLoss / 100.0 / zbase;
                    DoubleNode(EpPrf, "TransformerCoreAdmittance.g", val);
                    DoubleNode(EpPrf, "TransformerCoreAdmittance.g0", val);
                    val = pXf->pctImag / 100.0 / zbase;
                    DoubleNode(EpPrf, "TransformerCoreAdmittance.b", val);
                    DoubleNode(EpPrf, "TransformerCoreAdmittance.b0", val);
                    RefNode(EpPrf, "TransformerCoreAdmittance.TransformerEnd", WdgList[0]);
                    EndInstance(EpPrf, "TransformerCoreAdmittance");
                    seq = 1; // write mesh Z
                    for (i = 1; i <= pXf->get_NumWindings(); ++i)
                    {
                        for (k = i + 1; k <= pXf->get_NumWindings(); ++k)
                        {
                            val = pXf->Get_BasekVLL(i);
                            zbase = 1000.0 * val * val / pXf->Get_WdgkVA(1); // always based on Winding 1 kVA
                            StartInstance(EpPrf, "TransformerMeshImpedance", MeshList[seq - 1]);
                            val = zbase * (pXf->Get_WdgResistance(i) + pXf->Get_WdgResistance(k));
                            DoubleNode(EpPrf, "TransformerMeshImpedance.r", val);
                            DoubleNode(EpPrf, "TransformerMeshImpedance.r0", val);
                            val = zbase * pXf->Get_Xsc(seq);
                            seq++;
                            DoubleNode(EpPrf, "TransformerMeshImpedance.x", val);
                            DoubleNode(EpPrf, "TransformerMeshImpedance.x0", val);
                            RefNode(EpPrf, "TransformerMeshImpedance.FromTransformerEnd", WdgList[i - 1]);
                            RefNode(EpPrf, "TransformerMeshImpedance.ToTransformerEnd", WdgList[k - 1]);
                            EndInstance(EpPrf, "TransformerMeshImpedance");
                        }
                    }
                }

                // write the Ends, and a Terminal for each End
                for (i = 1; i <= pXf->get_NumWindings(); ++i)
                {
                    if (bTanks)
                    {
                        StartInstance(FunPrf, "TransformerTankEnd", WdgList[i - 1]);
                        XfmrTankPhasesAndGround(FunPrf, EpPrf, *pXf, i);
                        RefNode(FunPrf, "TransformerTankEnd.TransformerTank", pXf);
                    }
                    else
                    {
                        StartInstance(FunPrf, "PowerTransformerEnd", WdgList[i - 1]);
                        RefNode(FunPrf, "PowerTransformerEnd.PowerTransformer", pBank);
                        DoubleNode(EpPrf, "PowerTransformerEnd.ratedS", 1000 * pXf->Get_WdgkVA(i));
                        DoubleNode(EpPrf, "PowerTransformerEnd.ratedU", 1000 * pXf->WINDING_[i - 1].kVLL);
                        zbase = 1000.0 * pXf->Get_BasekVLL(i) * pXf->Get_BasekVLL(i) / pXf->Get_WdgkVA(i);
                        DoubleNode(EpPrf, "PowerTransformerEnd.r", zbase * pXf->Get_WdgResistance(i));
                        if (pXf->WINDING_[i - 1].Connection == 1)
                            WindingConnectionKindNode(FunPrf, "D");
                        else if ((pXf->WINDING_[i - 1].Rneut > 0.0) || (pXf->WINDING_[i - 1].Xneut > 0.0))
                            WindingConnectionKindNode(FunPrf, "Yn");
                        else
                            WindingConnectionKindNode(FunPrf, "Y");
                        if (pXf->WINDING_[i - 1].Connection != pXf->WINDING_[0].Connection)  // TODO - this assumes HV winding first, and normal usages
                            IntegerNode(FunPrf, "PowerTransformerEnd.phaseAngleClock", 1);
                        else
                            IntegerNode(FunPrf, "PowerTransformerEnd.phaseAngleClock", 0);
                        j = (i - 1) * pXf->Fnconds + pXf->Fnphases + 1;
                        if ((pXf->WINDING_[i - 1].Connection == 1))
                        { // delta
                            BooleanNode(FunPrf, "TransformerEnd.grounded", false);
                        }
                        else if ((pXf->NodeRef[j - 1] == 0))
                        { // last conductor is grounded solidly
                            BooleanNode(FunPrf, "TransformerEnd.grounded", true);
                            DoubleNode(EpPrf, "TransformerEnd.rground", 0.0);
                            DoubleNode(EpPrf, "TransformerEnd.xground", 0.0);
                        }
                        else if ((pXf->WINDING_[i - 1].Rneut < 0.0))
                        { // probably wye ungrounded
                            BooleanNode(FunPrf, "TransformerEnd.grounded", false);
                        }
                        else
                        { // not delta, not wye solidly grounded or ungrounded
                            BooleanNode(FunPrf, "TransformerEnd.grounded", true);
                            DoubleNode(EpPrf, "TransformerEnd.rground", pXf->WINDING_[i - 1].Rneut);
                            DoubleNode(EpPrf, "TransformerEnd.xground", pXf->WINDING_[i - 1].Xneut);
                        }
                    }
                    IntegerNode(FunPrf, "TransformerEnd.endNumber", i);
                    j = pXf->Terminals[i - 1].BusRef - 1;
                    pName2->LName = pXf->get_Name() + "_T" + IntToStr(i);
                    pName2->Set_UUID(GetTermUuid(*pXf, i));
                    RefNode(FunPrf, "TransformerEnd.Terminal", pName2);
                    UuidNode(FunPrf, "TransformerEnd.BaseVoltage", GetBaseVUuid(sqrt(3.0) * ActiveCircuit[ActiveActor]->Buses[j]->kVBase));
                    if (bTanks)
                        EndInstance(FunPrf, "TransformerTankEnd");
                    else
                        EndInstance(FunPrf, "PowerTransformerEnd");
                    // write the Terminal for this End
                    StartInstance(FunPrf, "Terminal", pName2);
                    RefNode(FunPrf, "Terminal.ConductingEquipment", pBank);
                    IntegerNode(FunPrf, "ACDCTerminal.sequenceNumber", i);
                    FD->WriteCimLn(TopoPrf, Format("  <cim:Terminal.ConnectivityNode rdf:resource=\"urn:uuid:%s\"/>", ActiveCircuit[ActiveActor]->Buses[j]->Get_CIM_ID().c_str()));
                    if (i == 1)
                    {   // write the current limit on HV winding, assuming that's winding 1
                        LimitName = GetOpLimIName(pXf->NormAmps, pXf->EmergAmps);
                        pILimit = GetOpLimit(LimitName);
                        if (pILimit == nullptr)
                        {
                            pILimit = new TOpLimitObject(pXf->NormAmps, pXf->EmergAmps);
                            pILimit->LName = LimitName;
                            pILimit->Set_UUID(GetDevUuid(OpLimI, LimitName, 0));
                            AddOpLimit(pILimit);
                        }
                        LimiTUuid = GetDevUuid(OpLimI, LimitName, 0);
                        UuidNode(FunPrf, "ACDCTerminal.OperationalLimitSet", LimiTUuid);
                    }
                    EndInstance(FunPrf, "Terminal");
                }
            }
            pXf = (TTransfObj*) ActiveCircuit[ActiveActor]->Transformers.Get_Next();
        }

        // finally, write all the transformer banks (CIM PowerTransformer), including autotransformers
        for (i = 0; i < BankList.size(); ++i)
        {
            pBank = BankList[i];
            if (pBank == nullptr)
                break;
            pBank->BuildVectorGroup();
            // we don't want = sign in the name.  These should still be unique names
            if (AnsiPos("=", pBank->LName) == 1)
                pBank->LName = pBank->LName.substr(1);
            StartInstance(FunPrf, "PowerTransformer", pBank);
            CircuitNode(FunPrf, ActiveCircuit[ActiveActor]);
            StringNode(FunPrf, "PowerTransformer.vectorGroup", pBank->vectorGroup);
            UuidNode(GeoPrf, "PowerSystemResource.Location", GetDevUuid(XfLoc, pBank->pd_unit->get_Name(), 1));
            EndInstance(FunPrf, "PowerTransformer");
        }

        if (!WdgList.empty())
        {
            for (i = 0; i < WdgList.size(); ++i)
                if (WdgList[i] != nullptr)
                    delete WdgList[i];
            WdgList.clear();
        }
        if (!CoreList.empty())
        {
            for (i = 0; i < CoreList.size(); ++i)
                if (CoreList[i] != nullptr)
                    delete CoreList[i];
            CoreList.clear();
        }
        if (!MeshList.empty())
        {
            for (i = 0; i < MeshList.size(); ++i)
                if (MeshList[i] != nullptr)
                    delete MeshList[i];
            MeshList.clear();
        }

        // voltage regulators
        pReg = (TRegControlObj*) ActiveCircuit[ActiveActor]->RegControls.Get_First();
        while ((pReg != nullptr))
        {
            // with pReg do
            // {
            v1 = pReg->Get_Transformer()->Get_BaseVoltage(pReg->TapWinding) / pReg->get_PTRatio();
            pName2->LName = pReg->LName + "_Ctrl";
            pName2->Set_UUID(GetDevUuid(TapCtrl, pReg->LName, 1));
            StartInstance(FunPrf, "TapChangerControl", pName2);
            RegulatingControlEnum(FunPrf, "voltage");
            UuidNode(FunPrf, "RegulatingControl.Terminal", GetTermUuid(*pReg->Get_Transformer(), pReg->TapWinding));
            MonitoredPhaseNode(FunPrf, FirstPhaseString(*pReg->Get_Transformer(), pReg->TapWinding));
            BooleanNode(FunPrf, "RegulatingControl.enabled", pReg->FEnabled);
            BooleanNode(EpPrf, "RegulatingControl.discrete", true);
            DoubleNode(EpPrf, "RegulatingControl.targetValue", pReg->Vreg);
            DoubleNode(EpPrf, "RegulatingControl.targetDeadband", pReg->Bandwidth);
            BooleanNode(EpPrf, "TapChangerControl.lineDropCompensation", pReg->LDCActive);
            DoubleNode(EpPrf, "TapChangerControl.lineDropR", pReg->R);
            DoubleNode(EpPrf, "TapChangerControl.lineDropX", pReg->X);
            if (pReg->IsReversible)
            {
                BooleanNode(EpPrf, "TapChangerControl.reversible", true);
                BooleanNode(EpPrf, "TapChangerControl.reverseToNeutral", pReg->ReverseNeutral);
                DoubleNode(EpPrf, "TapChangerControl.reversingDelay", pReg->revDelay);
                DoubleNode(EpPrf, "TapChangerControl.reversingPowerThreshold", pReg->RevPowerThreshold);
                DoubleNode(EpPrf, "TapChangerControl.reverseLineDropR", pReg->revR);
                DoubleNode(EpPrf, "TapChangerControl.reverseLineDropX", pReg->revX);
                DoubleNode(EpPrf, "RegulatingControl.reverseTargetValue", pReg->revVreg);
                DoubleNode(EpPrf, "RegulatingControl.reverseTargetDeadband", pReg->revBandwidth);
            }
            else
            {
                BooleanNode(EpPrf, "TapChangerControl.reversible", false);
            }
            if (pReg->VLimitActive)
            { // maxLimitVoltage only in OpenDSS
                DoubleNode(EpPrf, "TapChangerControl.maxLimitVoltage", pReg->Vlimit);
            }
            else
            {
                DoubleNode(EpPrf, "TapChangerControl.maxLimitVoltage", pReg->Get_MaxTap() * v1);
            }
            DoubleNode(EpPrf, "TapChangerControl.minLimitVoltage", pReg->Get_MinTap() * v1);
            UuidNode(GeoPrf, "PowerSystemResource.Location", GetDevUuid(XfLoc, pReg->Get_Transformer()->get_Name(), 1));
            EndInstance(FunPrf, "TapChangerControl");

            StartInstance(FunPrf, "RatioTapChanger", *pReg);
            UuidNode(FunPrf, "RatioTapChanger.TransformerEnd", GetDevUuid(Wdg, pReg->Get_Transformer()->get_Name(), pReg->TapWinding));
            UuidNode(FunPrf, "TapChanger.TapChangerControl", pName2->Get_UUID());
            DoubleNode(EpPrf, "RatioTapChanger.stepVoltageIncrement", 100.0 * pReg->Get_TapIncrement());
            TransformerControlEnum(FunPrf, "volt");
            IntegerNode(EpPrf, "TapChanger.highStep", pReg->Get_NumTaps() / 2);
            IntegerNode(EpPrf, "TapChanger.lowStep", -pReg->Get_NumTaps() / 2);
            IntegerNode(EpPrf, "TapChanger.neutralStep", 0);
            IntegerNode(EpPrf, "TapChanger.normalStep", 0);
            DoubleNode(EpPrf, "TapChanger.neutralU", v1 * pReg->get_PTRatio());
            DoubleNode(EpPrf, "TapChanger.initialDelay", pReg->TimeDelay);
            DoubleNode(EpPrf, "TapChanger.subsequentDelay", pReg->TapDelay);
            BooleanNode(EpPrf, "TapChanger.ltcFlag", true);
            BooleanNode(SshPrf, "TapChanger.controlEnabled", pReg->FEnabled);
            DoubleNode(SshPrf, "TapChanger.step", pReg->Get_TapNum());
            DoubleNode(EpPrf, "TapChanger.ptRatio", pReg->get_PTRatio());
            DoubleNode(EpPrf, "TapChanger.ctRatio", pReg->get_CTRating() / 0.2);
            DoubleNode(EpPrf, "TapChanger.ctRating", pReg->get_CTRating());
            UuidNode(GeoPrf, "PowerSystemResource.Location", GetDevUuid(XfLoc, pReg->Get_Transformer()->get_Name(), 1));
            EndInstance(FunPrf, "RatioTapChanger");
            //}
            pReg = (TRegControlObj*) ActiveCircuit[ActiveActor]->RegControls.Get_Next();
        }

        // done with the transformers

        // series reactors, exported as SeriesCompensators
        pReac = (TReactorObj*) ActiveCircuit[ActiveActor]->Reactors.Get_First();
        while (pReac != nullptr)
        {
            if (pReac->FEnabled)
            {
                StartInstance(FunPrf, "SeriesCompensator", pReac);
                CircuitNode(FunPrf, ActiveCircuit[ActiveActor]);
                VbaseNode(FunPrf, *pReac);
                geoUUID = GetDevUuid(ReacLoc, pReac->get_Name(), 1);
                UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
                DoubleNode(EpPrf, "SeriesCompensator.r", pReac->R);
                DoubleNode(EpPrf, "SeriesCompensator.x", pReac->X);
                DoubleNode(EpPrf, "SeriesCompensator.r0", pReac->R);
                DoubleNode(EpPrf, "SeriesCompensator.x0", pReac->X);
                EndInstance(FunPrf, "SeriesCompensator");
                // AttachLinePhases (F_, pReac); // for the 8500-node circuit, we only need 3 phase series reactors
                WriteTerminals(*pReac, geoUUID, crsUUID, pReac->NormAmps, pReac->EmergAmps);
            }
            pReac = (TReactorObj*) ActiveCircuit[ActiveActor]->Reactors.Get_Next();
        }

        pLine = (TLineObj*) ActiveCircuit[ActiveActor]->Lines.Get_First();
        while (pLine != nullptr)
        {
            if (pLine->FEnabled)
            // with pLine do
            {
                bval = false; // flag to write a \"line code\" of PULengthPhaseZ
                v1 = To_Meters(pLine->FUserLengthUnits);
                geoUUID = GetDevUuid(LineLoc, pLine->get_Name(), 1);
                if (pLine->IsSwitch)
                {
                    ParseSwitchClass(*pLine, swtCls, ratedAmps, breakingAmps);
                    StartInstance(FunPrf, swtCls, pLine);
                    CircuitNode(FunPrf, ActiveCircuit[ActiveActor]);
                    VbaseNode(FunPrf, *pLine);
                    if (breakingAmps > 0.0)
                        DoubleNode(EpPrf, "ProtectedSwitch.breakingCapacity", breakingAmps); // Fuse and Sectionaliser don't have this, others do
                    DoubleNode(EpPrf, "Switch.ratedCurrent", ratedAmps);
                    // some OpenDSS models have enabled=false to signal open switches, but we can't actually
                    // export them because disabled elements don't have terminal references in memory
                    if (pLine->FEnabled)
                    {
                        BooleanNode(FunPrf, "Switch.normalOpen", !pLine->Get_ConductorClosed(0, ActiveActor));
                        BooleanNode(SshPrf, "Switch.open", !pLine->Get_ConductorClosed(0, ActiveActor));
                    }
                    else
                    {
                        BooleanNode(FunPrf, "Switch.normalOpen", true);
                        BooleanNode(SshPrf, "Switch.open", true);
                    }
                    BooleanNode(FunPrf, "Switch.retained", true);
                    UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
                    EndInstance(FunPrf, swtCls);
                    AttachSwitchPhases(*pLine);
                }
                else
                {
                    StartInstance(FunPrf, "ACLineSegment", pLine);
                    CircuitNode(FunPrf, ActiveCircuit[ActiveActor]);
                    VbaseNode(FunPrf, *pLine);
                    if (pLine->FLineCodeSpecified)
                    {
                        if ((pLine->FUserLengthUnits == UNITS_NONE))
                            v1 = To_Meters(pLine->FLineCodeUnits);
                        DoubleNode(FunPrf, "Conductor.length", pLine->Len * v1);
                        LineCodeRefNode(EpPrf, *clsLnCd, pLine->CondCode);
                    }
                    else if (pLine->GeometrySpecified)
                    {
                        DoubleNode(FunPrf, "Conductor.length", pLine->Len * v1);
                        LineSpacingRefNode(CatPrf, *clsGeom, pLine->GeometryCode);
                    }
                    else if (pLine->SpacingSpecified)
                    {
                        DoubleNode(FunPrf, "Conductor.length", pLine->Len * v1);
                        LineSpacingRefNode(CatPrf, *clsSpac, pLine->SpacingCode);
                    }
                    else
                    {
                        if (pLine->SymComponentsModel && (pLine->Fnphases == 3))
                        {
                            val = 1.0e-9 * TwoPi * pLine->BaseFrequency; // convert nF to mhos
                            DoubleNode(FunPrf, "Conductor.length", 1.0); // we don't know the physical length
                            DoubleNode(EpPrf, "ACLineSegment.r", pLine->Len * pLine->R1); // total ohms
                            DoubleNode(EpPrf, "ACLineSegment.x", pLine->Len * pLine->X1);
                            DoubleNode(EpPrf, "ACLineSegment.bch", pLine->Len * pLine->C1 * val);
                            DoubleNode(EpPrf, "ACLineSegment.gch", 0.0);
                            DoubleNode(EpPrf, "ACLineSegment.r0", pLine->Len * pLine->R0);
                            DoubleNode(EpPrf, "ACLineSegment.x0", pLine->Len * pLine->X0);
                            DoubleNode(EpPrf, "ACLineSegment.b0ch", pLine->Len * pLine->C0 * val);
                            DoubleNode(EpPrf, "ACLineSegment.b0ch", 0.0);
                        }
                        else
                        {
                            bval = true;
                            pName1->LName = pLine->get_Name() + "_PUZ";
                            pName1->Set_UUID(GetDevUuid(PUZ, pLine->get_Name(), 1));
                            RefNode(EpPrf, "ACLineSegment.PerLengthImpedance", pName1);
                            // TODO - we no longer have proper length units if matrices were specified
                            DoubleNode(FunPrf, "Conductor.length", pLine->Len * v1);
                        }
                    }
                    UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
                    EndInstance(FunPrf, "ACLineSegment");
                    if (!(pLine->SymComponentsModel && (pLine->Fnphases == 3)))
                        AttachLinePhases(*pLine);
                    if (bval)
                    {  // writing PuZ on the fly
                        StartInstance(EpPrf, "PerLengthPhaseImpedance", pName1);
                        IntegerNode(EpPrf, "PerLengthPhaseImpedance.conductorCount", pLine->Fnphases);
                        EndInstance(EpPrf, "PerLengthPhaseImpedance");
                        seq = 1;
                        for (i = 1; i <= pLine->Fnphases; ++i)
                        {
                            for (j = 1; j <= i; ++j)
                            {
                                StartFreeInstance(EpPrf, "PhaseImpedanceData", GetDevUuid(ZData, pName1->LName, seq));
                                RefNode(EpPrf, "PhaseImpedanceData.PhaseImpedance", pName1);
                                IntegerNode(EpPrf, "PhaseImpedanceData.row", i);
                                IntegerNode(EpPrf, "PhaseImpedanceData.column", j);
                                DoubleNode(EpPrf, "PhaseImpedanceData.r", pLine->Z->GetElement(i, j).re / 1609.34);
                                DoubleNode(EpPrf, "PhaseImpedanceData.x", pLine->Z->GetElement(i, j).im / 1609.34);
                                DoubleNode(EpPrf, "PhaseImpedanceData.b", pLine->YC->GetElement(i, j).im / 1609.34);
                                EndInstance(EpPrf, "PhaseImpedanceData");
                                seq++;
                            }
                        }
                    }
                }
                WriteTerminals(*pLine, geoUUID, crsUUID, pLine->NormAmps, pLine->EmergAmps);
            }
            pLine = (TLineObj*) ActiveCircuit[ActiveActor]->Lines.Get_Next();
        }

        // create the DSS-like load models
        id1_ConstkVA = GetDevUuid(LoadResp, "ConstkVA", 1);
        id2_ConstZ = GetDevUuid(LoadResp, "ConstZ", 1);
        id3_ConstPQuadQ = GetDevUuid(LoadResp, "ConstPQuadQ", 1);
        id4_LinPQuadQ = GetDevUuid(LoadResp, "LinPQuadQ", 1);
        id5_ConstI = GetDevUuid(LoadResp, "ConstI", 1);
        id6_ConstPConstQ = GetDevUuid(LoadResp, "ConstQ", 1);  // P can vary, Q not
        id7_ConstPConstX = GetDevUuid(LoadResp, "ConstX", 1);

        WriteLoadModel("Constant kVA", id1_ConstkVA,
            0, 0, 100,
            0, 0, 100,
            0, 0);
        WriteLoadModel("Constant Z", id2_ConstZ,
            100, 0, 0,
            100, 0, 0,
            0, 0);
        WriteLoadModel("Motor", id3_ConstPQuadQ,
            0, 0, 100,
            100, 0, 0,
            0, 0);
        WriteLoadModel("Mix Motor/Res", id4_LinPQuadQ,
            0, 0, 0,
            0, 0, 0,
            1, 2);
        WriteLoadModel("Constant I", id5_ConstI,
            0, 100, 0,
            0, 100, 0,
            0, 0);
        WriteLoadModel("Variable P, Fixed Q", id6_ConstPConstQ,
            0, 0, 100,
            0, 0, 100,
            0, 0);
        WriteLoadModel("Variable P, Fixed X", id7_ConstPConstX,
            0, 0, 100,
            100, 0, 0,
            0, 0);

        pLoad = (TLoadObj*) ActiveCircuit[ActiveActor]->Loads.Get_First();
        while (pLoad != nullptr)
        {
            if (pLoad->FEnabled)
            // with pLoad do
            {
                StartInstance(FunPrf, "EnergyConsumer", pLoad);
                CircuitNode(FunPrf, ActiveCircuit[ActiveActor]);
                VbaseNode(FunPrf, *pLoad);
                switch (pLoad->FLoadModel)
                {
                    case 1:
                        UuidNode(FunPrf, "EnergyConsumer.LoadResponse", id1_ConstkVA);
                        break;
                    case 2:
                        UuidNode(FunPrf, "EnergyConsumer.LoadResponse", id2_ConstZ);
                        break;
                    case 3:
                        UuidNode(FunPrf, "EnergyConsumer.LoadResponse", id3_ConstPQuadQ);
                        break;
                    case 4:
                        UuidNode(FunPrf, "EnergyConsumer.LoadResponse", id4_LinPQuadQ);
                        break;
                    case 5:
                        UuidNode(FunPrf, "EnergyConsumer.LoadResponse", id5_ConstI);
                        break;
                    case 6:
                        UuidNode(FunPrf, "EnergyConsumer.LoadResponse", id6_ConstPConstQ);
                        break;
                    case 7:
                        UuidNode(FunPrf, "EnergyConsumer.LoadResponse", id7_ConstPConstX);
                        break;
                }
                DoubleNode(SshPrf, "EnergyConsumer.p", 1000.0 * pLoad->kWBase);
                DoubleNode(SshPrf, "EnergyConsumer.q", 1000.0 * pLoad->kvarBase);
                IntegerNode(FunPrf, "EnergyConsumer.customerCount", pLoad->NumCustomers);
                if (pLoad->Connection == 0)
                {
                    ShuntConnectionKindNode(FunPrf, "EnergyConsumer", "Y");
                    BooleanNode(FunPrf, "EnergyConsumer.grounded", true);  // TODO - check bus 2
                }
                else
                {
                    ShuntConnectionKindNode(FunPrf, "EnergyConsumer", "D");
                    BooleanNode(FunPrf, "EnergyConsumer.grounded", false);
                }
                geoUUID = GetDevUuid(LoadLoc, pLoad->get_Name(), 1);
                UuidNode(GeoPrf, "PowerSystemResource.Location", geoUUID);
                EndInstance(FunPrf, "EnergyConsumer");
                AttachLoadPhases(*pLoad, geoUUID);
                WriteTerminals(*pLoad, geoUUID, crsUUID);
                AddLoadECP(*pLoad);
            }
            pLoad = (TLoadObj*) ActiveCircuit[ActiveActor]->Loads.Get_Next();
        }

        pLnCd = (TLineCodeObj*) clsLnCd->ElementList.Get_First();
        while (pLnCd != nullptr)
        {
            // with pLnCd do
            {
                if (pLnCd->Units == UNITS_NONE)
                { // we need the real units for CIM
                    pLine = (TLineObj*) ActiveCircuit[ActiveActor]->Lines.Get_First();
                    while (pLine != nullptr)
                    {
                        if (pLine->FEnabled)
                        {
                            if (pLine->CondCode == pLnCd->LName)
                            {
                                pLnCd->Units = pLine->FUserLengthUnits;
                                // WriteLn ("Setting Units on " + pLnCd.LName + " to " + LineUnitsStr(pLnCd.Units));
                                break;
                            }
                        }
                        pLine = (TLineObj*) ActiveCircuit[ActiveActor]->Lines.Get_Next();
                    }
                }
                v1 = To_per_Meter(pLnCd->Units); // TODO: warn if still UNITS_NONE
                if (pLnCd->SymComponentsModel && (pLnCd->Fnphases == 3))
                {
                    v2 = 1.0e-9 * TwoPi * pLnCd->BaseFrequency; // convert nF to mhos
                    StartInstance(EpPrf, "PerLengthSequenceImpedance", pLnCd);
                    DoubleNode(EpPrf, "PerLengthSequenceImpedance.r", pLnCd->R1 * v1);
                    DoubleNode(EpPrf, "PerLengthSequenceImpedance.x", pLnCd->X1 * v1);
                    DoubleNode(EpPrf, "PerLengthSequenceImpedance.bch", pLnCd->C1 * v1 * v2);
                    DoubleNode(EpPrf, "PerLengthSequenceImpedance.gch", 0.0);
                    DoubleNode(EpPrf, "PerLengthSequenceImpedance.r0", pLnCd->R0 * v1);
                    DoubleNode(EpPrf, "PerLengthSequenceImpedance.x0", pLnCd->X0 * v1);
                    DoubleNode(EpPrf, "PerLengthSequenceImpedance.b0ch", pLnCd->C0 * v1 * v2);
                    DoubleNode(EpPrf, "PerLengthSequenceImpedance.g0ch", 0.0);
                    EndInstance(EpPrf, "PerLengthSequenceImpedance");
                }
                else
                {
                    StartInstance(EpPrf, "PerLengthPhaseImpedance", pLnCd);
                    IntegerNode(EpPrf, "PerLengthPhaseImpedance.conductorCount", pLnCd->Fnphases);
                    EndInstance(EpPrf, "PerLengthPhaseImpedance");
                    seq = 1;
                    for (i = 1; i <= pLnCd->Fnphases; ++i)
                    {
                        for (j = 1; j <= i; ++j)
                        {
                            StartFreeInstance(EpPrf, "PhaseImpedanceData", GetDevUuid(ZData, pLnCd->LName, seq));
                            RefNode(EpPrf, "PhaseImpedanceData.PhaseImpedance", pLnCd);
                            IntegerNode(EpPrf, "PhaseImpedanceData.row", i);
                            IntegerNode(EpPrf, "PhaseImpedanceData.column", j);
                            DoubleNode(EpPrf, "PhaseImpedanceData.r", pLnCd->Z->GetElement(i, j).re * v1);
                            DoubleNode(EpPrf, "PhaseImpedanceData.x", pLnCd->Z->GetElement(i, j).im * v1);
                            DoubleNode(EpPrf, "PhaseImpedanceData.b", pLnCd->YC->GetElement(i, j).im * v1);
                            EndInstance(EpPrf, "PhaseImpedanceData");
                            seq++;
                        }
                    }
                }
            }
            pLnCd = (TLineCodeObj*) clsLnCd->ElementList.Get_Next();
        }

        pWire = (TWireDataObj*) clsWire->ElementList.Get_First();
        while (pWire != nullptr)
        {
            StartInstance(CatPrf, "OverheadWireInfo", pWire);
            WriteWireData(*pWire);
            BooleanNode(CatPrf, "WireInfo.insulated", false);
            EndInstance(CatPrf, "OverheadWireInfo");
            pWire = (TWireDataObj*) clsWire->ElementList.Get_Next();
        }

        pTape = (TTSDataObj*) clsTape->ElementList.Get_First();
        while (pTape != nullptr)
        {
            StartInstance(CatPrf, "TapeShieldCableInfo", pTape);
            WriteWireData(*pTape);
            WriteCableData(*pTape);
            WriteTapeData(*pTape);
            EndInstance(CatPrf, "TapeShieldCableInfo");
            pTape = (TTSDataObj*) clsTape->ElementList.Get_Next();
        }

        pConc = (TCNDataObj*) clsConc->ElementList.Get_First();
        while (pConc != nullptr)
        {
            StartInstance(CatPrf, "ConcentricNeutralCableInfo", pConc);
            WriteWireData(*pConc);
            WriteCableData(*pConc);
            WriteConcData(*pConc);
            EndInstance(CatPrf, "ConcentricNeutralCableInfo");
            pConc = (TCNDataObj*) clsConc->ElementList.Get_Next();
        }

        pGeom = (TLineGeometryObj*) clsGeom->ElementList.Get_First();
        while (pGeom != nullptr)
        {
            // with pGeom do
            {
                StartInstance(CatPrf, "WireSpacingInfo", pGeom);
                ConductorUsageEnum(CatPrf, "distribution");
                IntegerNode(CatPrf, "WireSpacingInfo.phaseWireCount", 1);
                DoubleNode(CatPrf, "WireSpacingInfo.phaseWireSpacing", 0.0);
                if (pGeom->FPhaseChoice[0] == Overhead)
                    BooleanNode(CatPrf, "WireSpacingInfo.isCable", false);
                else
                    BooleanNode(CatPrf, "WireSpacingInfo.isCable", true);
                EndInstance(CatPrf, "WireSpacingInfo");

                for (i = 1; i <= pGeom->Fnconds; ++i)
                {
                    pName1->LName = "WP_" + pGeom->get_Name() + "_" + IntToStr(i);
                    pName1->Set_UUID(GetDevUuid(WirePos, pName1->LName, 1));  // 1 for pGeom
                    StartInstance(CatPrf, "WirePosition", pName1);
                    RefNode(CatPrf, "WirePosition.WireSpacingInfo", pGeom);
                    IntegerNode(CatPrf, "WirePosition.sequenceNumber", i);
                    v1 = To_Meters(pGeom->FUnits[i - 1]);
                    DoubleNode(CatPrf, "WirePosition.xCoord", pGeom->Get_FX(i) * v1);
                    DoubleNode(CatPrf, "WirePosition.yCoord", pGeom->Get_FY(i) * v1);
                    EndInstance(CatPrf, "WirePosition");
                }
            }
            pGeom = (TLineGeometryObj*) clsGeom->ElementList.Get_Next();
        }

        pSpac = (TLineSpacingObj*) clsSpac->ElementList.Get_First();
        while (pSpac != nullptr)
        {
            // with pSpac do
            {
                v1 = To_Meters(pSpac->FUnits);
                StartInstance(CatPrf, "WireSpacingInfo", pSpac);
                ConductorUsageEnum(CatPrf, "distribution");
                IntegerNode(CatPrf, "WireSpacingInfo.phaseWireCount", 1);
                DoubleNode(CatPrf, "WireSpacingInfo.phaseWireSpacing", 0.0);
                if (pSpac->Get_FY(1) > 0.0)
                    BooleanNode(CatPrf, "WireSpacingInfo.isCable", false);
                else
                    BooleanNode(CatPrf, "WireSpacingInfo.isCable", true);
                EndInstance(CatPrf, "WireSpacingInfo");

                for (i = 1; i < pSpac->Fnconds; ++i)
                {
                    pName1->LName = "WP_" + pSpac->get_Name() + "_" + IntToStr(i);
                    pName1->Set_UUID(GetDevUuid(WirePos, pName1->LName, 2)); // 2 for pSpac
                    StartInstance(CatPrf, "WirePosition", pName1);
                    RefNode(CatPrf, "WirePosition.WireSpacingInfo", pSpac);
                    IntegerNode(CatPrf, "WirePosition.sequenceNumber", i);
                    DoubleNode(CatPrf, "WirePosition.xCoord", pSpac->Get_FX(i) * v1);
                    DoubleNode(CatPrf, "WirePosition.yCoord", pSpac->Get_FY(i) * v1);
                    EndInstance(CatPrf, "WirePosition");
                }
            }
            pSpac = (TLineSpacingObj*) clsSpac->ElementList.Get_Next();
        }

        for (i = 0; i < ECPList.size(); ++i)
        {
            pECP = ECPList[i];
            if (pECP == nullptr)
                break;
            StartInstance(SshPrf, "EnergyConnectionProfile", pECP);
            if (pECP->daily != "")
                StringNode(SshPrf, "EnergyConnectionProfile.dssDaily", pECP->daily);
            if (pECP->duty != "")
                StringNode(SshPrf, "EnergyConnectionProfile.dssDuty", pECP->duty);
            if (pECP->yearly != "")
                StringNode(SshPrf, "EnergyConnectionProfile.dssYearly", pECP->yearly);
            if (pECP->growth != "")
                StringNode(SshPrf, "EnergyConnectionProfile.dssLoadGrowth", pECP->growth);
            if (pECP->spectrum != "")
                StringNode(SshPrf, "EnergyConnectionProfile.dssSpectrum", pECP->spectrum);
            if (pECP->cvr != "")
                StringNode(SshPrf, "EnergyConnectionProfile.dssLoadCvrCurve", pECP->cvr);
            if (pECP->Tdaily != "")
                StringNode(SshPrf, "EnergyConnectionProfile.dssPVTDaily", pECP->Tdaily);
            if (pECP->Tduty != "")
                StringNode(SshPrf, "EnergyConnectionProfile.dssPVTDuty", pECP->Tduty);
            if (pECP->Tyearly != "")
                StringNode(SshPrf, "EnergyConnectionProfile.dssPVTYearly", pECP->Tyearly);
            for (j = 0; j < pECP->nconn; ++j)
                UuidNode(SshPrf, "EnergyConnectionProfile.EnergyConnections", pECP->connections[j]);
            EndInstance(SshPrf, "EnergyConnectionProfile");
        }

        // export the operational current limits that were created on-the-fly
        for (i = 0; i < OpLimitList.size(); ++i)
        {
            pILimit = OpLimitList[i];
            if (pILimit == nullptr)
                break;
            StartInstance(FunPrf, "OperationalLimitSet", pILimit);
            EndInstance(FunPrf, "OperationalLimitSet");
            pName1->LName = pILimit->LName + "_Norm";
            pName1->Set_UUID(GetDevUuid(NormAmps, pILimit->LName, 1));
            StartInstance(FunPrf, "CurrentLimit", pName1);
            RefNode(FunPrf, "OperationalLimit.OperationalLimitSet", pILimit);
            RefNode(FunPrf, "OperationalLimit.OperationalLimitType", pNormLimit);
            DoubleNode(FunPrf, "CurrentLimit.value", pILimit->NormAmps);
            EndInstance(FunPrf, "CurrentLimit");
            pName2->LName = pILimit->LName + "_Emerg";
            pName2->Set_UUID(GetDevUuid(EmergAmps, pILimit->LName, 1));
            StartInstance(FunPrf, "CurrentLimit", pName2);
            RefNode(FunPrf, "OperationalLimit.OperationalLimitSet", pILimit);
            RefNode(FunPrf, "OperationalLimit.OperationalLimitType", pEmergLimit);
            DoubleNode(FunPrf, "CurrentLimit.value", pILimit->EmergAmps);
            EndInstance(FunPrf, "CurrentLimit");
        }

        delete pName1;
        delete pName2;
        delete pCRS;
        delete pRegion;
        delete pSubRegion;
        delete pLocation;
        delete pSubstation;
        delete pSwing;
        delete pIsland;
        delete pNormLimit;
        delete pEmergLimit;
        delete pRangeALoLimit;
        delete pRangeAHiLimit;
        delete pRangeBLoLimit;
        delete pRangeBHiLimit;

        // FreeUuidList;  // this is deferred for UUID export
        FreeBankList();
        FreeECPList();
        FreeOpLimitList();

        GlobalResult = Filenm;
    }
    catch (...) {
        delete FD;
        throw;
    }

    delete FD;
}
