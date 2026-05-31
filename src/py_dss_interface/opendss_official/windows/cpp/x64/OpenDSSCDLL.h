// OpenDSSC.h : Include file for standard system include files,
// or project specific include files.


#include <stdint.h>
#include <iostream>
#include "ControlElem.h"

// TODO: Reference additional headers your program requires here.
using namespace std;
#ifdef WIN32
#define DSS_DLL __declspec(dllexport)
#else
#define DSS_DLL
#endif

struct TAction
{
	int ActionCode,
		DeviceHandle;
};

typedef TAction* pAction;
	

class TCOMControlProxyObj : public TControlElem 
{
public:
	TList ActionList;
	void ClearActionList();
	bool PopAction();

	TCOMControlProxyObj(DSSClass::TDSSClass* ParClass, const String COMProxyName);
	void DopendingAction(const int Code, int ProxyHdl, int ActorID);
	void Reset(int ActorID);
};

#ifdef __cplusplus
extern "C" {
#endif
	// DSS interface
	DSS_DLL int DSSI(int mode, int arg);
	DSS_DLL char* DSSS(int mode, char* arg);
	DSS_DLL void DSSV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int LinesI(int mode, int arg);
	DSS_DLL double LinesF(int mode, double arg);
	DSS_DLL char* LinesS(int mode, char* arg);
	DSS_DLL void LinesV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL char* DSSPut_Command(char* myCmd);
	DSS_DLL int DSSLoads(int mode, int arg);
	DSS_DLL	double DSSLoadsF(int mode, double arg);
	DSS_DLL char* DSSLoadsS(int mode, char* arg);
	DSS_DLL void DSSLoadsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int CapacitorsI(int mode, int arg);
	DSS_DLL double CapacitorsF(int mode, double arg);
	DSS_DLL const char* CapacitorsS(int mode, const char* arg);
	DSS_DLL void CapacitorsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int ActiveClassI(int mode, int arg);
	DSS_DLL char* ActiveClassS(int mode, char* arg);
	DSS_DLL void ActiveClassV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int BUSI(int mode, int arg);
	DSS_DLL double BUSF(int mode, double arg);
	DSS_DLL char* BUSS(int mode, char* arg);
	DSS_DLL void BUSV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int CapControlsI(int mode, int arg);
	DSS_DLL double CapControlsF(int mode, double arg);
	DSS_DLL char* CapControlsS(int mode, char* arg);
	DSS_DLL void CapControlsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int CircuitI(int mode, int arg);
	DSS_DLL double CircuitF(int mode, double arg1, double arg2);
	DSS_DLL char* CircuitS(int mode, char* arg);
	DSS_DLL void CircuitV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int CktElementI(int mode, int arg);
	DSS_DLL double CktElementF(int mode, double arg);
	DSS_DLL char* CktElementS(int mode, char* arg);
	DSS_DLL void CktElementV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL double CmathLibF(int mode, double arg1, double arg2);
	DSS_DLL void CmathLibV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int GeneratorsI(int mode, int arg);
	DSS_DLL double GeneratorsF(int mode, double arg);
	DSS_DLL char* GeneratorsS(int mode, char* arg);
	DSS_DLL void GeneratorsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int DSSElementI(int mode, int arg);
	DSS_DLL char* DSSElementS(int mode, char* arg);
	DSS_DLL void DSSElementV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL	int DSSProgressI(int mode, int arg);
	DSS_DLL char* DSSProgressS(int mode, char* arg);

	//**************************************************************************************************

	DSS_DLL int DSSExecutiveI(int mode, int arg);
	DSS_DLL char* DSSExecutiveS(int mode, char* arg);

	//**************************************************************************************************

	DSS_DLL int __cdecl ErrorCode();
	DSS_DLL char* __cdecl ErrorDesc();

	//**************************************************************************************************

	DSS_DLL int FusesI(int mode, int arg);
	DSS_DLL double FusesF(int mode, double arg);
	DSS_DLL char* FusesS(int mode, char* arg);
	DSS_DLL void FusesV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int GICSourcesI(int mode, int arg);
	DSS_DLL double GICSourcesF(int mode, double arg);
	DSS_DLL char* GICSourcesS(int mode, char* arg);
	DSS_DLL void GICSourcesV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int IsourceI(int mode, int arg);
	DSS_DLL double IsourceF(int mode, double arg);
	DSS_DLL char* IsourceS(int mode, char* arg);
	DSS_DLL void IsourceV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int LineCodesI(int mode, int arg);
	DSS_DLL double LineCodesF(int mode, double arg);
	DSS_DLL char* LineCodesS(int mode, char* arg);
	DSS_DLL void LineCodesV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int LoadShapeI(int mode, int arg);
	DSS_DLL double LoadShapeF(int mode, double arg);
	DSS_DLL char* LoadShapeS(int mode, char* arg);
	DSS_DLL void LoadShapeV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int MetersI(int mode, int arg);
	DSS_DLL double MetersF(int mode, double arg);
	DSS_DLL char* MetersS(int mode, char* arg);
	DSS_DLL void MetersV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int MonitorsI(int mode, int arg);
	DSS_DLL char* MonitorsS(int mode, char* arg);
	DSS_DLL void MonitorsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int ParallelI(int mode, int arg);
	DSS_DLL void ParallelV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int ParserI(int mode, int arg);
	DSS_DLL double ParserF(int mode, double arg);
	DSS_DLL char* ParserS(int mode, char* arg);
	DSS_DLL void ParserV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int PDElementsI(int mode, int arg);
	DSS_DLL double PDElementsF(int mode, double arg);
	DSS_DLL char* PDElementsS(int mode, char* arg);

	//**************************************************************************************************

	DSS_DLL int PVsystemsI(int mode, int arg);
	DSS_DLL double PVsystemsF(int mode, double arg);
	DSS_DLL char* PVsystemsS(int mode, char* arg);
	DSS_DLL void PVsystemsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int ReactorsI(int mode, int arg);
    DSS_DLL double ReactorsF(int mode, double arg);
    DSS_DLL char* ReactorsS(int mode, char* arg);
    DSS_DLL void ReactorsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

    //**************************************************************************************************

	DSS_DLL int ReclosersI(int mode, int arg);
	DSS_DLL double ReclosersF(int mode, double arg);
	DSS_DLL char* ReclosersS(int mode, char* arg);
	DSS_DLL void ReclosersV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int ReduceCktI(int mode, int arg);
	DSS_DLL double ReduceCktF(int mode, double arg);
	DSS_DLL char* ReduceCktS(int mode, char* arg);

	//**************************************************************************************************

	DSS_DLL int RegControlsI(int mode, int arg);
	DSS_DLL double RegControlsF(int mode, double arg);
	DSS_DLL char* RegControlsS(int mode, char* arg);
	DSS_DLL void RegControlsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int RelaysI(int mode, int arg);
	DSS_DLL char* RelaysS(int mode, char* arg);
	DSS_DLL void RelaysV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int SensorsI(int mode, int arg);
	DSS_DLL double SensorsF(int mode, double arg);
	DSS_DLL char* SensorsS(int mode, char* arg);
	DSS_DLL void SensorsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int SettingsI(int mode, int arg);
	DSS_DLL double SettingsF(int mode, double arg);
	DSS_DLL char* SettingsS(int mode, char* arg);
	DSS_DLL void SettingsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int SolutionI(int mode, int arg);
	DSS_DLL double SolutionF(int mode, double arg);
	DSS_DLL char* SolutionS(int mode, char* arg);
	DSS_DLL void SolutionV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

    DSS_DLL int StoragesI(int mode, int arg);
    DSS_DLL double StoragesF(int mode, double arg);
    DSS_DLL char* StoragesS(int mode, char* arg);
    DSS_DLL void StoragesV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int SwtControlsI(int mode, int arg);
	DSS_DLL double SwtControlsF(int mode, double arg);
	DSS_DLL char* SwtControlsS(int mode, char* arg);
	DSS_DLL void SwtControlsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int TopologyI(int mode, int arg);
	DSS_DLL char* TopologyS(int mode, char* arg);
	DSS_DLL void TopologyV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int TransformersI(int mode, int arg);
	DSS_DLL double TransformersF(int mode, double arg);
	DSS_DLL char* TransformersS(int mode, char* arg);
	DSS_DLL void TransformersV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int VsourcesI(int mode, int arg);
	DSS_DLL double VsourcesF(int mode, double arg);
	DSS_DLL char* VsourcesS(int mode, char* arg);
	DSS_DLL void VsourcesV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

    DSS_DLL int WindGensI(int mode, int arg);
    DSS_DLL double WindGensF(int mode, double arg);
    DSS_DLL char* WindGensS(int mode, char* arg);
    DSS_DLL void WindGensV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int XYCurvesI(int mode, int arg);
	DSS_DLL double XYCurvesF(int mode, double arg);
	DSS_DLL char* XYCurvesS(int mode, char* arg);
	DSS_DLL void XYCurvesV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int CtrlQueueI(int mode, int arg);
	DSS_DLL void CtrlQueueV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL void ExportV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL char* DSSProperties(int mode, char* arg);

	//**************************************************************************************************

	DSS_DLL int InitAndGetYparams(uintptr_t* hY, unsignedint* nBus, unsignedint* nNZ);
	DSS_DLL void GetCompressedYMatrix(uintptr_t hY, unsignedint nBus, unsignedint nNz, int** ColPtr, int** RowIdx, complex** cVals);
	DSS_DLL int SystemYChanged(int mode, int arg);
	DSS_DLL int UseAuxCurrents(int mode, int arg);
	DSS_DLL void AddInAuxCurrents(int SType);
	DSS_DLL void BuildYMatrixD(int BuildOps, int AllocateVI);
	DSS_DLL void GetPCInjCurr(void);
	DSS_DLL void GetSourceInjCurrents(void);
	DSS_DLL void ZeroInjCurr(void);
	DSS_DLL int SolveSystem(complex** NodeV);
	DSS_DLL void getIpointer(complex** IvectorPtr);
	DSS_DLL void getVpointer(complex** VvectorPtr);

	/// DSSDisposeString must be called to dispose the memory used by any string returned by the API,
	/// except for string arrays from the `*V()` variant functions, which use a global buffer.
	DSS_DLL void DSSDisposeString(char* value);

#ifdef __cplusplus
}
#endif
