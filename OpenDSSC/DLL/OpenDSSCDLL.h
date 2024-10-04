// OpenDSSX.h : Include file for standard system include files,
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
	DSS_DLL int __stdcall DSSI(int mode, int arg);
	DSS_DLL char* __stdcall DSSS(int mode, char* arg);
	DSS_DLL void __stdcall DSSV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall LinesI(int mode, int arg);
	DSS_DLL double __stdcall LinesF(int mode, double arg);
	DSS_DLL char* __stdcall LinesS(int mode, char* arg);
	DSS_DLL void __stdcall LinesV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL char* __stdcall DSSPut_Command(char* myCmd);
	DSS_DLL int __stdcall DSSLoads(int mode, int arg);
	DSS_DLL	double __stdcall DSSLoadsF(int mode, double arg);
	DSS_DLL char* __stdcall DSSLoadsS(int mode, char* arg);
	DSS_DLL void __stdcall DSSLoadsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall CapacitorsI(int mode, int arg);
	DSS_DLL double __stdcall CapacitorsF(int mode, double arg);
	DSS_DLL const char* __stdcall CapacitorsS(int mode, const char* arg);
	DSS_DLL void __stdcall CapacitorsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall ActiveClassI(int mode, int arg);
	DSS_DLL char* __stdcall ActiveClassS(int mode, char* arg);
	DSS_DLL void __stdcall ActiveClassV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall BUSI(int mode, int arg);
	DSS_DLL double __stdcall BUSF(int mode, double arg);
	DSS_DLL char* __stdcall BUSS(int mode, char* arg);
	DSS_DLL void __stdcall BUSV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall CapControlsI(int mode, int arg);
	DSS_DLL double __stdcall CapControlsF(int mode, double arg);
	DSS_DLL char* __stdcall CapControlsS(int mode, char* arg);
	DSS_DLL void __stdcall CapControlsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall CircuitI(int mode, int arg);
	DSS_DLL double __stdcall CircuitF(int mode, double arg1, double arg2);
	DSS_DLL char* __stdcall CircuitS(int mode, char* arg);
	DSS_DLL void __stdcall CircuitV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall CktElementI(int mode, int arg);
	DSS_DLL double __stdcall CktElementF(int mode, double arg);
	DSS_DLL char* __stdcall CktElementS(int mode, char* arg);
	DSS_DLL void __stdcall CktElementV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL double __stdcall CmathLibF(int mode, double arg1, double arg2);
	DSS_DLL void __stdcall CmathLibV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall GeneratorsI(int mode, int arg);
	DSS_DLL double __stdcall GeneratorsF(int mode, double arg);
	DSS_DLL char* __stdcall GeneratorsS(int mode, char* arg);
	DSS_DLL void __stdcall GeneratorsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall DSSElementI(int mode, int arg);
	DSS_DLL char* __stdcall DSSElementS(int mode, char* arg);
	DSS_DLL void __stdcall DSSElementV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL	int __stdcall DSSProgressI(int mode, int arg);
	DSS_DLL char* __stdcall DSSProgressS(int mode, char* arg);

	//**************************************************************************************************

	DSS_DLL int __stdcall DSSExecutiveI(int mode, int arg);
	DSS_DLL char* __stdcall DSSExecutiveS(int mode, char* arg);

	//**************************************************************************************************

	DSS_DLL int __cdecl ErrorCode();
	DSS_DLL char* __cdecl ErrorDesc();

	//**************************************************************************************************

	DSS_DLL int __stdcall FusesI(int mode, int arg);
	DSS_DLL double __stdcall FusesF(int mode, double arg);
	DSS_DLL char* __stdcall FusesS(int mode, char* arg);
	DSS_DLL void __stdcall FusesV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall GICSourcesI(int mode, int arg);
	DSS_DLL double __stdcall GICSourcesF(int mode, double arg);
	DSS_DLL char* __stdcall GICSourcesS(int mode, char* arg);
	DSS_DLL void __stdcall GICSourcesV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall IsourceI(int mode, int arg);
	DSS_DLL double __stdcall IsourceF(int mode, double arg);
	DSS_DLL char* __stdcall IsourceS(int mode, char* arg);
	DSS_DLL void __stdcall IsourceV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall LineCodesI(int mode, int arg);
	DSS_DLL double __stdcall LineCodesF(int mode, double arg);
	DSS_DLL char* __stdcall LineCodesS(int mode, char* arg);
	DSS_DLL void __stdcall LineCodesV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall LoadShapeI(int mode, int arg);
	DSS_DLL double __stdcall LoadShapeF(int mode, double arg);
	DSS_DLL char* __stdcall LoadShapeS(int mode, char* arg);
	DSS_DLL void __stdcall LoadShapeV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall MetersI(int mode, int arg);
	DSS_DLL double __stdcall MetersF(int mode, double arg);
	DSS_DLL char* __stdcall MetersS(int mode, char* arg);
	DSS_DLL void __stdcall MetersV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall MonitorsI(int mode, int arg);
	DSS_DLL char* __stdcall MonitorsS(int mode, char* arg);
	DSS_DLL void __stdcall MonitorsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall ParallelI(int mode, int arg);
	DSS_DLL void __stdcall ParallelV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall ParserI(int mode, int arg);
	DSS_DLL double __stdcall ParserF(int mode, double arg);
	DSS_DLL char* __stdcall ParserS(int mode, char* arg);
	DSS_DLL void __stdcall ParserV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall PDElementsI(int mode, int arg);
	DSS_DLL double __stdcall PDElementsF(int mode, double arg);
	DSS_DLL char* __stdcall PDElementsS(int mode, char* arg);

	//**************************************************************************************************

	DSS_DLL int __stdcall PVsystemsI(int mode, int arg);
	DSS_DLL double __stdcall PVsystemsF(int mode, double arg);
	DSS_DLL char* __stdcall PVsystemsS(int mode, char* arg);
	DSS_DLL void __stdcall PVsystemsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall ReactorsI(int mode, int arg);
    DSS_DLL double __stdcall ReactorsF(int mode, double arg);
    DSS_DLL char* __stdcall ReactorsS(int mode, char* arg);
    DSS_DLL void __stdcall ReactorsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

    //**************************************************************************************************

	DSS_DLL int __stdcall ReclosersI(int mode, int arg);
	DSS_DLL double __stdcall ReclosersF(int mode, double arg);
	DSS_DLL char* __stdcall ReclosersS(int mode, char* arg);
	DSS_DLL void __stdcall ReclosersV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall ReduceCktI(int mode, int arg);
	DSS_DLL double __stdcall ReduceCktF(int mode, double arg);
	DSS_DLL char* __stdcall ReduceCktS(int mode, char* arg);

	//**************************************************************************************************

	DSS_DLL int __stdcall RegControlsI(int mode, int arg);
	DSS_DLL double __stdcall RegControlsF(int mode, double arg);
	DSS_DLL char* __stdcall RegControlsS(int mode, char* arg);
	DSS_DLL void __stdcall RegControlsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall RelaysI(int mode, int arg);
	DSS_DLL char* __stdcall RelaysS(int mode, char* arg);
	DSS_DLL void __stdcall RelaysV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall SensorsI(int mode, int arg);
	DSS_DLL double __stdcall SensorsF(int mode, double arg);
	DSS_DLL char* __stdcall SensorsS(int mode, char* arg);
	DSS_DLL void __stdcall SensorsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall SettingsI(int mode, int arg);
	DSS_DLL double __stdcall SettingsF(int mode, double arg);
	DSS_DLL char* __stdcall SettingsS(int mode, char* arg);
	DSS_DLL void __stdcall SettingsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall SolutionI(int mode, int arg);
	DSS_DLL double __stdcall SolutionF(int mode, double arg);
	DSS_DLL char* __stdcall SolutionS(int mode, char* arg);
	DSS_DLL void __stdcall SolutionV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

    DSS_DLL int __stdcall StoragesI(int mode, int arg);
    DSS_DLL double __stdcall StoragesF(int mode, double arg);
    DSS_DLL char* __stdcall StoragesS(int mode, char* arg);
    DSS_DLL void __stdcall StoragesV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall SwtControlsI(int mode, int arg);
	DSS_DLL double __stdcall SwtControlsF(int mode, double arg);
	DSS_DLL char* __stdcall SwtControlsS(int mode, char* arg);
	DSS_DLL void __stdcall SwtControlsV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall TopologyI(int mode, int arg);
	DSS_DLL char* __stdcall TopologyS(int mode, char* arg);
	DSS_DLL void __stdcall TopologyV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall TransformersI(int mode, int arg);
	DSS_DLL double __stdcall TransformersF(int mode, double arg);
	DSS_DLL char* __stdcall TransformersS(int mode, char* arg);
	DSS_DLL void __stdcall TransformersV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall VsourcesI(int mode, int arg);
	DSS_DLL double __stdcall VsourcesF(int mode, double arg);
	DSS_DLL char* __stdcall VsourcesS(int mode, char* arg);
	DSS_DLL void __stdcall VsourcesV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

    DSS_DLL int __stdcall WindGensI(int mode, int arg);
    DSS_DLL double __stdcall WindGensF(int mode, double arg);
    DSS_DLL char* __stdcall WindGensS(int mode, char* arg);
    DSS_DLL void __stdcall WindGensV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall XYCurvesI(int mode, int arg);
	DSS_DLL double __stdcall XYCurvesF(int mode, double arg);
	DSS_DLL char* __stdcall XYCurvesS(int mode, char* arg);
	DSS_DLL void __stdcall XYCurvesV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL int __stdcall CtrlQueueI(int mode, int arg);
	DSS_DLL void __stdcall CtrlQueueV(int mode, uintptr_t* myPtr, int* myType, int* mySize);

	//**************************************************************************************************

	DSS_DLL char* __stdcall DSSProperties(int mode, char* arg);

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

#ifdef __cplusplus
}
#endif
