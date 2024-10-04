#ifndef DSSCALLBACKSTRUCTDEFH
#define DSSCALLBACKSTRUCTDEFH


#include "System.h"
#include "d2c_structures.h"

struct TDSSCallBacks {
    void (__stdcall *MsgCallBack)(pUTF8Char S, unsignedint maxlen); /*Make use of DSS Message handling*/

          /*Routines for using DSS Parser.  This allows you to write models that accept
           syntax like other DSS scripts.*/
    void (__stdcall *GetIntValue)(int& i); /*Get next param as an integer*/
    void (__stdcall *GetDblValue)(double& x);  /*Get next param as a double*/
    void (__stdcall *GetStrValue)(pUTF8Char S, unsignedint maxlen);
    /*Get next param as a string <= maxlen characters  (Cardinal = 32-bit unsigned)*/
    /*caller must allocate space for s (Maxlen chars)*/
    void (__stdcall *LoadParser)(pUTF8Char S, unsignedint maxlen); // Copies a string into a special instance of the DSS parser
    int (__stdcall *NextParam)(pUTF8Char ParamName, unsignedint maxlen);
    /*Advance to the next parameter and
     Get name of the param just retrieved, if one was given.
     Returns length of parameter found.  If 0, then end of string.
     This is to handle the syntax "paramname=paramvalue" commonly used in DSS scripts
     Copies the string to the location specified by s up to maxlen characters.
     Caller must allocate space (Maxlen chars)*/
    void (__stdcall *DoDSSCommand)(pUTF8Char S, unsignedint maxlen);
    void (__stdcall *GetActiveElementBusNames)(pUTF8Char Name1, unsignedint Len1, pUTF8Char Name2, unsignedint Len2);
    void (__stdcall *GetActiveElementVoltages)(int& NumVoltages, pComplexArray V);
    void (__stdcall *GetActiveElementCurrents)(int& NumCurrents, pComplexArray Curr, int ActorID);
    void (__stdcall *GetActiveElementLosses)(complex& TotalLosses, complex& LoadLosses, complex& NoLoadLosses, int ActorID);
    void (__stdcall *GetActiveElementPower)(int Terminal, complex& TotalPower);
    void (__stdcall *GetActiveElementNumCust)(int& Numcust, int& TotalCust);
    void (__stdcall *GetActiveElementNodeRef)(int Maxsize, pIntegerArray NodeReferenceArray);// calling program must allocate
    int (__stdcall *GetActiveElementBusRef)(int Terminal);
    void (__stdcall *GetActiveElementTerminalInfo)(int& NumTerminals, int& NumConds, int& NumPhases);
    void (__stdcall *GetPtrToSystemVarray)(void*& V, int& iNumNodes); // Returns pointer to Solution.V and size
    int (__stdcall *GetActiveElementIndex)();
    bool (__stdcall *IsActiveElementEnabled)();
    bool (__stdcall *IsBusCoordinateDefined)(int BusRef, int ActorID);
    void (__stdcall *GetBusCoordinate)(int BusRef, double& x, double& y, int ActorID);
    double (__stdcall *GetBuskVBase)(int BusRef, int ActorID);
    double (__stdcall *GetBusDistFromMeter)(int BusRef, int ActorID);
    void (__stdcall *GetDynamicsStruct)(void*& pDynamicsStruct, int ActorID);  // Returns pointer to dynamics variables structure
    double (__stdcall *GetStepSize)(int ActorID);  // Return just 'h' from dynamics record
    double (__stdcall *GetTimeSec)(int ActorID); // returns t in sec from top of hour
    double (__stdcall *GetTimeHr)(int ActorID); // returns time as a double in hours
    void (__stdcall *GetPublicDataPtr)(void*& pPublicData, int& PublicDataBytes, int ActorID);
    int (__stdcall *GetActiveElementName)(pUTF8Char FullName, unsignedint MaxNameLen, int ActorID);
    void* (__stdcall *GetActiveElementPtr)(int ActorID);  // Returns pointer to active circuit element
    int (__stdcall *ControlQueuePush)(const int Hour, const double Sec, const int Code, const int ProxyHdl, void* Owner, int ActorID);
    void (__stdcall *GetResultStr)(pUTF8Char S, unsignedint maxlen);
};

/*NOTE: Maxlen argument is to better accommodate Fortran strings.  VB also*/
/*      Caller must allocate space for pchar values       */
typedef TDSSCallBacks* pDSSCallBacks;  /*Pointer to callback structure*/

#endif // DSSCALLBACKSTRUCTDEFH

