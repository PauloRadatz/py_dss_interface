


#include "DSSCallBackStructDef.h"

#pragma pack(push, 1)

void (*MsgCallBack)(pUTF8Char S, unsignedint maxlen); /*Make use of DSS Message handling*/

      /*Routines for using DSS Parser.  This allows you to write models that accept
       syntax like other DSS scripts.*/
void (*GetIntValue)(int& i); /*Get next param as an integer*/
void (*GetDblValue)(double& x);  /*Get next param as a double*/
void (*GetStrValue)(pUTF8Char S, unsignedint maxlen);
/*Get next param as a string <= maxlen characters  (Cardinal = 32-bit unsigned)*/
/*caller must allocate space for s (Maxlen chars)*/
void (*LoadParser)(pUTF8Char S, unsignedint maxlen); // Copies a string into a special instance of the DSS parser
int (*NextParam)(pUTF8Char ParamName, unsignedint maxlen);
/*Advance to the next parameter and
 Get name of the param just retrieved, if one was given.
 Returns length of parameter found.  If 0, then end of string.
 This is to handle the syntax "paramname=paramvalue" commonly used in DSS scripts
 Copies the string to the location specified by s up to maxlen characters.
 Caller must allocate space (Maxlen chars)*/
void (*DoDSSCommand)(pUTF8Char S, unsignedint maxlen);
void (*GetActiveElementBusNames)(pUTF8Char Name1, unsignedint Len1, pUTF8Char Name2, unsignedint Len2);
void (*GetActiveElementVoltages)(int& NumVoltages, pComplexArray V);
void (*GetActiveElementCurrents)(int& NumCurrents, pComplexArray Curr, int ActorID);
void (*GetActiveElementLosses)(complex& TotalLosses, complex& LoadLosses, complex& NoLoadLosses, int ActorID);
void (*GetActiveElementPower)(int Terminal, complex& TotalPower);
void (*GetActiveElementNumCust)(int& Numcust, int& TotalCust);
void (*GetActiveElementNodeRef)(int Maxsize, pIntegerArray NodeReferenceArray);// calling program must allocate
int (*GetActiveElementBusRef)(int Terminal);
void (*GetActiveElementTerminalInfo)(int& NumTerminals, int& NumConds, int& NumPhases);
void (*GetPtrToSystemVarray)(void*& V, int& iNumNodes); // Returns pointer to Solution.V and size
int (*GetActiveElementIndex)();
bool (*IsActiveElementEnabled)();
bool (*IsBusCoordinateDefined)(int BusRef, int ActorID);
void (*GetBusCoordinate)(int BusRef, double& x, double& y, int ActorID);
double (*GetBuskVBase)(int BusRef, int ActorID);
double (*GetBusDistFromMeter)(int BusRef, int ActorID);
void (*GetDynamicsStruct)(void*& pDynamicsStruct, int ActorID);  // Returns pointer to dynamics variables structure
double (*GetStepSize)(int ActorID);  // Return just 'h' from dynamics record
double (*GetTimeSec)(int ActorID); // returns t in sec from top of hour
double (*GetTimeHr)(int ActorID); // returns time as a double in hours
void (*GetPublicDataPtr)(void*& pPublicData, int& PublicDataBytes, int ActorID);
int (*GetActiveElementName)(pUTF8Char FullName, unsignedint MaxNameLen, int ActorID);
void* (*GetActiveElementPtr)(int ActorID);  // Returns pointer to active circuit element
int (*ControlQueuePush)(const int Hour, const double Sec, const int Code, const int ProxyHdl, void* Owner, int ActorID);
void (*GetResultStr)(pUTF8Char S, unsignedint maxlen);








