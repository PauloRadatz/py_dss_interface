#ifndef DynamicExpH
#define DynamicExpH
/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/


#include "System.h"

/*The dynamics experssion object implements differential equations
and state variables that can be called by other objects to represent
their dynamic behavior when operating in dynamics simulation mode.

Last update by Davis Montenegro 04/13/2022
*/
#include "Sysutils.h"
#include "Command.h"
#include "DSSClass.h"
#include "DSSObject.h"
#include "Ucmatrix.h"
#include "Arraydef.h"
#include "d2c_structures.h"

namespace DynamicExp
{

class TDynamicExp;
class TDynamicExpObj;




class TDynamicExp : public TDSSClass {
    typedef TDSSClass inherited;
    friend class TDynamicExpObj;
private:
    bool SymComponentsChanged;
    bool MatrixChanged;
    String Get_Code();  // Returns active line code string
    void Set_Code(const String Value);  // sets the  active linecode
protected:
    void DefineProperties();
    virtual int MakeLike(const String LineName);
public:
    TDynamicExp();
    virtual ~TDynamicExp();
    virtual void* Find(const String ObjName);
    virtual int Edit(int ActorID)     // uses global parser
        ;
    virtual int Init(int Handle, int ActorID);
    virtual int NewObject(const String ObjName)

        // Set this property to point ActiveLineCodeObj to the right value
        ;
    //__declspec ( property ( get = Get_Code, put = Set_Code ) ) String Code;
};


class TDynamicExpObj : public TDSSObject {
    typedef TDSSObject inherited;
    friend class TDynamicExp;
private:
    int FVarIdx, FNumvars;                                                  // Number of state variables
    TStringList FvarNames;                                                  // List containing the state variable names
    vector < double > FVarConst;                                            // Array containing the numeric constants of the expression
    vector < int > FCmd;                                                    // Sequence of commands that implement the expression
    // Name of the active variable
    String FActiveVar, FXpression;                                          // Differential equiation in RPN format
public:
    double BaseFrequency;
    int myDomain;
    TDynamicExpObj(DSSClass::TDSSClass* ParClass, const String LineCodeName);
    virtual ~TDynamicExpObj();
    bool InterpretDiffEq(String Exp);
    virtual String GetPropertyValue(int Index);
    virtual void InitPropertyValues(int ArrayOffset);
    virtual void DumpProperties(TTextRec& F, bool Complete);
    int Get_Closer_Op(String myExp, String& myOp, int& OpCode);
    int Get_Var_Idx(String myVar);
    bool Check_If_CalcValue(String myVal, int& myOp);
    int Get_Out_Idx(String myVar);                               // gets the index for the given variable if it is an output
    void SolveEq(vector <DynSlot>* MemSpace);        // Equation solver
    bool IsInitVal(int myCode);                                  // returns true if the given code is for an initialization value
    double Get_DynamicEqVal(int myIdx, std::vector <DynSlot>* MemSpace);
    String Get_VarName(int myIdx);
    int get_FNumVars();
    void set_FNumvars(int value);
    //__declspec ( property ( get = get_FNumVars, put = set_FNumvars) ) int NumVars;

    TDynamicExpObj(DSSClass::TDSSClass* ParClass);
    TDynamicExpObj(String ClassName);
    TDynamicExpObj();
};


extern TDynamicExp* DynamicExpClass;
extern TDynamicExpObj* ActiveDynamicExpObj;

}  // namespace DynamicExp

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace DynamicExp;
#endif

#endif //  DynamicExpH