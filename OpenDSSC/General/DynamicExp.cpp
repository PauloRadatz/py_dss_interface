
#pragma hdrstop

#include "DynamicExp.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include <Sysutils.h>
#include "Ucomplex.h"
#include "Utilities.h"
#include "LineUnits.h"
#include <math.h>
#include "RPN.h"

#include <System.h>

using namespace std;
using namespace Bus;
using namespace CapControlVars;
using namespace CapUserControl;
using namespace Capacitor;
using namespace Circuit;
using namespace CktElement;
using namespace Command;
using namespace ControlClass;
using namespace ControlElem;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace ParserDel;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace DynamicExp
{

TDynamicExpObj::TDynamicExpObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TDynamicExpObj::TDynamicExpObj(String ClassName) : inherited(ClassName) {}
TDynamicExpObj::TDynamicExpObj() {}

TDynamicExp* DynamicExpClass = NULL;
TDynamicExpObj* ActiveDynamicExpObj = NULL;



const int NumPropsThisClass = 6;
const string myOps[29] = { "dt", "=", "+", "-", "*", "/", "(", ")", ";", "[", "]", "sqr", "sqrt", "inv", "ln", "exp", "log10", "sin",
                            "cos", "tan", "asin", "acos", "atan", "atan2", "rollup", "rolldn", "swap", "pi", "^"};

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



TDynamicExp::TDynamicExp()  // Creates superstructure for all DynamicExp objects
{
    Class_Name = "DynamicExp";
    DSSClassType = DSS_OBJECT;
    ActiveElement = 0;
    DefineProperties();
    std::string* slc = Slice((PropertyName), NumProperties);
    CommandList = TCommandList(slc, NumProperties);
    delete[] slc;
    CommandList.set_AbbrevAllowed(true);
    DynamicExpClass         = this;
    SymComponentsChanged    = false;
    MatrixChanged           = false;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



TDynamicExp::~TDynamicExp()
{
    // ElementList and  CommandList freed in inherited destroy
// todo check:  inherited::Destroy;
}
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



void TDynamicExp::DefineProperties()
{
    NumProperties = NumPropsThisClass;
    CountProperties();   // Get inherited property count
    AllocatePropertyArrays();
    PropertyName[0] = "nvariables";
    PropertyName[1] = "varnames";
    PropertyName[2] = "var";
    PropertyName[3] = "varidx";
    PropertyName[4] = "expression";
    PropertyName[5] = "Domain";
    PropertyHelp[0] = "(Int) Number of state variables to be considered in the differential equation.";
    PropertyHelp[1] = "([String]) Array of strings with the names of the state variables.";
    PropertyHelp[2] = "(String) Activates the state variable using the given name.";
    PropertyHelp[3] = "(Int) read-only, returns the index of the active state variable.";
    PropertyHelp[4] = "It is the differential expression using OpenDSS RPN syntax. The expression must be contained within brackets in case of having multiple equations, for example:" + CRLF + CRLF + "expression = \"[w dt = 1 M / (P_m D*w - P_e -) *]\"";
    PropertyHelp[5] = "It is the domain for which the equation is defined, it can be one of [time*, dq]. By deafult, dynamic epxressions are defined in the time domain.";
    ActiveProperty = NumPropsThisClass - 1;
    inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



int TDynamicExp::NewObject(const String ObjName)
{
    int result = 0;
    // create a new object of this class and add to list
 /*# with ActiveCircuit[ActiveActor] do */
    {
        //auto with0 = ActiveCircuit[ActiveActor];
        {
            ActiveDSSObject[ActiveActor] = new TDynamicExpObj(this, ObjName);
            result = AddObjectToList(ActiveDSSObject[ActiveActor]);
        }
    }
    return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



void* TDynamicExp::Find(const String ObjName)
{
    void* result = NULL;
    if ((ObjName.size() == 0) || (CompareText(ObjName, "none") == 0))
        result = NULL;
    else
        result = inherited::Find(ObjName);
    return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



int TDynamicExp::Edit(int ActorID)
{
    int result = 0;
    int idx = 0, ParamPointer = 0;
    String ParamName, Param;
    result = 0;
    // continue parsing with contents of Parser
    ActiveDynamicExpObj = (TDynamicExpObj*) ElementList.Get_Active();
    ActiveDSSObject[ActorID] = ActiveDynamicExpObj;
    SymComponentsChanged = false;
    MatrixChanged = false;
    /*# with ActiveDynamicExpObj do */
    auto with0 = ActiveDynamicExpObj;
    {
        ParamPointer = 0;
        ParamName = Parser[ActorID]->GetNextParam();
        Param = Parser[ActorID]->MakeString_();
        while (Param.size() > 0)
        {
            if (ParamName.size() == 0)
                ParamPointer++;
            else
                ParamPointer = CommandList.Getcommand(ParamName);
            if ((ParamPointer > 0) && (ParamPointer <= NumProperties))
                with0->Set_PropertyValue(ParamPointer,Param);
            switch (ParamPointer)
            {
            case 0:
                DoSimpleMsg(String("Unknown parameter \"") + ParamName + "\" for Object \"" + Class_Name + "." + with0->get_Name() + "\"", 50001);
                break;
            case 1:
            {
                ActiveDynamicExpObj->FNumvars = Parser[ActorID]->MakeInteger_();  // Use property value to force reallocations
            }
            break;
            case 2:
            {
                InterpretTStringListArray(Param, ActiveDynamicExpObj->FvarNames);
                // ensuring they are lower case
                for (int stop = (ActiveDynamicExpObj->FvarNames.size() - 1), idx = 0; idx <= stop; idx++)
                    ActiveDynamicExpObj->FvarNames[idx] = LowerCase(ActiveDynamicExpObj->FvarNames[idx]);
            }
            break;
            case 3:
            {
                with0->FActiveVar = LowerCase(Parser[ActorID]->MakeString_());
                with0->FVarIdx = FindInStrVector((&with0->FvarNames), with0->FActiveVar);
                if (ActiveDynamicExpObj->FVarIdx < 0)
                {
                    // Being here means that the given name doesn't exist
                    DoSimpleMsg(String("DynamicExp \"") + ActiveDynamicExpObj->FActiveVar + "\" not found.", 50001);
                    ActiveDynamicExpObj->FActiveVar = "";
                }
            }
            break;
            case 5:
            {
                if (ActiveDynamicExpObj->InterpretDiffEq(Parser[ActorID]->MakeString_()))
                    DoSimpleMsg("There are errors in the differential equation.", 50003);
            }
            break;
            case 6:
            {
                if (Parser[ActorID]->MakeString_() == "time")
                    ActiveDynamicExpObj->myDomain = 0;
                else
                    ActiveDynamicExpObj->myDomain = 1;
            }
            break;
            default:
                ClassEdit(ActiveDynamicExpObj, ParamPointer - NumPropsThisClass);
            }
            /*
                       CASE ParamPointer OF
                           1 : ;
                       END;

            */
            ParamName = Parser[ActorID]->GetNextParam();
            Param = Parser[ActorID]->MakeString_();
        }
    }
    return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



int TDynamicExp::MakeLike(const String LineName)
{
    int result = 0;
    TDynamicExpObj* OtherDynExpCode = NULL;
    int i = 0;
    result = 0;
    /*See if we can find this DynamicExp code in the present collection*/
    return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



int TDynamicExp::Init(int Handle, int ActorID)
{
    int result = 0;
    DoSimpleMsg("Need to implement TDynamicExp.Init", -1);
    result = 0;
    return result;
}


String TDynamicExp::Get_Code()  // Returns active line code string

{
    String result;
    result = "";
    return result;
}


void TDynamicExp::Set_Code(const String Value)  // sets the  active linecode

{
    TDynamicExpObj* DynExpCodeObj = NULL;
    ActiveDynamicExpObj = NULL;
    DynExpCodeObj = (TDynamicExpObj*) ElementList.Get_First();
    while (DynExpCodeObj != NULL)
    {
        if (CompareText(DynExpCodeObj->get_Name(), Value) == 0)
        {
            ActiveDynamicExpObj = DynExpCodeObj;
            return;
        }
        DynExpCodeObj = (TDynamicExpObj*) ElementList.Get_Next();
    }
    DoSimpleMsg(String("DynamicExp: \"") + Value + "\" not Found.", 50004);
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TDynamicExp Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


TDynamicExpObj::TDynamicExpObj(TDSSClass* ParClass, const String LineCodeName)
    : inherited(ParClass),
    FVarIdx(0),
    FNumvars(0),
    BaseFrequency(0.0),
    myDomain(0)
{
    Set_Name(LowerCase(LineCodeName));
    DSSObjType = ParClass->DSSClassType;
    FvarNames.resize( 0 );
    FNumvars = 20;
    FActiveVar = "";
    FVarIdx = -1;
    FvarNames.clear();
    FCmd.resize( 0 );
    FVarConst.resize( 0 );
    myDomain = 0;
    InitPropertyValues(0);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
/*Checks if the given string is a value calculated by the element using the eq model*/

bool TDynamicExpObj::Check_If_CalcValue(String myVal, int& myOp)
{
    bool result = false;
    bool found = false;
    String Val;
    int idx = 0;
    String ValNames[12/*# range 0..11*/];
    ValNames[0] = "p";
    ValNames[1] = "q";
    ValNames[2] = "vmag";
    ValNames[3] = "vang";
    ValNames[4] = "imag";
    ValNames[5] = "iang";
    ValNames[6] = "s";
    ValNames[7] = "p0";
    ValNames[8] = "q0";
    ValNames[9] = "edp";
    ValNames[10] = "kvdc";
    ValNames[11] = "mod";
    myOp = -1;
    found = false;
    Val = LowerCase(myVal);
    for (int stop = /*# High( ValNames ) */ 11, idx = 0; idx <= stop; idx++)
    {
        if (Val == ValNames[idx])
        {
            myOp = idx;
            found = true;
            break;
        }
    }
    result = found;
    return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



TDynamicExpObj::~TDynamicExpObj()
{
    FvarNames.clear();
    FVarConst.resize( 0 );
    // todo check:  inherited::Destroy;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



int TDynamicExpObj::Get_Closer_Op(String myExp, String& myOp, int& OpCode)
{
    int result = 0;
    bool SetMark = false;  // For validating special chars

    size_t myPos = 0;
    int idx = 0;
    result = 10000;
    for (int stop = /*# High( myOps ) */ 28, idx = 0; idx <= stop; idx++)
    {
        myPos = myExp.find(myOps[idx]);
        if ((((int) myPos) < result) && (myPos != String::npos))
        {
            SetMark = true;
            if (myOps[idx] == "-")    // Verify in case it is a negative number
                if (myExp[myPos + 1] != ' ')
                    SetMark = false;
            if (SetMark)
            {
                result = (int) myPos;
                myOp = myOps[idx];
                OpCode = idx;
            }
        }
    }
    return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
/*returns the index of the variable if it exists in the state variable list,
otherwise, it returns 50001 if the string entered is a numeric constant (dbl)
or -1 if the string entered is neither a numeric constant or state varaible*/


int TDynamicExpObj::Get_Var_Idx(String myVar)
{
    int result = 0;
    double dblval = 0.0;
    int idx = 0;
    dblval = 0.0;
    result = -1;   // error
    for (int stop = (FvarNames.size() - 1), idx = 0; idx <= stop; idx++)
        if (LowerCase(Trim(myVar)) == FvarNames[idx])
        {
            result = idx;
            break;
        }
    if (result < 0)
    {
        // so, it's not a state variable, maybe a constant
        try
        {
            dblval = stod(myVar);
            result = 50001;  // returns this code to indicate that it is a constant
        }
        catch (...)
        {
            result = -1;  // it's not a number
        }
    }
    return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



int TDynamicExpObj::Get_Out_Idx(String myVar)
/*returns the index of the variable if it exists in the state variable list,
and if it is an output (-50 in the next cell ot the FCmd automation array)*/
{
    int result = 0;
    double dblval = 0.0;
    int CmdIdx = 0, idx = 0;
    dblval = 0.0;
    result = -1;   // error
    for (int stop = (FvarNames.size() - 1), idx = 0; idx <= stop; idx++)
        if (LowerCase(myVar) == FvarNames[idx])
        {
            // now, check if the index corresponds to an output
            for (int stop = ( FCmd.size() - 1 ), CmdIdx = 0; CmdIdx <= stop; CmdIdx++)
            {
                if ((idx == FCmd[CmdIdx]) && (CmdIdx < (FCmd.size() - 1) ) )
                    if (FCmd[CmdIdx + 1] == -50)
                    {
                        // Means that the variable found is an output, we can leave
                        result = idx;
                        break;
                    }
            }
        }
    return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



String TDynamicExpObj::Get_VarName(int myIdx)
{
    String result;
    String diffstr;
    DynSlot myProt;

    int mylen = 0, myCol = 0, myRow = 0;
    myProt.resize(2);
    mylen = myProt.size();
    myRow = floor(((double)myIdx) / ((double)mylen));
    myCol = myIdx - (myRow * mylen);
    diffstr = "";
    if (myCol > 0)
    {
        diffstr = "d";
        if (myCol > 1)
            diffstr = diffstr + to_string(myCol);
    }
    result = diffstr + FvarNames[myRow];
    return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



double TDynamicExpObj::Get_DynamicEqVal(int myIdx, std::vector <DynSlot>* MemSpace)
{
    double result = 0.0;
    int mylen = 0, myCol = 0, myRow = 0;
    mylen = (*MemSpace)[0].size();
    myRow = floor(((double)myIdx) / ((double)mylen));
    myCol = myIdx - (myRow * mylen);
    result = (*MemSpace)[myRow][myCol];
    return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



bool TDynamicExpObj::IsInitVal(int myCode)
{
    bool result = false;
    result = false;
    switch (myCode)
    {
    case 7: case 8: case 9:
        result = true;
        break;
    }
    return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



void TDynamicExpObj::SolveEq(vector <DynSlot>* MemSpace)
{
    int OutIdx = 0, idx = 0, NxtIdx = 0;
    TRPNCalc RPN;

    OutIdx = -1;
    for (int stop = ( FCmd.size() - 1 ), idx = 0; idx <= stop; idx++)
    {
        if ((idx + 1) < FCmd.size())    NxtIdx = idx + 1;
        else                            NxtIdx = idx;
        if ((FCmd[NxtIdx] == -50) || (FCmd[idx] == -50))    // it's the begining of an equation
        {
            if (FCmd[idx] != -50)                            // The index
            {
                if (OutIdx >= 0)                                   // It's not the first equation
                    (*MemSpace)[OutIdx][1] = RPN.Get_X();                     // Uploads value into memory space
                OutIdx = FCmd[idx];
            }
        }
        else
        {
            switch (FCmd[idx])
            {
            case -2:
                RPN.Add();
                break;             //Add

            case -3:
                RPN.Subtract();
                break;        //Sub

            case -4:
                RPN.Multiply();
                break;        //Mult

            case -5:
                RPN.Divide();
                break;          //Div

            case -11:
                RPN.Square();
                break;          //Sqr

            case -12:
                RPN.Sqrt();
                break;            //Sqrt

            case -13:
                RPN.Inv();
                break;             //Inv

            case -14:
                RPN.NatLog();
                break;          //ln

            case -15:
                RPN.etothex();
                break;         //exp

            case -16:
                RPN.TenLog();
                break;          //log10

            case -17:
                RPN.Sindeg();
                break;          //Sin

            case -18:
                RPN.Cosdeg();
                break;          //Cos

            case -19:
                RPN.Tandeg();
                break;          //Tan

            case -20:
                RPN.aSindeg();
                break;         //ASin

            case -21:
                RPN.aCosdeg();
                break;         //ACos

            case -22:
                RPN.aTandeg();
                break;         //ATan

            case -23:
                RPN.aTan2deg();
                break;        //ATan2

            case -24:
                RPN.RollUp();
                break;          //RollUp

            case -25:
                RPN.RollDn();
                break;          //RollDn

            case -26:
                RPN.SwapXY();
                break;          //Swap

            case -27:
                RPN.EnterPi();
                break;         //Pi

            case -28:
                RPN.YToTheXPower();
                break;    //^
            default:
            {
                if (FCmd[idx] >= 50000)
                    RPN.Set_X(FVarConst[FCmd[idx] - 50000]);
                else
                    RPN.Set_X((*MemSpace)[FCmd[idx]][0]);       // It's a variable
            }
            }
        }
    }
    (*MemSpace)[OutIdx][1] = RPN.Get_X();               // Uploads value into memory space
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



bool TDynamicExpObj::InterpretDiffEq(String Exp)
/*Builds the list of commands required for operating the equation declared, this
automation is intended to acelerate the calculation in run time.
Notation:
  Positive integers represent the index to a variable slot (dbl)
  If the integer is a value >= 50000, it means that it is the index to a
  numeric constant that can be located at FVarConst
  If is a negative integer, represents one of the following operations:
    -2 = Add
    -3 = Subtraction
    -4 = Mult
    -5 = Div .... etc. For details, check myOps array defined above
  If the negative integer is -50, it means the begining of a new equation*/
{
    bool result = false;
    int idx = 0, OpCode = 0, OpIdx = 0;
    String ErrorSrc, SubXp, Op;
    TStringList myVars;

    result = false;
    ErrorSrc = "";
    myVars.clear();
    FCmd.resize( 0 );
    FXpression = "[" + LowerCase(Exp) + "]";
    while (FXpression.size() > 0)
    {
        OpIdx = Get_Closer_Op(FXpression, Op, OpCode);
        if (OpIdx == 10000)
        {
            FXpression = "";   // we're done
        }
        else
        {
            SubXp = FXpression.substr(0, OpIdx);
            if (Op.size() >= 1)
                OpIdx = OpIdx + Op.size();
            FXpression = FXpression.substr(OpIdx, FXpression.size());
            InterpretTStringListArray(SubXp, myVars);
            switch (OpCode)
            {
            case 0:
            {
                OpIdx = Get_Var_Idx(myVars[0]);     // the result is always placed at the begin
                if (OpIdx == 50001)
                {
                    DoSimpleMsg("DynamicExp: the expression preceeding the \"dt\" operand has to be a state variable.", 50006);
                    ErrorSrc = "preceeding differential output";
                }
                else
                {
                    if (OpIdx < 0)
                        ErrorSrc = myVars[0];
                    else
                    {
                        FCmd.push_back(OpIdx);
                        FCmd.push_back(-50);   // denotes the begining of an equation
                    }
                }
            }
            break;
            case 1: case 6: case 7: case 8: case 9:
            {
                // Do nothing, it's just for notation reference at the user side
            }
            break;
            default:   // it is one of the basic operations or end of the diff eq
            {
                for (int stop = (myVars.size() - 1), idx = 0; idx <= stop; idx++)
                {
                    OpIdx = Get_Var_Idx(myVars[idx]);
                    if (OpIdx == 50001)
                    {
                        FVarConst.push_back( stod( myVars[idx] ) );
                        FCmd.push_back( 50000 + ( FVarConst.size() - 1 ) );
                    }
                    else
                    {
                        if (OpIdx < 0)
                            ErrorSrc = "\"" + myVars[0] + "\"";
                        else
                            FCmd.push_back( Get_Var_Idx( myVars[idx] ) );
                    }
                }
                if (OpCode != 10)
                {
                    FCmd.push_back( - 1 * OpCode );    // assings the operator -> + - * /
                }
            }
            }
        }
        if (!ErrorSrc.empty())
        {
            DoSimpleMsg(String("DynamicExp: Variable ") + ErrorSrc + " not Found.", 50005);
            FXpression = "";
            result = true;
        }
    }
    if (!result)
        FXpression = Exp;    // assings the expression again to keep a local copy
    return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



void TDynamicExpObj::DumpProperties(TTextRec& F, bool Complete)
{
    int i = 0;
    inherited::DumpProperties(F, Complete);
    /*# with ParentClass do */
    {
        auto with0 = ParentClass;
        for (int stop = with0->NumProperties, i = 1; i <= stop; i++)
        {
            Write(F, "~ "); Write(F, (with0->PropertyName)[i - 1]); Write(F, '='); WriteLn(F, Get_PropertyValue(i));
        }
    }
}


String TDynamicExpObj::GetPropertyValue(int Index)
{
    String result;
    switch (Index)
    {
    case 1:
        result = Format("%d", FNumvars);
        break;
    case 4:
        result = Format("%d", FVarIdx);
        break;
    default:
        result = inherited::GetPropertyValue(Index);
    }
    return result;
}

int TDynamicExpObj::get_FNumVars() 
{
    return FNumvars;
}
void TDynamicExpObj::set_FNumvars(int value)
{
    FNumvars = value;
}

void TDynamicExpObj::InitPropertyValues(int ArrayOffset)
{
    Set_PropertyValue(1,"0");      // 'nvariables';
    Set_PropertyValue(2,"[]");     // 'varnames';
    Set_PropertyValue(3,"[0.0]");  // 'varinit';
    Set_PropertyValue(4,"");       // 'var';
    Set_PropertyValue(5,"0.0");    // 'varvalue';
    Set_PropertyValue(6,"");       // 'expression';
    inherited::InitPropertyValues(NumPropsThisClass);
}


}  // namespace DynamicExp