

#pragma hdrstop

#include "Conductor.h"

#include "DSSGlobals.h"

namespace Conductor
{

    TConductor::TConductor()
        : AmbientTemp(0.0),
        Accum_Isqt(0.0),
        Closed(true),
        FuseBlown(false)
    {
        // inherited::Create();
        Accum_Isqt = 0.0;
        //TCC := nil;
        TCCName = "";
    }


    TConductor::~TConductor()
    {
        // todo check:  inherited::Destroy;
    }


    void TConductor::Set_Ambient(double Value)
    {
        AmbientTemp = Value;
    }

    std::string TConductor::Get_TCCName()
    {
        return TCCName;
    }

    void TConductor::Set_TCCname(const String Value)
    {
        TCCName = LowerCase(Value);
    }


    void TConductor::CalcIsqt(double CurrentMag)  // Computes whether conductor has burned down

    {
        DoSimpleMsg("Need to implement Tconductor.CalcIsqrt", 770);
    }


    void TConductor::ResetIsqt()  // restore the conductor and reset the i2t calcs

    {
        DoSimpleMsg("Need to implement Tconductor.ResetIsqt", 771);
    }



} // namespace conductor





