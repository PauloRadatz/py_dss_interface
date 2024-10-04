

#pragma hdrstop

#include "CktElementClass.h"

#include "CktElement.h"
#include "ParserDel.h"
#include "Utilities.h"
#include "DSSGlobals.h"


/* TCktElementClass */

namespace CktElementClass
{

    int TCktElementClass::ClassEdit(const void* ActiveCktElemObj, const int ParamPointer)
    {
        int result = 0;
        result = 0;
        // continue parsing with contents of Parser
        if (ParamPointer > 0)
            /*# with TDSSCktElement(ActiveCktElemObj) do */
        {
            auto with0 = (TDSSCktElement*)ActiveCktElemObj;
            {
                switch (ParamPointer)
                {
                case 1:
                    with0->BaseFrequency = Parser[ActiveActor]->MakeDouble_();
                    break;
                case 2:
                    with0->Set_Enabled(InterpretYesNo(Parser[ActiveActor]->MakeString_()));
                    break;
                default:
                    inherited::ClassEdit(ActiveCktElemObj, ParamPointer - NumCktElemClassProps);
                }
            }
        }
        return result;
    }


    void TCktElementClass::ClassMakeLike(const void* OtherObj)
    {
        TDSSCktElement* OtherCktObj;
        OtherCktObj = (TDSSCktElement*) OtherObj;
        /*# with TDSSCktElement(ActiveDSSObject[ActiveActor]) do */
        {
            auto with0 = (TDSSCktElement*) ActiveDSSObject[ActiveActor];
            {
                with0->BaseFrequency = OtherCktObj->BaseFrequency;
                with0->Set_Enabled(true);
            }
        }
    }


    void TCktElementClass::CountProperties()
    {
        NumProperties = NumProperties + NumCktElemClassProps;
        inherited::CountProperties();
    }


    TCktElementClass::TCktElementClass()
        : NumCktElemClassProps(2)
    {
        inherited();
    }


    void TCktElementClass::DefineProperties()

        // Define the properties for the base power delivery element class

    {
        PropertyName[ActiveProperty + 1] = "basefreq";
        PropertyName[ActiveProperty + 2] = "enabled";
        PropertyHelp[ActiveProperty + 1] = "Base Frequency for ratings.";
        PropertyHelp[ActiveProperty + 2] = "{Yes|No or True|False} Indicates whether this element is enabled.";
        ActiveProperty = ActiveProperty + NumCktElemClassProps;
        inherited::DefineProperties();
    }


    TCktElementClass::~TCktElementClass()
    {
        // todo check:  inherited::Destroy;
    }

}// namespace CktElementClass







