

#pragma hdrstop

#include "DSSClass.h"

#include "DSSObject.h"
#include "CktElement.h"
#include "DSSGlobals.h"


namespace DSSClass
{


    /*--------------------------------------------------------------*/
    /* DSSClasses Implementation
    {--------------------------------------------------------------*/



    //TDSSClasses::TDSSClasses() {};

    TDSSClasses::TDSSClasses()
    {
       //inherited();
    }

    /*--------------------------------------------------------------*/

    TDSSClasses::~TDSSClasses()
    {
        // todo check:  inherited::Destroy;
    }

    /*--------------------------------------------------------------*/



    void TDSSClasses::Set_New(void* Value)
    {
        DSSClassList[ActiveActor].Set_New(Value); // Add to pointer list
        ActiveDSSClass[ActiveActor] = (TDSSClass*)Value;   // Declare to be active
        ClassNames[ActiveActor].Add(ActiveDSSClass[ActiveActor]->get_myClass_name()); // Add to classname list
    }


    /*--------------------------------------------------------------*/
    /*  DSSClass Implementation
    {--------------------------------------------------------------*/


    TDSSClass::TDSSClass()
        : ElementNameList(THashList(100)),
        ElementList(TPointerList(20))
    {
        ActiveElement = 0;
        ActiveProperty = 0;
        NumProperties = 0;
        DSSClassType = 0;
        ElementNamesOutOfSynch = false;
        Saved = false;

        // inherited::Create();
        PropertyName = NULL;
        PropertyHelp = NULL;
        PropertyIdxMap = NULL;
        RevPropertyIdxMap = NULL;
        ActiveElement = 0;
        ActiveProperty = 0;

        ElementNamesOutOfSynch = false;
    }

    /*--------------------------------------------------------------*/


    TDSSClass::~TDSSClass()
    {
        int i = 0;
        // Get rid of space occupied by strings
        /*for (int stop = NumProperties, i = 1; i <= stop; i++)
            PropertyName[i] = "";
        for (int stop = NumProperties, i = 1; i <= stop; i++)
            PropertyHelp[i] = "";
        delete[] PropertyName;
        delete[] PropertyHelp;
        delete[] PropertyIdxMap;
        delete[] RevPropertyIdxMap; */
        // todo check:  inherited::Destroy;
    }


    /*--------------------------------------------------------------*/

    int TDSSClass::get_ActiveElement()
    {
        return ActiveElement;
    }

    /*--------------------------------------------------------------*/

    int TDSSClass::NewObject(std::string ObjName)
    {
        int result = 0;
        result = 0;
        DoErrorMsg(String("Reached base class of TDSSClass for device \"") + ObjName + "\"", "N/A", "Should be overridden.", 780);
        return result;
    }


    void TDSSClass::Set_Active(int Value)
    {
        if ((Value > 0) && (Value <= ElementList.get_myNumList()))
        {
            ActiveElement = Value;
            ActiveDSSObject[ActiveActor] = ElementList.Get(ActiveElement);
            // Make sure Active Ckt Element agrees if is a ckt element
            // So COM interface will work
            if (dynamic_cast<TDSSCktElement*>((TDSSObject*)ActiveDSSObject[ActiveActor]) != nullptr)
                ActiveCircuit[ActiveActor]->Set_ActiveCktElement((TDSSCktElement*)ActiveDSSObject[ActiveActor]);
        }
    }


    int TDSSClass::Edit(int ActorID)
    {
        int result = 0;
        result = 0;
        DoSimpleMsg("virtual function TDSSClass.Edit called.  Should be overriden.", 781);
        return result;
    }


    int TDSSClass::Init(int Handle, int ActorID)
    {
        int result = 0;
        result = 0;
        DoSimpleMsg("virtual function TDSSClass.Init called.  Should be overriden.", 782);
        return result;
    }


    int TDSSClass::AddObjectToList(void* Obj)
    {
        int result = 0;
        ElementList.Set_New(Obj); // Stuff it in this collection's element list
        ElementNameList.Add(((TDSSObject*)Obj)->get_Name());
        if (((unsigned int)ElementList.get_myNumList()) > 2 * ElementNameList.InitialAllocation)
            ReallocateElementNameList();
        ActiveElement = ElementList.get_myNumList();
        result = ActiveElement; // Return index of object in list
        return result;
    }


    bool TDSSClass::SetActive(string ObjName)
    {
        bool result = false;
        int idx = 0;
        result = false;
        // Faster to look in hash list 7/7/03
        if (ElementNamesOutOfSynch)
            ResynchElementNameList();
        idx = ElementNameList.Find(ObjName);
        if (idx > 0)
        {
            ActiveElement = idx;
            ActiveDSSObject[ActiveActor] = ElementList.Get(idx);
            result = true;
        }
        return result;
    }


    void* TDSSClass::Find(string ObjName)
    {
        void* result = NULL;
        int idx = 0;
        result = NULL;
        if (ElementNamesOutOfSynch)
            ResynchElementNameList();
        // Faster to look in hash list 7/7/03
        idx = ElementNameList.Find(ObjName);
        if (idx > 0)
        {
            ActiveElement = idx;
            result = ElementList.Get(idx);
        }
        return result;
    }


    void* TDSSClass::GetActiveObj() // Get address of active obj of this class

    {
        void* result = NULL;
        ActiveElement = ElementList.get_myActiveItem();
        if (ActiveElement > 0)
            result = ElementList.Get(ActiveElement);
        else
            result = NULL;
        return result;
    }

    String TDSSClass::Get_FirstPropertyName()
    {
        String result;
        ActiveProperty = 0;
        result = Get_NextPropertyName();
        return result;
    }


    String TDSSClass::Get_NextPropertyName()
    {
        String result;
        ActiveProperty++;
        if (ActiveProperty <= NumProperties)
            result = PropertyName[ActiveProperty];
        else
            result = "";
        return result;
    }

    string TDSSClass::get_myClass_name()
    {
        return Class_Name;
    }


    int TDSSClass::PropertyIndex(string Prop)
        // find property value by string

    {
        int result  = 0; // Default result if not found
        int i       = 0;

        for (i = 0; i < NumProperties; i++)
        {
            if (CompareText(Prop, PropertyName[i]) == 0)
            {
                result = PropertyIdxMap[i];
                break;
            }
        }
        return result;
    }


    void TDSSClass::CountProperties()
    {
        NumProperties = NumProperties + 1;
    }


    void TDSSClass::DefineProperties()
    {
        ActiveProperty = ActiveProperty + 1;
        PropertyName[ActiveProperty] = "like";
        PropertyHelp[ActiveProperty] = "Make like another object, e.g.:" + CRLF + CRLF + "New Capacitor.C2 like=c1  ...";
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    int TDSSClass::ClassEdit(const void* ActiveObj, const int ParamPointer)
    {
        int result = 0;
        // continue parsing with contents of Parser
        result = 0;
        if (ParamPointer > 0)
            /*# with TDSSObject(ActiveObj) do */
        {
            const TDSSObject& with0 = *(TDSSObject*)ActiveObj;
            {
                switch (ParamPointer)
                {
                case 1:
                    MakeLike(Parser[ActiveActor]->MakeString_());
                    break;    // Like command (virtual)
                }
            }
        }
        return result;
    }


    int TDSSClass::MakeLike(string ObjName)
    {
        int result = 0;
        result = 0;
        DoSimpleMsg("virtual function TDSSClass.MakeLike called.  Should be overriden.", 784);
        return result;
    }


    int TDSSClass::Get_ElementCount()
    {
        int result = 0;
        result = ElementList.get_myNumList();
        return result;
    }


    int TDSSClass::Get_First()
    {
        int result = 0;
        if (ElementList.get_myNumList() == 0)
            result = 0;
        else
        {
            ActiveElement = 1;
            ActiveDSSObject[ActiveActor] = ElementList.Get_First();
            // Make sure Active Ckt Element agrees if is a ckt element
            // So COM interface will work
            if (dynamic_cast<TDSSCktElement*>((TDSSObject*)ActiveDSSObject[ActiveActor]) != nullptr)
                ActiveCircuit[ActiveActor]->Set_ActiveCktElement((TDSSCktElement*)(ActiveDSSObject[ActiveActor]));
            result = ActiveElement;
        }
        return result;
    }

    int TDSSClass::Get_Next()
    {
        int result = 0;
        ActiveElement++;
        if (ActiveElement > ElementList.get_myNumList())
            result = 0;
        else
        {
            ActiveDSSObject[ActiveActor] = ElementList.Get_Next();
            // Make sure Active Ckt Element agrees if is a ckt element
            // So COM interface will work
            if (dynamic_cast<TDSSCktElement*>((TDSSObject*)ActiveDSSObject[ActiveActor]) != nullptr)
                ActiveCircuit[ActiveActor]->Set_ActiveCktElement((TDSSCktElement*)(ActiveDSSObject[ActiveActor]));
            result = ActiveElement;
        }
        return result;
    }


    void TDSSClass::AddProperty(string PropName, int CmdMapIndex, string HelpString)
    {
        ActiveProperty++;
        PropertyName[ActiveProperty - 1] = PropName;
        PropertyHelp[ActiveProperty - 1] = HelpString;
        PropertyIdxMap[ActiveProperty - 1] = CmdMapIndex;   // Maps to internal object property index
        RevPropertyIdxMap[CmdMapIndex - 1] = ActiveProperty;
    }


    void TDSSClass::AllocatePropertyArrays()
    {
        int i = 0;
        PropertyName = new string[NumProperties + 1];
        PropertyHelp = new string[NumProperties + 1];
        PropertyIdxMap = new int[NumProperties + 1];
        RevPropertyIdxMap = new int[NumProperties + 1];
        ActiveProperty = 0;    // initialize for AddPropert
           /*initialize PropertyIdxMap to take care of legacy items*/
        for (int stop = NumProperties, i = 1; i <= stop; i++)
            PropertyIdxMap[i - 1] = i;
        for (int stop = NumProperties, i = 1; i <= stop; i++)
            RevPropertyIdxMap[i - 1] = i;
    }


    void TDSSClass::ReallocateElementNameList()
    {
        int i = 0;
        /*Reallocate the device name list to improve the performance of searches*/
        ElementNameList = THashList(2 * ElementList.get_myNumList());  // make a new one

          // Do this using the Names of the Elements rather than the old list because it might be
          // messed up if an element gets renamed
        for (int stop = ElementList.get_myNumList(), i = 1; i <= stop; i++)
            ElementNameList.Add((*(TDSSObject*)(ElementList.Get(i))).get_Name());
    }


    void TDSSClass::ResynchElementNameList()
    {
        ReallocateElementNameList();
        ElementNamesOutOfSynch = false;
    }


}// namespace DSSClass





