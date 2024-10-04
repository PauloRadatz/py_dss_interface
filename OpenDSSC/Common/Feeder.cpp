
#pragma hdrstop

#include "Feeder.h"



#include "ParserDel.h"
#include "Circuit.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Utilities.h"
#include "Sysutils.h"
#include "Command.h"
#include "EnergyMeter.h"
#include "PDElement.h"

#include "System.h"




namespace Feeder
{
    TFeederObj* ActiveFeederObj = NULL;
    int NumPropsThisClass = 0;

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    TFeeder::TFeeder()  // Creates superstructure for all Line objects
        :inherited()
    {
        Class_Name = "Feeder";
        DSSClassType = FEEDER_ELEMENT; /*+ PC_ELEMENT; */ // add to PCElement list
        ActiveElement = 0;
        DefineProperties();
        std::string* slc = Slice(PropertyName, NumProperties);
        CommandList = TCommandList(slc, NumProperties);
        delete[] slc;
        CommandList.set_AbbrevAllowed(true);
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    TFeeder::~TFeeder()
    {
        // ElementList and  CommandList freed in inherited destroy
      // todo check:  inherited::Destroy;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TFeeder::DefineProperties()
    {
        NumPropsThisClass = 0;
        NumProperties = NumPropsThisClass;
        CountProperties();   // Get inherited property count
        AllocatePropertyArrays();

        // Can't Think of any properties we want the user to be able to set

             // Define Property names
        //     PropertyName[1] := 'bus1';

             // define Property help values
        //     PropertyHelp[1] := 'Name of bus to which source is connected.'+CRLF+'bus1=busname'+CRLF+'bus1=busname.1.2.3';
        ActiveProperty = NumPropsThisClass - 1;
        inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    int TFeeder::NewObject(const String ObjName)

        // Called from EnergyMeter

    {
        int result = 0;
        TFeederObj* Obj = NULL;
        //Make a new Feeder object
        // Get_First() see if this one already exists. If so, just reinitialize
        Obj = (TFeederObj*) Find(ObjName);
        /*# with ActiveCircuit[ActiveActor] do */
        {
            auto with0 = ActiveCircuit[ActiveActor];
            if (Obj != NULL)
            {
                with0->Set_ActiveCktElement(Obj);
                result = 0;
            }
            else
            {
                with0->Set_ActiveCktElement(new TFeederObj(this, ObjName));
                result = AddObjectToList(ActiveDSSObject[ActiveActor]);
                ActiveCircuit[ActiveActor]->AddCktElement(result);
                // done here because feeder objects are instantiated from energy meters
            }
        }
        return result;
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    int TFeeder::Edit(int ActorID)
    {
        int result = 0;
        int ParamPointer = 0;
        String ParamName, Param;
        // continue parsing with contents of Parser
        ActiveFeederObj = (TFeederObj*) ElementList.Get_Active();
        ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveFeederObj);
        result = 0;
        /*# with ActiveFeederObj do */
        {
            auto with1 = ActiveFeederObj;
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
                   with1->Set_PropertyValue(ParamPointer, Param);
                switch (ParamPointer)
                {
                case 0:
                    DoSimpleMsg(String("Unknown parameter \"") + ParamName + "\" for Object \"" + Class_Name + "." + get_myClass_name() + "\"", 630);
                    break;
                default:
                    ClassEdit(ActiveFeederObj, ParamPointer - NumPropsThisClass);
                }
                ParamName = Parser[ActorID]->GetNextParam();
                Param = Parser[ActorID]->MakeString_();
            }
            ActiveFeederObj->RecalcElementData(ActorID);
            with1->Set_YprimInvalid(ActorID,true);
        }
        return result;
    }

    //----------------------------------------------------------------------------



    int TFeeder::MakeLike(const String OtherFeederName)
    {
        int result = 0;
        TFeederObj* OtherFeeder = NULL;
        int i = 0;
        result = 0;
        /*See if we can find this name in the present collection*/
        OtherFeeder = (TFeederObj*) Find(OtherFeederName);
        if (OtherFeeder != NULL)
            /*# with ActiveFeederObj do */
        {
            auto with0 = ActiveFeederObj;
            if ( ( ( TDSSCktElement*) with0 )->Fnphases != ( (TDSSCktElement*) OtherFeeder )->Fnphases)
            {
                ((TDSSCktElement*)with0)->Set_NPhases(((TDSSCktElement*)OtherFeeder)->Fnphases);
                ((TDSSCktElement*)with0)->Set_Nconds(((TDSSCktElement*)OtherFeeder)->Fnphases);  // Forces reallocation of terminal stuff
                ((TDSSCktElement*)with0)->Yorder = ((TDSSCktElement*)OtherFeeder)->Fnconds * ((TDSSCktElement*)OtherFeeder)->Fnterms;
                ((TDSSCktElement*)with0)->Set_YprimInvalid(ActiveActor,true);
            }

            // Put properties to copy here
            ClassMakeLike(OtherFeeder); // set spectrum,  base frequency
            for (int stop = ( (TDSSObject*) with0 )->ParentClass->NumProperties, i = 1; i <= stop; i++)
                ((TDSSObject*)with0)->Set_PropertyValue(i, ((TDSSObject*)OtherFeeder)->Get_PropertyValue(i));
            result = 1;
        }
        else
            DoSimpleMsg(String("Error in Feeder MakeLike: \"") + OtherFeederName + "\" Not Found.", 631);
        return result;
    }

    //----------------------------------------------------------------------------



    int TFeeder::Init(int Handle, int ActorID)
    {
        int result = 0;
        DoSimpleMsg("Need to implement TFeeder.Init", -1);
        result = 0;
        return result;
    }

    //----------------------------------------------------------------------------



    TFeederObj::TFeederObj(TDSSClass* ParClass, const String MeterName)
    : inherited(ParClass),
        FromTerminalOffset(0),
        IsSynched(false)
    {

        Set_Name(LowerCase(MeterName));
        DSSObjType = ParClass->DSSClassType; // This will be a current source (PCElement)
        sequenceList = TPointerList(50);
        ShuntList = TPointerList(50);
        IsSynched = false;

        // Bus names and Nphases, etc are set up from EnergyMeter

        // Ready to rock 'n roll
        RecalcElementData(ActiveActor);
        InitPropertyValues(0);
    }


    //----------------------------------------------------------------------------



    TFeederObj::~TFeederObj()
    {
        // todo check:  inherited::Destroy;
    }


    void TFeederObj::InitializeFeeder( TCktTree BranchList, int ActorID)
    {
        int i = 0, bref = 0;
        TDSSCktElement* pElement;
        TDSSCktElement* pShunt;
        sequenceList.Clear();  // Get rid of any previous definitions
        ShuntList.Clear();
        IsSynched = false;
        // Now set up Feeder terminals and BusRef to match the from node of the first branch
        // if (&BranchList != NULL) // original Delphi code was "If BranchList <> Nil Then Begin", but here BranchList is a copy passed via the stack, so this pointer is always non-NULL
        {
            RootElement = *( (TDSSCktElement*) BranchList.Get_First()  );
            Set_NPhases(RootElement.Get_NPhases()); // Take care of allocating Terminal stuff
            Fnconds = RootElement.Get_NConds();
            Set_NTerms(1);
            Yorder = Fnterms * Fnconds;
            Terminals[0].BusRef = BranchList.PresentBranch->FromBusReference;
            SetBus(1, RootElement.GetBus(BranchList.PresentBranch->FromTerminal));  // set bus name same as first element
            FromTerminalOffset = (BranchList.PresentBranch->FromTerminal - 1) * Fnconds;
            SetNodeRef(1, *(pIntegerArray*)  & RootElement.NodeRef[FromTerminalOffset]);

            // Build The Sequence List  and ShuntList
            pElement = &RootElement;
            while (pElement != NULL)
            {
                sequenceList.Add(pElement);

                // Mark all the To buses for this branch as radial buses
                BranchList.PresentBranch->ResetToBusList();  // reset pointer to first to bus
                for (int stop = pElement->Get_NTerms() - 1, i = 1; i <= stop; i++)
                {
                    bref = BranchList.PresentBranch->Get_ToBusReference(); // each call pops off a new one
                    if (bref > 0)
                        ActiveCircuit[ActorID]->Buses[bref - 1]->IsRadialBus = true;
                }
                pShunt = (TDSSCktElement*) BranchList.PresentBranch->Get_FirstObject();
                while (pShunt != NULL)
                {
                    ShuntList.Add(pShunt);
                    pShunt = (TDSSCktElement*) BranchList.PresentBranch->Get_NextObject();
                }
                pElement = (TDSSCktElement*) BranchList.Get_Forward();
            }
            IsSynched = true;
            SetCktElementFeederFlags(true);
        }
    }

    //----------------------------------------------------------------------------



    void TFeederObj::RecalcElementData(int ActorID)
    {

        /*Nothing to Do?? - Maybe remake bus lists*/
    }

    //----------------------------------------------------------------------------



    void TFeederObj::CalcYPrim(int ActorID)
    {

        // For now, YPrim is null

         // Build only YPrim Series
        if (Get_YprimInvalid(ActorID, 0))
        {
            if (YPrim_Series != NULL)
                delete YPrim_Series; // YPrim_Series->~TcMatrix();
            YPrim_Series = new TcMatrix(Yorder);
            if (YPrim != NULL)
                delete YPrim; // YPrim->~TcMatrix();
            YPrim = new TcMatrix(Yorder);
        }
        else
        {
            YPrim_Series->Clear();
            YPrim->Clear();
        }


        /*Yprim = 0  for Ideal Current Source;  just leave it zeroed*/

        /*Now Account for Open Conductors*/
        /*For any conductor that is open, zero out row and column*/
        inherited::CalcYPrim(ActorID);
        Set_YprimInvalid(ActorID,false);
    }


    int TFeederObj::InjCurrents(int ActorID)

        /*Sum Currents directly into solution array*/

        /* This is where we do the backward Sweep - computing the currents from the present voltages*/
    {
        int result = 0;


        // old implementation deleted.
        result = 0;
        return result;
    }


    void TFeederObj::GetCurrents(pComplexArray Curr, int ActorID)

        /*Total currents into a feeder which are equal to the currents into the first element*/
        /*Return the currents in the From terminal of the first element in the sequence list*/
    {
        int i = 0;
        //   cBuffer:pComplexArray;
        //   pElem :TCktElement;

           // If the feeder exists and we switch away from radial solution we don' want
           // to report a current
           // Do this only if doing a radial solution
        /*   If ActiveCircuit[ActiveActor].RadialSolution Then
           Begin
             TRY
               pElem :=  TCktElement(SequenceList.Get(1));
               Getmem(cBuffer, Sizeof(cBuffer^[1])*pElem.Yorder );
               pElem.GetCurrents(cBuffer);   // get all currents in first element in sequence list

              // Return only FROM terminal current
               FOR i := 1 TO Yorder DO Curr^[i] := cBuffer^[i+FromTerminalOffset];

               Freemem(cBuffer); // dump temp buffer

            EXCEPT
              On E: Exception
              Do DoErrorMsg(('GetCurrents for Feeder Element: ' + Name + '.'), E.Message,
                'Inadequate storage allotted for circuit element?', 632);
            End;
          End Else
          */
        for (int stop = Yorder, i = 1; i <= stop; i++)
            (Curr)[i - 1] = CZero; // no contribution if not radial solution
    }

    void TFeederObj::GetInjCurrents(pComplexArray Curr, int ActorID)

        /*Fill Up an array of injection currents*/

        /*Only thing this is used for is for GetCurrents.  Ignore for Feeder*/
    {
        /*# with ActiveCircuit[ActorID].Solution do */
        {

            /***** Do Nothing!*/
        }
    }


    void TFeederObj::DumpProperties(Textfile& F, bool Complete)
    {
        inherited::DumpProperties(F, Complete);

        /*Do Not dump any properties for a Feeder unless Debug*/
        /*  With ParentClass Do
           For i := 1 to NumProperties Do
           Begin
              Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
           End;
        */
        if (Complete)
        {
            /*Dump sequence lists, etc here...*/
            WriteLn(F);
            WriteLn(F);
        }
    }


    void TFeederObj::InitPropertyValues(int ArrayOffset)
    {

        //   PropertyValue[1]  := GetBus(1);
        inherited::InitPropertyValues(NumPropsThisClass);
    }


    void TFeederObj::MakePosSequence(int ActorID)
    {
        /* Do Nothing
         If Fnphases>1 Then
         Begin
            Parser.CmdString := 'phases=1';
            Edit;
         End;
         inherited;
        */
    }


    void TFeederObj::SetCktElementFeederFlags(bool Value)
    {
        int i = 0;
        for (int stop = ShuntList.get_myNumList(), i = 1; i <= stop; i++)
        {
            ( (TDSSCktElement*) ShuntList.Get(i) )->IsPartofFeeder = Value;
        }
        for (int stop = sequenceList.get_myNumList(), i = 1; i <= stop; i++)
        {
            ( (TDSSCktElement*) sequenceList.Get(i) )->IsPartofFeeder = Value;
        }
    }

}// namespace Feeder







