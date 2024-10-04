
#pragma hdrstop

#include "ReduceAlgs.h"

#include "Line.h"
#include "Utilities.h"
#include "DSSGlobals.h"
#include "DSSClassDefs.h"
#include "Load.h"
#include "Ucomplex.h"
#include "ParserDel.h"
#include "CktElement.h"
#include "ExecHelper.h"
#include "Bus.h"

using namespace std;
using namespace Bus;
using namespace CktElement;
using namespace CktTree;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace ExecHelper;
using namespace Line;
using namespace Load;
using namespace PDELement;
using namespace ParserDel;
using namespace System;
using namespace Ucomplex;
using namespace Utilities;

namespace ReduceAlgs
{


const bool SERIESMERGE = true;
const bool PARALLELMERGE = false;

void DoMergeParallelLines(TCktTree*& BranchList)
{
	void* dummy;
	TLineObj* LineElement = nullptr;
	if(BranchList != nullptr)
	{
		dummy = BranchList->Get_First();
		LineElement = ((TLineObj*) BranchList->Get_Forward()); // Always keep the first element
		while(LineElement != nullptr)
		{
			if(BranchList->PresentBranch->IsParallel)
               /*There will always be two lines in parallel.  The first operation will disable the second*/
			{
				if( ( (TDSSCktElement*) LineElement )->Get_Enabled())
					LineElement->MergeWith(((TLineObj*&) BranchList->PresentBranch->LoopLineObj), PARALLELMERGE);  // Guaranteed to be a line
			}
			LineElement = ((TLineObj*) BranchList->Get_Forward());
		}
	}
}
/*Merge all lines in this zone that are marked in parallel*/

void DoBreakLoops(TCktTree*& BranchList)
{
	void* dummy;
	TLineObj* LineElement = nullptr;
	if(BranchList != nullptr)
	{
		dummy = BranchList->Get_First();
		LineElement = ((TLineObj*) BranchList->Get_Forward()); // Always keep the first element
		while(LineElement != nullptr)
		{
			if(BranchList->PresentBranch->IsLoopedHere)
               /*There will always be two lines in the loop.  The first operation will disable the second*/
			{
				if(((TDSSCktElement*)LineElement)->Get_Enabled())
					((TDSSCktElement*) BranchList->PresentBranch->LoopLineObj)->Set_Enabled(false); // Disable the other
			}
			LineElement = ((TLineObj*) BranchList->Get_Forward());
		}
	}
}

/*Break loops*/


/*

procedure DoReduceTapEnds(var BranchList:TCktTree);
(*Var
   pLineElem1, pLineElem2:TLineObj;
   ToBusRef:Integer;
   AngleTest:Double;
   ParentNode:TCktTreeNode;
*)
begin

end;
*/

void DoReduceDangling(TCktTree*& BranchList)
{
	void* dummy;
	TDSSCktElement* pLineElem1 = nullptr;
	int ToBusRef = 0;
	if(BranchList != nullptr)
     /*Let's throw away all dangling end branches*/
	{
		dummy = BranchList->Get_First();
		pLineElem1 = ((TDSSCktElement*) BranchList->Get_Forward()); // Always keep the first element
		while(pLineElem1 != nullptr)
		{
			if(IslineElement(pLineElem1))
				/*# with BranchList.PresentBranch do */
				{
					auto with0 = BranchList->PresentBranch;

             /*If it is at the end of a section and has no load,cap, reactor,
             or coordinate, just throw it away*/
					if(with0->IsDangling)
					{
						ToBusRef = with0->Get_ToBusReference();  // only access this property once!
						if(ToBusRef > 0)
							/*# with ActiveCircuit[ActiveActor]->Buses^[ToBusRef] do */
							{
								
								if(!(ActiveCircuit[ActiveActor]->Buses[ToBusRef - 1]->Keep))
									pLineElem1->Set_Enabled(false);
							}
					} /*IF*/
				}  /*If-With*/
			pLineElem1 = ((TDSSCktElement*) BranchList->Get_Forward());
		}
	}
}

bool IsShortLine(TDSSCktElement* const Elem)
{
	bool result = false;
	double Ztest = 0.0;
	TLineObj* LineElement = nullptr;
	LineElement = ((TLineObj*) Elem);
     /*Get Positive Sequence or equivalent from matrix*/
	if(LineElement->SymComponentsModel)
		/*# with LineElement do */
		{
			auto with0 = LineElement;
			Ztest = cabs(cmplx(with0->R1, with0->X1)) * with0->Len;
		}
	else
		/*# with LineElement do */
		{
			auto with1 = LineElement; /*Get impedance from Z matrix*/  /*Zs - Zm ... approximates Z1*/
			if( ( (TDSSCktElement*) with1 )->Get_NPhases() > 1)
				Ztest = cabs(csub(with1->Z->GetElement(1, 1), with1->Z->GetElement(1, 2))) * with1->Len;
			else
				Ztest = cabs(with1->Z->GetElement(1, 1)) * with1->Len;
		}
	if(Ztest <= ActiveCircuit[ActiveActor]->ReductionZmag)
		result = true;
	else
		result = false;
	return result;
}

void DoReduceShortLines(TCktTree*& BranchList)
{
	TLineObj*		LineElement1 = nullptr;
	TLineObj*		LineElement2 = nullptr;
	TDSSCktElement* shuntElement = nullptr;
	TCktTreeNode*	ParentNode = nullptr;
	bool			MergeOK = false;
	if(BranchList != nullptr)  /*eliminate really short lines*/
      /*Get_First(), flag all elements that need to be merged*/
	{
		LineElement1 = (TLineObj*) BranchList->Get_First();
		LineElement1 = (TLineObj*) BranchList->Get_Forward(); // Always keep the first element
		while(LineElement1 != nullptr)
		{
			if(IslineElement(LineElement1))
			{
				( (TDSSObject*) LineElement1 )->Flag = IsShortLine( (TDSSCktElement*) LineElement1 );    /*Too small: Mark for merge with something*/
			} /*IF*/
			LineElement1 = ((TLineObj*) BranchList->Get_Forward());  // traverse the whole meter zone  (circuit tree)
		} /*WHILE*/
		LineElement1 = (TLineObj*) BranchList->Get_First();
		LineElement1 = (TLineObj*) BranchList->Get_Forward(); // Always keep the first element in the Tree
		while(LineElement1 != nullptr)
		{
			if(LineElement1->Get_Enabled())
			{
				if(!LineElement1->HasControl)
				{
					if(!LineElement1->IsMonitored)
					{
						if(LineElement1->Flag)    // else skip
   // Skip if controlled element or control is monitoring ,,,
  // too short; Try to merge this element out
						{
							/*# with BranchList do */
							{
								auto with0 = BranchList;
     //   {****} WriteDLLDebugFile(Format('Processing Line.%s Bus1=%s Bus2=%s',[Uppercase(LineElement1.Name), LineElement1.GetBus(1), LineElement1.GetBus(2)]));
								if((with0->PresentBranch->Get_NumChildren() == 0) && (with0->PresentBranch->Get_NumObjects() == 0))     // just discard it
									LineElement1->Set_Enabled(false);
								else
								{
									if(with0->PresentBranch->Get_NumChildren() == 0) /*****OR (PresentBranch.Get_NumChildren()>1)***/                    /*Merge with Parent and move shunt elements to TO node on parent branch*/
									{
										ParentNode = with0->PresentBranch->Get_Parent();
										if(ParentNode != nullptr)
										{
											if(ParentNode->Get_NumChildren() == 1)
											{
												if(!ActiveCircuit[ActiveActor]->Buses[with0->PresentBranch->Get_ToBusReference() - 1]->Keep)   // only works for in-line
     // Check Keeplist
													                               /*Let's consider merging*/
                                /*Get_First() Check for any Capacitors. Skip if any*/
													{
														MergeOK = true;
														if(ParentNode->Get_NumObjects() > 0)
														{
															shuntElement = ((TDSSCktElement*) ParentNode->Get_FirstObject());
															while(shuntElement != nullptr)
															{
																if(((shuntElement->DSSObjType & CLASSMASK) == CAP_ELEMENT) || ((shuntElement->DSSObjType & CLASSMASK) == REACTOR_ELEMENT))
																{
																	MergeOK = false;
																	break;
																}
																shuntElement = ((TDSSCktElement*) with0->PresentBranch->Get_NextObject());
															}  /*While*/
														}
														if(MergeOK)
														{
															LineElement2 = ((TLineObj*) ParentNode->CktObject);
															if(((TDSSCktElement*)LineElement2)->Get_Enabled())
															{
																if(IslineElement(LineElement2))
																{
																	if(LineElement2->MergeWith(LineElement1, SERIESMERGE))  // Check to make sure it hasn't been merged out
																		 /*Move any loads to ToBus Reference of parent branch*/
                          //    {****} WriteDLLDebugFile(Format('TOP Loop: Eliminating Line %s and merging into Line %s ',[Uppercase(LineElement1.Name), Uppercase(LineElement2.Name) ]));
																		{
																			if(ParentNode->Get_NumObjects() > 0)
                                         /*Redefine bus connection for PC elements hanging on the bus that is eliminated*/
																			{
																				shuntElement = ((TDSSCktElement*) ParentNode->Get_FirstObject());
																				while(shuntElement != nullptr)
																				{
																					Parser[ActiveActor]->SetCmdString(String("bus1=\"") + ActiveCircuit[ActiveActor]->BusList.Get(with0->PresentBranch->Get_ToBusReference())
	           + GetNodeString(shuntElement->GetBus(1))
	           + "\"");
                          //   {****} WriteDLLDebugFile(Format('Moving Shunt.%s from %s to %s ',[ShuntElement.Name, ShuntElement.GetBus(1), Parser.get_CmdBuffer() ]));
																					shuntElement->Edit(ActiveActor);
																					shuntElement = ((TDSSCktElement*) ParentNode->Get_NextObject());
																				}  /*While*/
																			} /*IF*/
                                   //+++ LineElement1 := BranchList.Get_Forward(); // skip to next branch since we eliminated a bus
																		}
																}
															}
														}
													} /*IF*/
											}
										} /*IF ParentNode*/
									}
									else
									{
										if(with0->PresentBranch->Get_NumChildren() == 1) /*Merge with child*/
										{
											if(!ActiveCircuit[ActiveActor]->Buses[with0->PresentBranch->Get_ToBusReference() - 1]->Keep)    // check keeplist
												
                         /*Let's consider merging*/
                          /*Get_First() Check for any Capacitors. Skip if any*/
												{
													MergeOK = true;
													if(with0->PresentBranch->Get_NumObjects() > 0)
													{
														shuntElement = ((TDSSCktElement*) with0->PresentBranch->Get_FirstObject());
														while(shuntElement != nullptr)
														{
															if(((shuntElement->DSSObjType & CLASSMASK) == CAP_ELEMENT) || ((shuntElement->DSSObjType & CLASSMASK) == REACTOR_ELEMENT))
															{
																MergeOK = false;
																break;
															}
															shuntElement = ((TDSSCktElement*) with0->PresentBranch->Get_NextObject());
														}  /*While*/
													}
													if(MergeOK)
													{
														LineElement2 = ((TLineObj*) with0->PresentBranch->Get_FirstChild()->CktObject); // child of PresentBranch
														if(((TDSSCktElement*)LineElement2)->Get_Enabled())
														{
															if(IslineElement(LineElement2))
															{
																if(LineElement2->MergeWith(LineElement1, SERIESMERGE))  // Check to make sure it hasn't been merged out

                      //  {****} WriteDLLDebugFile(Format('BOT Loop: Eliminating Line %s and merging into Line %s ',[Uppercase(LineElement1.Name), Uppercase(LineElement2.Name)]));
																{
																	if(with0->PresentBranch->Get_NumObjects() > 0)
                                   /*Redefine bus connection to upline bus*/
																	{
																		shuntElement = ((TDSSCktElement*) with0->PresentBranch->Get_FirstObject());
																		while(shuntElement != nullptr)
																		{
																			Parser[ActiveActor]->SetCmdString(String("bus1=\"") + ActiveCircuit[ActiveActor]->BusList.Get(with0->PresentBranch->FromBusReference)
	           + GetNodeString(shuntElement->GetBus(1))
	           + "\"");
                  //  {****} WriteDLLDebugFile(Format('Moving Shunt.%s from %s to %s ',[ShuntElement.Name, ShuntElement.GetBus(1), Parser.get_CmdBuffer() ]));
																			shuntElement->Edit(ActiveActor);
																			shuntElement = ((TDSSCktElement*) with0->PresentBranch->Get_NextObject());
																		}  /*While*/
																	} /*IF*/
																	LineElement1 = ((TLineObj*) BranchList->Get_Forward()); // skip to next branch since we eliminated a bus
																}
															}
														}
													}
												} /*IF not*/
										}
									} /*ELSE*/
								}
							}
						}
					}
				}
			}
			LineElement1 = ((TLineObj*) BranchList->Get_Forward());
		}
		/*# with ActiveCircuit[ActiveActor] do */
		{
			auto with3 = ActiveCircuit[ActiveActor];
			with3->ReProcessBusDefs(ActiveActor);  // to get new load added and account for disabled devices
			with3->DoResetMeterZones(ActiveActor);  // without eliminated devices
			with3->Solution->SystemYChanged = true; // force rebuild of Y
		}
	}
}
/*Eliminate short lines with impedance < Zmag and merge with lines on either side*/

void DoReduceSwitches(TCktTree*& BranchList)
{
	TLineObj* LineElement1 = nullptr;
	TLineObj* LineElement2 = nullptr;
	if(BranchList != nullptr)
	{
		LineElement1 = ((TLineObj*) BranchList->Get_First());
		LineElement1 = ((TLineObj*) BranchList->Get_Forward()); // Always keep the first element
		while(LineElement1 != nullptr)
		{
			if( ( (TDSSCktElement*) LineElement1 )->Get_Enabled())
			{
				if(IslineElement(LineElement1))
				{
					if(LineElement1->IsSwitch)
						/*# with BranchList.PresentBranch do */
						{
							auto with0 = BranchList->PresentBranch;
							switch(with0->Get_NumChildren())
							{   // maybe we threw it away already
								
             /*see if eligble for merging*/
								case 	0:
								if(with0->Get_NumObjects() == 0) /*Throw away if dangling*/
									((TDSSCktElement*)LineElement1)->Set_Enabled(false);
								break;
								case 	1:
								if(with0->Get_NumObjects() == 0)
								{
									if(!ActiveCircuit[ActiveActor]->Buses[with0->Get_ToBusReference() - 1]->Keep)
                     /*Let's consider merging*/
									{
										LineElement2 = ((TLineObj*) with0->Get_FirstChild()->CktObject);
										if(IslineElement(LineElement2))
										{
											if(!LineElement2->IsSwitch)/*Series Merge*/
												LineElement2->MergeWith(LineElement1, SERIESMERGE);
										}
									}
								}
								break; /*Nada*/
								default:
								  ;
								break;
							}
						}
				}
			}
			LineElement1 = ((TLineObj*) BranchList->Get_Forward());
		}
	}
}

/*Merge switches in with lines or delete if dangling*/

void DoReduceDefault(TCktTree*& BranchList)
{
	TLineObj* LineElement1 = nullptr;
	TLineObj* LineElement2 = nullptr;
	if(BranchList != nullptr)

     /*Now merge remaining lines*/
	{
		LineElement1 = ((TLineObj*) BranchList->Get_First());
		LineElement1 = ((TLineObj*) BranchList->Get_Forward()); // Always keep the first element
		while(LineElement1 != nullptr)
		{
			if(IslineElement(LineElement1))
			{
				if(!LineElement1->IsSwitch)
				{
					if(!((TDSSCktElement*)LineElement1)->HasControl)
					{
						if(!((TDSSCktElement*)LineElement1)->IsMonitored)
						{
							if(((TDSSCktElement*)LineElement1)->Get_Enabled())
								/*# with BranchList do */
								{
									auto with0 = BranchList;         // Exceptions
   // maybe we threw it away already
									
                 /*see if eligble for merging*/
									if(with0->PresentBranch->Get_NumChildren() == 1)
									{
										if(with0->PresentBranch->Get_NumObjects() == 0)
										{
											if(!ActiveCircuit[ActiveActor]->Buses[with0->PresentBranch->Get_ToBusReference() - 1]->Keep)
                     /*Let's consider merging*/
											{
												LineElement2 = ((TLineObj*) with0->PresentBranch->Get_FirstChild()->CktObject);
												if(IslineElement(LineElement2))
												{
													if(!LineElement2->IsSwitch)/*Series Merge*/
														LineElement2->MergeWith(LineElement1, SERIESMERGE);
												}
											}
										}
									}
								}
						}
					}
				}
			}
			LineElement1 = ((TLineObj*) BranchList->Get_Forward());
		}
	}
}

void DoRemoveBranches(TCktTree*& BranchList, TPDElement* FirstPDElement, bool KeepLoad, const String EditStr)
{
	TPDElement* PDElem = nullptr;
	String Busname;
	complex TotalkVA = {};
  // pLoad : TLoadObj;
	String NewLoadName;
	TDSSCktElement* pShunt = nullptr;
	TDSSBus* LoadBus = nullptr;
	double LoadBasekV = 0.0;
	int StartLevel = 0;

// Position BranchList at "FirstPDElement"
	PDElem = ((TPDElement*) BranchList->Get_First());
	while((PDElem != FirstPDElement) && (PDElem != nullptr))
		PDElem = ((TPDElement*) BranchList->Get_Forward());
	StartLevel = BranchList->Get_Level();
	if(PDElem == nullptr)
	{
		DoSimpleMsg(((TDSSObject*)FirstPDElement)->ParentClass->get_myClass_name() + "." + ((TDSSObject*)FirstPDElement)->get_Name() + " Not Found(Remove Command).", 5432100);
	}
	else


     /* If KeepLoad, create a new Load object at upstream bus (from bus).*/
	{
		if(KeepLoad)
			/*# with BranchList.PresentBranch do */
			{
				auto with0 = BranchList->PresentBranch;
				Busname = ( (TDSSCktElement*) FirstPDElement )->GetBus(with0->FromTerminal);
				TotalkVA = cdivreal( ( (TDSSCktElement*) PDElem )->Get_Power(with0->FromTerminal, ActiveActor), 1000.0);
				NewLoadName = "Eq_"+ ( (TDSSCktElement*) FirstPDElement )->get_Name() + "_" + StripExtension(Busname) ;
       /*Pick up the kV Base for the From bus*/
				LoadBus = ActiveCircuit[ActiveActor]->Buses[with0->FromBusReference - 1];
				if(LoadBus->kVBase > 0.0)
					LoadBasekV = LoadBus->kVBase;
				else
    // Try to guess from the present voltage at the first node on the bus
				{
					ActiveCircuit[ActiveActor]->Solution->UpdateVBus(ActiveActor);
					LoadBasekV = cabs((LoadBus->VBus)[1 - 1]) * 0.001;
				}
				if(((TDSSCktElement*)FirstPDElement)->Get_NPhases() > 1)
					LoadBasekV = LoadBasekV * SQRT3;
       /*Load up parser with definition of equivalent load*/
				Parser[ActiveActor]->SetCmdString(Format(" phases=%d Bus1=",  (FirstPDElement)->Get_NPhases() ) +
					Busname + Format(" kW=%g kvar=%g kV=%g ",  TotalkVA.re, TotalkVA.im, LoadBasekV ) + EditStr);
				AddObject("load", NewLoadName); // Add new load to circuit
			}

     /*Disable all elements in the tree downline from the start element*/

   //  {****} WriteDLLDebugFile(Format('StartLevel = %d ',[ StartLevel]));
		while(PDElem != nullptr)

   // {****} WriteDLLDebugFile(Format('Disabling cktelement %d %s.%s',[ BranchList.Level, PDelem.ParentClass.Name, PDElem.Name ]));
		{
			/*# with BranchList do */
			{
				auto with1 = BranchList;
				pShunt = ((TDSSCktElement*) with1->PresentBranch->Get_FirstObject());
				while(pShunt != nullptr)
				{
					pShunt->Set_Enabled(false);
   //  {****} WriteDLLDebugFile(Format('Disabling shunt element %s.%s',[ pShunt.ParentClass.Name, pShunt.Name ]));
					pShunt = ((TDSSCktElement*) with1->PresentBranch->Get_NextObject());
				}
			}
			( (TDSSCktElement*) PDElem )->Set_Enabled(false);
			PDElem = ((TPDElement*) BranchList->Get_Forward());

         // Check to see if we are back where we started. If so, stop.
			if(BranchList->Get_Level() <= StartLevel)
				PDElem = nullptr;
		}
	}
	/*# with ActiveCircuit[ActiveActor] do */
	{
		auto with3 = ActiveCircuit[ActiveActor];
		with3->ReProcessBusDefs(ActiveActor);  // to get new load added and account for disabled devices
		with3->DoResetMeterZones(ActiveActor);  // without eliminated devices
		with3->Solution->SystemYChanged = true; // force rebuild of Y
	}
}

void DoRemoveAll_1ph_Laterals(TCktTree*& BranchList)
{
	TPDElement* PDElem = nullptr;
	String Busname;
	TDSSCktElement* pShunt = nullptr;
	TDSSBus* HeadBus = nullptr;
	double HeadBasekV = 0.0;
	int StartLevel = 0;
	TDSSBus* pBus = nullptr;
	String strNodes;
 /*
  Just march down the feeder until we encounter a 1-phase PD element
 */
   // Position BranchList at "beginning"
	PDElem = ((TPDElement*) BranchList->Get_First());
	while(PDElem != nullptr)
	{
		if( ( (TDSSCktElement*) PDElem )->Get_NPhases() == 1)   // ELIMINATE THIS LATERAL
			
        /*Check to see if this is a 1-phase switch or other branch in the middle of a 3-phase branch and go on*/
        /*If the To bus has more than 1 phase, keep this branch else lump the load at the From node*/
			{
				pBus = ActiveCircuit[ActiveActor]->Buses[BranchList->PresentBranch->Get_ToBusReference() - 1];  //To Bus
				if(pBus->get_FNumNodesThisBus() == 1) // Eliminate the lateral starting with this branch
					

             /* If KeepLoad (ReduceLateralsKeepLoad), create a new Load object at upstream bus (from bus).*/
					{
						if(ActiveCircuit[ActiveActor]->ReduceLateralsKeepLoad)
							/*# with BranchList do */
							{
								auto with0 = BranchList;
								Busname = ((TDSSCktElement*)PDElem)->GetBus(with0->PresentBranch->FromTerminal);
                 // Make sure there is a node reference .. default to 1
								if(Pos(".", Busname) == 0)
									Busname = Busname + ".1";

                 /*Pick up the kV Base for the From bus*/
								HeadBus = ActiveCircuit[ActiveActor]->Buses[with0->PresentBranch->FromBusReference - 1];
								if(HeadBus->kVBase > 0.0)
									HeadBasekV = HeadBus->kVBase;
								else
    // Try to guess voltage base from the present voltage at the first node on the bus
								{
									ActiveCircuit[ActiveActor]->Solution->UpdateVBus(ActiveActor);
									HeadBasekV = cabs((HeadBus->VBus)[1 - 1]) * 0.001;
								}
							}

             /*

               Disable all PDelements in the tree downline from the beginning of the 1-phase lateral
               Move 1-phase shunts to Headbus

             */
						StartLevel = BranchList->Get_Level();   // record level of first 1-phase branch in this lateral
						while(PDElem != nullptr)

            // {****} WriteDLLDebugFile(Format('Disabling cktelement %d %s.%s',[ BranchList.Level, PDelem.ParentClass.Name, PDElem.Name ]));
						                 /* Get rid of loads and other shunt elements connected to this branch */
						{
							/*# with BranchList do */
							{
								auto with1 = BranchList;
								pShunt = ((TDSSCktElement*) with1->PresentBranch->Get_FirstObject());
								while(pShunt != nullptr)
								{
									Parser[ActiveActor]->SetCmdString(Format("Bus1=%s kV=%.6g ",Busname.c_str(), HeadBasekV));
									pShunt->Edit(ActiveActor);
									pShunt = ((TDSSCktElement*) with1->PresentBranch->Get_NextObject());
								}
							}
							((TDSSCktElement*)PDElem)->Set_Enabled(false);
							PDElem = ((TPDElement*) BranchList->Get_Forward());

                 // Check to see if we are back where we started. If so, stop with this lateral and get on to the next.
							if(PDElem != nullptr)
							{
								if(BranchList->Get_Level() <= StartLevel)
									break;
							}
						}
					}
				else
					PDElem = ((TPDElement*) BranchList->Get_Forward());
			}
		else
			PDElem = ((TPDElement*) BranchList->Get_Forward());


   // {****} If PDElem<>Nil then WriteDLLDebugFile(Format('Going on.. cktelement %d %s.%s phases=%d',[ BranchList.Level, PDelem.ParentClass.Name, PDElem.Name, PDelem.NPhases  ]));
	}
	/*# with ActiveCircuit[ActiveActor] do */
	{
		auto with3 = ActiveCircuit[ActiveActor];

		with3->ReProcessBusDefs(ActiveActor);  // to get new load added and account for disabled devices
		with3->DoResetMeterZones(ActiveActor);  // without eliminated devices
		with3->Solution->SystemYChanged = true; // force rebuild of Y
	}
}
/*Remove all 1-phase laterals in Branchlist and lump total load back to main feeder*/
/*
  This removes all elements on all 1ph laterals and moves the net load back to the main feeder tap point
  Does not
*/




}  // namespace ReduceAlgs





