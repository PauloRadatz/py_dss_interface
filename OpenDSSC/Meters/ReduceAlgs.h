#ifndef ReduceAlgsH
#define ReduceAlgsH

#include "System.h"
#include "Sysutils.h"

#include "CktTree.h"
#include "PDElement.h"
#include "d2c_structures.h"




namespace ReduceAlgs
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*Reduction Algorithms*/

/*Primarily called from EnergyMeter*/
void DoReduceDefault(CktTree::TCktTree*& BranchList);
void DoReduceShortLines(CktTree::TCktTree*& BranchList);
void DoReduceDangling(CktTree::TCktTree*& BranchList);
  /*{procedure DoReduceTapEnds(var BranchList:TCktTree);*/
void DoBreakLoops(CktTree::TCktTree*& BranchList);
void DoMergeParallelLines(CktTree::TCktTree*& BranchList);
void DoReduceSwitches(CktTree::TCktTree*& BranchList);
void DoRemoveAll_1ph_Laterals(CktTree::TCktTree*& BranchList);
void DoRemoveBranches(CktTree::TCktTree*& BranchList, PDELement::TPDElement* FirstPDElement, bool KeepLoad, const String EditStr);


}  // namespace ReduceAlgs

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace ReduceAlgs;
#endif

#endif // ReduceAlgsH





