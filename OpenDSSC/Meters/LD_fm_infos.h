#ifndef LD_fm_infosH
#define LD_fm_infosH

#include "System.h"

#include "Ucomplex.h"

namespace LD_fm_infos
{



/*
  ----------------------------------------------------------
  Copyright (c) 2017, Electric Power Research Institute, Inc.
  Added by Ying.
  ----------------------------------------------------------

  Definition of Fmonitor (virtue leader) Public Data Record
*/
      //PointerList;


   /*Fmonitor public data/state variable structure*/
#pragma pack (push, 1)

   //properties for Nodes
         // highest voltage node

struct TLD_fm_infos
{
	int ndnum_hghst;
	bool b_ctrl_hghst; //can contribute more to the high volt problem
	double volt_hghst;    //low volt in pu
	double volt_hgh_lmt;   //low limit in pu
	double Pinjec_hghst;  //net P injection on this node

         // lowest voltage node
	int ndnum_lwst;
	bool b_ctrl_lwst; //can contribute more to the high volt problem
	double volt_lwst;  //low volt in pu
	double volt_lw_lmt; //low limit in pu
	double Pinjec_lwst; // net P injection on this node

         // overview information
	double volt_avg;
	double total_pg; //total generation of this cluster
	double total_pl; //total load of this cluster
	bool b_Curt_Ctrl;
};
#pragma pack (pop)



}  // namespace LD_fm_infos

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace LD_fm_infos;
#endif

#endif // LD_fm_infosH





