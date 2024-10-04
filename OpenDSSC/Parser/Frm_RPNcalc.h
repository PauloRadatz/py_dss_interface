#ifndef Frm_RPNcalcH
#define Frm_RPNcalcH

#include <System.hpp>
#include "../Support/d2c_system.h"

#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.StdCtrls.hpp>



/*
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

class TRPNForm : public TForm
{
__published:
	TLabel* Label1;
	TEdit* ParserIn;
	TButton* Button1;
	TLabel* Label2;
	TEdit* ParserOut;
	TButton* Cancel_Button;
	TLabel* Label3;
	TButton* OK_Button;
	TLabel* Label10;
	TLabel* Label11;
	TLabel* Label12;
	TLabel* Label5;
	TLabel* Label16;
	TLabel* Label8;
	TGroupBox* GroupBox1;
	TLabel* Label4;
	TLabel* Label9;
	TLabel* Label13;
	TLabel* Label14;
	TLabel* Label15;
	TLabel* Label6;
	TLabel* Label17;
	TLabel* Label18;
	void FormCreate(TObject* Sender);
	void Button1Click(TObject* Sender);
	void OK_ButtonClick(TObject* Sender);
	void Cancel_ButtonClick(TObject* Sender);
private:
    /* Private declarations */
public:
    /* Public declarations */
	double Answer;
	bool Cancelled;
	typedef TForm inherited;	
	TRPNForm(System::Classes::TComponent* AOwner);
};
extern PACKAGE TRPNForm* RPNForm;
#endif // Frm_RPNcalcH




