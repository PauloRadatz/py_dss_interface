#include <vcl.h>
#pragma hdrstop

#include "Frm_RPNcalc.h"
#include "../Parser/ParserDel.h"

using namespace std;
using namespace d2c_system;
using namespace ParserDel;
using namespace System;
using namespace System::Classes;
using namespace System::Sysutils;
using namespace System::Uitypes;
using namespace Vcl::Forms;


TRPNForm::TRPNForm(System::Classes::TComponent* AOwner) : inherited(AOwner) {}


TRPNForm* RPNForm = nullptr;
#pragma resource "*.DFM" 

TParser* Parser = nullptr;

void TRPNForm::FormCreate(TObject* Sender)
{
	if (Parser != nullptr)
		delete Parser;
	Parser = new TParser();
}

void TRPNForm::Button1Click(TObject* Sender)
{
	Parser->CmdString = String(L"[") + ParserIn->Text + L"]";
	Parser->NextParam;
	Answer = Parser->DblValue;
	ParserOut->Text = Format(L"%10.5g", ARRAYOFCONST((Answer)));
}

void TRPNForm::OK_ButtonClick(TObject* Sender)
{
	ParserOut->SelectAll();
	ParserOut->CopyToClipboard();
	Cancelled = false;
	ModalResult = (TModalResult) mrOk;
}

void TRPNForm::Cancel_ButtonClick(TObject* Sender)
{
	Cancelled = true;
	ModalResult = (TModalResult) mrCancel;
}





