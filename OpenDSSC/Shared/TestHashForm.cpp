#include <vcl.h>
#pragma hdrstop

#include "TestHashForm.h"
#include "../Shared/HashList.h"
#include "../Shared/Command.h"
#include "../Support/d2c_sysfile.h"
#include "d2c_openarray.h"

using namespace std;
using namespace d2c_system;
using namespace Command;
using namespace Hashlist;
using namespace System;
using namespace System::Classes;
using namespace System::Sysutils;
using namespace System::Uitypes;
using namespace Vcl::Dialogs;

#define TestHashForm__0 (TMsgDlgButtons() << System::Uitypes::TMsgDlgBtn::mbOK)

TTestForm::TTestForm(System::Classes::TComponent* AOwner) : inherited(AOwner) {}


TTestForm* TestForm = nullptr;
#pragma resource "*.DFM" 

THashList* HashLst = nullptr;
TCommandList* cmdList = nullptr;

void TTestForm::Button1Click(TObject* Sender)
{
	d2c_system::TTextRec f = {};
	String s;
	int i = 0;
	HashLst = new THashList(50);
	try
	{
		AssignFile(f, L"HashNames.Txt");
		Reset(f);
		IOResultToException();
		while(!Eof(f))
		{
			ReadLn(f, s);
			i = HashLst->Add(s);
		}
	}
	catch(...)
	{
		MessageDlg(L"Error with input file", System::Uitypes::TMsgDlgType::mtError, TestHashForm__0, 0);
	}
	CloseFile(f);
	HashLst->DumpToFile();
}

void TTestForm::Button2Click(TObject* Sender)
{
	Edit2->Text = IntToStr(HashLst->Find(Edit1->Text));
}

void TTestForm::Button4Click(TObject* Sender)
{
	int i = 0;
	i = HashLst->FindAbbrev(Edit1->Text);
	Edit2->Text = IntToStr(i);
	Edit1->Text = HashLst->Get((unsigned int) i);
}

void TTestForm::Button3Click(TObject* Sender)
{
	Edit1->Text = HashLst->Get((unsigned int) StrToInt(Edit2->Text));
}

void TTestForm::Button5Click(TObject* Sender)
{
	String cmd[10/*# range 1..10*/];
	cmd[1 - 1] = L"AddCapacitor";
	cmd[2 - 1] = L"Secondcommand";
	cmd[3 - 1] = L"Thirdcommand";
	cmd[4 - 1] = L"4thcommand";
	cmd[5 - 1] = L"5thcommand";
	cmd[6 - 1] = L"6thcommand";
	cmd[7 - 1] = L"7thcommand";
	cmd[8 - 1] = L"8thcommand";
	cmd[9 - 1] = L"9thcommand";
	cmd[10 - 1] = L"Lastcommand";
	cmdList = new TCommandList(OpenArrayEx<String>(cmd, 10), 9);
}

void TTestForm::Button6Click(TObject* Sender)
{
	Label1->Caption = IntToStr(cmdList->Getcommand(Edit3->Text));
}

void TTestForm::Button7Click(TObject* Sender)
{
	cmdList->set_AbbrevAllowed(!cmdList->get_AbbrevAllowed);
}





