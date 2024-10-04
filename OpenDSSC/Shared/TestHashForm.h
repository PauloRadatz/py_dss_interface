#ifndef TestHashFormH
#define TestHashFormH

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



class TTestForm : public TForm
{
__published:
	TButton* Button1;
	TEdit* Edit1;
	TButton* Button2;
	TEdit* Edit2;
	TButton* Button3;
	TButton* Button4;
	TButton* Button5;
	TEdit* Edit3;
	TButton* Button6;
	TLabel* Label1;
	TButton* Button7;
	void Button1Click(TObject* Sender);
	void Button2Click(TObject* Sender);
	void Button4Click(TObject* Sender);
	void Button3Click(TObject* Sender);
	void Button5Click(TObject* Sender);
	void Button6Click(TObject* Sender);
	void Button7Click(TObject* Sender);
private:
    /* Private declarations */
public:
    /* Public declarations */
	typedef TForm inherited;	
	TTestForm(System::Classes::TComponent* AOwner);
};
extern PACKAGE TTestForm* TestForm;
#endif // TestHashFormH




