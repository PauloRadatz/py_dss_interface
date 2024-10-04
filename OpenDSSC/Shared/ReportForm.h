#ifndef ReportFormH
#define ReportFormH

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



class TReport : public TForm
{
__published:
	TMemo* Memo1;
private:
    /* Private declarations */
public:
    /* Public declarations */
	typedef TForm inherited;	
	TReport(System::Classes::TComponent* AOwner);
};
extern PACKAGE TReport* Report;
#endif // ReportFormH




