
#pragma hdrstop

#include "XYcurve.h"

#include "DSSClassDefs.h"
#include "DSSGlobals.h"

using namespace std;

namespace XYCurve
{

	//TXYcurveObj::TXYcurveObj(TDSSClass* ParClass, const string XYCurveName) : inherited(ParClass->get_Name()) {}
	TXYcurveObj::TXYcurveObj(string ClassName) : inherited(ClassName) {}
	TXYcurveObj::TXYcurveObj() {}


	TXYcurveObj* ActiveXYcurveObj = nullptr;
	const int NumPropsThisClass = 13;

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	  // Creates superstructure for all Line objects

	TXYcurve::TXYcurve()
	{
		TempPointsBuffer.clear();
		Class_Name = "XYcurve";
		DSSClassType = DSS_OBJECT;
		ActiveElement = 0;
		DefineProperties();
		std::string* slc = Slice(PropertyName, NumProperties);
		CommandList = TCommandList(slc, NumProperties);
		delete[] slc;
		CommandList.set_AbbrevAllowed(true);
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	TXYcurve::~TXYcurve()
	{

		// ElementList and  CommandList freed in inherited destroy
		// inherited::Destroy();
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	void TXYcurve::DefineProperties()
	{
		NumProperties = NumPropsThisClass;
		CountProperties();   // Get inherited property count
		AllocatePropertyArrays();


		 // Define Property names
		PropertyName[1 - 1] = "npts";     // Number of points to expect
		PropertyName[2 - 1] = "Points";
		PropertyName[3 - 1] = "Yarray";     // vector of Y values
		PropertyName[4 - 1] = "Xarray";     // vector of X values corresponding to Y values
		PropertyName[5 - 1] = "csvfile";  // Switch input to a csvfile
		PropertyName[6 - 1] = "sngfile";  // switch input to a binary file of singles
		PropertyName[7 - 1] = "dblfile";    // switch input to a binary file of singles
		PropertyName[8 - 1] = "x";
		PropertyName[9 - 1] = "y";
		PropertyName[10 - 1] = "Xshift";
		PropertyName[11 - 1] = "Yshift";
		PropertyName[12 - 1] = "Xscale";
		PropertyName[13 - 1] = "Yscale";

		 // define Property help values
		PropertyHelp[1 - 1] = "Max number of points to expect in curve. This could get reset to the actual number of points defined "
				   "if less than specified.";     // Number of points to expect
		PropertyHelp[2 - 1] = String("One way to enter the points in a curve. Enter x and y values as one array " "in the order [x1, y1, x2, y2, ...]. For example:") + CRLF
				   + CRLF
				   + "Points=[1,100 2,200 3, 300] "
				   + CRLF
				   + CRLF
				   + "Values separated by commas or white space. Zero fills arrays if insufficient number of values.";
		PropertyHelp[3 - 1] = String("Alternate way to enter Y values. Enter an array of Y values corresponding to the X values.  " "You can also use the syntax: ") + CRLF
				   + "Yarray = (file=filename)     !for text file one value per line"
				   + CRLF
				   + "Yarray = (dblfile=filename)  !for packed file of doubles"
				   + CRLF
				   + "Yarray = (sngfile=filename)  !for packed file of singles "
				   + CRLF
				   + CRLF
				   + "Note: this property will reset Npts to a smaller value if the  number of values in the files are fewer.";     // vextor of hour values
		PropertyHelp[4 - 1] = String("Alternate way to enter X values. Enter an array of X values corresponding to the Y values.  " "You can also use the syntax: ") + CRLF
				   + "Xarray = (file=filename)     !for text file one value per line"
				   + CRLF
				   + "Xarray = (dblfile=filename)  !for packed file of doubles"
				   + CRLF
				   + "Xarray = (sngfile=filename)  !for packed file of singles "
				   + CRLF
				   + CRLF
				   + "Note: this property will reset Npts to a smaller value if the  number of values in the files are fewer.";     // vextor of hour values
		PropertyHelp[5 - 1] = "Switch input of  X-Y curve data to a CSV file "
				   "containing X, Y points one per line. "
				   "NOTE: This action may reset the number of points to a lower value.";   // Switch input to a csvfile
		PropertyHelp[6 - 1] = "Switch input of  X-Y curve data to a binary file of SINGLES "
				   "containing X, Y points packed one after another. "
				   "NOTE: This action may reset the number of points to a lower value.";  // switch input to a binary file of singles
		PropertyHelp[7 - 1] = "Switch input of  X-Y  curve data to a binary file of DOUBLES "
				   "containing X, Y points packed one after another. "
				   "NOTE: This action may reset the number of points to a lower value.";   // switch input to a binary file of singles
		PropertyHelp[8 - 1] = "Enter a value and then retrieve the interpolated Y value from the Y property. On input shifted then scaled to original curve. Scaled then shifted on output.";
		PropertyHelp[9 - 1] = "Enter a value and then retrieve the interpolated X value from the X property. On input shifted then scaled to original curve. Scaled then shifted on output.";
		PropertyHelp[10 - 1] = "Shift X property values (in/out) by this amount of offset. Default = 0. Does not change original definition of arrays.";
		PropertyHelp[11 - 1] = "Shift Y property values (in/out) by this amount of offset. Default = 0. Does not change original definition of arrays.";
		PropertyHelp[12 - 1] = "Scale X property values (in/out) by this factor. Default = 1.0. Does not change original definition of arrays.";
		PropertyHelp[13 - 1] = "Scale Y property values (in/out) by this factor. Default = 1.0. Does not change original definition of arrays.";
		ActiveProperty = NumPropsThisClass - 1;
		inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	int TXYcurve::NewObject(const String ObjName)
	{
		int result = 0;
	   // create a new object of this class and add to list
		/*# with ActiveCircuit[ActiveActor] do */
		{
		
			ActiveDSSObject[ActiveActor] = new TXYcurveObj(this, ObjName);
			result = AddObjectToList(ActiveDSSObject[ActiveActor]);
		}
		return result;
	}


	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	int TXYcurve::Edit(int ActorID)
	{
		int result = 0;
		int ParamPointer = 0;
		String ParamName;
		String Param;
		int i = 0;
		result = 0;
	  // continue parsing with contents of Parser
		ActiveXYcurveObj = ((TXYcurveObj*) ElementList.Get_Active());
		ActiveDSSObject[ActorID] = ActiveXYcurveObj;
		/*# with ActiveXYcurveObj do */
		{
			auto with0 = ActiveXYcurveObj;
			ParamPointer = 0;
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
			while(Param.length() > 0)
			{
				if(ParamName.length() == 0)
					++ParamPointer;
				else
					ParamPointer = CommandList.Getcommand(ParamName);
				if((ParamPointer > 0) && (ParamPointer <= NumProperties))
					with0->Set_PropertyValue(ParamPointer,Param);
				switch(ParamPointer)
				{
					case 	0:
					DoSimpleMsg(String("Unknown parameter \"") + ParamName
				   + "\" for Object \""
				   + Class_Name
				   + "."
				   + with0->get_Name()
				   + "\"", 610);
					break;
					case 	1:
					with0->Set_NumPoints(Parser[ActorID]->MakeInteger_());
					break;
					case 	2:
					{
						int stop = 0;
						TempPointsBuffer.resize(with0->FNumPoints * 2);
					 // Allow possible Resetting (to a lower value) of num points when specifying temperatures not Hours
						with0->Set_NumPoints( (int)(InterpretDblArray(Param, (with0->FNumPoints * 2), &(TempPointsBuffer[0])) / 2));  // Parser.ParseAsVector(Npts, Temperatures);
						with0->YValues.resize(with0->FNumPoints);
						with0->XValues.resize(with0->FNumPoints);
						for(stop = with0->FNumPoints, i = 1; i <= stop; i++)
						{
							with0->XValues[i - 1] = TempPointsBuffer[2 * i - 0];
							with0->YValues[i - 1] = TempPointsBuffer[2 * i];
						}
						with0->Set_X(with0->XValues[1]);
						with0->Set_Y(with0->YValues[1]);
						TempPointsBuffer.clear();  // Throw away temp array
					}
					break;
					case 	3:
					{
						with0->YValues.resize(with0->get_FNumPoints());
						with0->Set_NumPoints( InterpretDblArray(Param, with0->get_FNumPoints(), &(with0->YValues[0])));
						with0->Set_Y(with0->YValues[0]);
					}
					break;
					case 	4:
					{
						with0->XValues.resize(with0->get_FNumPoints());
						with0->Set_NumPoints(InterpretDblArray(Param, with0->get_FNumPoints(), &(with0->XValues[0])));
						with0->Set_X(with0->XValues[0]);
					}
					break;
					case 	5:
					DoCSVFile(Param);
					break;   // file of x,y points, one to a line
					case 	6:
					DoSngFile(Param);
					break;
					case 	7:
					DoDblFile(Param);
					break;
					case 	8:
					with0->Set_X(Parser[ActorID]->MakeDouble_());
					break;
					case 	9:
					with0->Set_Y(Parser[ActorID]->MakeDouble_());
					break;
					case 	10:
					with0->FXshift = Parser[ActorID]->MakeDouble_();
					break;
					case 	11:
					with0->FYshift = Parser[ActorID]->MakeDouble_();
					break;
					case 	12:
					with0->FXscale = Parser[ActorID]->MakeDouble_();
					break;
					case 	13:
					with0->FYscale = Parser[ActorID]->MakeDouble_();
					break;
			   // Inherited parameters
					default:
					ClassEdit(ActiveXYcurveObj, ParamPointer - NumPropsThisClass);
					break;
				}
				switch(ParamPointer)
				{
					case 5: case 6: case 7:
					{
						if ((!with0->XValues.empty()) && (!with0->YValues.empty()))
						{
							with0->Set_X(with0->XValues[1 - 1]);
							with0->Set_Y(with0->YValues[1 - 1]);
						}
					}
					break;
					default:
					  ;
					break;
				}
				switch(ParamPointer)
				{
					case 2: case 3: case 4: case 5: case 6: case 7:
					{
						with0->ArrayPropertyIndex = ParamPointer;
						with0->Set_NumPoints(with0->get_FNumPoints());  // Keep Properties in order for save command
						with0->LastValueAccessed = 1;
					}
					break;
					default:
					  ;
					break;
				}
				ParamName = Parser[ActorID]->GetNextParam();
				Param = Parser[ActorID]->MakeString_();
			} /*While*/
		} /*WITH*/
		return result;
	}

	void* TXYcurve::Find(const String ObjName)
	{
		void* result = nullptr;
		if((ObjName.length() == 0) || (CompareText(ObjName, "none") == 0))
			result = nullptr;
		else
			result = inherited::Find(ObjName);
		return result;
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	int TXYcurve::MakeLike(const String CurveName)
	{
		int result = 0;
		TXYcurveObj* OtherXYCurve = nullptr;
		int i = 0;
		result = 0;
	   /*See if we can find this curve in the present collection*/
		OtherXYCurve = ((TXYcurveObj*) Find(CurveName));
		if(OtherXYCurve != nullptr)
			/*# with ActiveXYcurveObj do */
			{
				auto with0 = ActiveXYcurveObj;
				int stop = 0;
				with0->Set_NumPoints(OtherXYCurve->get_FNumPoints());
				with0->XValues.resize(with0->get_FNumPoints());
				with0->YValues.resize(with0->get_FNumPoints());
				for(stop = with0->get_FNumPoints(), i = 1; i <= stop; i++)
				{
					with0->XValues[i - 1] = OtherXYCurve->XValues[i - 1];
					with0->YValues[i - 1] = OtherXYCurve->YValues[i];
				}
				with0->FXshift = OtherXYCurve->FXshift;
				with0->FYshift = OtherXYCurve->FYshift;
				with0->FXscale = OtherXYCurve->FXscale;
				with0->FYscale = OtherXYCurve->FYscale;
				for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
				{
					with0->Set_PropertyValue(i,OtherXYCurve->Get_PropertyValue(i));
				}
			}
		else
			DoSimpleMsg(String("Error in XYCurve MakeLike: \"") + CurveName
				   + "\" Not Found.", 611);
		return result;
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	int TXYcurve::Init(int Handle, int ActorID)
	{
		int result = 0;
		DoSimpleMsg("Need to implement TXYcurve.Init", -1);
		result = 0;
		return result;
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	  // Returns active line code string

	String TXYcurve::Get_Code()
	{
		String result;
		TXYcurveObj* XYCurveObj = nullptr;
		XYCurveObj = ((TXYcurveObj*) ElementList.Get_Active());
		result = XYCurveObj->get_Name();
		return result;
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	  // sets the  active TShape

	void TXYcurve::Set_Code(const String Value)
	{
		TXYcurveObj* XYCurveObj = nullptr;
		ActiveXYcurveObj = nullptr;
		XYCurveObj = ((TXYcurveObj*) ElementList.Get_First());
		while(XYCurveObj != nullptr)
		{
			if(CompareText(XYCurveObj->get_Name(), Value) == 0)
			{
				ActiveXYcurveObj = XYCurveObj;
				return;
			}
			XYCurveObj = ((TXYcurveObj*) ElementList.Get_Next());
		}
		DoSimpleMsg(String("XYCurve: \"") + Value + "\" not Found.", 612);
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	void TXYcurve::DoCSVFile(const String FileName)
	{
		System::TTextRec f = {};
		int i = 0;
		String dummy, s;
		try
		{
			System::AssignFile(f, FileName);
			System::Reset(f);
			IOResultToException();
		}
		catch(...)
		{
			DoSimpleMsg("Error Opening File: " + FileName, 613);
			System::CloseFile(f); System::InOutRes = 0;
			return;
		}
		try
		{
			/*# with ActiveXYcurveObj do */
			{
				auto with0 = ActiveXYcurveObj;
				with0->XValues.resize(with0->get_FNumPoints());
				with0->YValues.resize(with0->get_FNumPoints());
				i = 0;
				while((!Eof(f)) && (i < with0->FNumPoints))
				{
					++i;
					System::ReadLn(f, s); // read entire line  and parse with AuxParser
				/*AuxParser allows commas or white space*/
					/*# with AuxParser[ActiveActor] do */
					auto with1 = AuxParser[ActiveActor];
					{
						with1->SetCmdString(s);
						dummy = with1->GetNextParam();
						with0->XValues[i] = with1->MakeDouble_();
						dummy = with1->GetNextParam();
						with0->YValues[i] = with1->MakeDouble_();
					}
				}
				System::CloseFile(f);
				if(i != with0->FNumPoints)
					with0->Set_NumPoints(i);
			}
		}
		catch(exception & e)
		{
			{
				DoSimpleMsg(String("Error Processing XYCurve CSV File: \"") + FileName
				   + ". "
				   + ( string ) e.what(), 614);
				System::CloseFile(f); System::InOutRes = 0;
				return;
			}
		}
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	void TXYcurve::DoSngFile(const String FileName)
	{
		System::TTypedFile<float> f;
		float sX = 0.0F;
		float sY = 0.0F;
		int i = 0;
		try
		{
			System::AssignFile(f, FileName);
			System::Reset(f);
			IOResultToException();
		}
		catch(...)
		{
			DoSimpleMsg(String("Error Opening File: \"") + FileName, 615);
			System::CloseFile(f); System::InOutRes = 0;
			return;
		}
		try
		{
			/*# with ActiveXYcurveObj do */
			{
				auto with0 = ActiveXYcurveObj;
				with0->XValues.resize(with0->get_FNumPoints());
				with0->YValues.resize(with0->get_FNumPoints());
				i = 0;
				while((!Eof(f)) && (i < with0->get_FNumPoints()))
				{
					++i;
					System::Read(f, &sX);
					with0->XValues[i - 1] = sX;
					System::Read(f, &sY);
					with0->YValues[i - 1] = sY;
				}
				System::CloseFile(f);
				if(i != with0->get_FNumPoints())
					with0->Set_NumPoints(i);
			}
		}
		catch(...)
		{
			DoSimpleMsg(String("Error Processing binary (single) XYCurve File: \"") + FileName, 616);
			System::CloseFile(f); System::InOutRes = 0;
			return;
		}
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	void TXYcurve::DoDblFile(const String FileName)
	{
		System::TTypedFile<double> f;
		int i = 0;
		try
		{
			System::AssignFile(f, FileName);
			System::Reset(f);
			IOResultToException();
		}
		catch(...)
		{
			DoSimpleMsg(String("Error Opening File: \"") + FileName, 617);
			System::CloseFile(f); System::InOutRes = 0;
			return;
		}
		try
		{
			/*# with ActiveXYcurveObj do */
			{
				auto with0 = ActiveXYcurveObj;
				with0->XValues.resize(with0->get_FNumPoints());
				with0->YValues.resize(with0->get_FNumPoints());
				i = 0;
				while((!Eof(f)) && (i < with0->get_FNumPoints()))
				{
					++i;
					System::Read(f, &with0->XValues[i - 1]);
					System::Read(f, &with0->YValues[i - 1]);
				}
				System::CloseFile(f);
				if(i != with0->get_FNumPoints())
					with0->Set_NumPoints(i);
			}
		}
		catch(...)
		{
			DoSimpleMsg(String("Error Processing binary (double) XYCurve File: \"") + FileName, 618);
			System::CloseFile(f); System::InOutRes = 0;
			return;
		}
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	//      TTShape Obj
	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	TXYcurveObj::TXYcurveObj(TDSSClass* ParClass, const String XYCurveName)
	 : inherited(ParClass),
				LastValueAccessed(1),
				FNumPoints(0),
				ArrayPropertyIndex(0),
				FX(0.0),
				FY(0.0),
				FXshift(0.0),
				FYshift(0.0),
				FXscale(0.0),
				FYscale(0.0)
	{
		Set_Name(LowerCase(XYCurveName));
		DSSObjType = ParClass->DSSClassType;
		XValues.clear();
		YValues.clear();
		FX = 0.0;
		FY = 0.0;
		FXshift = 0.0;
		FYshift = 0.0;
		FXscale = 1.0;
		FYscale = 1.0;
		InitPropertyValues(0);
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	TXYcurveObj::~TXYcurveObj()
	{
		if(!XValues.empty())
			XValues.clear();
		if(!YValues.empty())
			YValues.clear();
		// inherited::Destroy();
	}


	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


	// This function returns the interpolated Y value for the given X.
	// If no points exist in the curve, the result is  0.0
	// If Xvalue is outside the range of defined X values,
	// the curve is extrapolated from the Ends.

	double TXYcurveObj::GetYValue_(double X)
	{
		double result = 0.0;
		int i = 0;
		result = 0.0;    // default return value if no points in curve
		if(FNumPoints > 0)
		{
			if(FNumPoints == 1)         // Handle Exceptional cases
				result = YValues[1 - 1];
			else


		/* Start with previous value accessed under the assumption that most
		  of the time, the values won't change much*/
			{
				int stop = 0;
				if(XValues[LastValueAccessed - 1] > X)
					LastValueAccessed = 1; // Start over from Beginning

		 // if off the curve for the first point, extrapolate from the first two points
				if((LastValueAccessed == 1) && (XValues[1 - 1] > X))
				{
					result = InterpolatePoints(1, 2, X, &(XValues[0]), &(YValues[0]));
					return result;
				}

		 // In the middle of the arrays
				for(stop = FNumPoints, i = LastValueAccessed + 1; i <= stop; i++)
				{
					if(Abs((XValues[i - 1] - X)) < 0.00001)  // If close to an actual point, just use it.
					{
						result = YValues[i - 1];
						LastValueAccessed = i;
						return result;
					}
					else
					{
						if(XValues[i - 1] > X)
			 // INTERPOLATE between two values
						{
							LastValueAccessed = i - 1;
							result = InterpolatePoints(i, LastValueAccessed, X, &(XValues[0]), &(YValues[0]));
							return result;
						}
					}
				}

		 // If we fall through the loop, Extrapolate from last two points
				LastValueAccessed = FNumPoints - 1;
				result = InterpolatePoints(FNumPoints, LastValueAccessed, X, &(XValues[0]), &(YValues[0]));
			}
		}
		return result;
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


	// This function returns the coefficients of the line interpolated line for the given X (a*X + b).
	// If no points exist in the curve (or just a single point), the result is  (a = 0, b = 0)
	// If Xvalue is outside the range of defined X values,
	// the curve is extrapolated from the Ends (a = 0, b = extrapolated value)

	TCoeff TXYcurveObj::GetCoefficients(double X)
	{
		int i = 0;
	//   coef: pDoubleArray;
		TCoeff coef;
		TCoeff result;

	  // default return value if no points in curve
		coef[0] = 0.0;
		coef[1] = 0.0;
		result = coef;
		if(FNumPoints > 0)
		{
			if (FNumPoints == 1)         // Handle Exceptional cases
			{
				result = coef;
			}

			else


		/* Start with previous value accessed under the assumption that most
		  of the time, the values won't change much*/
			{
				int stop = 0;
				if(XValues[LastValueAccessed - 1] > X)
					LastValueAccessed = 1; // Start over from Beginning

		  // if off the curve for the first point, extrapolate from the first two points
				if((LastValueAccessed == 1) && (XValues[0] > X))

			// Assume the same coefficients determined by the first two points. Necessary to keep
			// consistency with TXYcurveObj.GetYValue function.
				{
					coef[0] = (YValues[1] - YValues[0]) / (XValues[1] - XValues[0]);
					coef[1] = YValues[1] - coef[0] * XValues[1];
					result = coef;
					return result;
				}

		  // In the middle of the arrays
				for(stop = FNumPoints, i = LastValueAccessed; i <= stop; i++)
				{
					if(XValues[i - 1] > X)
			  // INTERPOLATE between two values
					{
						LastValueAccessed = i - 1;
						coef[0] = (YValues[i - 1] - YValues[i - 2]) / (XValues[i - 1] - XValues[i - 2]);
						coef[1] = YValues[i - 1] - coef[0] * XValues[i - 1];
						result = coef;
						return result;
					}
				}


		  // Assume the same coefficients determined by the last two points. Necessary to keep
		  // consistency with TXYcurveObj.GetYValue function.
				coef[0] = (YValues[FNumPoints - 1] - YValues[FNumPoints - 2]) / (XValues[FNumPoints - 1] - XValues[FNumPoints - 2]);
				coef[1] = YValues[FNumPoints - 1] - coef[0] * XValues[FNumPoints - 1];
				result = coef;
			}
		}
		return result;
	}

	double TXYcurveObj::Get_Y()
	{
		double result = 0.0;
		result = FY * FYscale + FYshift;
		return result;
	}

	double TXYcurveObj::Get_YValue(int i)
	{
		double result = 0.0;
		if((i <= FNumPoints) && (i > 0))
		{
			result = YValues[i - 1];
			LastValueAccessed = i;
		}
		else
		result = 0.0;
		return result;
	}

	double TXYcurveObj::Get_X()
	{
		double result = 0.0;
		result = FX * FXscale + FXshift;
		return result;
	}

	double TXYcurveObj::Get_XValue(int i)
	{
		double result = 0.0;
		if((i <= FNumPoints) && (i > 0))
		{
			result = XValues[i - 1];
			LastValueAccessed = i;
		}
		else
		result = 0.0;
		return result;
	}

	void TXYcurveObj::DumpProperties(System::TTextRec& f, bool Complete)
	{
		int i = 0;
		inherited::DumpProperties(f, Complete);
		/*# with ParentClass do */
		{
			auto with0 = ParentClass;
			int stop = 0;
			for(stop = with0->NumProperties, i = 1; i <= stop; i++)
			{
				switch(i)
				{
					case 	3: case 4:
					{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, "=("); Write(f, Get_PropertyValue(i)); WriteLn(f, L')'); }
					break;
					default:
					{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(i)); }
					break;
				}
			}
		}
	}

	String TXYcurveObj::GetPropertyValue(int Index)
	{
		String result;
		int i = 0;
		switch(Index)
		{
			case 2: case 3: case 4:
			result = "[";
			break;
			default:
			result = "";
			break;
		}
		switch(Index)
		{
			case 	2:
			if((!XValues.empty()) && (!YValues.empty()))
			{
				int stop = 0;
				for(stop = FNumPoints, i = 1; i <= stop; i++)
				{
					result = result + Format("%.8g, %.8g ", XValues[i - 1], YValues[i - 1]);
				}
			}
			else
			result = "0, 0";
			break;
			case 	3:
			if(!YValues.empty())
			{
				int stop = 0;
				for(stop = FNumPoints, i = 1; i <= stop; i++)
				{
					result = result + Format("%-g, ", YValues[i - 1]);
				}
			}
			else
			result = "0";
			break;
			case 	4:
			if(!XValues.empty())
			{
				int stop = 0;
				for(stop = FNumPoints, i = 1; i <= stop; i++)
				{
					result = result + Format("%-g, ", XValues[i - 1]);
				}
			}
			else
			result = "0";
			break;
			case 	8:
			result = Format("%.8g", Get_X());
			break;
			case 	9:
			result = Format("%.8g", Get_Y());
			break;
			case 	10:
			result = Format("%.8g", FXshift);
			break;
			case 	11:
			result = Format("%.8g", FYshift);
			break;
			case 	12:
			result = Format("%.8g", FXscale);
			break;
			case 	13:
			result = Format("%.8g", FYscale);
			break;
			default:
			result = inherited::GetPropertyValue(Index);
			break;
		}
		switch(Index)
		{
			case 2: case 3: case 4:
			result = result + "]";
			break;
			default:
			  ;
			break;
		}
		return result;
	}

	int TXYcurveObj::get_FNumPoints()
	{
		return FNumPoints;
	}


	// This FUNCTION returns the interpolated X value for the given Y.
	// If no points exist in the curve, the result is  0.0
	// If Xvalue is outside the range of defined X values,
	// the curve is extrapolated from the Ends.
	// TEMc: change to relax assumption that Y values are increasing monotonically
	//       if Y is not monotonic (increasing or decreasing) then X is not unique

	double TXYcurveObj::GetXValue(double Y)
	{
		double result = 0.0;
		int i = 0;
		result = 0.0;    // default return value if no points in curve
		if(FNumPoints > 0)
		{
			if(FNumPoints == 1)
				result = XValues[1];
			else
			{
				int stop = 0;
				for(stop = FNumPoints, i = 2; i <= stop; i++)
				{
					if((Y >= YValues[i - 2]) && (Y <= YValues[i - 1]))
					{
						result = InterpolatePoints(i - 1, i, Y, &(YValues[0]), &(XValues[0]));
						return result;
					}
					if((Y <= YValues[i - 2]) && (Y >= YValues[i - 1]))
					{
						result = InterpolatePoints(i - 1, i, Y, &(YValues[0]), &(XValues[0]));
						return result;
					}
				}
		  // Y is out of range, need to determine which end to extrapolate from
				if(YValues[1 - 1] <= YValues[FNumPoints]) // increasing Y values
				{
					if(Y <= YValues[1])
					{
						result = InterpolatePoints(1, 2, Y, &(YValues[0]), &(XValues[0]));
					}
					else
					{
						result = InterpolatePoints(FNumPoints - 1, FNumPoints, Y, &(YValues[0]), &(XValues[0]));
					}
				}
				else
	 // decreasing Y values
				{
					if(Y >= YValues[1 - 1])
					{
						result = InterpolatePoints(1, 2, Y, &(YValues[0]), &(XValues[0]));
					}
					else
					{
						result = InterpolatePoints(FNumPoints - 1, FNumPoints, Y, &(YValues[0]), &(XValues[0]));
					}
				}
			}
		/*
	  IF FNumPoints>0 Then         // Handle Exceptional cases
	  IF FNumPoints=1 Then Result := XValues^[1]
	  ELSE
		Begin

		 IF (YValues^[LastValueAccessed] > Y) Then LastValueAccessed := 1; // Start over from Beginning

		 // if off the curve for the first point, extrapolate from the first two points
		 IF (LastValueAccessed = 1) AND (YValues[1] > Y) Then Begin
			 Result := InterpolatePoints(1, 2, Y, YValues, XValues);
			 Exit;
		 End;

		 FOR i := LastValueAccessed+1 TO FNumPoints do
		   Begin
			 IF (Abs(YValues^[i]-Y) < 0.00001) Then  // If close to an actual point, just use it.
			   Begin
				   Result := XValues^[i];
				   LastValueAccessed := i;
				   Exit;
			   End
			 ELSE IF (YValues^[i] > Y) Then
	// INTERPOLATE
			   Begin
				 LastValueAccessed := i-1;
				 Result := InterpolatePoints(i, LastValueAccessed, Y, YValues, XValues);
				 Exit ;
			   End;
		   End;

		 // If we fall through the loop, Extrapolate from last two points
		 LastValueAccessed := FNumPoints-1;
		 Result := InterpolatePoints(FNumPoints, LastValueAccessed,  Y, YValues, XValues);
		End;
		*/
		}
		return result;
	}

	void TXYcurveObj::InitPropertyValues(int ArrayOffset)
	{
		Set_PropertyValue(1,"0");     // Number of points to expect
		Set_PropertyValue(2,"");
		Set_PropertyValue(3,"");
		Set_PropertyValue(4,"");
		Set_PropertyValue(5,"");
		Set_PropertyValue(6,"");
		Set_PropertyValue(7,"");
		Set_PropertyValue(8,"");
		Set_PropertyValue(9,"");
		Set_PropertyValue(10,"0");
		Set_PropertyValue(11,"0");
		Set_PropertyValue(12,"1");
		Set_PropertyValue(13,"1");
		inherited::InitPropertyValues(NumPropsThisClass);
	}

	double TXYcurveObj::InterpolatePoints(int i, int j, double X, pDoubleArray Xarray, pDoubleArray Yarray)
	{
		double result = 0.0;
		double Den = 0.0;
		Den = (Xarray[i - 1] - Xarray[j - 1]);
		if(Den != 0.0)
			result = Yarray[j - 1] + (X - Xarray[j - 1]) / Den * (Yarray[i - 1] - Yarray[j - 1]);
		else
			result = Yarray[i - 1]; // Y is undefined, return ith value
		return result;
	}

	/*************************************************

	PROCEDURE TXYcurveObj.SaveToDblFile;

	Var
	   F:File of Double;
	   i:Integer;
	   Fname :String;
	Begin
	   If Assigned(TValues) then  Begin
		TRY
		  FName := Format('%s.dbl',[Name]);
		  AssignFile(F, Fname);
		  Rewrite(F);
		  IOResultToException();
		  FOR i := 1 to NumPoints Do  Write(F, TValues^[i]);
		  GlobalResult := 'Temp=[dblfile='+FName+']';
		FINALLY
		  CloseFile(F);
		End;

	   End
	   ELSE DoSimpleMsg('Tshape.'+Name + ' Temperatures not defined.', 622);
	End;

	PROCEDURE TXYcurveObj.SaveToSngFile;

	Var
	   F:File of Single;
	   i:Integer;
	   Fname :String;
	   Temp  :Single;

	Begin
	   If Assigned(TValues) then  Begin
		TRY
			FName := Format('%s.sng',[Name]);
			AssignFile(F, Fname);
			Rewrite(F);
			IOResultToException();
			FOR i := 1 to NumPoints Do  Begin
				Temp := TValues^[i] ;
				Write(F, Temp);
			End;
			GlobalResult := 'Temp=[sngfile='+FName+']';
		FINALLY
		  CloseFile(F);
		End;


	   End
	   ELSE DoSimpleMsg('Tshape.'+Name + ' Temperatures not defined.', 623);


	End;

	****************************************************/

	void TXYcurveObj::Set_X(double Value)
	{
		FX = (Value - FXshift) / FXscale;
		FY = GetYValue_(FX); //Keep In synch
	}

	void TXYcurveObj::Set_XValue(int Index, double Value)
	{
		if(Index <= FNumPoints)
			XValues[Index - 1] = Value;
	}

	void TXYcurveObj::Set_Y(double Value)
	{
		FY = (Value - FYshift) / FYscale;
		FX = GetXValue(FY); //Keep In synch
	}

	void TXYcurveObj::Set_YValue(int Index, double Value)
	{
		if(Index <= FNumPoints)
			YValues[Index - 1] = Value;
	}

	/*Override standard SaveWrite*/
	/*Transformer structure not conducive to standard means of saving*/

	void TXYcurveObj::SaveWrite(System::TTextRec& f)
	{
		int iProp = 0;
	   /*Write only properties that were explicitly set in the final order they were actually set*/

	   /*Write Npts out first so that arrays get allocated properly*/
		System::Write(f, Format(" Npts=%d", FNumPoints));
		iProp = GetNextPropertySet(0); // Works on ActiveDSSObject
		while(iProp > 0)
		{
			/*# with ParentClass do */
			{
				auto with0 = ParentClass;
				switch((with0->RevPropertyIdxMap)[iProp - 1])
				{
		   /*Trap npts= and write out array properties instead*/
					case 	1: /*Ignore Npts*/
					;
					break;
					default:
						System::Write(f, ( " " + with0->PropertyName[with0->RevPropertyIdxMap[iProp - 1] - 1] + "=" + CheckForBlanks( String(Get_PropertyValue(iProp) ) ) ) );
					break;
				}
			}
			iProp = GetNextPropertySet(iProp);
		}
	}

	void TXYcurveObj::Set_NumPoints(int Value)
	{
		Set_PropertyValue(1,IntToStr(Value));   // Update property list variable

		// Reset array property values to keep them in propoer order in Save
		if(ArrayPropertyIndex > 0)
			Set_PropertyValue(ArrayPropertyIndex,Get_PropertyValue(ArrayPropertyIndex));
		FNumPoints = Value;   // Now assign the value

		// reallocate the curve memory
		YValues.resize(FNumPoints);
		XValues.resize(FNumPoints);
	}

}// namespace XYCurve








