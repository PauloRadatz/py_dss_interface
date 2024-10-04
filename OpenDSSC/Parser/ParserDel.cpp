

#pragma hdrstop

#include "ParserDel.h"

#include "CmdForms.h"

using namespace std;

namespace ParserDel
{

	TParserVar::TParserVar() {}
	
	TParserVar* ParserVars;
	//ParserVars = nullptr;
	const WideChar Commentchar = L'!';
	const WideChar VariableDelimiter = L'@';  // first character of a variable

	EParserProblem::EParserProblem(const String &Msg) : inherited(Msg) {}

	/*=======================================================================================================================*/

	int ProcessRPNCommand(const String TokenBuffer, TRPNCalc* RPN)
	{
		int result = 0;
		String s;
		double Number = 0.0;
		result = 0;  // Error Code on conversion error


		 /*Get_First() Try to make a valid number. If that fails, check for RPN command*/
		val(TokenBuffer, &Number, &result);
		if (result == 0)  // Enters number in X register
			RPN->Set_X(Number);
		else
			/*Check for RPN command. */
		{
			result = 0; // reset error return
			s = LowerCase(TokenBuffer);
			/*# with RPN do */
			{
				auto with0 = RPN;
				if (CompareStr(s, "+") == 0)
					with0->Add();
				else 
				{
					if (CompareStr(s, "-") == 0)			with0->Subtract();
					else if (CompareStr(s, "*") == 0)		with0->Multiply();
					else if (CompareStr(s, "/") == 0)		with0->Divide();
					else if (CompareStr(s, "sqrt") == 0)	with0->Sqrt();
					else if (CompareStr(s, "sqr") == 0)		with0->Square();
					else if (CompareStr(s, "^") == 0)		with0->YToTheXPower();
					else if (CompareStr(s, "sin") == 0)		with0->Sindeg();
					else if (CompareStr(s, "cos") == 0)		with0->Cosdeg();
					else if (CompareStr(s, "tan") == 0)		with0->Tandeg();
					else if (CompareStr(s, "asin") == 0)	with0->aSindeg();
					else if (CompareStr(s, "acos") == 0) 	with0->aCosdeg();
					else if (CompareStr(s, "atan") == 0)	with0->aTandeg();
					else if (CompareStr(s, "atan2") == 0)	with0->aTan2deg();
					else if (CompareStr(s, "swap") == 0)	with0->SwapXY();
					else if (CompareStr(s, "rollup") == 0)	with0->RollUp();
					else if (CompareStr(s, "rolldn") == 0)	with0->RollDn();
					else if (CompareStr(s, "ln") == 0)		with0->NatLog();
					else if (CompareStr(s, "pi") == 0)		with0->EnterPi();
					else if (CompareStr(s, "log10") == 0)	with0->TenLog();
					else if (CompareStr(s, "exp") == 0)		with0->etothex();
					else if (CompareStr(s, "inv") == 0)		with0->Inv();
					else result = 1;  // error
					//throw EParserProblem("Invalid inline math entry: \"" + TokenBuffer + "\"");
				}

			}
		}
		return result;
	}

	string TParser::get_CmdBuffer()
	{
		return CmdBuffer;
	}

	/*=======================================================================================================================*/

	String StriptoDotPos(int dotpos, String& s)
	{
		String result;
		if (dotpos == 0)
			result = s;
		result = s.substr(0, dotpos - 1);
		return result;
	}

	/*Strips off everything up to a period.*/

	/*=======================================================================================================================*/

	void TParser::CheckforVar(String& TokenBuffer)
	{
		String VariableValue;
		String VariableName;
		size_t dotpos = 0;
		size_t CaratPos = 0;

		/*-------------------------------------*/

		auto ReplaceToDotPos = [&](const String s) -> void
		{
			if (dotpos != String::npos)
				TokenBuffer = s + TokenBuffer.substr(dotpos, TokenBuffer.length() - dotpos + 1);
			else
				TokenBuffer = s;
		};
		/*-------------------------------------*/

		/*Replace TokenBuffer with Variable value if first character is VariableDelimiter character*/
		if (TokenBuffer.length() > 1)
		{
			if (TokenBuffer[0] == VariableDelimiter)  // looking for '@'
			{
				dotpos = TokenBuffer.find(".");
				CaratPos = TokenBuffer.find("^");
				if (CaratPos != String::npos)
					dotpos = CaratPos;   // Carat takes precedence
				if (dotpos != String::npos)
					VariableName = StriptoDotPos(dotpos, TokenBuffer);
				else
					VariableName = TokenBuffer;
				if (ParserVars->Lookup(VariableName) > 0)
				{
					VariableValue = ParserVars->Get_Value(); // Retrieve the value of the variable
					if (VariableValue[0] == L'{')
					{
						ReplaceToDotPos(VariableValue.substr(1, VariableValue.length() - 2));    // get rid of closed brace added by parservar
						IsQuotedString = true;  // force RPN parser to handle
					}
					else
						ReplaceToDotPos(VariableValue);
				}
			}
		}
	}

	/*=======================================================================================================================*/

	TParser::TParser()
		: FPosition(0),
		LastDelimiter(L'\0'),
		MatrixRowTerminator(L'\0'),
		FAutoIncrement(false),
		ConvertError(false),
		IsQuotedString(false)
	{
		;
		DelimChars = ",=";
		WhiteSpaceChars = " \t";   // blank + tab
		FBeginQuoteChars = "(\"'[{";
		FEndQuoteChars = ")\"']}";
		FPosition = 0;
		MatrixRowTerminator = '|';
		FAutoIncrement = false;
	}

	/*=======================================================================================================================*/

	TParser::~TParser()
	{
		// inherited::Destroy();
	}


	/*=======================================================================================================================*/

	void TParser::SetCmdString(const String Value)
	{
		CmdBuffer.clear();
		CmdBuffer = Value + " "; // add some white space at end to get last param
		FPosition = 0;
		SkipWhitespace(CmdBuffer, &FPosition);   // position at first non whitespace character
	}

	/*=======================================================================================================================*/

	void TParser::ResetDelims()
	{
		DelimChars = ",=";
		WhiteSpaceChars = " \t";
		MatrixRowTerminator = '|';
		FBeginQuoteChars = "(\"'[{";
		FEndQuoteChars = ")\"']}";
	}

	/*=======================================================================================================================*/

	bool TParser::IsWhiteSpace(Char ch)
	{
		bool result = false;
		int i = 0;
		int stop = 0;
		result = false;
		for (stop = (int)WhiteSpaceChars.length(), i = 1; i <= stop; i++)
		{
			if (ch == WhiteSpaceChars[i - 1])
			{
				result = true;
				return result;
			}
		}
		return result;
	}


	/*=======================================================================================================================*/

	bool TParser::IsDelimiter(const String LineBuffer, int& LinePos)
	{
		bool result = false;
		int i = 0;
		Char ch = L'\0';
		int stop = 0;
		result = false;
		if (IsCommentChar(LineBuffer, LinePos))
		{
			result = true;
			LastDelimiter = Commentchar;
			return result;
		}
		ch = LineBuffer[LinePos];
		for (stop = (int)DelimChars.length(), i = 1; i <= stop; i++)
		{
			if (ch == DelimChars[i - 1])
			{
				result = true;
				LastDelimiter = ch;
				return result;
			}
		}
		for (stop = (int)WhiteSpaceChars.length(), i = 1; i <= stop; i++)
		{
			if (ch == WhiteSpaceChars[i - 1])
			{
				result = true;
				LastDelimiter = L' ';  // to indicate stopped on white space
				return result;
			}
		}
		return result;
	}


	/*=======================================================================================================================*/

	bool TParser::IsDelimChar(Char ch)
	{
		bool result = false;
		int i = 0;
		int stop = 0;
		result = false;
		for (stop = (int)DelimChars.length(), i = 1; i <= stop; i++)
		{
			if (ch == DelimChars[i - 1])
			{
				result = true;
				return result;
			}
		}
		return result;
	}

	/*=======================================================================================================================*/

	int TParser::get_position()
	{
		return FPosition;
	}

	/*=======================================================================================================================*/

	void TParser::set_position(int myPos)
	{
		FPosition = myPos;
	}

	/*=======================================================================================================================*/

	std::string TParser::get_delimchars()
	{
		return DelimChars;
	}

	/*=======================================================================================================================*/

	void TParser::set_delimchars(std::string S)
	{
		DelimChars = S;
	}

	/*=======================================================================================================================*/

	std::string TParser::get_WhiteSpaceChars()
	{
		return WhiteSpaceChars;
	}

	/*=======================================================================================================================*/

	void TParser::set_WhiteSpaceChars(std::string S)
	{
		WhiteSpaceChars = S;
	}

	/*=======================================================================================================================*/

	std::string TParser::get_FBeginQuoteChars()
	{
		return FBeginQuoteChars;
	}

	/*=======================================================================================================================*/

	void TParser::set_FBeginQuoteChars(std::string S)
	{
		FBeginQuoteChars = S;
	}

	/*=======================================================================================================================*/

	std::string TParser::get_FEndQuoteChars()
	{
		return FEndQuoteChars;
	}

	/*=======================================================================================================================*/

	void TParser::set_FEndQuoteChars(std::string S)
	{
		FEndQuoteChars = S;
	}

	/*=======================================================================================================================*/

	bool TParser::get_FAutoIncrement()
	{
		return FAutoIncrement;
	}

	/*=======================================================================================================================*/

	void TParser::set_FAutoIncrement(bool S)
	{
		FAutoIncrement = S;
	}

	/*=======================================================================================================================*/

	void TParser::SkipWhitespace(const String LineBuffer, int * LinePos)
	{
		while ((*LinePos < LineBuffer.length()) && IsWhiteSpace(LineBuffer[*LinePos]))
			*LinePos = *LinePos + 1;
	}

	/*=======================================================================================================================*/

	String TParser::GetToken(const String LineBuffer, int* LinePos)
	{
		String result;
		int TokenStart = 0;
		int CmdBufLength = 0;
		int QuoteIndex = 0;  // value of quote character found


	   /*---------------- Local Function -----------------------*/

		auto ParseToEndChar = [&](Char Endchar) -> void
		{
			*LinePos = *LinePos + 1;
			TokenStart = *LinePos;
			while ((*LinePos < CmdBufLength) && (LineBuffer[*LinePos] != Endchar))
				*LinePos = *LinePos + 1;
			result = LineBuffer.substr(TokenStart, *LinePos - TokenStart);
			if (*LinePos < CmdBufLength)
				*LinePos = *LinePos + 1;  // Increment past endchar
		};

		/*---------------- Local Function -----------------------*/

		auto ParseToEndQuote = [&]() -> void
		{
			ParseToEndChar(FEndQuoteChars[QuoteIndex - 1]);
			IsQuotedString = true;
		};

		/*---------------- Local Function -----------------------*/

		auto IsBeginQuote = [&](Char ch) -> bool
		{
			bool result = false;
			QuoteIndex = Pos(ch, FBeginQuoteChars);
			if (QuoteIndex > 0)
				result = true;
			else
				result = false;
			return result;
		};
		result = "";   // if it doesn't find anything, return null string
		CmdBufLength = (int)LineBuffer.length();
		if (*LinePos <= CmdBufLength)

			/*Handle Quotes and Parentheses around tokens*/
		{
			IsQuotedString = false;
			if (IsBeginQuote(LineBuffer[*LinePos]))
				ParseToEndQuote();
			else
				/* Copy to next delimiter or whitespace*/
			{
				TokenStart = *LinePos;
				while ((*LinePos < CmdBufLength) && !IsDelimiter(LineBuffer, *LinePos))
					*LinePos = *LinePos + 1;
				
				result = LineBuffer.substr(TokenStart, (*LinePos - TokenStart));
			}


			/* Check for stop on comment */

			// if stop on comment, ignore rest of line.
			if (LastDelimiter == Commentchar)
				*LinePos = (LineBuffer.length() + 1);
			else


				/*Get Rid of Trailing White Space*/
			{
				if (LastDelimiter ==  L' ')
					SkipWhitespace(LineBuffer, LinePos);
				if (IsDelimChar(LineBuffer[*LinePos]))
				{
					LastDelimiter = LineBuffer[*LinePos];
					*LinePos = *LinePos + 1;  // Move past terminating delimiter
				}
				SkipWhitespace(LineBuffer, LinePos);
			}
		}
		return result;
	}


	/*=======================================================================================================================*/

	String TParser::GetNextParam()
	{
		String result;
		if (FPosition <= CmdBuffer.length())
		{
			LastDelimiter = L' ';
			TokenBuffer = GetToken(CmdBuffer, &FPosition); // Get entire token and put in token Buffer
			if (LastDelimiter == L'=')
			{
				ParameterBuffer = TokenBuffer;     // put first token in Parameterbuffer
				TokenBuffer = GetToken(CmdBuffer, &FPosition);   // get token value after the =
			}
			else
			{
				ParameterBuffer = "";  //init to null string
			}
		}
		else
			// return null strings if none left
		{
			ParameterBuffer = "";
			TokenBuffer = "";
		}
		CheckforVar(TokenBuffer);
		result = ParameterBuffer;
		return result;
	}

	/*=======================================================================================================================*/

	/* Looking for "BusName.1.2.3" in the TokenBuffer
	  Assumes NodeArray is big enough to hold the numbers*/

	String TParser::ParseAsBusName(int& NumNodes, Arraydef::pIntegerArray NodeArray, int ActorID)
	{
		String result;
		size_t dotpos = 0;
		int NodeBufferPos = 0;
		String NodeBuffer;
		String DelimSave;
		String TokenSave;
		if (FAutoIncrement)
			GetNextParam();
		NumNodes = 0;
		dotpos = TokenBuffer.find(".");
		if (dotpos == String::npos)
			result = TokenBuffer;
		else
		{
			result = Trim(TokenBuffer.substr(0, dotpos)); // Bus Name
			TokenSave = TokenBuffer;
			/*now Get nodes*/
			NodeBuffer = TokenBuffer.substr(dotpos + 1, TokenBuffer.length() - dotpos) + " ";
			NodeBufferPos = 0;
			DelimSave = DelimChars;
			DelimChars = ".";
			TokenBuffer = GetToken(NodeBuffer, &NodeBufferPos);
			try
			{
				while (TokenBuffer.length() > 0)
				{
					++NumNodes;
					NodeArray[NumNodes - 1] = MakeInteger_();
					if (ConvertError)
						NodeArray[NumNodes - 1] = -1;  // Indicate an error
					TokenBuffer = GetToken(NodeBuffer, &NodeBufferPos);
				}
			}
			catch (std::exception e)
			{
				//DSSMessageDlg("Node Buffer Too Small: " + (string) e.what(), true);
			}
			DelimChars = DelimSave;   //restore to original delimiters
			TokenBuffer = TokenSave;
		}
		return result;
	}

	/*=======================================================================================================================*/

	int TParser::ParseAsVector(int ExpectedSize, pDoubleArray VectorBuffer)
	{
		int result = 0;
		int ParseBufferPos = 0;
		int NumElements = 0;
		int i = 0;
		String ParseBuffer;
		String DelimSave;
		if (FAutoIncrement)
			GetNextParam();
		NumElements = 0;
		result = 0;  // return 0 if none found or error occurred
		try
		{
			int stop = 0;
			for (stop = ExpectedSize, i = 1; i <= stop; i++)
			{
				(VectorBuffer)[i - 1] = 0.0;
			}

			/*now Get Vector values*/
			ParseBuffer = TokenBuffer + " ";
			ParseBufferPos = 0;
			DelimSave = DelimChars;
			DelimChars = DelimChars + MatrixRowTerminator;
			SkipWhitespace(ParseBuffer, &ParseBufferPos);
			TokenBuffer = GetToken(ParseBuffer, &ParseBufferPos);
			CheckforVar(TokenBuffer);
			while (TokenBuffer.length() > 0)
			{
				++NumElements;
				if (NumElements <= ExpectedSize)
					(VectorBuffer)[NumElements - 1] = MakeDouble_();
				if (LastDelimiter == MatrixRowTerminator)
					break;
				TokenBuffer = GetToken(ParseBuffer, &ParseBufferPos);
				CheckforVar(TokenBuffer);
			}
			result = NumElements;
		}
		catch (std::exception e)
		{
			//DSSMessageDlg("Vector Buffer in ParseAsVector Probably Too Small: " + (string) e.what(), true);
		}
		DelimChars = DelimSave;   //restore to original delimiters
		TokenBuffer = ParseBuffer.substr(ParseBufferPos, ParseBuffer.length());  // prepare for next trip
		return result;
	}

	/*=======================================================================================================================*/

	int TParser::ParseAsMatrix(int ExpectedOrder, pDoubleArray MatrixBuffer)
	{
		int result = 0;
		int i = 0;
		int j = 0;
		int k = 0;
		int ElementsFound = 0;
		pDoubleArray RowBuf;
		if (FAutoIncrement)
			GetNextParam();
		RowBuf = NULL;
		try
		{
			int stop = 0;
			RowBuf = new double[ExpectedOrder];
			for (stop = (ExpectedOrder * ExpectedOrder), i = 1; i <= stop; i++)
			{
				(MatrixBuffer)[i - 1] = 0.0;
			}
			for (stop = ExpectedOrder, i = 1; i <= stop; i++)
			{
				int stop1 = 0;
				ElementsFound = ParseAsVector(ExpectedOrder, RowBuf);

				/* Returns matrix in Column Order (Fortran order) */
				k = i;
				for (stop1 = ElementsFound, j = 1; j <= stop1; j++)
				{
					(MatrixBuffer)[k - 1] = (RowBuf)[j - 1];
					k += ExpectedOrder;
				}
			}
		}
		catch (std::exception e)
		{
			//DSSMessageDlg("Matrix Buffer in ParseAsMatrix Probably Too Small: " + (string) e.what(), true);
		}
		if (RowBuf != NULL)
			free(RowBuf); //# FreeMemory accepts one parameter only;
		result = ExpectedOrder;
		return result;
	}

	/*=======================================================================================================================*/

	int TParser::ParseAsSymMatrix(int ExpectedOrder, pDoubleArray MatrixBuffer)
	{
		int result = 0;
		int i = 0;
		int j = 0;
		int ElementsFound = 0;
		pDoubleArray RowBuf;

		/*---------------- Local Function -----------------------*/

		auto ElementIndex = [&](int II, int jj) -> int
		{
			int result = 0;
			result = (jj - 1) * ExpectedOrder + II;
			return result;
		};
		if (FAutoIncrement)
			GetNextParam();
		RowBuf = NULL;
		try
		{
			int stop = 0;
			RowBuf = new double[ExpectedOrder];
			for (stop = (ExpectedOrder * ExpectedOrder), i = 1; i <= stop; i++)
			{
				(MatrixBuffer)[i - 1] = 0.0;
			}
			for (stop = ExpectedOrder, i = 1; i <= stop; i++)
			{
				int stop1 = 0;
				ElementsFound = ParseAsVector(ExpectedOrder, RowBuf);

				/* Returns matrix in Column Order (Fortran order) */
				for (stop1 = ElementsFound, j = 1; j <= stop1; j++)
				{
					(MatrixBuffer)[ElementIndex(i, j) - 1] = (RowBuf)[j - 1];
					if (i != j)
						(MatrixBuffer)[ElementIndex(j, i) - 1] = (RowBuf)[j - 1];
				}
			}
		}
		catch (std::exception e)
		{
			//DSSMessageDlg("Matrix Buffer in ParseAsSymMatrix Probably Too Small: " + (string) e.what(), true);
		}
		if (RowBuf != NULL)
			delete[] RowBuf; //# FreeMemory accepts one parameter only;
		result = ExpectedOrder;
		return result;
	}



	/*=======================================================================================================================*/

	String TParser::MakeString_()
	{
		String result;
		if (FAutoIncrement)
			GetNextParam();
		result = TokenBuffer;
		return result;
	}

	/*=======================================================================================================================*/
	 // Hex integers must be preceeded by "$"

	int TParser::MakeInteger_()
	{
		int result = 0;
		int Code = 0;
		double Temp = 0.0;
		ConvertError = false;
		if (FAutoIncrement)
			GetNextParam();
		if (TokenBuffer.length() == 0)
		{
			result = 0;
		}
		else
		{
			if (IsQuotedString)
			{
				Temp = InterpretRPNString(Code);
				result = (int)Round(Temp);
			}
			else
			{
				double mytemp1 = (double)result;
				val(TokenBuffer, &(mytemp1), &Code);  // Try direct conversion to integer
				result = (int)mytemp1;
			}
			if (Code != 0) // on error for integer conversion
				 // Try again with an double result in case value specified in decimal or some other technique
			{
				val(TokenBuffer, &Temp, &Code);
				if (Code != 0)
					// not needed with Raise ...  Result := 0;
				{
					ConvertError = true;
					//throw EParserProblem("Integer number conversion error for string: \"" + TokenBuffer
					//	+ "\"");
				}
				else
					result = (int)Round(Temp);
			}
		}
		return result;
	}

	/*=======================================================================================================================*/

	double TParser::MakeDouble_()
	{
		double result = 0.0;
		int Code = 0;
		if (FAutoIncrement)
			GetNextParam();
		ConvertError = false;
		if (TokenBuffer.length() == 0)
			result = 0.0;
		else
		{
			if (IsQuotedString)
				result = InterpretRPNString(Code);
			else
				val(TokenBuffer, &result, &Code);
			if (Code != 0)
				// not needed with Raise ...  Result := 0.0;
			{
				ConvertError = true;
				//throw EParserProblem("Floating point number conversion error for string: \"" + TokenBuffer
				//	+ "\"");
			}
		}
		return result;
	}

	/*=======================================================================================================================*/

	String TParser::Get_Remainder()
	{
		String result;
		result = CmdBuffer.substr(FPosition, CmdBuffer.length() - FPosition + 1);
		return result;
	}

	/*=======================================================================================================================*/

	/*Checks for CommentChar and '//'*/

	bool TParser::IsCommentChar(const String LineBuffer, int& LinePos)
	{
		bool result = false;
		switch (LineBuffer[LinePos])
		{
		case 	Commentchar:
			result = true;
			break;
		case 	L'/':
		{
			if ((LineBuffer.length() > LinePos) && (LineBuffer[LinePos + 1] == L'/'))
				result = true;
			else
				result = false;
		}
		break;
		default:
			result = false;
			break;
		}
		return result;
	}

	/*=======================================================================================================================*/

	double TParser::InterpretRPNString(int& Code)
	{
		double result = 0.0;
		int ParseBufferPos = 0;
		String ParseBuffer;
		Code = 0;
		ParseBuffer = TokenBuffer + " ";
		ParseBufferPos = 0;
		SkipWhitespace(ParseBuffer, &ParseBufferPos);
		TokenBuffer = GetToken(ParseBuffer, &ParseBufferPos);
		CheckforVar(TokenBuffer);
		while (TokenBuffer.length() > 0)
		{
			Code = ProcessRPNCommand(TokenBuffer, &RPNCalculator);
			if (Code > 0)
				break;  // Stop on any floating point error
			TokenBuffer = GetToken(ParseBuffer, &ParseBufferPos);
			CheckforVar(TokenBuffer);
		}
		result = RPNCalculator.Get_X();
		TokenBuffer = ParseBuffer.substr(ParseBufferPos, ParseBuffer.length());  // prepare for next trip
		return result;
	}

	/*===================================== Variable Support =============================================================*/

	/* TParserVar */

	void ReallocStr(pStringArray s, int OldSize, int NewSize)
	{
		pStringArray X = new std::string[1];
		X = (pStringArray)realloc(X, sizeof(X[0]) * NewSize);  // Zero fills new string pointer array (important!)
		if (OldSize > 0)
		{
			Move(s, &X, OldSize);
			free(s); //# FreeMemory accepts one parameter only;
		}
		s = X;
	}
	// Make a bigger block to hold the pointers to the strings


	/*=======================================================================*/

	int TParserVar::Add(const String VarName, const String varValue)
	{
		int result = 0;
		unsigned int Idx = 0;
		String VarDefinition;

		auto EncloseQuotes = [&](const String s) -> String
		{
			String result;
			result = String("{") + s + "}";
			return result;
		};

		// Get_First(), check to see if the varname already exists
		// if so, just change the value
		Idx = (unsigned int)VarNames->Find(LowerCase(VarName));
		if (Idx == 0)
		{
			Idx = (unsigned int)VarNames->Add(VarName);  // Hashlist will take care of itself
			if (Idx > StringArraySize)
				// resize String array
			{
				VarValues = (pStringArray) realloc(VarValues, sizeof(VarValues[0]) * (StringArraySize + FsizeIncrement));
				StringArraySize += FsizeIncrement;
			}
		}

		/*If a variable used in the definition of a variable, enclose in quotes.*/
		if (varValue.find("@") != String::npos)
			VarDefinition = EncloseQuotes(varValue);
		else
			VarDefinition = varValue;
		(VarValues)[Idx - 1] = VarDefinition;
		NumVariables = VarNames->Get_NumElements();
		result = (int)Idx;
		return result;
	}

	/*=======================================================================*/

	string TParser::get_Token()
	{
		return TokenBuffer;
	}

	/*=======================================================================*/

	void TParser::set_Token(std::string S)
	{
		TokenBuffer = S;
	}

	/*=======================================================================*/

	TParserVar::TParserVar(unsigned int initSize)
		: ActiveVariable(0),
		StringArraySize(0),
		FsizeIncrement(0),
		VarNames(nullptr),
		NumVariables(0)
	{
		VarNames = new THashList(initSize);
		VarValues = AllocStringArray((int)initSize);
		StringArraySize = initSize;
		FsizeIncrement = initSize;

		// Intrinsic Variables go here...
		ActiveVariable = (unsigned int)VarNames->Add("@lastfile");
		(VarValues)[ActiveVariable - 1] = "null";  // null value
		ActiveVariable = (unsigned int)VarNames->Add("@lastexportfile");
		(VarValues)[ActiveVariable - 1] = "null";  // null value
		ActiveVariable = (unsigned int)VarNames->Add("@lastshowfile");
		(VarValues)[ActiveVariable - 1] = "null";  // null value
		ActiveVariable = (unsigned int)VarNames->Add("@lastplotfile");
		(VarValues)[ActiveVariable - 1] = "null";  // null value
		ActiveVariable = (unsigned int)VarNames->Add("@lastredirectfile");
		(VarValues)[ActiveVariable - 1] = "null";  // null value
		ActiveVariable = (unsigned int)VarNames->Add("@lastcompilefile");
		(VarValues)[ActiveVariable - 1] = "null";  // null value
		ActiveVariable = (unsigned int)VarNames->Add("@result");
		(VarValues)[ActiveVariable - 1] = "null";  // null value
		NumVariables = VarNames->Get_NumElements();
	}

	/*=======================================================================*/

	TParserVar::~TParserVar()
	{
		delete VarNames;
		FreeStringArray(VarValues, (int)StringArraySize);
		// inherited;
	}


	/*=======================================================================*/

	String TParserVar::Get_Value()
	{
		String result;
		if (ActiveVariable > 0)
			result = (VarValues)[ActiveVariable - 1];
		else
			result = "";
		return result;
	}

	String TParserVar::Get_VarString(unsigned int Idx)
	{
		String result;

		auto TestEmpty = [&](const String s) -> String
		{
			String result;
			if (s.length() == 0)
				result = "null";
			else
				result = s;
			return result;
		};
		if ((Idx > 0) && (Idx <= NumVariables))
			result = VarNames->Get(Idx) + ". " + TestEmpty((VarValues)[Idx - 1]);
		else
			result = "Variable index out of range";
		return result;
	}

	int TParserVar::Lookup(const String VarName)
	{
		int result = 0;
		ActiveVariable = (unsigned int)VarNames->Find(VarName);
		result = (int)ActiveVariable;
		return result;
	}

	void TParserVar::Set_Value(const String Value)
	{
		if ((ActiveVariable > 0) && (ActiveVariable <= NumVariables))
			(VarValues)[ActiveVariable - 1] = Value;
	}

	// Variables


	void ParserDel_initialization()
	{
		ParserVars = new TParserVar(100);  // start with space for 100 variables
	}

	void ParserDel_finalization()
	{
		delete ParserVars;
	}

	class 		ParserDel_unit
	{
	public:
		ParserDel_unit()
		{
			//AssertSystemInitialization();
			ParserDel_initialization();
		}
		~ParserDel_unit() { ParserDel_finalization(); }
	};
	ParserDel_unit _ParserDel_unit;



}// namespace ParseDel


