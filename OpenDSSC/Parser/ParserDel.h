#ifndef ParserDelH
#define ParserDelH

#include "System.h"
#include "Sysutils.h"

#include "Arraydef.h"
#include "RPN.h"
#include "HashList.h"

namespace ParserDel
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
/*
   Command Line Parser Class

   This Version is a Simple version for embedding in Delphi Programs.

   3/20/01  Added Quote char properties and strings
*/

/*$M+*//*controls,*/

class EParserProblem : public std::runtime_error
{
public:
	typedef std::runtime_error inherited;
	EParserProblem(const String &Msg);
};

     /*Class for keeping a list of variablel and associate values*/

class TParserVar : public System::TObject
{
public:
	typedef TObject inherited;	
//private:
	unsigned int ActiveVariable;
	unsigned int StringArraySize;
	unsigned int FsizeIncrement;
	Hashlist::THashList* VarNames;
	pStringArray VarValues;
	std::string Get_Value();
	void Set_Value(const std::string Value);
	std::string Get_VarString(unsigned int Idx);
public:
	unsigned int NumVariables;
	TParserVar(unsigned int initSize);
	virtual ~TParserVar();
	int Add(const String VarName, const String varValue);      // returns number of variables
	int Lookup(const String VarName);                  // returns index or 0
	TParserVar();
};

class TParser : public System::TObject
{
public:
	typedef TObject inherited;	
//private:
	String CmdBuffer;
	int FPosition;
	String ParameterBuffer;
	String TokenBuffer;
	String DelimChars;
	String WhiteSpaceChars;
	String FBeginQuoteChars;
	String FEndQuoteChars;
	Char LastDelimiter;
	Char MatrixRowTerminator;
	bool FAutoIncrement;
	bool ConvertError;
	bool IsQuotedString;
	RPN::TRPNCalc RPNCalculator;
	String Get_Remainder();
	void SetCmdString(const String Value);
	String MakeString_();
	int MakeInteger_();
	double MakeDouble_();
	String GetNextParam();
	void SkipWhitespace(const String LineBuffer, int * LinePos);
	bool IsWhiteSpace(Char ch);
	bool IsDelimiter(const String LineBuffer, int& LinePos);
	bool IsDelimChar(Char ch);
	bool IsCommentChar(const String LineBuffer, int& LinePos);
	String GetToken(const String LineBuffer, int* LinePos);
	double InterpretRPNString(int& Code);
	std::string get_CmdBuffer();
	std::string get_Token();
	void set_Token(std::string S);
//protected:
public:
	TParser();
	virtual ~TParser();
	String ParseAsBusName(int& NumNodes, Arraydef::pIntegerArray NodeArray, int ActorID);
	int ParseAsVector(int ExpectedSize, Arraydef::pDoubleArray VectorBuffer);
	int ParseAsMatrix(int ExpectedOrder, Arraydef::pDoubleArray MatrixBuffer);
	int ParseAsSymMatrix(int ExpectedOrder, Arraydef::pDoubleArray MatrixBuffer);
	void ResetDelims();   // resets delimiters to default
	void CheckforVar(String& TokenBuffer);
	int get_position();
	void set_position(int myPos);
	std::string get_delimchars();
	void set_delimchars(std::string S);
	std::string get_WhiteSpaceChars();
	void set_WhiteSpaceChars(std::string S);
	std::string get_FBeginQuoteChars();
	void set_FBeginQuoteChars(std::string S);
	std::string get_FEndQuoteChars();
	void set_FEndQuoteChars(std::string S);
	bool get_FAutoIncrement();
	void set_FAutoIncrement(bool S);
};  //Parser : TParser;
extern TParserVar* ParserVars;

}  // namespace ParserDel

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace ParserDel;
#endif

#endif // ParserDelH

