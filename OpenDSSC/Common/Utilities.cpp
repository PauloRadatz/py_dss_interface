
//#include <vcl.h>
#pragma hdrstop

#include "Utilities.h"
#include <string>
#include <algorithm>
#include <vector>
#include <string>
#ifndef windows
#include <errno.h> // errno
#include <locale.h> // uselocale
#include <cstdlib> // abort
#include <string.h> // strerror_l, strsignal
#include <sstream> // stringstream
#include <sys/types.h> // fork, waitpid
#include <sys/wait.h> // waitpid
#include <unistd.h> // execvp, fork
#endif

namespace Utilities
{

    char* ZERONULL = NULL;
    String padString = "                                                  "; //50 blanks

    String paddotsString = " ................................................."; //50 dots
    bool show = true;


    void IOResultToException()
    {
      WORD res = IOResult();
      // The original IOResult from d2c didn't actually clear the error state, although it does in the original Delphi function!
      // We modified to behave correctly. In case that is reverted, remember to uncomment the following line:
      // System::InOutRes = 0;
      switch (res)
      {
        case 100: throw std::runtime_error("I/O error 100: Disk read error");
        case 101: throw std::runtime_error("I/O error 101: Disk write error");
        case 102: throw std::runtime_error("I/O error 102: File not assigned");
        case 103: throw std::runtime_error("I/O error 103: File not open");
        case 104: throw std::runtime_error("I/O error 104: File not open for input");
        case 105: throw std::runtime_error("I/O error 105: File not open for output");
        case 106: throw std::runtime_error("I/O error 106: Invalid numeric format");
        case 0: return;
        default: throw std::runtime_error("I/O error");
      }
    }

    int CompareTextShortest( const String s1, const String s2 )
    {
      int result = 0;
      String TestStr;
      if ( s1.size() < s2.size() )
      {
        TestStr = s2.substr( 0, s1.size() );
        result = CompareText( TestStr, s1 );
      }
      else
      {
        TestStr = s1.substr( 0, s2.size() );
        result = CompareText( TestStr, s2 );
      }
      return result;
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    string Pad(const String S, int Width)
        // Pad out a string with blanks to Width characters

    {
        String result = "", StrXtra = "";
        /*
        if (S.find('$') != String::npos)
        {
            S._Equal(" ");
            StrXtra._Equal(" ");
        }
        */
        int myLen = Width - S.size();
        if (myLen < 0)
            StrXtra = "";
        else
            StrXtra = padString.substr(0, (Width - S.size()));
        result = S.substr(0, S.size()) + StrXtra;
        // For i := 1 to Width-Length(S) DO Result := Result + ' ';
        return result;
    }




    String Paddots( const String S, int Width )
    // Pad out a string with dots to Width characters

    {
      String result, StrXtra;
      int myLen = Width - S.size();
      if (myLen < 0)
          StrXtra = "";
      else
          StrXtra = paddotsString.substr(0, (Width - S.size()));
      result = S.substr( 0, S.size() ) + StrXtra;
      return result;
    }
    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    String PadTrunc( const String S, int Width )
    // Pad out a string with blanks to Width characters or truncate to Width Chars

    {
      String result;
      result = Pad( S, Width ).substr( 0, Width );
      return result;
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    String FullName( TDSSCktElement* pElem )
    {
      String result;
      result = EncloseQuotes( pElem->Get_myPName() + "." + UpperCase( pElem->get_Name() ) );
      return result;
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    String StripExtension( const String S )

    /*Strips off everything up to a period.*/
    {
      String result;
      size_t dotpos = 0;
      dotpos = S.find( "." );
      if ( dotpos == String::npos )
        dotpos = S.size( );
      result = S.substr( 0, dotpos );
      return result;
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    String StripClassName( const String S )
    /*Returns everything past the first period*/
    {
      String result;
      size_t dotpos = 0;
      dotpos = S.find( "." );
      result = S.substr(( dotpos) + 1, S.size() );
      return result;
    }

#ifndef windows
    static int fork_execvp(const char* file, const char* argv[], int* _wstatus, std::string* _error_message)
    {
      // return 0 on success, or -1 on failure with errno set.
      // on success, *_wstatus is set to value from waitpid.
      // on failure, *_error_message is set to the name of the library function which failed and set errno.

      pid_t pid = fork();
      if (! pid) {
        // We're the fork'ed child process.cw
        //
        // Don't bother checking the return of execvp(), because it shouldn't
        // return, and if it does, then it is an error.
        errno = 0; // clearing for paranoia w/ execvp()

        /* int retval = */ execvp(file, (char*const*)argv);

        // Here, we'll use "/bin/sh -c" to parse and execute the string, but if
        // we ever want to parse this string within this code, check out the
        // wordexp() function.
        //
        // Also, someday, if we want to detect if we're running in a Linux
        // graphics desktop, which most likely would be powered by X Windows,
        // we can detect X by testing if the DISPLAY environment variable is
        // set:
        //
        // if( getenv("DISPLAY") ) ...
        //
        // Then we can exec this process by spawning a new terminal window,
        // such as "xterm" or "rxvt" if available.

        // Handle errors here:
        int the_errno = errno;
        char *errno_message = strerror_l(the_errno, uselocale(locale_t(0)));
        std::cerr << "fork_execvp error: execvp() returned with errno=" << the_errno << ": " << errno_message << "\n";

        // We must not allow any return to the caller.
        std::abort();
      }

      // If we're here, then we're the original, parent process.
      if (pid==-1) {
        int the_errno = errno;
        if (_error_message)
          *_error_message = "fork()";
        errno = the_errno;
        return -1;
      }

      // If we're here, then fork() succeeded, and pid contains the ID for the
      // new process.  We need to wait for it to exit.
      int wstatus=0;
      do {
        wstatus=0;
        pid_t pid_ret = waitpid(pid, &wstatus, 0 /*options*/);

        if (pid_ret == -1) {
          // waitpid() returned an error.
          int the_errno = errno;
          if (_error_message)
            *_error_message = "waitpid()";
          errno = the_errno;
          return -1;
        }

        // Ignoring WIFSTOPPED(wstatus) and WIFCONTINUED(wstatus)
      } while (!WIFEXITED(wstatus) && !WIFSIGNALED(wstatus));

      if (_wstatus)
        *_wstatus=wstatus;

      return 0;
    }
#endif

    void ShowReport(bool ShowR)
    {
        if (ShowR == true)
        {
            show = true;
        }
        else
            show = false;
    }

    void FireOffEditor( String Filenm )
    {
#ifdef windows
      WORD retval = 0;
      try
      {
          if (FileExists(Filenm))
          {
              AnsiString Utilities__1(EncloseQuotes(Filenm));
              {
                  AnsiString Utilities__0(EncloseQuotes(DefaultEditor));
                  if (show == true)
                  {
                      ShellExecuteA(0, 0, Utilities__0.c_str(), Utilities__1.c_str(), 0, SW_SHOW);
                  }
                  retval = 100;
              };
              SetLastResultFile(Filenm);
              switch (retval)
              {
              case 0:
                  DoSimpleMsg("System out of memory. Cannot start Editor.", 700);
                  break;
              case ERROR_BAD_FORMAT:
                  DoSimpleMsg("Editor File is Invalid.", 701);
                  break;
              case ERROR_FILE_NOT_FOUND:
                  DoSimpleMsg("Editor \"" + DefaultEditor + "\"  Not Found." + CRLF + "Did you set complete path name?", 702);
                  break;
              case ERROR_PATH_NOT_FOUND:
                  DoSimpleMsg("Path for Editor \"" + DefaultEditor + "\" Not Found.", 703);
                  break;
              }
          }
      }
      catch (std::exception &E)
      {
        DoErrorMsg( "FireOffEditor.", E.what(), "Default Editor correctly specified???", 704 );
      }
#else
      const char* argv[] = { DefaultEditor.c_str(), Filenm.c_str(), NULL };
      int wstatus = 0;
      std::string error_message;
      int the_errno = fork_execvp(argv[0], argv, &wstatus, &error_message);
      if (the_errno) {
        // fork_execvp() returned an error.
        char *errno_message = strerror_l(the_errno, uselocale(locale_t(0)));
        std::stringstream error_string;
        error_string << "FireOffEditor Error: " << error_message << " returned with errno=" << the_errno << ": " << errno_message;
        DoSimpleMsg( error_string.str().c_str(), 704 );
      } else {
        if (WIFEXITED(wstatus)) {
          // the child terminated normally.
          int exit_status = WEXITSTATUS(wstatus);
          if (exit_status) {
            // The command finished with a nonzero status, which indicates an error.
            std::stringstream error_string;
            error_string << "FireOffEditor Error: command exited with nonzero status " << exit_status << ".  The original command was: " << argv[0] << ' ' << argv[1];
            DoSimpleMsg( error_string.str().c_str(), 704 );
          }
        }

        if (WIFSIGNALED(wstatus)) {
          // the child process was terminated by a signal.
          int exit_signal = WTERMSIG(wstatus);
          std::stringstream error_string;
          const char *exit_signal_name = strsignal(exit_signal);
          if (!exit_signal_name)
            exit_signal_name = "invalid signal number";
          error_string << "FireOffEditor Error: command terminated due to signal " << exit_signal << ", \"" << exit_signal_name << "\".  The original command was: " << argv[0] << ' ' << argv[1];
          DoSimpleMsg( error_string.str().c_str(), 704 );
        }
      }
#endif
    }


    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void DoDOSCmd( String CmdString )
    {
#ifdef windows
      HWND Handle = 0;
      try
      {
        Handle = 0;
        ShellExecute( Handle, "open", ( PChar ) ( "cmd.exe" ), CmdString.c_str(), NULL, SW_SHOW );
      }
      catch (std::exception& E)
      {
        DoSimpleMsg( "DoDOSCmd Error:" + (string) E.what() + ".Error in Command " + CmdString, 704 );
      }
#else
      const char* argv[] = { "/bin/sh", "-c", CmdString.c_str(), NULL };
      int wstatus = 0;
      std::string error_message;
      int the_errno = fork_execvp(argv[0], argv, &wstatus, &error_message);
      if (the_errno) {
        // fork_execvp() returned an error.
        char *errno_message = strerror_l(the_errno, uselocale(locale_t(0)));
        std::stringstream error_string;
        error_string << "DoDOSCmd Error: " << error_message << " returned with errno=" << the_errno << ": " << errno_message;
        DoSimpleMsg( error_string.str().c_str(), 704 );
      } else {
        if (WIFEXITED(wstatus)) {
          // the child terminated normally.
          int exit_status = WEXITSTATUS(wstatus);
          if (exit_status) {
            // The command finished with a nonzero status, which indicates an error.
            std::stringstream error_string;
            error_string << "DoDOSCmd Error: command exited with nonzero status " << exit_status << ".  The original command was: " << CmdString.c_str();
            DoSimpleMsg( error_string.str().c_str(), 704 );
          }
        }

        if (WIFSIGNALED(wstatus)) {
          // the child process was terminated by a signal.
          int exit_signal = WTERMSIG(wstatus);
          std::stringstream error_string;
          const char *exit_signal_name = strsignal(exit_signal);
          if (!exit_signal_name)
            exit_signal_name = "invalid signal number";
          error_string << "DoDOSCmd Error: command terminated due to signal " << exit_signal << ", \"" << exit_signal_name << "\".  The original command was: " << CmdString.c_str();
          DoSimpleMsg( error_string.str().c_str(), 704 );
        }
      }
#endif
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


    String IntArrayToString( pIntegerArray iarray, int count )
    // Put array values in parentheses separated by commas.

    {
      String result;
      int i = 0;
      result = "[NULL]";
      if ( count > 0 )
      {
        result = "[";
        for ( int stop = count, i = 1; i <= stop; i++)
        {
          result = result + to_string( (iarray)[i] );
          if ( i != count )
            result = result + ", ";
        }
        result = result + "]";
      }
      return result;
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    String DblArrayToString( pDoubleArray dblarray, int count )
    // Put array values in brackets separated by commas.

    {
      String result;
      int i = 0;
      result = "[NULL]";
      if (count > 0)
      {
        result = Format( "[%.10g", dblarray[1] );
        for ( int stop = count, i = 2; i <= stop; i++)
          result = result + Format( ", %.10g", dblarray[i]);
        result = result + "]";
      }
      return result;
    }

    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    String CmplxArrayToString( pComplexArray cpxarray, int count )
    // Put array values in brackets separated by commas.

    {
      String result;
      int i = 0;
      result = "[NULL]";
      if ( count > 0 )
      {
        result = Format("[%.10g +j %.10g", cpxarray[1].re, cpxarray[1].im);
        for ( int stop = count, i = 2; i <= stop; i++)
          result = result + Format( ", %.10g +j %.10g", (cpxarray)[i].re, ((cpxarray)[i].im ) );
        result = result + "]";
      }
      return result;
    }


    String EncloseQuotes( const String S )
    {
      String result;
      result = String( "\"" ) + S + "\"";
      return result;
    }



    //----------------------------------------------------------------------------



    int InterpretSolveMode( const String S )

    // interpret solution mode
    // could be "nominal" "daily"  "yearly" "montecarlo" "dutycycle"  "loadduration" "peakdays" , etc.

    {
      int result = 0;
      String SLC;
      SLC = ToLowerCaseStr( S );
      switch ( SLC[0] )
      {
        case 's':
          result = SNAPSHOT;
        break;
        case 'd':
            switch ( SLC[1] )
            {
            case 'u':
                result = DUTYCYCLE;
            break;
            case 'i':
                result = DIRECT;
            break;
            case 'y':
                result = DYNAMICMODE;
            break;
            default:
            result = DAILYMODE;
            }
        break;
        case 'e':
            switch (SLC.size() > 3 ? SLC[3] : '\0')
            {
                case 'd':
                result = EMPDAILYMODE;
                break;
                default:
                    result = EMPMODE;
            }
        break;
        case 'f':
          result = FAULTSTUDY;
        break;
        case 'h':
          switch (SLC.size() > 8 ? SLC[8] : '\0')
          {
            case                   //Modification added by Davis Montenegro 25/06/2014
            't':
              result = HARMONICMODET;
            break;     // For adding the harmoncis mode in time domain
          default:
            result = HARMONICMODE;
          }
        break;
        case 'y':
          result = YEARLYMODE;
        break;
        case 'm':
          switch ( SLC[1] )
          {
            case '1':
              result = MONTECARLO1;
            break;
            case '2':
              result = MONTECARLO2;
            break;
            case '3':
              result = MONTECARLO3;
            break;
            case 'f':
              result = MONTEFAULT;
            break;
          default:
            result = MONTECARLO1;
          }
        break;
        case 'p':
          result = PEAKDAY;
        break;
        case 'a':
          result = AUTOADDFLAG;
        break;
        case 'l':
          switch ( SLC[1] )
          {
            case 'd':
              switch (SLC.size() > 2 ? SLC[2] : '\0')
              {
                case '1':
                  result = LOADDURATION1;
                break;
                case '2':
                  result = LOADDURATION2;
                break;
              default:
                result = LOADDURATION1;
              }
            break;
          default:
            result = LOADDURATION1;
          }
        break;
        case 't':
          result = GENERALTIME;
        break;
      default:
        result = SNAPSHOT;
      }
      return result;
    }

    //----------------------------------------------------------------------------


    int InterpretControlMode( const String S )

    // interpret solution control mode

    {
      int result = 0;
      String SLC;
      SLC = ToLowerCaseStr( S );
      switch ( SLC[0] )
      {
        case 'o':
          result = CONTROLSOFF;
        break;
        case 'e':
          result = EVENTDRIVEN;
        break;    // "event"

        case 't':
          result = TIMEDRIVEN;
        break;     // "time"

        case 'm':
          result = MULTIRATE;
        break;     // "MultiRate"
      default:
        result = CTRLSTATIC;
      }
      return result;
    }
    //----------------------------------------------------------------------------



    int InterpretLoadModel( const String S )
    {
      int result = 0;
      String s2;
      s2 = ToLowerCaseStr( S );
      switch ( s2[0] )
      {
        case 'a':
          result = ADMITTANCE;
        break;
        case 'p':
          result = POWERFLOW;
        break;
      default:
        result = ADMITTANCE;
      }
    /* If this represents a change, invalidate all the PC Yprims*/
      if ( result != (*(ActiveCircuit[ActiveActor])).Solution->LoadModel )
        (*(ActiveCircuit[ActiveActor])).InvalidateAllPCElements();
      return result;
    }

    //----------------------------------------------------------------------------



    bool InterpretYesNo( const String S )

    //' Interpret Yes / no properties  - can also be True/False

    {
      bool result = false;
      Char s2 = '\0';
      s2 = ToLowerCaseStr( S )[0];
      switch ( s2 )
      {
        case 'y': case 't':
          result = true;
        break;
        case 'n': case 'f':
          result = false;
        break;
      default:
        result = false;
      }
      return result;
    }

    //----------------------------------------------------------------------------



    int InterpretRandom( const String S )

    // interpret the type of random variation in the load
    // none|gaussian|uniform |lognormal

    {
      int result = 0;
      String SLC;
      SLC = ToLowerCaseStr( S );
      switch ( SLC[0] )
      {
        case 'g':
          result = GAUSSIAN;
        break;  //gaussian

        case 'u':
          result = UNIFORM;
        break;  //uniform

        case 'l':
          result = LOGNORMAL;
        break; // Log-Normal
      default:
        result = 0;  // no variation for any other entry
      }
      return result;
    }


    //----------------------------------------------------------------------------



    int InterpretAddType( const String S )
    // type of device to automatically add. Default is capacitor

    {
      int result = 0;
      String SLC;
      SLC = ToLowerCaseStr( S );
      switch ( SLC[0] )
      {
        case 'g':
          result = GENADD;
        break;
      default:
        result = CAPADD;
      }
      return result;
    }

    //----------------------------------------------------------------------------



    int InterpretConnection( const String S )
    /* Accepts  (Case insensitive)
        delta or LL    Result=1       
        Y, wye, or LN  Result=0
    */
    {
      int result = 0;
      result = 0;
      switch ( ToLowerCaseStr( S )[0] )
      {
        case 'y': case 'w':
          result = 0;
        break;  /*Wye*/
        case 'd':
          result = 1;
        break;  /*Delta or line-Line*/
        case 'l':
          switch ( ToLowerCaseStr( S )[1] )
          {
            case 'n':
              result = 0;
            break;
            case 'l':
              result = 1;
            break;
          }
        break;
      }
      return result;
    }

    //----------------------------------------------------------------------------



    int InterpretSolveAlg( const String S )
    {
      int result    = 0;
      string mytemp = S;
      mytemp = ToLowerCaseStr(mytemp).substr(0,2);
      if (mytemp == "ne")                       // Newton method
          result = NEWTONSOLVE;
      else if (mytemp == "nc")                  // NCIM method
          result = NCIMSOLVE;
      else                                      // floating-point method (default)
          result = NORMALSOLVE;
 
      return result;
    }


    //----------------------------------------------------------------------------



    bool InterpretCktModel( const String S )

    /*Returns True if Positive Sequence*/
    {
      bool result = false;
      switch ( S[0] )
      {
        case 'p': case 'P':
          result = true;
        break;
      default:
        result = false;
      }
      return result;
    }

    //----------------------------------------------------------------------------



    complex InterpretComplex( const String S )

    // interpret first two entries as complex numbers

    {
        complex result = CZero;
      String ParmName;
      AuxParser[ActiveActor]->SetCmdString(S);
      ParmName = AuxParser[ActiveActor]->GetNextParam();
      result.re = AuxParser[ActiveActor]->MakeDouble_();
      ParmName = AuxParser[ActiveActor]->GetNextParam();
      result.im = AuxParser[ActiveActor]->MakeDouble_();
      return result;
    }

    //----------------------------------------------------------------------------


    void InitDblArray( int NumValues, pDoubleArray Xarray, double Value )
    {
      int i = 0;
    /*Set all elements of a double array*/
      for ( int stop = NumValues, i = 1; i <= stop; i++)
        Xarray[i - 1] = Value;
    }

    //----------------------------------------------------------------------------



    void InitIntArray( int NumValues, pIntegerArray Xarray, int Value )
    {
      int i = 0;
    /*Set all elements of a Integer array*/
      for ( int stop = NumValues, i = 1; i <= stop; i++)
        Xarray[i - 1] = Value;
    }

    //----------------------------------------------------------------------------



    int InterpretDblArray( const String S, int MaxValues, pDoubleArray ResultArray )

    /* Get numeric values from an array specified either as a list on numbers or a text file spec.
      ResultArray must be allocated to MaxValues by calling routine.

      9/7/2011 Modified to allow multi-column CSV files and result file

      CSV File my have one value per line or multiple columns and a header row.
      Example:
              ... mult=[file = myfilename, column=2, header=yes]
              ... mult=[file = %result%, column=2, header=yes]    // last result file

              file= must be first
              the %result% variable implies the last result file

              or

              Use the Array=@lastfile variable syntax and the parser will automativally replace with last file name

    */
    {
      int result = 0;
      String ParmName, Param;
      TTextRec F;
      char* MyStream = nullptr;
      int i = 0;
      float temp = 0.0;
      String CSVFileName;
      int CSVColumn = 0;
      bool CSVHeader = false;
      String InputLIne;
      int iskip = 0;
      AuxParser[ActiveActor]->SetCmdString(S);
      ParmName = AuxParser[ActiveActor]->GetNextParam();
      Param = AuxParser[ActiveActor]->MakeString_();
      result = MaxValues; // Default Return Value;

         /*Syntax can be either a list of numeric values or a file specification:  File= ...*/
      if ( CompareText( ParmName, "file" ) == 0 )
      {
             /*Default values*/
        if ( CompareText( Param, "%result%" ) == 0 )
          CSVFileName = LastResultFile;
        else
          CSVFileName = Param;
        if ( ! FileExists( CSVFileName ) )
        {
          DoSimpleMsg( "CSV file " + CSVFileName + " does not exist", 70401 );
          return result;
        }

             // Default options
        CSVColumn = 1;
        CSVHeader = false;

             // Look for other options  (may be in either order)
        ParmName = AuxParser[ActiveActor]->GetNextParam();
        Param = AuxParser[ActiveActor]->MakeString_();
        while ( Param.size() > 0 )
        {
          if ( CompareTextShortest( ParmName, "column" ) == 0 )
            CSVColumn = AuxParser[ActiveActor]->MakeInteger_();
          if ( CompareTextShortest( ParmName, "header" ) == 0 )
            CSVHeader = InterpretYesNo( Param );
          ParmName = AuxParser[ActiveActor]->GetNextParam();
          Param = AuxParser[ActiveActor]->MakeString_();
        }

             // load the list from a file
        try
        {
          System::AssignFile( F, CSVFileName );
          System::Reset( F );
          IOResultToException();
          if ( CSVHeader )
              System::ReadLn( F, InputLIne );  // skip the header row
          for ( int stop = MaxValues, i = 1; i <= stop; i++)
          {
            try
            {
              if ( ! Eof( F ) )
              {
                  System::ReadLn( F, InputLIne );
                AuxParser[ActiveActor]->SetCmdString(InputLIne);
                for ( int stop = CSVColumn, iskip = 1; iskip <= stop; iskip++)
                  ParmName = AuxParser[ActiveActor]->GetNextParam();
                ResultArray[i - 1] = AuxParser[ActiveActor]->MakeDouble_();
              }
              else
              {
                result = i - 1;  // This will be different if less found;
                break;
              }
            }
            catch( exception & E )
            {
            {
              DoSimpleMsg("Error reading " + to_string(i) + "-th numeric array value from file: " + Param  + " Error is:" + (string)E.what(), 705 );
              result = i - 1;
              break;
            }
            }
          }
          System::CloseFile(F);
        }
        catch (exception &E)
        {
            DoSimpleMsg("An error has occurred: " + (string)E.what(), 760);
        }
      }
      else
        if ( ( ParmName.size() > 0 ) && ( CompareTextShortest( ParmName, "dblfile" ) == 0 ) )
        {
             // load the list from a file of doubles (no checking done on type of data)
          //MyStream = TMemoryStream.Create;
          if ( FileExists( Param ) )
          {
            int i = 0;
            streampos size;
            ifstream file( AdjustPath(Param), std::ios::binary );
            if (file.is_open())
            {
                file.seekg(0, ios_base::end);
                size = file.tellg();
                MyStream = new char[size];
                file.seekg(0, ios::beg);
                file.read(MyStream, size);
                file.close();
                double* double_values = (double*)MyStream;
                int totaldata = (int)( size / sizeof( ResultArray[0] ) );
                // Now move the doubles from the file into the destination array
                for (i = 0; i < totaldata; i++)
                {
                    ResultArray[i] = double_values[i];
                }
            }
          }
          else
            DoSimpleMsg( "File of doubles " + Param +" not found.", 70501 );
          free(MyStream);
        }
        else
          if ( ( ParmName.size() > 0 ) && ( CompareTextShortest( ParmName, "sngfile" ) == 0 ) )
          {
             // load the list from a file of singles (no checking done on type of data)
              if (FileExists(Param))
              {
                  int i = 0;
                  streampos size;
                  ifstream file(AdjustPath(Param), std::ios::binary);
                  if (file.is_open())
                  {
                      file.seekg(0, ios_base::end);
                      size = file.tellg();
                      MyStream = new char[size];
                      file.seekg(0, ios::beg);
                      file.read(MyStream, size);
                      file.close();
                      float* single_values = (float*)MyStream;
                      int totaldata = (int)(size / sizeof(float));
                      // Now move the doubles from the file into the destination array
                      for (i = 0; i < totaldata; i++)
                      {
                          ResultArray[i] = (double) single_values[i];
                      }
                  }
              }
              else
                  DoSimpleMsg("File of doubles " + Param + " not found.", 70501);
              free(MyStream);
          }
          else
          {  // Parse list of values off input string

             // Parse Values of array list
            for ( int stop = MaxValues, i = 1; i <= stop; i++)
            {
              ResultArray[i - 1] = AuxParser[ActiveActor]->MakeDouble_();    // Fills array with zeros if we run out of numbers
              String dummy = AuxParser[ActiveActor]->GetNextParam();
            }
          }
      return result;
    }

    //----------------------------------------------------------------------------


    double InterpretDblArrayMMF( unsigned char* myMap, int FileType, int Column, int Index, int DataSize )

    /* Gets the value for the value at INDEX within the file mapped (myMap)
      Considers the flags FileType, Column for locating the data within the file
      FileType :
        0 - normal file (ANSI char)
        1 - dblfile
        2 - sngfile
    */
    {
      double result = 0.0;
      unsigned char DBLByteArray[8/*# range 0..7*/]{};
      unsigned char SGLByteArray[4/*# range 0..3*/]{};
      String InputLIne, myContent;
      unsigned char myByte = 0;
      int OffSet = 0, i = 0, j = 0;
      result = 1.0; // Default Return Value;
      OffSet = ( Index - 1 ) * DataSize;
      if ( FileType == 0 )  // Normal file (CSV, txt, ASCII based file)
      {
        myContent = "";
        myByte = 0;
        i = OffSet;
        if ( myMap[i] == 0x0A )
          i++; // in case we are at the end of the previous line
        j = 0;
        while ( myByte != 0x0A )
        {
          myByte = myMap[i];
          // Concatenates avoiding special chars (EOL)
          if ( ( myByte >= 46 ) && ( myByte < 58 ) )
            myContent = myContent + ( AnsiChar ) ( myByte );
          if ( myByte == 44 )       // a comma char was found
          {                     // If we are at the column, exit, otherwise, keep looking
            j++;                 // discarding the previous number (not needed anyway)
            if ( j == Column )
              break;
            else
              myContent = "";
          }
          i++;
        }
        try
        {
          // checks if the extraction was OK, othwerwise, forces the default value
          if ( myContent.empty())
            myContent = "1.0";
          result = stod(myContent);
        }
        catch( exception & E )
        {
        {
          DoSimpleMsg( "Error reading " + to_string(i) + "- th numeric array value.Error is : " + (string) E.what(), 785 );
          result = i - 1;
        }
        }
      }
      else
        if ( FileType == 1 )     // DBL files
        {
        // load the list from a file of doubles (no checking done on type of data)
          for ( int stop = ( DataSize - 1 ), i = 0; i <= stop; i++)
            DBLByteArray[i] = myMap[i + OffSet];  // Load data into the temporary buffer
          result = *(double*) DBLByteArray;                                        // returns the number (double)
        }
        else
          if ( FileType == 2 )     // SGL files
          {
        // load the list from a file of doubles (no checking done on type of data)
            for ( int stop = ( DataSize - 1 ), i = 0; i <= stop; i++)
              SGLByteArray[i] = myMap[i + OffSet]; // Load data into the temporary buffer
            result = (double) *(float*)SGLByteArray;                                        // returns the number formatted as double
          }
      return result;
    }


    int InterpretIntArray( const String S, int MaxValues, pIntegerArray ResultArray )

    /* Get numeric values from an array specified either as a list on numbers or a text file spec.
      ResultArray must be allocated to MaxValues by calling routine.
      File is assumed to have one value per line.*/
    {
      int result = 0;
      String ParmName, Param;
      TTextRec F;
      int i = 0;
      AuxParser[ActiveActor]->SetCmdString(S);
      ParmName = AuxParser[ActiveActor]->GetNextParam();
      Param = AuxParser[ActiveActor]->MakeString_();
      result = MaxValues;  // Default return value

         /*Syntax can be either a list of numeric values or a file specification:  File= ...*/
      if ( CompareText( ParmName, "file" ) == 0 )
      {
             // load the list from a file
        try
        {
          System::AssignFile( F, Param );
          System::Reset( F );
          IOResultToException();
          for ( int stop = MaxValues, i = 1; i <= stop; i++)
          {
            if (!Eof(F))
            {
                System::ReadLn(F, (char*) &ResultArray[i]);
            }
            else
            {
              result = i - 1;
              break;
            }
          }
          System::CloseFile( F );
        }
        catch( exception & E )
        {
          DoSimpleMsg( "Error trying to read numeric array values from file: " + Param + "  Error is: " + (string) E.what(), 706 );
        }
      }
      else
      {  // Parse list of values off input string

             // Parse Values of array list
        for ( int stop = MaxValues, i = 1; i <= stop; i++)
        {
          ResultArray[i - 1] = AuxParser[ActiveActor]->MakeInteger_();    // Fills array with zeros if we run out of numbers
          String dummy = AuxParser[ActiveActor]->GetNextParam();
        }
      }
      return result;
    }

    double InterpretTimeStepSize( const String S )
    /*Return stepsize in seconds*/
    {
      double result = 0.0;
      int Code = 0;
      Char ch = '\0';
      String s2;
         /*Try to convert and see if we get an error*/
      val( S, &result, &Code );
      if ( Code == 0 )
        return result;  // Only a number was specified, so must be seconds

         /*Error occurred so must have a units specifier*/
      ch = S[S.size() - 1];  // get last character
      s2 = S.substr( 0, S.size() - 1 );
      val( s2, &result, &Code );
      if ( Code > 0 )
      {   /*check for error*/
        result = ActiveCircuit[ActiveActor]->Solution->DynaVars.h; // Don't change it
        DoSimpleMsg( "Error in specification of StepSize: " + S, 99933 );
        return result;
      }
      switch ( ch )
      {
        case 'h':
          result = result * 3600.0;
        break;
        case 'm':
          result = result * 60.0;
        break;
        case 's':
        break; // Do nothing
      default:
        result = ActiveCircuit[ActiveActor]->Solution->DynaVars.h; // Don't change it
        DoSimpleMsg( String( "Error in specification of StepSize: \"" ) + S + "\" Units can only be h, m, or s (single char only) ", 99934 );
      }
      return result;
    }


    //----------------------------------------------------------------------------



    void InitStringToNull( String& S )
    {
      Move( ZERONULL, S, 4 );
    }

    //----------------------------------------------------------------------------



    void ReallocStringArray( int& Maxsize, pStringArray ResultArray, int& Size )
    {
      int j = 0;
      ResultArray = (pStringArray) realloc(ResultArray, sizeof(ResultArray[0]) * Maxsize);
      for ( int stop = Maxsize, j = Size + 1; j <= stop; j++)
        InitStringToNull( ResultArray[j] );    // Init string values
    }


    void BumpUpStringArray( int& Maxsize, pStringArray ResultArray, int& Size )
    {
      Maxsize += 100;
      ReallocStringArray(Maxsize, ResultArray, Size);
    }


    void FreeStringArray( pStringArray ResultArray, int& Size )
    {
      int j = 0;
      if (( ResultArray != NULL))
      {
        for ( int stop = Size, j = 1; j <= stop; j++)
        {
          (ResultArray)[j] = "";
        }
        free(ResultArray);
      }
    }


    void InterpretAndAllocStrArray( const String S, int& Size, pStringArray ResultArray )

    /* Get string values from an array specified either as a list on strings or a text file spec.
      ResultArray is allocated as needed.
      File is assumed to have one value per line.*/
    {
      String ParmName, Param;
      TTextRec F;
      int Maxsize = 0;

         //  Throw Away any Previous Allocation
      Arraydef::FreeStringArray(ResultArray, Size);

         // Now Reallocate
      Maxsize = 100;  // initialize
      Size = 0;
      ReallocStringArray(Maxsize, ResultArray, Size);
      AuxParser[ActiveActor]->SetCmdString(S);
      ParmName = AuxParser[ActiveActor]->GetNextParam();
      Param = AuxParser[ActiveActor]->MakeString_();

         /*Syntax can be either a list of string values or a file specification:  File= ...*/
      if ( CompareText( ParmName, "file" ) == 0 )
      {
             // load the list from a file
        try
        {
          System::AssignFile( F, Param );
          System::Reset( F );
          IOResultToException();
          while ( ! Eof( F ) )
          {
              System::ReadLn( F, Param );
            if ( !Param.empty())
            {     // Ignore Blank Lines in File
              Size++;
              if ( Size > Maxsize )
                BumpUpStringArray(Maxsize, ResultArray, Size);
              ResultArray[Size] = Param;
            }
          }
          System::CloseFile( F );
        }
        catch( exception & E )
        {
          DoSimpleMsg( "Error trying to read numeric array values from a file. Error is: " + (string) E.what(), 707 );
        }
      }
      else
      {  // Parse list of values off input string

             // Parse Values of array list
        while ( !Param.empty())
        {
          Size++;
          if ( Size > Maxsize )
            BumpUpStringArray(Maxsize, ResultArray, Size);
          ResultArray[Size] = Param;
          ParmName = AuxParser[ActiveActor]->GetNextParam();
          Param = AuxParser[ActiveActor]->MakeString_();
        }
      }
      Maxsize = Size;   // Get rid of Excess Allocation
      ReallocStringArray(Maxsize, ResultArray, Size);
    }

    //----------------------------------------------------------------------------


    void InterpretTStringListArray( const String S, TStringList& ResultList )

    /* Get string values from an array specified either as a list on strings or a text file spec.
      ResultArray is allocated as needed.
      File is assumed to have one value per line.*/
    {
      String ParmName, Param, NextParam;
      TTextRec F;

         //  Throw Away any Previous Allocation
      ResultList.clear();
      AuxParser[ActiveActor]->SetCmdString(S);
      ParmName = AuxParser[ActiveActor]->GetNextParam();
      Param = AuxParser[ActiveActor]->MakeString_();

         /*Syntax can be either a list of string values or a file specification:  File= ...*/
      if ( CompareText( ParmName, "file" ) == 0 )
      {
             // load the list from a file
        try
        {
          System::AssignFile( F, Param );
          System::Reset( F );
          IOResultToException();
          while ( ! Eof( F ) )
          {
              System::ReadLn( F, Param );
            AuxParser[ActiveActor]->SetCmdString(Param);
            ParmName = AuxParser[ActiveActor]->GetNextParam();
            NextParam = AuxParser[ActiveActor]->MakeString_();
            if ( NextParam.size() > 0 )
            {     // Ignore Blank Lines in File
              ResultList.push_back( NextParam );
            }
          }
          System::CloseFile( F );
        }
        catch( exception & E )
        {
          DoSimpleMsg( "Error trying to read numeric array values from a file. Error is: " + (string) E.what(), 708 );
        }
      }
      else
      {  // Parse list of values off input string

             // Parse Values of array list
        while ( !Param.empty())
        {
          ResultList.push_back( Param );
          ParmName = AuxParser[ActiveActor]->GetNextParam();
          Param = AuxParser[ActiveActor]->MakeString_();
        }
      }
    }


    int InterpretCoreType( const String str )
    {
      int result = 0;
      switch ( str[1] )
      {
        case '1':
          result = 1;
        break;  // 1-phase

        case '3':
          result = 3;
        break;  // 3-Leg

        case '4':
          result = 4;
        break;  // 4-Leg

        case '5':
          result = 5;
        break;  // 5-Leg

        case 'c': case 'C':
          result = 9;
        break; // Core-1-phase
      default:
        result = 0; // shell
      }
      return result;
    }

    //----------------------------------------------------------------------------


    void ParseObjectClassandName( const String FullObjName, String& ClassName, String& ObjName )
    {
      size_t dotpos = 0;

          // Split off Obj class and name
      dotpos = FullObjName.find( "." );
      switch ( dotpos )
      {
        case String::npos:
        {
          ObjName = FullObjName.substr( 0, FullObjName.size() );  // assume it is all objname; class defaults
          ClassName = "";
        }
        break;
      default:
      {
        ClassName = FullObjName.substr( 0, dotpos );
        ObjName = FullObjName.substr((dotpos) + 1, FullObjName.size() );
      }
      }

          // Check object name in case it is a variable
      Parser[ActiveActor]->CheckforVar( ObjName );
    }


    String GetSolutionModeIDName( int idx )
    {
      String result;
      switch ( idx )
      {
        case SNAPSHOT:
          result = "Snap";
        break;
        case DAILYMODE:
          result = "Daily";
        break;
        case YEARLYMODE:
          result = "Yearly";
        break;
        case MONTECARLO1:
          result = "M1";
        break;
        case MONTECARLO2:
          result = "M2";
        break;
        case MONTECARLO3:
          result = "M3";
        break;
        case LOADDURATION1:
          result = "LD1";
        break;
        case LOADDURATION2:
          result = "LD2";
        break;
        case PEAKDAY:
          result = "Peakday";
        break;
        case DUTYCYCLE:
          result = "DUtycycle";
        break;
        case DIRECT:
          result = "DIrect";
        break;
        case DYNAMICMODE:
          result = "DYnamic";
        break;
        case MONTEFAULT:
          result = "MF";
        break;
        case FAULTSTUDY:
          result = "Faultstudy";
        break;
        case AUTOADDFLAG:
          result = "Autoadd";
        break;
        case HARMONICMODE:
          result = "Harmonic";
        break;
        case HARMONICMODET:
          result = "HarmonicT";
        break;
        case GENERALTIME:
          result = "Time";
        break;
        case EMPMODE:
            result = "EMPMODE";
        break;
        case EMPDAILYMODE:
            result = "EMPDAILYMODE";
        break;
      default:
        result = "UNKNOWN";
      }
      return result;
    }


    String GetSolutionModeID( )
    {
      String result;
      result = "UNKNOWN";
      if ( ActiveCircuit[ActiveActor] != NULL )
        result = GetSolutionModeIDName(ActiveCircuit[ActiveActor]->Solution->Get_SolMode() );
      return result;
    }


    String GetControlModeID( )
    {
      String result;
      result = "Unknown";
      if ( ActiveCircuit[ActiveActor] != NULL )
        switch ( ActiveCircuit[ActiveActor]->Solution->ControlMode )
        {
          case CTRLSTATIC:
            result = "STATIC";
          break;
          case EVENTDRIVEN:
            result = "EVENT";
          break;
          case TIMEDRIVEN:
            result = "TIME";
          break;
          case CONTROLSOFF:
            result = "OFF";
          break;
          case MULTIRATE:
            result = "MULTIRATE";
          break;
        default:
          result = "UNKNOWN";
        }
      return result;
    }


    String GetRandomModeID( )
    {
      String result;
      result = "Unknown";
      if ( ActiveCircuit[ActiveActor] != NULL )
        switch ( ActiveCircuit[ActiveActor]->Solution->RandomType )
        {
          case 0:
            result = "None";
          break;
          case GAUSSIAN:
            result = "Gaussian";
          break;
          case UNIFORM:
            result = "Uniform";
          break;
          case LOGNORMAL:
            result = "LogNormal";
          break;
        default:
          result = "Unknown";
        }
      return result;
    }


    String GetLoadModel( )
    {
      String result;
      switch ( ActiveCircuit[ActiveActor]->Solution->LoadModel )
      {
        case ADMITTANCE:
          result = "Admittance";
        break;
      default:
        result = "PowerFlow";
      }
      return result;
    }

    void ParseIntArray( pIntegerArray iarray, int& count, const String S )
    {
      String ParamName;
      String Param;
      int i = 0;

    // Parse the line once to get the count of tokens on string, S
      AuxParser[ActiveActor]->SetCmdString(S);
      count = 0;
      do
      {
        ParamName = AuxParser[ActiveActor]->GetNextParam();
        Param = AuxParser[ActiveActor]->MakeString_();
        if ( Param.size() > 0 )
          count++;
      }
      while ( ! ( Param.size() == 0 ) );

    //  reallocate iarray  to new size
      iarray = (pIntegerArray) realloc( iarray, sizeof(iarray[0]) * count);

    // Parse again for real
      AuxParser[ActiveActor]->SetCmdString(S);
      for ( int stop = count, i = 1; i <= stop; i++)
      {
        ParamName = AuxParser[ActiveActor]->GetNextParam();
        (iarray)[i] = AuxParser[ActiveActor]->MakeInteger_();
      }
    }


    bool IsShuntElement( const TDSSCktElement* elem )
    {
      bool result = false;
        switch ( elem->DSSObjType & CLASSMASK )
      {
        case CAP_ELEMENT:
        {
            result = ( ( TCapacitorObj* ) elem )->IsShunt;
            break; 
        }
        case REACTOR_ELEMENT:
        {
            result = ( ( TReactorObj* ) elem )->IsShunt;
            break;
        }
      default:
        result = false;
      }
      return result;
    }


    bool IslineElement( const TDSSCktElement* elem )
    {
      bool result = false;
      if ( ( ( elem->DSSObjType & CLASSMASK ) ) == LINE_ELEMENT )
        result = true;
      else
        result = false;
      return result;
    }


    bool IsTransformerElement( const TDSSCktElement* elem )
    {
      bool result = false;
      if ( ( ( elem->DSSObjType & CLASSMASK ) ) == XFMR_ELEMENT )
        result = true;
      else
        result = false;
      return result;
    }



    //----------------------------------------------------------------------------



    int GetCktElementIndex( const String FullObjName )

    // Given the full object name, return the index to the circuit element in the
    // active circuit.  Use full name if given. Else assume last class referenced.

    {
      int result = 0;
      int DevClassIndex = 0, Devindex = 0;
      String DevClassName, DevName;
      result = 0; // Default return value
      ParseObjectClassandName( FullObjName, DevClassName, DevName );
      DevClassIndex = ClassNames[ActiveActor].Find( DevClassName );
      if ( DevClassIndex == 0 )
        DevClassIndex = LastClassReferenced[ActiveActor];

         // Since there could be devices of the same name of different classes,
         // loop until we find one of the correct class
      /*# with ActiveCircuit[ActiveActor] do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActiveActor];
        {
          Devindex = with0->DeviceList.Find( DevName );
          while ( Devindex > 0 )
          {
            if (with0->DeviceRef[(Devindex) - 1].CktElementClass == DevClassIndex )   // we got a match
            {
              result = Devindex;
              return result;
            }
            Devindex = with0->DeviceList.FindNext();
          }
        }
      }
      return result;
    }

    //----------------------------------------------------------------------------



    String Str_Real( const double Value, int NumDecimals )
    {
      String result;
      try
      {
    //         Str(Value:0:NumDecimals, Result);
        result = FloatToStrF( Value, ffFixed, 0, NumDecimals );
      }
      catch(...)
      {
        result = "*****";
      }
      return result;
    }




    // - - - - - --------------------------------------------------



    String ReplaceCRLF( const String S )
    {
      String result;
      size_t nPos = 0;
        /*Replace CRLF with a \n character sequence*/
      result = S;
      nPos = result.find( CRLF );
      while ( nPos != String::npos )
      {
        result[nPos] = '\\';
        result[(nPos) + 1] = 'n';
        nPos = result.find( CRLF );
      }
      return result;
    }

    // - - - - - --------------------------------------------------


    String RestoreCRLF( const String S )
    {
      String result;
      size_t nPos = 0;
        /*Replace CRLF with a \n character sequence*/
      result = S;
      nPos = result.find( "\\n" );
      while ( nPos != String::npos )
      {
        result[nPos] = Char( 13 );
        result[(nPos) + 1] = Char( 10 );
        nPos = result.find( "\\n" );
      }
      return result;
    }

    // - - - - - --------------------------------------------------



    void DumpAllocationFactors( String& Filename )
    {
      TTextRec F;
      TLoadObj* pLoad;
      try
      {
        System::AssignFile( F, Filename );
        System::Rewrite( F );
        IOResultToException();
      }
      catch( exception & E )
      {
      {
        DoErrorMsg( String( "Error opening " ) + Filename + " for writing.", (string) E.what(), " File protected or other file error.", 709 );
        return;
      }
      }
      /*# with ActiveCircuit[ActiveActor] do */
      {
        Circuit::TDSSCircuit* with0 = ActiveCircuit[ActiveActor];
        {
          pLoad = (TLoadObj*) with0->Loads.Get_First();
          std::stringstream stream;
          while ( pLoad != NULL )
          {
            switch ( (*pLoad).LoadSpecType )
            {
              case 3:
              {
                  System::Write( F, "Load." + ( (TDSSCktElement*) pLoad )->get_Name() + ".AllocationFactor=" );
                  stream << std::fixed << std::setprecision(2) << pLoad->get_FkVAAllocationFactor();
                  System::WriteLn( F, stream.str());
              }
              break;
              case 4:
              {
                  System::Write( F, "Load." + ( (TDSSCktElement*) pLoad)->get_Name() + ".CFactor=" );
                  stream << std::fixed << std::setprecision(2) << pLoad->get_FCFactor();
                  System::WriteLn(F, stream.str());
              }
              break;
            }
            pLoad = (TLoadObj*) with0->Loads.Get_Next();
          } /*While*/
        }
      } /*With*/
      System::CloseFile(F);
      GlobalResult = Filename;
    }


    // - - - - - --------------------------------------------------



    void DumpAllDSSCommands( String& Filename )
    {
      TTextRec F;
      TDSSClass* pClass;
      int i = 0;
      try
      {
        Filename = GetOutputDirectory() + "DSSCommandsDump.Txt";
        System::AssignFile( F, Filename );
        System::Rewrite( F );
        IOResultToException();
      }
      catch( exception & E )
      {
      {
        DoErrorMsg( String( "Error opening " ) + Filename + " for writing.", (string) E.what(), "Disk protected or other file error", 710 );
        return;
      }
      }

      // dump Executive commands
      System::WriteLn( F, "[execcommands]" );
      for ( int stop = NumExecCommands, i = 1; i <= stop; i++)
      {
          System::Write( F, i, 0 ); 
          System::Write( F, ", \"" ); 
          System::Write( F, ExecCommand[i] ); 
          System::Write( F, "\", \"" ); 
          System::Write( F, ReplaceCRLF( CommandHelp[i] ) ); 
          System::WriteLn( F, '\"' );
      }

      // Dump Executive Options
      System::WriteLn( F, "[execoptions]" );
      for ( int stop = NumExecOptions, i = 1; i <= stop; i++)
      {
          System::Write( F, i, 0 ); 
          System::Write( F, ", \"" ); 
          System::Write( F, ExecOption[i] ); 
          System::Write( F, "\", \"" ); 
          System::Write( F, ReplaceCRLF( OptionHelp[i] ) ); 
          System::WriteLn( F, '\"' );
      }

      // Dump All presend DSSClasses
      pClass = (TDSSClass*) (DSSClassList[ActiveActor].Get_First());
      while ( pClass != NULL )
      {
          System::Write( F, '[' ); 
          System::Write( F, (*pClass).get_myClass_name() ); 
          System::WriteLn( F, ']' );
        for ( int stop = (*pClass).NumProperties, i = 1; i <= stop; i++)
        {
            System::Write( F, i, 0 ); 
            System::Write( F, ", \"" ); 
            System::Write( F, (*pClass).PropertyName[i] ); 
            System::Write( F, "\", \"" ); 
            System::Write( F, ReplaceCRLF( (*pClass).PropertyHelp[i] ) ); 
            System::WriteLn( F, '\"' );
        }
        pClass = (TDSSClass*) DSSClassList[ActiveActor].Get_Next();
      }
      System::CloseFile( F );
    }

    //----------------------------------------------------------------------------



    double NearestBasekV( double kV )

    /*Find closest base voltage*/
    {
      double result = 0.0;
      double TestkV = 0.0;
      int count = 0;
      double Diff = 0.0, MinDiff = 0.0;
      count = 0;
      TestkV = ActiveCircuit[ActiveActor]->LegalVoltageBases[1 - 1];
      result = TestkV;
      MinDiff = 1.0e50;  // Big whompin number
      while ( TestkV != 0.0 )
      {
        Diff = Abs( 1.0 - kV / TestkV );     // Get Per unit difference
        if ( Diff < MinDiff )
        {
          MinDiff = Diff;
          result = TestkV;
        }
        ++count;
        if (count < ActiveCircuit[ActiveActor]->LegalVoltageBases.size())
            TestkV = ActiveCircuit[ActiveActor]->LegalVoltageBases[count];
        else
            TestkV = 0;
      }
      return result;
    }

    //----------------------------------------------------------------------------



    bool SavePresentVoltages( )
    {
      bool result = false;
      TTypedFile< double > F;
      int i = 0;
      double dNumNodes = 0.0;
      result = true;
      try
      {
        System::AssignFile( F, GetOutputDirectory() + CircuitName_[ActiveActor] + "SavedVoltages.dbl" );
        System::Rewrite( F );
        IOResultToException();
      }
      catch( exception & E )
      {
      {
        DoSimpleMsg( "Error opening/creating file to save voltages: " + (string) E.what(), 711 );
        result = false;
        return result;
      }
      }
      try
      {
        /*# with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution do */
        {
          TDSSCircuit* with0 = ActiveCircuit[ActiveActor];
          {
            dNumNodes = (*with0).NumNodes;
            System::Write( F, &dNumNodes );
            for ( int stop = (*with0).NumNodes, i = 1; i <= stop; i++)
            {
                System::Write( F, &( with0->Solution->NodeV[i].re ) );
                System::Write( F, &( with0->Solution->NodeV[i].im ) );
            }
          }
        }
        System::CloseFile( F );
      }
      catch( exception & E )
      {
      {
        DoSimpleMsg( "Error writing file to save voltages: " + (string) E.what(), 712 );
        result = false;
      }
      }
      return result;
    }

    //----------------------------------------------------------------------------


    bool RetrieveSavedVoltages( )
    {
      bool result = false;
      TTypedFile< double > F;
      int i = 0;
      double dNumNodes = 0.0;
      result = true;
      try
      {
        System::AssignFile( F, GetOutputDirectory() + CircuitName_[ActiveActor] + "SavedVoltages.dbl" );
        System::Reset( F );
        IOResultToException();
      }
      catch( exception & E )
      {
      {
        DoSimpleMsg( "Error opening file to retrieve saved voltages: " + (string) E.what(), 713 );
        result = false;
        return result;
      }
      }
      try
      {
        /*# with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution do */
        {
          Circuit::TDSSCircuit* with0 = ActiveCircuit[ActiveActor];
          {
            Read( F, &dNumNodes );
            if ( (*with0).NumNodes == Round( dNumNodes ) )
              for ( int stop = (*with0).NumNodes, i = 1; i <= stop; i++)
              {
                Read( F, &( with0->Solution->NodeV[i].re ) ); Read( F, &( with0->Solution->NodeV[i].im ) );
              }
            else
            {
              DoSimpleMsg( "Saved results do not match present circuit. Aborting.", 714 );
              result = false;
            }
          }
        }
        System::CloseFile( F );
      }
      catch( exception & E )
      {
      {
        DoSimpleMsg( "Error reading file to retrieve saved voltages: " + (string) E.what(), 715 );
        result = false;
      }
      }
      return result;
    }

    //----------------------------------------------------------------------------



    bool InitializeForHarmonics( int ActorID )

    /*Intialize PCELEMENT base values for harmonics analysis*/
    {
      bool result = false;
      TPCElement* PCelem;
      if   // Zap voltage vector to disk
      ( SavePresentVoltages() )
        /*# with ActiveCircuit[ActorID] do */
        {
          Circuit::TDSSCircuit* with0 = ActiveCircuit[ActorID];
          {
        // Go through all PC Elements
            PCelem = (TPCElement*) with0->PCElements.Get_First();
            while ( PCelem != NULL )
            {
              if ( PCelem->Get_Enabled() )
                PCelem->InitHarmonics( ActorID );   // Virtual function
              PCelem = (TPCElement*) with0->PCElements.Get_Next();
            }
            result = true;
          }
        } /*With*/
      else
        result = false;
      return result;
    }


    //----------------------------------------------------------------------------



    void CalcInitialMachineStates( )
    {
      TPCElement* PCelem;

    // Do All PC Elements

    // If state variables not defined for a PC class, does nothing
      /*# with ActiveCircuit[ActiveActor] do */
      {
        auto with0 = ActiveCircuit[ActiveActor];
        {
          PCelem = (TPCElement*) with0->PCElements.Get_First();
          while ( PCelem != NULL )
          {
            if ( PCelem->Get_Enabled() )
                PCelem->InitStateVars( ActiveActor );
            PCelem = (TPCElement*) with0->PCElements.Get_Next();
          }
        }
      }
    }

    //----------------------------------------------------------------------------



    void InvalidateAllPCElements( )
    {
      TPCElement* PCelem;

    // Invalidate All PC Elements; Any could be a machine
      /*# with ActiveCircuit[ActiveActor] do */
      {
        auto with0 = ActiveCircuit[ActiveActor];
        {
          PCelem = (TPCElement*) with0->PCElements.Get_First();
          while ( PCelem != NULL )
          {
            if ((*PCelem).Get_Enabled() )
                (*PCelem).Set_YprimInvalid(ActiveActor,true);
            PCelem = (TPCElement*) with0->PCElements.Get_Next();
          }
        }
      }
    }


    double PresentTimeInSec( int ActorID )
    {
      double result = 0.0;
      /*# with ActiveCircuit[ActorID].Solution do */
      result = ActiveCircuit[ActiveActor]->Solution->DynaVars.T + ActiveCircuit[ActiveActor]->Solution->DynaVars.intHour * 3600.0;
      return result;
    }


    //----------------------------------------------------------------------------



    int DoResetFaults( )
    {
      int result = 0;
      TFaultObj* pFault;
      result = 0;
      /*# with ActiveCircuit[ActiveActor] do */
      {
        auto with0 = ActiveCircuit[ActiveActor];
        {
          pFault = (TFaultObj*) with0->Faults.Get_First();
          while ( pFault != NULL )
          {
            pFault->Reset();
            pFault = (TFaultObj*) with0->Faults.Get_Next();
          }
        }
      }  /*End With*/
      return result;
    }


    //----------------------------------------------------------------------------



    int DoResetControls( )
    {
      int result = 0;
      TControlElem* ControlDevice;
      result = 0;
      /*# with ActiveCircuit[ActiveActor] do */
      {
        auto with0 = ActiveCircuit[ActiveActor];
        {
          ControlDevice = (TControlElem*) with0 -> DSSControls.Get_First();
          while ( ControlDevice != NULL )
          {
            if ( ControlDevice->Get_Enabled() )
              ControlDevice->Reset( ActiveActor );
            ControlDevice = (TControlElem*) with0-> DSSControls.Get_Next();
          }
        }
      }  /*End With*/
      return result;
    }

    //----------------------------------------------------------------------------



    int GetNodeNum( int NodeRef )
    {
      int result = 0;
      if ( NodeRef == 0 )
        result = 0;
      else
        result = ActiveCircuit[ActiveActor]->MapNodeToBus[(NodeRef) - 1].NodeNum;
      return result;
    }


    //----------------------------------------------------------------------------



    void RotatePhasorDeg( complex& Phasor, const double h, const double AngleDeg )

    // rotate a phasor by an angle and harmonic

    {
      Phasor = cmul( Phasor, pdegtocomplex( 1.0, h * AngleDeg ) );
    }


    void RotatePhasorRad( complex& Phasor, const double h, const double AngleRad )

    // rotate a phasor by an angle and harmonic

    {
      Phasor = cmul( Phasor, pclx( 1.0, h * AngleRad ) );
    }

    //----------------------------------------------------------------------------


    double PFSign( const complex S )
    {
      double result = 0.0;
      if ( S.re * S.im < 0.0 )
        result = - 1.0;
      else
        result = 1.0;
      return result;
    }


    void ConvertComplexArrayToPowerandPF( const pComplexArray Buffer, int n )

    /*Creates continous PF function from 1 to 2 where 1-2 range is leading (opposite sign)*/
    {
      double mag = 0.0, PF = 0.0;
      int i = 0;

    /*Assume we get P + jQ*/
      for ( int stop = n, i = 1; i <= stop; i++)
      {
        mag = cabs( Buffer[i] );
        if ( mag > 0.0 )
        {
          PF = double( PFSign( Buffer[i] ) ) * abs( Buffer[i].re ) / mag;
          if ( PF < 0.0 )
            PF = 2.0 - Abs( PF );
        }
        else
          PF = 1.0;  // for zero power
        Buffer[i].im = PF;
      }
    }



    //----------------------------------------------------------------------------



    void ConvertComplexArrayToPolar( const pComplexArray Buffer, int n )
    {
      polar x;
      int i = 0;
      for ( int stop = n, i = 1; i <= stop; i++)
      {
        x = ctopolardeg( Buffer[i - 1] );
        /*# with Buffer^[i], x do */
        {
          Buffer[i - 1].re = x.mag;
          Buffer[i - 1].im = x.ang;
        }
      }
    }

    //----------------------------------------------------------------------------



    complex Residual( void* p, int Nph )
    // Assume p points to complex array
    // compute residual of the number of phases specified and convert to polar

    {
      complex result;
      pComplexArray pc;
      int i = 0;
      pc = (pComplexArray) p;
      result = CZero;
      for ( int stop = Nph, i = 1; i <= stop; i++)
          caccum( result, pc[i] );
      return result;
    }

    //----------------------------------------------------------------------------



    complex ResidualPolar( void* p, int Nph )
    // Assume p points to complex array
    // compute residual of the number of phases specified and convert to polar

    {
      complex result = CZero;
      complex x = CZero;
      x = Residual( p, Nph );
      result.re = cabs( x );
      result.im = cdang( x );
      return result;
    }


    double Sign( double x )
    {
      double result = 0.0;
      if ( x < 0.0 )
        result = - 1.0;
      else
        result = 1.0;
      return result;
    }


    double PowerFactor( const complex S )
    {
      double result = 0.0;
      if ( ( S.re != 0.0 ) && ( S.im != 0.0 ) )
        result = double( Sign( S.re * S.im ) ) * Abs( S.re ) / cabs( S );
      else
        result = 1.0;
      return result;
    }


    double ConvertPFToPFRange2( const double Value )
    /*Convert PF from +/- 1 to 0..2 Where 1..2 is leading*/
    {
      double result = 0.0;
      if ( Value < 0.0 )
        result = 2.0 + Value;
      else
        result = Value;
      return result;
    }


    double ConvertPFRange2ToPF( const double Value )
    {
      double result = 0.0;
      if ( Value > 1.0 )
        result = Value - 2.0;
      else
        result = Value;
      return result;
    }


    void ClearEventLog( )
    {
      try
      {
    /*****  WriteDLLDebugFile(Format('ClearEventLog: EventStrings= %p', [@EventStrings])); */
        EventStrings[ActiveActor].clear();
      }
      catch( exception & E )
      {
        DoSimpleMsg( "Exception clearing event log: " + (string)E.what() + ", @EventStrings=" /* + to_string(EventStrings[ActiveActor] */, 7151);
      }
    }


    void ClearErrorLog( )
    {
      try
      {
    /*****  WriteDLLDebugFile(Format('ClearEventLog: EventStrings= %p', [@EventStrings])); */
        ErrorStrings[ActiveActor].clear();
      }
      catch( exception & E )
      {
        DoSimpleMsg( "Exception clearing error log: " + (string) E.what() + ", @EventStrings=" /* + &EventStrings */, 71511 );
      }
    }


    void LogThisEvent( const String EventName, const int ActorID )
    {
        /*****  WriteDLLDebugFile(Format('LogThisEvent: EventStrings= %p', [@EventStrings])); */
      /*# with ActiveCircuit[ActorID].Solution do */
      auto with0 = ActiveCircuit[ActiveActor];
      EventStrings[ActorID].push_back( "Hour=" + to_string(with0->Solution->DynaVars.intHour) +
          ", Sec=" + to_string(with0->Solution->DynaVars.T) +
          ", Iteration=" + to_string(with0->Solution->Iteration) +
          ", ControlIter=" + to_string(with0->Solution->ControlIteration) +
          ", Event=" + EventName);

         //     'Time=' + TimeToStr(Time)+': '+EventName);
     /*****  ShowMessageForm(EventStrings); */
    }

    void AppendToEventLog( const String opdev, const String Action, const int ActorID )
    {
      String S;
      /*****  WriteDLLDebugFile(Format('LogThisEvent: EventStrings= %p', [@EventStrings])); */
      /*# with ActiveCircuit[ActorID].Solution do */
      auto with0 = ActiveCircuit[ActiveActor];
      S = Format("Hour=%d, Sec=%-.5g, ControlIter=%d, Element=%s, Action=%s", with0->Solution->DynaVars.intHour, with0->Solution->DynaVars.T, with0->Solution->ControlIteration, opdev.c_str(), UpperCase(Action).c_str());
      EventStrings[ActorID].push_back( S );
      /*****  ShowMessageForm(EventStrings); */
    }


    void DumpComplexMatrix( Textfile& F, TcMatrix* AMatrix )
    {
      int i = 0, j = 0;
      try
      {
        if (AMatrix != NULL)
        {
          System::WriteLn( F, "!(Real part)" );
          /*# with AMatrix do */
          {
            for ( int stop = AMatrix->get_Norder(), i = 1; i <= stop; i++)
            {
              System::Write(F, "! " );
              for ( int stop = i, j = 1; j <= stop; j++)
              {
                System::Write( F, Format( "%g ",  AMatrix->GetElement( i, j ).re ));
              }
              WriteLn( F );
            }
            System::WriteLn( F, "!(Imaginary part) = " );
            for ( int stop = AMatrix->get_Norder(), i = 1; i <= stop; i++)
            {
              System::Write( F, "! " );
              for ( int stop = i, j = 1; j <= stop; j++)
              {
                Write(  F, Format( "%g ", AMatrix->GetElement( i, j ).im ));
              }
              WriteLn( F );
            }
          }
        }
      }
      catch( exception & E )
      {
      {
        DoSimpleMsg( "Error in Dump Complex Matrix: " + (string) E.what() + "  Write aborted.", 716 );
      }
      }
    }


    bool AllTerminalsClosed( TDSSCktElement* ThisElement )
    // check all conductors of this element to see IF it is closed.
    // Make sure at least one phase on each terminal is closed.

    {
      bool result = false;
      int i = 0, j = 0;
      result = false;
      for ( int stop = ThisElement->Get_NTerms(), i = 1; i <= stop; i++)
      {
        result = false;
        ThisElement->Set_ActiveTerminal(i);
        for ( int stop = ThisElement->Get_NPhases(), j = 1; j <= stop; j++)
          if ( ThisElement->Get_ConductorClosed(j, ActiveActor) )
          {
            result = true;
            break;
          }
        if ( ! result )
          return result;  // didn't find a closed phase on this terminal
      }
      return result;
    }


    bool WriteVsourceClassFile(TDSSClass &Dss_Class, bool IsCktElement)
    {
        bool result = false;
        TTextRec F;
        String ClassName;
        result = true;
        if (Dss_Class.Get_ElementCount() == 0)
            return result;
        try
        {
            ClassName = Dss_Class.get_myClass_name();
            Assign(F, ClassName + ".dss");
            Rewrite(F);
            IOResultToException();
            SavedFileList[ActiveActor].push_back(ClassName + ".dss");
            Dss_Class.Get_First(); // Sets ActiveDSSObject
            WriteActiveDSSObject(F, "Edit"); // Write Get_First() Vsource out as an Edit
            while (Dss_Class.Get_Next() > 0)
            {
                // Skip Cktelements that have been checked before and written out by something else
                if (((TDSSCktElement*)ActiveDSSObject[ActiveActor])->HasBeenSaved)
                    continue;
                // Skip disabled circuit elements; write all general DSS objects
                WriteActiveDSSObject(F, "New"); // sets HasBeenSaved := TRUE
            }
            CloseFile(F);
            Dss_Class.Saved = true;
        }
        catch (exception& E)
        {
            DoSimpleMsg("WriteClassFile Error: " + (string)E.what(), 717);
            result = false;
        }
        return true;
    }
    bool WriteClassFile(TDSSClass &Dss_Class, String Filename, bool IsCktElement)
    {
        bool result = false;
        TTextRec F;
        String ClassName;
        bool IsEnabled;
        result = true;
        int Nrecords = 0;
        if (Dss_Class.Get_ElementCount() == 0)
            return result;
        try
        {
            ClassName = Dss_Class.get_myClass_name();
            Assign(F, ClassName + ".dss");
            Rewrite(F);
            IOResultToException();
            Dss_Class.Get_First(); // Sets ActiveDSSObject
            do
            {
                // Skip Cktelements that have been checked before and written out by something else
                if (IsCktElement && ((TDSSCktElement*)ActiveDSSObject[ActiveActor])->HasBeenSaved)
                {
                    continue;
                }
                IsEnabled = true;

                if (LowerCase(Dss_Class.Class_Name) == "loadshape")
                {
                    IsEnabled = ((TLoadShapeObj*)ActiveDSSObject[ActiveActor])->Enabled;
                }

                // Skip disabled circuit elements; write all general DSS objects
                if (IsEnabled)
                {
                    WriteActiveDSSObject(F, "New"); // sets HasBeenSaved := TRUE
                    Nrecords++;
                }
            }
            while (Dss_Class.Get_Next() > 0);
            CloseFile(F);
            Dss_Class.Saved = true;
            if (Nrecords > 0)
            {
                SavedFileList[ActiveActor].push_back(ClassName + ".dss");
            }
            else
            {
#ifdef WIN32
                Assign(F, ClassName + ".dss");
                Erase(F);
                IOResultToException();
#endif
            }
        }
        catch (exception& E)
        {
            DoSimpleMsg("WriteClassFile Error: " + (string)E.what(), 717);
            result = false;
        }
        return true;
    }


    String CheckForBlanks( const String &S )
    /*Checks for blanks in the name and puts quotes around it*/
    {
      String result = S;
      if ( S.find( " " ) != String::npos)
        if ( S[0] != '(' )  // Ignore if already quoted
          if ( S[0] != '[' )  // Ignore if already quoted
            if ( S[0] != '{' )  // Ignore if already quoted
              result = String( "\"" ) + S + "\"";
      return result;
    }


    void WriteActiveDSSObject( Textfile& F, const String NeworEdit )
    {
      TDSSClass &ParClass = *((TDSSCktElement*)ActiveDSSObject[ActiveActor])->ParentClass;
          //Write(F, NeworEdit, ' "', ParClass.Name + '.' + ActiveDSSObject[ActiveActor].Name,'"');
      System::Write(F, NeworEdit + " " + ParClass.get_myClass_name() + "." + ((TDSSCktElement*)ActiveDSSObject[ActiveActor])->get_Name());
      ((TDSSCktElement*)ActiveDSSObject[ActiveActor])->SaveWrite( F );



       // Handle disabled circuit elements;   Modified to allow applets to save disabled elements 12-28-06
      if ( ( (((TDSSCktElement*)ActiveDSSObject[ActiveActor])->DSSObjType & CLASSMASK ) ) != DSS_OBJECT )
        if ( !((TDSSCktElement*)ActiveDSSObject[ActiveActor])->Get_Enabled() )
        {
          System::Write( F, " ENABLED=NO" );
        }
      WriteLn( F ); // Terminate line
      ((TDSSCktElement*)ActiveDSSObject[ActiveActor])->HasBeenSaved = true;
    }


    void DoResetKeepList( )
    {
      int i = 0;
      /*# with ActiveCircuit[ActiveActor] do */
      {
        auto with0 = ActiveCircuit[ActiveActor];
        for ( int stop = with0->NumBuses, i = 1; i <= stop; i++)
            with0->Buses[(i) - 1]->Keep = false;
      }
    }


    String ExtractComment( const String S )
    {
      String result;
      result = S.substr( S.find( "!" ), S.size() );
      return result;
    }


    bool RewriteAlignedFile( const String Filename )
    {
      bool result = false;
      TTextRec Fin, Fout;
      String SaveDelims, Line, Field, AlignedFile;
      pIntegerArray FieldLength;
      int ArraySize = 0, FieldLen = 0, FieldNum = 0;
      result = true;
      try
      {
        AssignFile( Fin, Filename );
        Reset( Fin );
        IOResultToException();
      }
      catch( exception & E )
      {
      {
        DoSimpleMsg( "Error opening file: " + Filename + ", " +  (string) E.what(), 719 );
        result = false;
        return result;
      }
      }
      try
      {
        AlignedFile = ExtractFilePath( Filename ) + "Aligned_" + ExtractFileName( Filename );
        AssignFile( Fout, AlignedFile );
        Rewrite( Fout );
        IOResultToException();
      }
      catch( exception & E )
      {
      {
        DoSimpleMsg( "Error opening file: " + AlignedFile + ", " + (string) E.what(), 720 );
        CloseFile( Fin );
        result = false;
        return result;
      }
      }
      SaveDelims = AuxParser[ActiveActor]->get_delimchars();
      AuxParser[ActiveActor]->set_delimchars(", ");
      ArraySize = 10;
      FieldLength = new longInt[ ArraySize ];
      try
      {
          /*Scan once to set field lengths*/
          while (!Eof(Fin))
          {
              ReadLn(Fin, Line);
              AuxParser[ActiveActor]->SetCmdString(Line);  // Load the parsr
              FieldNum = 0;
              do
              {
                  String dummy = AuxParser[ActiveActor]->GetNextParam();
                  Field = AuxParser[ActiveActor]->MakeString_();
                  FieldLen = Field.length();
                  if (Field.find(" ") != String::npos)
                      FieldLen = FieldLen + 2;
                  if (FieldLen > 0)
                  {
                      FieldNum++;
                      if (FieldNum > ArraySize)
                      {
                          ArraySize = FieldNum;
                          FieldLength = (pIntegerArray)realloc(FieldLength, sizeof(FieldLength[0]) * ArraySize);
                          FieldLength[FieldNum] = FieldLen;
                      }
                      else
                          if (FieldLen > FieldLength[FieldNum])
                              FieldLength[FieldNum] = FieldLen;
                  }
              } while (!(FieldLen == 0));
          }

          /*Now go back and re-read while writing the new file*/
          Reset(Fin);
          IOResultToException();
          while (!Eof(Fin))
          {
              ReadLn(Fin, Line);
              AuxParser[ActiveActor]->SetCmdString(Line);  // Load the parser
              FieldNum = 0;
              do
              {
                  String dummy = AuxParser[ActiveActor]->GetNextParam();
                  Field = AuxParser[ActiveActor]->MakeString_();
                  if (Field.find(" ") != String::npos)
                      Field = String("\"") + Field + "\"";  // add quotes if a space in field
                  FieldLen = Field.length();
                  if (FieldLen > 0)
                  {
                      FieldNum++;
                      Write(Fout, Pad(Field, FieldLength[FieldNum] + 1));
                  }
              } while (!(FieldLen == 0));
              if (Line.find("!") != String::npos)
                  Write(Fout, ExtractComment(Line));
              WriteLn(Fout);
          }
    //  }
    //  __finally
    //  {
         /*Make sure we do this stuff ...*/
        CloseFile( Fin );
        CloseFile( Fout );
        free(FieldLength);
        AuxParser[ActiveActor]->set_delimchars(SaveDelims);
      }
      catch (std::exception &E)
      { 
          //  added to facilitate implementation in C++ native statements
          // It does nothing though
      }
      GlobalResult = AlignedFile;
      return result;
    }

    int DoExecutiveCommand( const String S )
    {
      int result = 0;
      DSSExecutive[ActiveActor]->Set_Command(S);
      result = DSSExecutive[ActiveActor]->Get_ErrorResult();
      return result;
    }


    bool CheckParallel( const TDSSCktElement* Line1, const TDSSCktElement* Line2 )
      /*Check to see if two lines are in parallel*/
    {
      bool result = false;
      result = false;
      if ( Line1->Terminals[0].BusRef == Line2->Terminals[0].BusRef )
        if ( Line1->Terminals[1].BusRef == Line2->Terminals[1].BusRef )
        {
          result = true;
          return result;
        }
      if ( Line1->Terminals[1].BusRef == Line2->Terminals[0].BusRef )
        if ( Line1->Terminals[0].BusRef == Line2->Terminals[1].BusRef )
        {
          result = true;
          return result;
        }
      return result;
    }


    double GetMaxPUVoltage( )
    {
      double result = 0.0;
      int i = 0, j = 0, Nref = 0;
      result = - 1.0;
      /*# with ActiveCircuit[ActiveActor] do */
      {
        auto with0 = ActiveCircuit[ActiveActor];
        {
          for ( int stop = with0->NumBuses, i = 1; i <= stop; i++)
          {
            if ((with0->Buses[i - 1 ]->kVBase) > 0.0 )
            {
              for ( int stop = with0->Buses[i - 1]->get_FNumNodesThisBus(), j = 1; j <= stop; j++)
              {
                Nref = with0->Buses[i - 1]->GetRef( j );
                if ( Nref > 0 )
                  result = max( result, double( cabs(with0->Solution->NodeV[Nref] ) ) / (with0->Buses[i - 1]->kVBase) );
              }
            }
          }
          result = result * 0.001;
        }
      }
      return result;
    }


    double GetMinPUVoltage( bool IgnoreNeutrals )
    {
      double result = 0.0;
      int i = 0, j = 0, Nref = 0;
      bool MinFound = false;
      double Vmagpu = 0.0;
      result = 1.0e50; // start with big number
      MinFound = false;
      /*# with ActiveCircuit[ActiveActor] do */
      {
         auto with0 = ActiveCircuit[ActiveActor];
        {
          for ( int stop = with0->NumBuses, i = 1; i <= stop; i++)
            /*# with Buses^[i] do */
            {
              auto with1 = with0->Buses[i - 1];
              if ( with1->kVBase > 0.0 )
              {
                for ( int stop = with1->get_FNumNodesThisBus(), j = 1; j <= stop; j++)
                {
                  Nref = with1->GetRef( j );
                  if ( Nref > 0 )
                  {
                    Vmagpu = double( cabs( with0->Solution->NodeV[Nref] ) ) / with1->kVBase;
                    if ( IgnoreNeutrals )
                    {
                      if ( Vmagpu > 100.0 )
                      {  // 0.1 pu
                        result = min( result, Vmagpu );   // only check buses greater than 10%
                        MinFound = true;
                      }
                    }
                    else
                    {
                      result = min( result, Vmagpu );
                      MinFound = true;
                    }
                  }
                }
              }
            }
          result = result * 0.001;
        }
      }
      if ( ! MinFound )
        result = - 1.0;
      return result;
    }


    complex GetTotalPowerFromSources( int ActorID )
    {
      complex result;
      TDSSCktElement* cktElem;
      result = CZero;
      cktElem = (TDSSCktElement*) ActiveCircuit[ActorID]->Sources.Get_First();
      while ( cktElem != NULL )
      {
         //----CktElem.ActiveTerminalIdx := 1;
        caccum( result, cnegate( cktElem->Get_Power(1, ActorID) ) );
        cktElem = (TDSSCktElement*) ActiveCircuit[ActorID]->Sources.Get_Next();
      }
      return result;
    }


    void WriteUniformGenerators( Textfile& F, double kW, double PF, bool DoGenerators )
     /* Distribute the generators uniformly amongst the feeder nodes that have loads*/
    {
      double kWeach = 0.0;
      TDSSClass LoadClass;
      TLoadObj* pLoad;
      int count = 0, i = 0;
      LoadClass = * ( (TDSSClass*) GetDSSClassPtr( "load" ) );
      count = LoadClass.ElementList.get_myNumList();
      kWeach = kW / max( 1, count );
      if ( ActiveCircuit[ActiveActor]->PositiveSequence )
        kWeach = kWeach / 3.0;
      for ( int stop = count, i = 1; i <= stop; i++)
      {
        pLoad = (TLoadObj*) LoadClass.ElementList.Get( i ) ;
        if ( ( (TDSSCktElement*) pLoad )->Get_Enabled() )
        {
          if ( DoGenerators )
          {
            System::Write( F, "new generator.DG_" + to_string(i) + "bus1=" + pLoad->GetBus( 1 ));
          }
          else
          {
            System::Write( F, "new load.DL_" + to_string(i) + "  bus1=" + pLoad->GetBus( 1 ));
          }
          /*# with ActiveCircuit[ActiveActor] do */
          {
              Circuit::TDSSCircuit* with0 = ActiveCircuit[ActiveActor];;
            {
              System::Write( F, Format( " phases=%d kV=%-g", pLoad->Get_NPhases(), pLoad->kVLoadBase ));
              System::Write( F, Format( " kW=%-g",  kWeach ));
              System::Write( F, Format( " PF=%-.3g",  PF ));
            }
          }
          System::Write( F, " model=1" );
          System::WriteLn( F );
        }
      }
    }

    void WriteRandomGenerators( Textfile& F, double kW, double PF, bool DoGenerators )
    /*Distribute Generators randomly to loaded buses*/
    {
      double kWeach = 0.0;
      TDSSClass LoadClass;
      TLoadObj* pLoad;
      int count = 0, i = 0, LoadCount = 0;
      LoadClass = *( (TDSSClass*) GetDSSClassPtr( "load" ) );
      count = LoadClass.ElementList.get_myNumList();
       /*Count enabled loads*/
      LoadCount = 0;
      for ( int stop = count, i = 1; i <= stop; i++)
      {
        pLoad = (TLoadObj*) LoadClass.ElementList.Get( i );
        if (((TDSSCktElement*)pLoad)->Get_Enabled() )
          LoadCount++;
      }
      kWeach = kW / LoadCount;  // median sized generator
      if ( ActiveCircuit[ActiveActor]->PositiveSequence )
        kWeach = kWeach / 3.0;
      //Randomize; // pending to verify if such function has an eq in C++

       /*Place random sizes on load buses so that total is approximately what was spec'd*/
      for ( int stop = count, i = 1; i <= stop; i++)
      {
        pLoad = (TLoadObj*) LoadClass.ElementList.Get( i ) ;
        if (((TDSSCktElement*)pLoad)->Get_Enabled() )
        {
          if ( DoGenerators )
          {
            System::Write( F, "new generator.DG_" + to_string(i) + "  bus1=" + (pLoad)->GetBus( 1 ));
          }
          else
          {
            System::Write( F, "new load.DL_" + to_string(i) + "  bus1=" + (pLoad)->GetBus( 1 ));
          }
          /*# with ActiveCircuit[ActiveActor] do */
          {
            Circuit::TDSSCircuit* with0 = ActiveCircuit[ActiveActor];
            {
              System::Write( F, Format( " phases=%d kV=%-g", pLoad->Get_NPhases(), pLoad->kVLoadBase ));
              System::Write( F, Format(" kW=%-g", kWeach * rand() * 2.0));
              System::Write( Format( " PF=%-.3g",  PF ));
            }
          }
          System::Write( " model=1" );
          System::WriteLn( F );
        }
      }
    }


    void WriteEveryOtherGenerators( Textfile& F, double kW, double PF, int Skip, bool DoGenerators )

    /*distribute generators on every other load, skipping the number specified*/

    /*Distribute the generator Proportional to load*/
    {
      double kWeach = 0.0, TotalkW = 0.0;
      TDSSClass LoadClass;
      TLoadObj* pLoad;
      int count = 0, i = 0, skipcount = 0;
      LoadClass = *( (TDSSClass*) GetDSSClassPtr( "load" ) );
      count = LoadClass.ElementList.get_myNumList();
       /*Add up the rated load in the enabled loads where gens will be placed*/
      TotalkW = 0.0;
      skipcount = Skip;
      for ( int stop = count, i = 1; i <= stop; i++)
      {
        pLoad = (TLoadObj*) LoadClass.ElementList.Get( i ) ;
        if (((TDSSCktElement*)pLoad)->Get_Enabled() )
        {
            /*Do not count skipped loads*/
          if ( skipcount == 0 )
          {
            TotalkW = TotalkW + pLoad->kWBase;  // will be right value if pos seq, too
            skipcount = Skip;  // start counter over again
          }
          else
            skipcount--;
        }
      }
      if ( ActiveCircuit[ActiveActor]->PositiveSequence )
        kWeach = kW / TotalkW / 3.0;
      else
        kWeach = kW / TotalkW;
      skipcount = Skip;
      for ( int stop = count, i = 1; i <= stop; i++)
      {
        pLoad = (TLoadObj*) LoadClass.ElementList.Get( i ) ;
        if (((TDSSCktElement*)pLoad)->Get_Enabled() )
        {
          if ( skipcount == 0 )
          {
            if ( DoGenerators )
            {
              System::Write( F, "new generator.DG_" + to_string(i) + "  bus1=" + (pLoad)->GetBus( 1 ));
            }
            else
            {
              System::Write( F, "new load.DL_" + to_string(i) + "  bus1=" + (pLoad)->GetBus( 1 ));
            }
            /*# with ActiveCircuit[ActiveActor] do */
            {
              Circuit::TDSSCircuit* with0 = ActiveCircuit[ActiveActor];
              {
                System::Write( F, Format( " phases=%d kV=%-g", (pLoad)->Get_NPhases(), pLoad->kVLoadBase ));
                System::Write( F, Format( " kW=%-g ",  kWeach * pLoad->kWBase ));
                System::Write( F, Format( " PF=%-.3g",  PF ));
              }
            }
            System::Write( F, " model=1" );
            WriteLn( F );
            skipcount = Skip;
          }
          else
            skipcount--;
        }
      }
    }


    void WriteProportionalGenerators( Textfile& F, double kW, double PF, bool DoGenerators )
    /*Distribute the generator Proportional to load*/
    {
      double kWeach = 0.0, TotalkW = 0.0;
      TDSSClass LoadClass;
      TLoadObj pLoad;
      int count = 0, i = 0;
      LoadClass = *( (TDSSClass*) GetDSSClassPtr( "load" ) );
      count = LoadClass.ElementList.get_myNumList();
       /*Add up the rated load in the enabled loads*/
      TotalkW = 0.0;
      for ( int stop = count, i = 1; i <= stop; i++)
      {
        pLoad =  *(TLoadObj*) LoadClass.ElementList.Get( i ); // showing other way to do it, just to make it fun :)
        if (((TDSSCktElement*) &pLoad)->Get_Enabled() )
        {
          TotalkW = TotalkW + pLoad.kWBase;  // will be right value if pos seq, too
        }
      }
      if ( ActiveCircuit[ActiveActor]->PositiveSequence )
        kWeach = kW / TotalkW / 3.0;
      else
        kWeach = kW / TotalkW;
      for ( int stop = count, i = 1; i <= stop; i++)
      {
        pLoad = *(TLoadObj*)LoadClass.ElementList.Get( i ) ;
        if (((TDSSCktElement*) &pLoad)->Get_Enabled() )
        {
          if ( DoGenerators )
          {
            System::Write( F, "new generator.DG_" + to_string(i) + "  bus1=" + (pLoad).GetBus( 1 ));
          }
          else
          {
            System::Write( F, "new load.DL_" + to_string(i) + " bus1=" + pLoad.GetBus( 1 ));
          }
          /*# with ActiveCircuit[ActiveActor] do */
          {
            Circuit::TDSSCircuit* with0 = ActiveCircuit[ActiveActor];
            {
              System::Write( F, Format( " phases=%d kV=%-g", pLoad.Get_NPhases(), pLoad.kVLoadBase ));
              System::Write( F, Format( " kW=%-g",  kWeach * pLoad.kWBase ));
              System::Write( F, Format( " PF=%-.3g",  PF ));
            }
          }
          System::Write( F, " model=1" );
          System::WriteLn( F );
        }
      }
    }


    void MakeDistributedGenerators( double kW, double PF, String How, int Skip, String Fname, bool DoGenerators )
    {
      TTextRec F;
      String WhatStr;
        /*Write outputfile and then redirect command parser to it.*/
      try
      {
        if ( FileExists( Fname ) )
          DoSimpleMsg( String( "File \"" ) + Fname + "\" is about to be overwritten. Rename it now before continuing if you wish to keep it.", 721 );
        AssignFile( F, Fname );
        Rewrite( F );
        IOResultToException();
      }
      catch( exception &E )
      {
      {
        DoSimpleMsg( String( "Error opening \"" ) + Fname + "\" for writing. Aborting.", 722 );
        return;
      }
      }
      try
      {
        if ( DoGenerators )
          WhatStr = "Generators";
        else
          WhatStr = "Loads";
        System::WriteLn( F, "! Created with Distribute Command:" );
        System::WriteLn( F, "! Distribute kW=" + to_string(kW) +
            " PF=" + to_string(PF) + 
            " How=" + How + 
            " Skip=" + to_string(Skip) + 
            "  file=" + Fname + "  what=" + WhatStr);
        System::WriteLn( F );
         // Writeln(F, 'Set allowduplicates=yes');
        if ( How.size() == 0 )
          How = "P";
        switch ( UpperCase( How )[0] )
        {
          case 'U':
            WriteUniformGenerators( F, kW, PF, DoGenerators );
          break;
          case 'R':
            WriteRandomGenerators( F, kW, PF, DoGenerators );
          break;
          case 'S':
            WriteEveryOtherGenerators( F, kW, PF, Skip, DoGenerators );
          break;
        default:
          WriteProportionalGenerators( F, kW, PF, DoGenerators );
        }
        GlobalResult = Fname;
    //  }
    //  __finally
    //  {
       // Writeln(F, 'Set allowduplicates=no');
        CloseFile( F );
        SetLastResultFile( Fname );
      }
      catch (exception &E)
      {
          //Inserted to facilitate adoption under native C
      }
    }

    /*Feeder Utilities*/


    void EnableFeeders( )
    {
      TEnergyMeterObj* pMeter;

        // Let EnergyMeter Objects control re-enabling of Feeders
        // Feeder could have been dumped in meantime by setting Feeder=False in EnergyMeter
      /*# with ActiveCircuit[ActiveActor] do */
      {
        Circuit::TDSSCircuit* with0 = ActiveCircuit[ActiveActor];
        {
          pMeter = (TEnergyMeterObj*) with0->EnergyMeters.Get_First();
          while ( pMeter != NULL )
          {
            pMeter->EnableFeeder(); // also sets CktElement feeder flags true   if a valid feeder
            pMeter = (TEnergyMeterObj * )with0->EnergyMeters.Get_Next();
          }
        }
      }
    }


    void DisableFeeders( )
    {
      TFeederObj* pFeeder;
      /*# with ActiveCircuit[ActiveActor] do */
      {
        Circuit::TDSSCircuit* with0 = ActiveCircuit[ActiveActor];
        {
          pFeeder = (TFeederObj*)with0->Feeders.Get_First();
          while ( pFeeder != NULL )
          {
            pFeeder->Set_Enabled(false);
            pFeeder->SetCktElementFeederFlags( false );
            pFeeder = (TFeederObj*)with0->Feeders.Get_Next();
          }
        }
      }
    }


    void InitializeFeeders( )
    // Var i:Integer;

    {
        /*    Do Nothing for now
        With ActiveCircuit[ActiveActor] Do
        For i := 1 to Feeders.get_myNumList() Do Begin
            If Not SolutionAbort Then TFeederObj(Feeders.Get(i)).InitForSweep;
        End;
        */
    }


    void ForwardSweepAllFeeders( )
    // Var i:Integer;

    {
        /*    Do Nothing for now
        With ActiveCircuit[ActiveActor] Do
        For i := 1 to Feeders.get_myNumList() Do Begin
            If Not SolutionAbort Then TFeederObj(Feeders.Get(i)).ForwardSweep;
        End;
    */
    }


    void BackwardSweepAllFeeders( )
    // Var i:Integer;

    {
        /*    Do Nothing for now
        With ActiveCircuit[ActiveActor] Do
        For i := 1 to Feeders.get_myNumList() Do Begin
            If Not SolutionAbort Then TFeederObj(Feeders.Get(i)).BackwardSweep;
        End;
        */
    }


    String GetDSSArray_Real( int n, pDoubleArray dbls )
    {
      String result;
      int i = 0;
      result = "[";
      for ( int stop = n, i = 1; i <= stop; i++)
        result = result + Format(" %-.6g", dbls[i - 1] );
      result = result + "]";
      return result;
    }


    String GetDSSArray_Integer( int n, pIntegerArray ints )
    {
      String result;
      int i = 0;
      result = "[";
      for ( int stop = n, i = 1; i <= stop; i++)
        result = result + Format( " %-.d",  ints[i - 1] );
      result = result + "]";
      return result;
    }


    complex CmulReal_im( const complex A, const double Mult )  // Multiply only imaginary part by a real

    {
      complex result;
      result = cmplx( A.re, A.im * Mult );
      return result;
    }


    void CmulArray( pComplexArray pc, double Multiplier, int Size )  // Multiply a complex array times a double

    {
      int i = 0;
      for ( int stop = Size, i = 1; i <= stop; i++)
        pc[i] = cmulreal( pc[i], Multiplier );
    }


    longInt GetMaxCktElementSize( )
    {
      longInt result = 0;
      int i = 0;
      result = 0;
      /*# with ActiveCircuit[ActiveActor] do */
      {
        auto with0 = ActiveCircuit[ActiveActor];
        for ( int stop = with0->NumDevices, i = 1; i <= stop; i++)
          result = max( result, ((TDSSCktElement*)with0->CktElements.Get( i ))->Yorder );
      }
      return result;
    }

    /*
    FUNCTION IsValidNumericField(const NumberField:TEdit):Boolean;

    Var
       Code  :Integer;
       Value :Double;

    Begin
         Result := TRUE;
         Val(NumberField.Text, Value, Code);
         If Code>0 Then Begin
             Beep;
             NumberField.SetFocus;
             Result := FALSE;
         End;
    End;
    */


    int GetUniqueNodeNumber( const String sBusName, int StartNode )
    /*To help avoid collisions of neutral numbers, this function returns a node number that is not being used,
     Starting at the StartNode value*/
    {
      int result = 0;
      int iBusidx = 0;
      result = StartNode;
      iBusidx = ActiveCircuit[ActiveActor]->BusList.Find( sBusName );
      if ( iBusidx > 0 )
        while ( ActiveCircuit[ActiveActor]->Buses[(iBusidx) - 1]->FindIdx( result ) != 0 )
          result++;
      ActiveCircuit[ActiveActor]->Buses[(iBusidx) - 1]->Add( result, ActiveActor );  // add it to the list so next call will be unique
      return result;
    }

    void ShowMessageBeep( const String S )
    {
      cout << '\a'; // equivalent to beep in C++
//      DSSInfoMessageDlg( S );
    }


    bool IsPathBetween( TPDElement* FromLine, TPDElement* ToLine )
    {
      bool result = false;
      TPDElement* PDElem;
      PDElem = FromLine;
      result = false;
      while ( PDElem != NULL )
      {
        if ( PDElem == ToLine )
        {
          result = true;
          return result;
        }
        PDElem = PDElem->ParentPDElement;
      }
      return result;
    }


    void TraceAndEdit( TPDElement* FromLine, TPDElement* ToLine, int NPhases, String EditStr )
    /*Trace back up a tree and execute an edit command string*/
    {
      TPDElement* pLine;
      pLine = FromLine;
      while ( pLine != NULL )
      {
        if ( ( pLine->Get_NPhases() == NPhases ) || ( NPhases == 0 ) )
        {
          Parser[ActiveActor]->SetCmdString(EditStr);
          pLine->Edit( ActiveActor );   // Uses Parser
        }
        if ( pLine == ToLine )
          break;
        pLine = pLine->ParentPDElement;
      }
    }


    void GoForwardAndRephase( TPDElement* FromLine, const String PhaseString, const String EditStr, const String ScriptFileName, bool TransStop )
    /*Trace forward down a tree and Generate a script file to change the phase*/
    {
      TPDElement* pPDelem;
      TDSSCktElement* pShuntObject;
      TEnergyMeterObj* pMeter;
      int i = 0;
      String S;
      TTextRec Fout;
      String Filename;
      int XfmrLevel = 0;
      pMeter = (TEnergyMeterObj*)FromLine->MeterObj;

       /*Search for starting line in branchlist*/
      pPDelem = (TPDElement*) pMeter->BranchList->Get_First();
      while ( pPDelem != NULL )
      {
        if ( FromLine == pPDelem )
        {
          break;
        }
        pPDelem = (TPDElement*) pMeter->BranchList->Get_Forward();
      }

       /*Error check*/
      if ( pPDelem == NULL )
      {
        DoSimpleMsg( FromLine->ParentClass->get_myClass_name() + "." + FromLine->get_Name() + " Not found in Meter Zone.", 723 );
        return;
      }
      try
      {
        Filename = GetOutputDirectory() + CircuitName_[ActiveActor] + ScriptFileName;
        GlobalResult = Filename;
        System::AssignFile( Fout, Filename );
        System::Rewrite( Fout );
        IOResultToException();
        pMeter->BranchList->StartHere();
        pPDelem = (TPDElement *) pMeter->BranchList->Get_Forward();
        while ( pPDelem != NULL )
        {
          S = "edit " + pPDelem->ParentClass->get_myClass_name() + "." + pPDelem->get_Name();

    /*----------------LINES---------------------------------------------------*/
          if ( IslineElement( pPDelem ) )
          {
            for ( int stop = pPDelem->Get_NTerms(), i = 1; i <= stop; i++)
            {
              S = S + " Bus" + to_string(i) + "=" + StripExtension(pPDelem->GetBus(i)) + PhaseString;
               //  Parser.CmdString := Format('Bus$d=%s%s',[i, StripExtension(pPDelem.GetBus(i)), PhaseString]);
               //  pPDelem.Edit;
            }

             /*When we're done with that, we'll send the Edit string*/
            if ( EditStr.size() > 0 )
            {
              S = S + "  " + EditStr;
               //  Parser.CmdString := EditStr;
               //  pPDelem.Edit;   // Uses Parser
            }
            WriteLn( Fout, S );

             /*Now get all shunt objects connected to this branch*/
            pShuntObject = (TDSSCktElement *) pMeter->BranchList->Get_FirstObject();
            while ( pShuntObject != NULL )
            {
                   /*1st Terminal Only*/
              i = 1;
              S = "edit " + pShuntObject->ParentClass->get_myClass_name() + "." + pShuntObject->get_Name();
              S = S + " Bus" + to_string(i) + "=" + StripExtension(pShuntObject->GetBus(i)) + PhaseString;
              if ( EditStr.size() > 0 )
                S = S + "  " + EditStr;
              WriteLn( Fout, S );
                 //  Parser.CmdString := Format('Bus$d=%s%s',[i, StripExtension(pShuntObject.GetBus(1)), PhaseString]);
                 //  pShuntObject.Edit;
              pShuntObject = (TPDElement*) pMeter->BranchList->Get_NextObject();
            }
            pPDelem = (TPDElement*) pMeter->BranchList->Get_Forward();
          }    /*IsLine*/

    /*----------------TRANSFORMERS---------------------------------------------------*/
          else
            if ( IsTransformerElement( pPDelem ) )
            {

         /*
           We'll stop at transformers and change only the primary winding.
           Then we'll cycle forward until the lexical level is less or we're done
         */
              XfmrLevel = pMeter->BranchList->Get_Level();
              S = S + " wdg=1 Bus=" + StripExtension(pPDelem->GetBus(1)) + PhaseString + EditStr;
              if ( ! TransStop )
                S = S + " wdg=2 Bus=" + StripExtension(pPDelem->GetBus(2)) + PhaseString + EditStr;
              WriteLn( Fout, S );

         /*Be default Go forward in the tree until we bounce back up to a line section above the transformer*/
              if ( TransStop )
              {
                do
                {
                  pPDelem = (TPDElement *) pMeter->BranchList->Get_Forward();
                }
                while ( ! ( ( pPDelem == NULL ) || ( pMeter->BranchList->Get_Level() <= XfmrLevel ) ) );
              }
              else
                pPDelem = (TPDElement*)pMeter->BranchList->Get_Forward();  /*Then we get lines and loads beyond transformer*/
            }
        }
    //  }
    //  __finally
    //  {
        CloseFile( Fout );
        FireOffEditor( Filename );
      }
      catch (std::exception &E)
      {
          // added for compatibility with native C
      }
    }

    double MaxdblArrayValue( int npts, pDoubleArray dbls )
    // returns max value of an array of doubles

    {
      double result = 0.0;
      int i = 0;
      result = 0.0;
      if ( npts == 0 )
        return result;
      result = dbls[1];
      for ( int stop = npts, i = 2; i <= stop; i++)
        result = max( result, dbls[i] );
      return result;
    }


    int iMaxAbsdblArrayValue( int npts, pDoubleArray dbls )
    // Returns index of max array value  in abs value

    {
      int result = 0;
      int i = 0;
      double MaxValue = 0.0;
      result = 0;
      if ( npts == 0 )
        return result;
      result = 1;
      MaxValue = Abs( (dbls)[0] );
      for ( int stop = npts, i = 2; i <= stop; i++)
        if ( Abs( (dbls)[i - 1] ) > MaxValue )
        {
          MaxValue = Abs( (dbls)[i - 1] );
          result = i;   // save index
        }
      return result;
    }


    int InterpretLoadShapeClass( const String S )
    {
      int result = 0;
      String ss;
      ss = ToLowerCaseStr( S );
      result = USENONE;
      switch ( ss[0] )
      {
        case 'd':
          switch ( ss[1] )
          {
            case 'a':
              result = USEDAILY;
            break;
            case 'u':
              result = USEDUTY;
            break;
          }
        break;
        case 'y':
          result = USEYEARLY;
        break;
        case 'n':
          result = USENONE;
        break;
      }
      return result;
    }


    int InterpretEarthModel( const String S )
    {
      int result = 0;
      String ss;
      ss = ToLowerCaseStr( S );
      result = SIMPLECARSON;
      switch ( ss[0] )
      {
        case 'c':
          result = SIMPLECARSON;
        break;
        case 'f':
          result = FULLCARSON;
        break;
        case 'd':
          result = DERI;
        break;
      }
      return result;
    }


    String GetActiveLoadShapeClass( )
    {
      String result;
      switch ( ActiveCircuit[ActiveActor]->ActiveLoadShapeClass )
      {
        case USEDAILY:
          result = "Daily";
        break;
        case USEYEARLY:
          result = "Yearly";
        break;
        case USEDUTY:
          result = "Duty";
        break;
        case USENONE:
          result = "None";
        break;
      }
      return result;
    }


    String GetEarthModel( int n )
    {
      String result;
      switch ( n )
      {
        case SIMPLECARSON:
          result = "Carson";
        break;
        case FULLCARSON:
          result = "FullCarson";
        break;
        case DERI:
          result = "Deri";
        break;
      }
      return result;
    }


    int InterpretColorName( const String S )
    {
      if (CompareTextShortest(S, "black") == 0)
        return clBlack;

      if (CompareTextShortest(S, "Maroon") == 0)
        return clMaroon;

      if (CompareTextShortest(S, "Green") == 0)
        return clGreen;

      if (CompareTextShortest(S, "Olive") == 0)
        return clOlive;

      if (CompareTextShortest(S, "Navy") == 0)
        return clNavy;

      if (CompareTextShortest(S, "Purple") == 0)
        return clPurple;

      if (CompareTextShortest(S, "Teal") == 0)
        return clTeal;

      if (CompareTextShortest(S, "Gray") == 0)
        return clGray;

      if (CompareTextShortest(S, "Silver") == 0)
        return clSilver;

      if (CompareTextShortest(S, "Red") == 0)
        return clRed;

      if (CompareTextShortest(S, "Lime") == 0)
        return clLime;

      if (CompareTextShortest(S, "Yellow") == 0)
        return clYellow;

      if (CompareTextShortest(S, "Blue") == 0)
        return clBlue;

      if (CompareTextShortest(S, "Fuchsia") == 0)
        return clFuchsia;

      if (CompareTextShortest(S, "Aqua") == 0)
        return clAqua;

      if (CompareTextShortest(S, "LtGray") == 0)
        return clLtGray;

      if (CompareTextShortest(S, "DkGray") == 0)
        return clDkGray;

      if (CompareTextShortest(S, "White") == 0)
        return clWhite;

      if (S.size() && S[0] == '$')
      {
        return stoi(S.substr(1), nullptr, 16);
      }
      return StrToInt(S);
    }

    String MakeNewCktElemName( const String oldname )
    {
      String result;
      SetObject( oldname );  // set opject active
      /*# with ActiveDSSObject[ActiveActor] do */
      {
        auto with0 = (TDSSObject*) ActiveDSSObject[ActiveActor];/*
        result = Format( "%s.%s%d",  with0->ParentClass->get_Name(), copy( 1, 4 , with0->ParentClass->get_Name()), with0->ClassIndex )) );*/
      }
      return result;
    }


    String ConstructElemName( const String Param )
    /*Construct an element name, sustituting @var values if any*/
    {
      String result;
      String FClassName, FObjName;
      ParseObjectClassandName( ToLowerCaseStr( Param ), FClassName, FObjName );  // insert @var test
      result = FClassName + "." + FObjName;
      return result;
    }


    void RenameCktElem( TDSSCktElement pElem ) // local proc

    {
      /*# with pElem do */
      {
        pElem.Set_Name(pElem.ParentClass->get_myClass_name().substr(0,4) + Format("%d", pElem.ClassIndex));
        ActiveCircuit[ActiveActor]->DeviceList.Add(pElem.get_Name() ); // Make a new device list corresponding to the CktElements List
        

       /*-------------------------------------------------------------*/

    /*Make sure buslist exists*/
        pElem.Checked = true;
      }
    }


    void Obfuscate( )
    /*Rename Buses and element names to generic names to remove identifiable names*/
    {
      int i = 0, bref = 0;
      size_t dotpos = 0;
      int DevListSize = 0;
      THashList TempBusList;
      TDSSCktElement* pCktElem;
      TDSSCktElement* pctrlelem;
      String S, Nodes;
      String OldBusName;
      String NewBusName;
      int Baseclass = 0;
      int ElemClass = 0;
      TStringList ControlUpDateStrings;
      list<TDSSCktElement*> ControlUpDatePtrs;

       /*-------------------------------------------------------------*/
      if ( ActiveCircuit[ActiveActor] == NULL )
        return;
      if ( ActiveCircuit[ActiveActor]->BusList.Get_NumElements() <= 0 )
        return;
      /*# with ActiveCircuit[ActiveActor] do */
      {
        Circuit::TDSSCircuit* with0 = ActiveCircuit[ActiveActor];
        {
          TempBusList = THashList(with0->BusList.Get_NumElements() );

        /*Rename Buses*/
          for ( int stop = with0->BusList.Get_NumElements(), i = 1; i <= stop; i++)
            TempBusList.Add( Format( "B_%d",  i ));
          with0->BusList.Clear();
          with0->BusList = TempBusList; // Reassign

        /*Rename the bus names in each circuit element before renaming the elements*/
          pCktElem = (TDSSCktElement*)with0->CktElements.Get_First();
          while ( pCktElem != NULL )
          {
              Baseclass = (pCktElem->DSSObjType && BaseClassMask);
            if ( ( Baseclass == PC_ELEMENT ) || ( Baseclass == PD_ELEMENT ) )
            {
              S = "";
              for ( int stop = pCktElem->Get_NTerms(), i = 1; i <= stop; i++)
              {
                OldBusName = pCktElem->GetBus( i );
                dotpos = OldBusName.find( "." );
                if ( dotpos == String::npos )
                  Nodes = "";
                else
                  Nodes = OldBusName.substr( dotpos, OldBusName.size() );    // preserve node designations if any
                bref = pCktElem->Terminals[i - 1].BusRef;
                NewBusName = "B_" + to_string(bref) + Nodes;
                        //Check for Transformer because that will be an exception
                switch ( pCktElem->DSSObjType & CLASSMASK )
                {
                  case XFMR_ELEMENT:
                    S = S + "Wdg=" + to_string(i) + " Bus=" + NewBusName;
                  break;
                default:
                  S = S + "Bus" + to_string(i) + "=" +NewBusName;
                }
              }
              Parser[ActiveActor]->SetCmdString(S);
              pCktElem->Edit( ActiveActor );
            }
            pCktElem = (TDSSCktElement*)with0->CktElements.Get_Next();
          }

        /*Rename the circuit elements to generic values*/
        /*Have to catch the control elements and edit some of their parameters*/

        /*first, make scripts to change the monitored element names in the controls to what they will be*/
          ControlUpDateStrings = TStringList();
    //      ControlUpDatePtrs = list();
          pCktElem = (TDSSCktElement*)with0->CktElements.Get_First();
          while ( pCktElem != NULL )
          {
            switch ( pCktElem->DSSObjType & CLASSMASK )
            {
              case CAP_CONTROL:
              {
                S = "Element=" + MakeNewCktElemName( pCktElem->GetPropertyValue( 1 ));
                ControlUpDateStrings.push_back( S + "Capacitor=" + MakeNewCktElemName( "capacitor." + pCktElem->GetPropertyValue( 3 ) ).substr( 10, 100 ) );
                ControlUpDatePtrs.push_back(pCktElem);
              }
              break;
              case REG_CONTROL:
              break;   // handled below

              case RELAY_CONTROL:
              {
                S = "MonitoredObj=" + MakeNewCktElemName( pCktElem->GetPropertyValue( 1 ));
                ControlUpDateStrings.push_back( S + "SwitchedObj=" + MakeNewCktElemName( pCktElem->GetPropertyValue( 3 )));
                ControlUpDatePtrs.push_back( pCktElem );
              }
              break;
              case RECLOSER_CONTROL:
              {
                S = "MonitoredObj=" + MakeNewCktElemName( pCktElem->GetPropertyValue( 1 ) );
                ControlUpDateStrings.push_back( S + "SwitchedObj=" + MakeNewCktElemName( pCktElem->GetPropertyValue( 3 ) ));
                ControlUpDatePtrs.push_back( pCktElem );
              }
              break;
              case FUSE_CONTROL:
              {
                S = "MonitoredObj=" + MakeNewCktElemName( pCktElem->GetPropertyValue( 1 ) );
                ControlUpDateStrings.push_back( S + "SwitchedObj=" + MakeNewCktElemName( pCktElem->GetPropertyValue( 3 ) ) );
                ControlUpDatePtrs.push_back( pCktElem );
              }
              break;
              case GEN_CONTROL:
              {
                ControlUpDateStrings.push_back( "Element=" + MakeNewCktElemName( pCktElem->GetPropertyValue( 1 ) ) );
                ControlUpDatePtrs.push_back( pCktElem );
              }
              break;
              case STORAGE_CONTROL:
              {
                ControlUpDateStrings.push_back( "Element=" + MakeNewCktElemName( pCktElem->GetPropertyValue( 1 ) ) );
                ControlUpDatePtrs.push_back( pCktElem );
              }
              break;
              case SWT_CONTROL:
              {
                ControlUpDateStrings.push_back( "SwitchedObj=" + MakeNewCktElemName( pCktElem->GetPropertyValue( 1 ) ) );
                ControlUpDatePtrs.push_back( pCktElem );
              }
              break;
            }
            pCktElem = (TDSSCktElement*) with0->CktElements.Get_Next();
          }
          pCktElem = (TDSSCktElement*)with0->CktElements.Get_First();
          while ( pCktElem != NULL )
          {
            pCktElem -> Checked = false;     // Initialize to not checked
            pCktElem = (TDSSCktElement*)with0->CktElements.Get_Next();
          }
          DevListSize = with0 -> DeviceList.Get_NumElements();
          with0 -> DeviceList.Clear();
          with0 -> DeviceList = THashList( DevListSize );
          pCktElem = (TDSSCktElement*)with0 -> CktElements.Get_First();
          while ( pCktElem != NULL )
          {
            if ( ! pCktElem->Checked )
            {
              ElemClass = ( pCktElem->DSSObjType & CLASSMASK );
              RenameCktElem( *pCktElem );
              switch ( ElemClass )
              {
                case XFMR_ELEMENT:
                  if ( pCktElem->HasControl )
                  {
                    pctrlelem = (TDSSCktElement*)pCktElem->ControlElementList.Get_First();
                    while (( pctrlelem != NULL ) )
                    {
                      if ( ( ( pctrlelem->DSSObjType & CLASSMASK ) ) == REG_CONTROL )
                      {
                        Parser[ActiveActor]->SetCmdString("Transformer=" + pCktElem->get_Name());
                        pctrlelem->Edit( ActiveActor );
                      }
                      pctrlelem = (TDSSCktElement*) pCktElem->ControlElementList.Get_Next();
                    }
                  }
                break;
                default: {}
                    /*nada*/
              }
            }
            pCktElem = (TDSSCktElement*) with0 -> CktElements.Get_Next();
          }


        /*Run the control update scripts now that everything is renamed*/
          auto myPtr = ControlUpDatePtrs.begin();
          auto myPtr2 = ControlUpDateStrings.begin();
          for ( int stop = ControlUpDatePtrs.size() - 1, i = 0; i <= stop; i++)
          {
            myPtr = std::next(ControlUpDatePtrs.begin(), i);
            myPtr2 = std::next(ControlUpDateStrings.begin(), i);
            pCktElem = (TDSSCktElement*) (*myPtr);
            Parser[ActiveActor]->SetCmdString(*myPtr2);
            pCktElem->Edit( ActiveActor );
          }
          ControlUpDateStrings.clear();
          ControlUpDatePtrs.clear();
        }
      }  /*With*/
    }
    /*Rename Buses and element names to generic names to remove identifiable names*/
   

    double QuadSolver( const double A, const double b, const double c ) // returns largest of two answers

    {
      double result = 0.0;
      double Ans1 = 0.0, Ans2 = 0.0, MidTerm = 0.0, a2 = 0.0;
      result = 0.0;   // default return
      if ( A == 0.0 )
      {
        if ( b != 0.0 )
          result = - c / b;
      }
      else
      {
        MidTerm = sqrt( b * b - 4.0 * A * c );
        a2 = 2.0 * A;
        Ans1 = ( - b + MidTerm ) / a2;
        Ans2 = ( - b - MidTerm ) / a2;
            // return most positive number
        if ( Ans1 > Ans2 )
          result = Ans1;
        else
          result = Ans2;
      }
      return result;
    }

    /*-------------------------------------------------------------------------------*/


    int GetOCPDeviceType( TDSSCktElement* pElem )
    {
      int result = 0;
      int i = 0;
      TDSSCktElement* pCktElement;
      result = 0;
      i = 1;
      do
      {
        pCktElement = (TDSSCktElement*) pElem->ControlElementList.Get( i );
        if ( pCktElement != NULL )
          switch ( pCktElement->DSSObjType & CLASSMASK )
          {
            case FUSE_CONTROL:
              result = 1;
            break;
            case RECLOSER_CONTROL:
              result = 2;
            break;
            case RELAY_CONTROL:
              result = 3;
            break;
          }
        i++;
      }
      while ( ! ( ( i > pElem->ControlElementList.get_myNumList() ) || ( result > 0 ) ) );
      return result;
    }


    String GetOCPDeviceTypeString( int icode )
    {
      String result;
      switch ( icode )
      {
        case 1:
          result = "FUSE";
        break;
        case 2:
          result = "RECLOSER";
        break;
        case 3:
          result = "RELAY";
        break;
      default:
        result = "Unknown";
      }
      return result;
    }


    String GetNodeString( const String BusName )
    {
      String result;
      size_t dotpos = 0;
      dotpos = BusName.find( "." );
      if ( dotpos == String::npos )
        result = "";
      else
        result = BusName.substr( dotpos, BusName.size() );    // preserve node designations if any
      return result;
    }

} // namesspace Utilities









