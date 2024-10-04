
#pragma hdrstop

#include "MeTIS_Exec.h"

namespace METIS_Exec
{

/*    int max(const int A, const int b)
    {
        int result = 0;
        if (A > b)
            result = A;
        else
            result = b;
        return result;
    }*/

    /* TFileSearchReplace */


    TFileSearchReplace::TFileSearchReplace(const String AFileName)
    {
        // inherited::Create();
    }


    TFileSearchReplace::~TFileSearchReplace()
    {
/*        String tmpFileName;
        if ((FtmpFile != NULL))
            tmpFileName = FtmpFile.Filename;
        delete FtmpFile;
        FtmpFile = NULL;
        delete FSourceFile;
        FSourceFile = NULL;
        TFile.Delete(tmpFileName);*/
        // todo check:  inherited::Destroy();
    }

/*
    void TFileSearchReplace::CopyPreamble()
    {
        int PreambleSize = 0;
        TBytes PreambleBuf;
        // Copy Encoding preamble
        SetLength(PreambleBuf, 100);
        FSourceFile.Read(PreambleBuf, Length(PreambleBuf));
        FSourceFile.Seek(0, soBeginning);
        PreambleSize = TEncoding.GetBufferEncoding(PreambleBuf, FEncoding);
        if (PreambleSize != 0)
            FtmpFile.CopyFrom(FSourceFile, PreambleSize);
    }

    
    int TFileSearchReplace::GetLastIndex(const String str, const String SubStr, TReplaceFlags& ReplaceFlags)
    {
        int result = 0;
        int i = 0;
        String tmpSubStr, tmpStr;
        if (!(ReplaceFlags.Contains(rfIgnoreCase)))
        {
            i = str.Pos(SubStr);
            result = i;
            while (i > 0)
            {
                i = PosEx(SubStr, str, i + 1);
                if (i > 0)
                    result = i;
            }
            if (result > 0)
                result += SubStr.Length() - 1;
        }
        else
        {
            tmpStr = UpperCase(str);
            tmpSubStr = UpperCase(SubStr);
            i = tmpStr.Pos(tmpSubStr);
            result = i;
            while (i > 0)
            {
                i = PosEx(tmpSubStr, tmpStr, i + 1);
                if (i > 0)
                    result = i;
            }
            if (result > 0)
                result += tmpSubStr.Length() - 1;
        }
        return result;
    }


    void TFileSearchReplace::ParseBuffer(TBytes Buf, bool& IsReplaced, const String& AFrom, const String& ATo, TReplaceFlags& ReplaceFlags)
    {
        int i = 0;
        int ReadedBufLen = 0;
        String BufStr;
        TBytes DestBytes;
        int LastIndex = 0;
        if (IsReplaced && (!(ReplaceFlags.Contains(rfReplaceAll))))
        {
            FtmpFile.Write(Buf, Length(Buf));
            return;
        }

        // 1. Get chars from buffer
        ReadedBufLen = 0;
        for (int stop = 0, i = Length(Buf); i >= stop; i--)
            if (FEncoding.GetCharCount(Buf, 0, i) != 0)
            {
                ReadedBufLen = i;
                Break;
            }
        if (ReadedBufLen == 0)
            throw EEncodingError.Create("Cant convert bytes to str");
        FSourceFile.Seek(ReadedBufLen - Length(Buf), soCurrent);
        BufStr = FEncoding.GetString(Buf, 0, ReadedBufLen);
        if (ReplaceFlags.Contains(rfIgnoreCase))
            IsReplaced = ContainsText(BufStr, AFrom);
        else
            IsReplaced = ContainsStr(BufStr, AFrom);
        if (IsReplaced)
        {
            LastIndex = GetLastIndex(BufStr, AFrom, ReplaceFlags);
            LastIndex = max(LastIndex, BufStr.Length() - AFrom.Length() + 1);
        }
        else
            LastIndex = BufStr.Length();
        BufStr.SetLength(LastIndex);
        FSourceFile.Seek(FEncoding.GetByteCount(BufStr) - ReadedBufLen, soCurrent);
        BufStr = stringreplace(BufStr, AFrom, ATo, ReplaceFlags);
        DestBytes = FEncoding.GetBytes(BufStr);
        FtmpFile.Write(DestBytes, Length(DestBytes));
    }
    */

    void TFileSearchReplace::Replace(const String AFrom, const String ATo, TReplaceFlags ReplaceFlags)
    {
/*        __int64 SourceSize = 0;
        TBytes Buf;
        int BufLen = 0;
        bool bReplaced = false;
        FSourceFile.Seek(0, soBeginning);
        FtmpFile.Size = 0;
        CopyPreamble();
        SourceSize = FSourceFile.Size;
        BufLen = max(FEncoding.GetByteCount(AFrom) * 5, 2048);
        BufLen = max(FEncoding.GetByteCount(ATo) * 5, BufLen);
        SetLength(Buf, BufLen);
        bReplaced = false;
        while (FSourceFile.Position < SourceSize)
        {
            BufLen = FSourceFile.Read(Buf, Length(Buf));
            SetLength(Buf, BufLen);
            ParseBuffer(Buf, bReplaced, AFrom, ATo, ReplaceFlags);
        }
        FSourceFile.Size = 0;
        FSourceFile.CopyFrom(FtmpFile, 0);*/
    }

    String GetNumEdges(String MeTISSrc)
    {
/*
        String result;
        int i = 0, j = 0;
        const int SOffset = 13;
        i = MeTISSrc.Pos("I only found ");
        j = MeTISSrc.Pos(" edges in the file.");
        result = MeTISSrc.SubString((i + SOffset), (j - (i + SOffset)));// Gets the # of edges proposed by MeTIS
        return result;
*/
        return "";
    }


    String RunMeTIS(String DosApp)
    {
/*
        String result;
        const int READ_BUFFER_SIZE = 2400;
        TSecurityAttributes Security;
        THandle readableEndOfPipe, writeableEndOfPipe;
        TStartUpInfo start;
        TProcessInformation ProcessInfo;
        char* Buffer = NULL;
        DWORD BytesRead = 0;
        DWORD AppRunning = 0;
        String AppReturn;
        Security.nLength = sizeof(TSecurityAttributes);
        Security.bInheritHandle = true;
        Security.lpSecurityDescriptor = NULL;
        if (CreatePipe(readableEndOfPipe, writeableEndOfPipe, &Security, 0))
        {
            Buffer = AllocMem(READ_BUFFER_SIZE + 1);
            FillChar(start, sizeof(start), '\x00');
            start.cb = sizeof(start);
            // Set up members of the STARTUPINFO structure.
            // This structure specifies the STDIN and STDOUT handles for redirection.
            // - Redirect the output and error to the writeable end of our pipe.
            // - We must still supply a valid StdInput handle (because we used STARTF_USESTDHANDLES to swear that all three handles will be valid)
            start.dwFlags = start.dwFlags | STARTF_USESTDHANDLES;
            start.hStdInput = GetStdHandle(STD_INPUT_HANDLE); //we're not redirecting stdInput; but we still have to give it a valid handle
            start.hStdOutput = writeableEndOfPipe; //we give the writeable end of the pipe to the child process; we read from the readable end
            start.hStdError = writeableEndOfPipe;

            //We can also choose to say that the wShowWindow member contains a value.
            //In our case we want to force the console window to be hidden.
            start.dwFlags = start.dwFlags + STARTF_USESHOWWINDOW;
            start.wShowWindow = SW_HIDE;

            // Don't forget to set up members of the PROCESS_INFORMATION structure.
            ProcessInfo = Default(TProcessInformation);

            //WARNING: The unicode version of CreateProcess (CreateProcessW) can modify the command-line "DosApp" string.
            //Therefore "DosApp" cannot be a pointer to read-only memory, or an ACCESS_VIOLATION will occur.
            //We can ensure it's not read-only with the RTL function: UniqueString
            UniqueString(DosApp);
            if (CreateProcess(NULL, DosApp.c_str(), NULL, NULL, true, NORMAL_PRIORITY_CLASS, NULL, NULL, start, ProcessInfo))
            {
                //Wait for the application to terminate, as it writes it's output to the pipe.
                //WARNING: If the console app outputs more than 2400 bytes (ReadBuffer),
                //it will block on writing to the pipe and *never* close.
                do
                {
                    AppRunning = WaitForSingleObject(ProcessInfo.hProcess, 100);
                } while (!(AppRunning != WAIT_TIMEOUT));
                //Read the contents of the pipe out of the readable end
                //WARNING: if the console app never writes anything to the StdOutput, then ReadFile will block and never return
                do
                {
                    BytesRead = 0;
                    ReadFile(readableEndOfPipe, Buffer[0], READ_BUFFER_SIZE, BytesRead, NULL); 
                    Buffer[BytesRead] = '\x00';
                    OemToAnsi(Buffer, Buffer);
                    result = result + ((String)Buffer);
                } while (!(BytesRead < READ_BUFFER_SIZE));
            }
            else
                result = "**Error**";
            free(Buffer);
            CloseHandle(ProcessInfo.hProcess);
            CloseHandle(ProcessInfo.hThread);
            CloseHandle(readableEndOfPipe);
            CloseHandle(writeableEndOfPipe);
        }
        return result;
*/
        return "";
    }
}

