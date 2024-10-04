#ifndef MeTIS_ExecH
#define MeTIS_ExecH


#include "System.h"
#include "Sysutils.h"

namespace METIS_Exec
{



	class TFileSearchReplace : public TObject {
		typedef TObject inherited;
	public:
		//  private:
		System::file FSourceFile;
		System::file FtmpFile;
		//TEncoding FEncoding;
	public:
		TFileSearchReplace(const String AFileName);
		virtual ~TFileSearchReplace();
		void Replace(const String AFrom, const String ATo, TReplaceFlags ReplaceFlags);

	};


	String RunMeTIS(String DosApp);
	String GetNumEdges(String MeTISSrc);

}

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace METIS_Exec;
#endif

#endif //  MeTIS_ExecH








