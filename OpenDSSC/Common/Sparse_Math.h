
/********************************************************************************
  Electric Power Resarch Insitute EPRI 2022

 Library created to handle sparse matrix linear algebra ops, the arguments need
 to be sparse and provided in compressed coordiante format
 Created by Davis Montenegro for EPRI 08/01/2018
 based on the code provided by Sudarshan Khasnis in Java at
 https://www.geeksforgeeks.org/operations-sparse-matrices/
********************************************************************************/
#ifndef Sparse_MathH
#define Sparse_MathH


#include "System.h"

#include "Ucomplex.h"
#include "Ucmatrix.h"
#include <vector>



namespace Sparse_Math
{

struct TCmplx_Data;
class TSparse_Complex;
class Tsparse_matrix;




struct TCmplx_Data {
  int Row, col;
  complex Value;
};



typedef std::vector < int > TData;
typedef TData* PData;
typedef TCmplx_Data TComplex;
typedef TComplex* PComplex;
typedef std::vector < complex > TComplexArr;
typedef TComplexArr* PComplexArr;


class Tsparse_matrix{
  friend class TSparse_Complex;
public:
  //  private:
  int Row, col, len;
  int checkifexists( int r, int c );
  void getrow( int Index, PData cols, PData vals );
  bool R_equal( PData acols, PData avals, PData bcols, PData bvals );
  public:
  typedef int* Sparse_Math__0;
  std::vector < TData > data;
  void sparse_matrix( int r, int c );
  int Insert( int r, int c, int val );
  Tsparse_matrix Add( Tsparse_matrix* b );
  Tsparse_matrix Transpose( );
  Tsparse_matrix multiply( Tsparse_matrix* b );
  void Reset( );
  int NZero( );
  int NCols( );
  int NRows( );
  int Rank( );
  void Sort();
  public:
  Tsparse_matrix();
};


class TSparse_Complex: public TObject {
  typedef TObject inherited;
  friend class Tsparse_matrix;
public:
  //private:
  int Row, col, len;
  int checkifexists( int r, int c );
  void getrow( int Index, PData cols, PComplexArr vals );
  complex getvalue( int Row, int col );
  bool R_equal( PData acols, PData bcols, PComplexArr avals, PComplexArr bvals );
  public:
  std::vector < TCmplx_Data > CData;
  void sparse_matrix_Cmplx( int r, int c );
  int Insert( int r, int c, complex val );
  TSparse_Complex Add( TSparse_Complex* b );
  TSparse_Complex Transpose( );
  TSparse_Complex TransposeConj( );
  TSparse_Complex multiply( TSparse_Complex* b );
  void Reset( );
  int NZero( );
  int NCols( );
  int NRows( );
  int Rank( );
  void Sort();
  public:
  TSparse_Complex();
};

} // namespace Sparse_Math

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Sparse_Math;
#endif

#endif //  Sparse_MathH








