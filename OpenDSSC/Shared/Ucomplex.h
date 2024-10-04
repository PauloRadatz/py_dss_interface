#ifndef UcomplexH
#define UcomplexH

#include "System.h"
#include "Sysutils.h"


namespace Ucomplex
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

struct complex
{
	double re;
	double im;
};

typedef complex* pcomplex;
typedef complex* ComplexArray;
typedef ComplexArray pComplexArray;

struct polar
{
	double mag;
	double ang;
};

  // 4-8-2010  added inlining selected often-used functions
complex cmplx(double A, double B);//#inline
complex cinv(const complex& A);//#inline
double cabs(const complex& A);//#inline
double cabs2(const complex& A);//#inline // best when you don't need sqrt
double cang(const complex& A);
double cdang(const complex& A); // angle of complex number, degrees
polar ctopolar(const complex& A);
polar ctopolardeg(const complex& A);  // complex to polar, degrees
complex cadd(const complex& A, const complex& B);//#inline
void caccum(complex& A, const complex& B);//#inline /*a := a + b*/
complex csub(const complex& A, const complex& B);//#inline
complex cmul(const complex& A, const complex& B);//#inline
void caccumarray(pComplexArray A, pComplexArray B, short int n);
complex cmulreal(const complex& A, double B);//#inline /* := a*b */
void cmulrealaccum(complex& A, double B);//#inline /* a=a*b*/
complex cdiv(const complex& A, const complex& B);//#inline
complex cdivreal(const complex& A, double B);//#inline /* := a /b*/
complex conjg(const complex& A);//#inline
complex cnegate(const complex& A);//#inline
complex csqrt(const complex& A);
complex CLn(const complex& A);
polar topolar(double A, double B);//#inline  // scalar to polar
double prel(const polar& A);  // real part of polar number   |a| cos()
double pimg(const polar& A);  // imag part of polar number   |a| sin()
complex ptocomplex(const polar& A);
polar padd(const polar& A, const polar& B);
polar psub(const polar& A, const polar& B);
polar pmul(const polar& A, const polar& B);
polar pdiv(const polar& A, const polar& B);
complex pdegtocomplex(double magn, double Angle);
complex pclx(double magn, double Angle);
extern complex CZero;
extern complex cONE;


}  // namespace Ucomplex

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Ucomplex;
#endif

#endif // UcomplexH




