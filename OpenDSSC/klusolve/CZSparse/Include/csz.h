/* ------------------------------------------------------------------------- */
/* CZSparse, Copyright (c) 2008, EnerNex Corporation. All rights reserved.   */
/* Licensed under the GNU Lesser General Public License (LGPL) v 2.1         */
/* ------------------------------------------------------------------------- */
cs *csz_spalloc (int m, int n, int nzmax, int values, int triplet);

int csz_sprealloc (cs *A, int nzmax);
cs *csz_compress (const cs *T);
int csz_dupl (cs *A);
int csz_gaxpy (const cs *A, const double *x, double *y);
int csz_entry (cs *T, int i, int j, double re, double im);
int csz_print (const cs *A, int brief);
cs *csz_transpose (const cs *A, int values);
cs *csz_spalloc (int m, int n, int nzmax, int values, int triplet);
int csz_dropzeros (cs *A);
int csz_fkeep (cs *A, int (*fkeep) (int, int, double, double, void *), void *other);
double csz_norm (const cs *A);
