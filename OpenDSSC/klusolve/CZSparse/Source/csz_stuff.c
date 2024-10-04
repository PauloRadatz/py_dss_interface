/* ------------------------------------------------------------------------- */
/* CZSparse, Copyright (c) 2008, EnerNex Corporation. All rights reserved.   */
/* Licensed under the GNU Lesser General Public License (LGPL) v 2.1         */
/* ------------------------------------------------------------------------- */

/* These are complex versions of selected functions from the CSparse package
 * by Timothy A. Davis. The purpose is to compile on Windows without a C99
 * compiler.  A->x stores alternate real/imaginary values.
 */

#include "cs.h"
#include "csz.h"

/* allocate a sparse matrix (triplet form or compressed-column form) */
cs *csz_spalloc (int m, int n, int nzmax, int values, int triplet)
{
    cs *A = cs_calloc (1, sizeof (cs));    /* allocate the cs struct */
    if (!A) return (NULL);		    /* out of memory */
    A->m = m;				    /* define dimensions and nzmax */
    A->n = n;
    A->nzmax = nzmax = CS_MAX (nzmax, 1);
    A->nz = triplet ? 0 : -1;		    /* allocate triplet or comp.col */
    A->p = cs_malloc (triplet ? nzmax : n+1, sizeof (int));
    A->i = cs_malloc (nzmax, sizeof (int));
    A->x = values ? cs_malloc (2*nzmax, sizeof (double)) : NULL;
    return ((!A->p || !A->i || (values && !A->x)) ? cs_spfree (A) : A);
}

/* change the max # of entries sparse matrix */
int csz_sprealloc (cs *A, int nzmax)
{
    int ok, oki, okj = 1, okx = 1;
    if (!A) return (0);
    if (nzmax <= 0) nzmax = (CS_CSC (A)) ? (A->p [A->n]) : A->nz;
    A->i = cs_realloc (A->i, nzmax, sizeof (int), &oki);
    if (CS_TRIPLET (A)) A->p = cs_realloc (A->p, nzmax, sizeof (int), &okj);
    if (A->x) A->x = cs_realloc (A->x, 2*nzmax, sizeof (double), &okx);
    ok = (oki && okj && okx);
    if (ok) A->nzmax = nzmax;
    return (ok);
}

/* add an entry to a triplet matrix; return 1 if ok, 0 otherwise */
int csz_entry (cs *T, int i, int j, double re, double im)
{
	int k;
    if (!CS_TRIPLET (T) || i < 0 || j < 0) return (0);	    /* check inputs */
    if (T->nz >= T->nzmax && !csz_sprealloc (T,2*(T->nzmax))) return (0);
	if (T->x) {
		k = 2 * T->nz;
		T->x [k] = re;
		T->x [k+1] = im;
	}
    T->i [T->nz] = i;
    T->p [T->nz++] = j;
    T->m = CS_MAX (T->m, i+1);
    T->n = CS_MAX (T->n, j+1);
    return (1);
}

/* print a sparse matrix */
int csz_print (const cs *A, int brief)
{
    int p, j, m, n, nzmax, nz, *Ap, *Ai;
    double *Ax;

    if (!A) { printf ("(null)\n"); return (0); }
    m = A->m; n = A->n; Ap = A->p; Ai = A->i; Ax = A->x;
    nzmax = A->nzmax; nz = A->nz;
    printf ("CSparse Version %d.%d.%d, %s.  %s\n", CS_VER, CS_SUBVER,
	CS_SUBSUB, CS_DATE, CS_COPYRIGHT);
    if (nz < 0) {
		printf ("%d-by-%d, nzmax: %d nnz: %d, 1-norm: %g\n", m, n, nzmax,
			Ap [n], csz_norm (A));
		for (j = 0; j < n; j++) {
			printf ("    col %d : locations %d to %d\n", j, Ap [j], Ap [j+1]-1);
			for (p = Ap [j]; p < Ap [j+1]; p++) {
				printf ("      %d : %g + j%g\n", 
					Ai [p], Ax ? Ax [2*p] : 1,  Ax ? Ax [2*p + 1] : 1);
				if (brief && p > 20) { printf ("  ...\n"); return (1); }
			}
		}
    } else {
		printf ("triplet: %d-by-%d, nzmax: %d nnz: %d\n", m, n, nzmax, nz);
		for (p = 0; p < nz; p++) {
			printf ("    %d %d : %g + j%g\n", 
				Ai [p], Ap [p], Ax ? Ax [2*p] : 1, Ax ? Ax [2*p + 1] : 1);
			if (brief && p > 20) { printf ("  ...\n"); return (1); }
		}
    }
    return (1);
}

/* C = A' */
cs *csz_transpose (const cs *A, int values)
{
    int p, q, j, *Cp, *Ci, n, m, *Ap, *Ai, *w;
    double *Cx, *Ax;
    cs *C;
	int p1, q1;

    if (!CS_CSC (A)) return (NULL);	/* check inputs */
    m = A->m; n = A->n; Ap = A->p; Ai = A->i; Ax = A->x;
    C = csz_spalloc (n, m, Ap [n], values && Ax, 0);	   /* allocate result */
    w = cs_calloc (m, sizeof (int));			   /* get workspace */
    if (!C || !w) return (cs_done (C, w, NULL, 0));	   /* out of memory */
    Cp = C->p; Ci = C->i; Cx = C->x;
    for (p = 0; p < Ap [n]; p++) w [Ai [p]]++;	   /* row counts */
    cs_cumsum (Cp, w, m);				   /* row pointers */
    for (j = 0; j < n; j++) {
		for (p = Ap [j]; p < Ap [j+1]; p++) {
			Ci [q = w [Ai [p]]++] = j;	/* place A(i,j) as entry C(j,i) */
			if (Cx) {
				q1 = 2 * q;
				p1 = 2 * p;
				Cx [q1] = Ax [p1];
				Cx [q1 + 1] = Ax [p1 + 1];
			}
		}
    }
    return (cs_done (C, w, NULL, 1));	/* success; free w and return C */
}

static int csz_nonzero (int i, int j, double re, double im, void *other)
{
    return (re != 0.0 || im != 0.0);
}

int csz_dropzeros (cs *A)
{
    return (csz_fkeep (A, &csz_nonzero, NULL));	/* keep all nonzero entries */
} 

/* drop entries for which fkeep(A(i,j)) is false; return nz if OK, else -1 */
int csz_fkeep (cs *A, int (*fkeep) (int, int, double, double, void *), void *other)
{
    int j, p, nz = 0, n, *Ap, *Ai;
    double *Ax;
	double re, im;
	int p1, p2, z1;

    if (!CS_CSC (A) || !fkeep) return (-1);	/* check inputs */
    n = A->n; Ap = A->p; Ai = A->i; Ax = A->x;
    for (j = 0; j < n; j++) {
		p = Ap [j];			    /* get current location of col j */
		Ap [j] = nz;			    /* record new location of col j */
		for (; p < Ap [j+1]; p++) {
			p1 = 2 * p;
			p2 = p1 + 1;
			if (Ax) {
				re = Ax[p1];
				im = Ax[p2];
			} else {
				re = im = 0.0;
			}
			if (fkeep (Ai [p], j, re, im, other)) {
				if (Ax) {
					z1 = 2 * nz;
					Ax [z1] = Ax [p1];  /* keep A(i,j) */
					Ax [z1 + 1] = Ax [p2];
				}
				Ai [nz++] = Ai [p];
			} else {
				re = im;
			}
		}
    }
    Ap [n] = nz;			    /* finalize A */
    csz_sprealloc (A, 0);		    /* remove extra space from A */
    return (nz);
}

/* remove duplicate entries from A */
int csz_dupl (cs *A)
{
    int i, j, p, q, nz = 0, n, m, *Ap, *Ai, *w;
    double *Ax;
	int p1, p2, w1, z1;

    if (!CS_CSC (A)) return (0);		/* check inputs */
    m = A->m; n = A->n; Ap = A->p; Ai = A->i; Ax = A->x;
    w = cs_malloc (m, sizeof (int));		/* get workspace */
    if (!w) return (0);			/* out of memory */
    for (i = 0; i < m; i++) w [i] = -1;	/* row i not yet seen */
    for (j = 0; j < n; j++) {
		q = nz;				/* column j will start at q */
		for (p = Ap [j]; p < Ap [j+1]; p++) {
			p1 = 2 * p;
			p2 = p1 + 1;
			i = Ai [p];			/* A(i,j) is nonzero */
			if (w [i] >= q) {
				w1 = 2 * w[i];
				Ax [w1] += Ax [p1];		/* A(i,j) is a duplicate */
				Ax [w1+1] += Ax [p2];
			} else {
				w [i] = nz;			/* record where row i occurs */
				z1 = 2 * nz;		/* keep A(i,j) */
				Ax [z1] = Ax [p1];
				Ax [z1+1] = Ax [p2];
				Ai [nz++] = i;	
			}
		}
		Ap [j] = q;				/* record start of column j */
    }
    Ap [n] = nz;				/* finalize A */
    cs_free (w);				/* free workspace */
    return (csz_sprealloc (A, 0));		/* remove extra space from A */
}

/* C = compressed-column form of a triplet matrix T */
cs *csz_compress (const cs *T)
{
    int m, n, nz, p, k, *Cp, *Ci, *w, *Ti, *Tj;
    double *Cx, *Tx;
    cs *C;
	int p1, k1;

    if (!CS_TRIPLET (T)) return (NULL);		/* check inputs */
    m = T->m; n = T->n; Ti = T->i; Tj = T->p; Tx = T->x; nz = T->nz;
    C = csz_spalloc (m, n, nz, Tx != NULL, 0);		/* allocate result */
    w = cs_calloc (n, sizeof (int));			/* get workspace */
    if (!C || !w) return (cs_done (C, w, NULL, 0));	/* out of memory */
    Cp = C->p; Ci = C->i; Cx = C->x;
    for (k = 0; k < nz; k++) w [Tj [k]]++;		/* column counts */
    cs_cumsum (Cp, w, n);				/* column pointers */
    for (k = 0; k < nz; k++) {
		Ci [p = w [Tj [k]]++] = Ti [k];    /* A(i,j) is the pth entry in C */
		if (Cx) {
			p1 = 2 * p;
			k1 = 2 * k;
			Cx [p1] = Tx [k1];
			Cx [p1+1] = Tx [k1+1];
		}
    }
    return (cs_done (C, w, NULL, 1));	    /* success; free w and return C */
}

/* 1-norm of a sparse matrix = max (sum (abs (A))), largest column sum */
double csz_norm (const cs *A)
{
    int p, j, n, *Ap;
    double *Ax,  norm = 0, s;
	double re, im;
	int p1;

    if (!CS_CSC (A) || !A->x) return (-1);		/* check inputs */
    n = A->n; Ap = A->p; Ax = A->x;
    for (j = 0; j < n; j++) {
		for (s = 0, p = Ap [j]; p < Ap [j+1]; p++) {
			p1 = 2 * p;
			re = Ax [p1];
			im = Ax [p1+1];
			s += (re * re + im * im);
		}
		norm = CS_MAX (norm, s);
    }
    return (sqrt (norm));
}

/* y = A*x+y for complex numbers, interleaved Re and Im parts */
int csz_gaxpy (const cs *A, const double *x, double *y)
{
    int p, j, n, *Ap, *Ai;
    double *Ax;
	int i1, j1, p1;
	double Are, Aim, Xre, Xim;

    if (!CS_CSC (A) || !x || !y) return (0);	    /* check inputs */
    n = A->n; Ap = A->p; Ai = A->i ; Ax = A->x;
	for (j = 0; j < n; j++) {
		j1 = 2 * j;
		for (p = Ap [j]; p < Ap [j+1]; p++) {
			p1 = 2 * p;
			i1 = 2 * Ai[p];
			Are = Ax[p1];
			Aim = Ax[p1+1];
			Xre = x[j1];
			Xim = x[j1+1];
			y [i1] += (Are * Xre - Aim * Xim);
			y [i1+1] += (Are * Xim + Aim * Xre);
		}
    }
    return (1);
}
