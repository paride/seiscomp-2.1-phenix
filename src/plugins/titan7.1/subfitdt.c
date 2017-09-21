/*======================================================================
    procedure subfitdt()

    Author: J.-F. Fels, OMP, Toulouse
*======================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "sismalp.h"

#define MAXPOL 6
typedef struct
{
    double beg_systime;
    double end_systime;
    int    ncoef;
    double coef[MAXPOL];
} Tcoef;
typedef struct {
    double     sec;
    int    min;
    float    dt;
} Top;


#define D 86400.0
/*
extern Paths paths;
int verb;
*/
static void fit();
extern svd_fit();

float     *d;
float     *dt;
int        nentries;
float      aa[MAXPOL+1];
double     AA[MAXPOL+1];


void
subfitdt(char *station,int ncoef)
{
    char     tmasc[40];
    char     year[5];
    char     date[512],line[255],rahfile[512],topfile[512];
    char    *token[4];
    char     fname[256];
    int      ntok,rah;
    int     i;
    int      npt;
    double   smin, smax;
    double   s, dblsecs,d0;
    double   days_this_year;
    double   time_drift;
    double   delta, xp;
    FILE    *Fp,*fp2,*fp3;
    Tcoef     time_coef;
    Top     top;
    double      calc_delta_t(double, Tcoef);

    Fp = NULL;


/* Open Reset Time file */
    sprintf(rahfile,"%s.rah",station);
    open_Frd(rahfile, &Fp);

/* Loop over top files */
    fscanf(Fp,"%d %s %d",&rah,date,&nentries);
    while(!feof(Fp)) {
       sprintf(topfile,"%d.%s.new",rah,station);
       fp2=fopen(topfile,"r");

/* How many entries do we have in the observed delta file ? */

      if (nentries < ncoef)
      {
        fprintf(stderr,"\n\tERROR: fitdt: less than %d entries ");
        fprintf(stderr,"in file '%s'\n\n", topfile, ncoef);
        exit(1);
      }

      npt = nentries + 20;

/* Allocate memory for t, dt */

      if (!(d = (float *) m_alloc(npt*sizeof(float))))
      {
        fprintf(stderr, "ERROR: fit: malloc failed (1) for ");
        fprintf(stderr, "%ld bytes\n", npt*sizeof(double));
        exit(1);
      }
      if (!(dt = (float *) m_alloc(npt*sizeof(float))))
      {
        fprintf(stderr, "ERROR: fit: malloc failed (2) for ");
        fprintf(stderr, "%ld bytes\n", npt*sizeof(float));
        exit(1);
      }

/* Read observed delta_t; find min-max; convert to days */
      npt = 0;
      smin = 1.0e35;
      smax = -1.0e35;
      fread(&top,sizeof(top),1,fp2);
      while ((!feof(fp2)) && (npt<=nentries))
      {
        if (top.min > smax) smax = top.min;
        if (top.min < smin) smin = top.min;
        d[npt] = (float) top.min/D;
        dt[npt] = top.dt;
        ++npt;
        fread(&top,sizeof(top),1,fp2);
      }
      fclose(fp2);


/* Fit line to observed delta */
/*
    fit(d, dt, npt, &a, &b);
*/

/* Shift data to the right by 1 (this is way SVDFIT works) */
/* Be sure that we keep d[0] */
      d0=d[0];
      for (i=npt; i>=1; i--)
      {
        d[i]  =   d[i-1] - d[0]; 
        dt[i] =  dt[i-1];
      }

if (0) for (i=1; i<=npt; i++) printf("%E %E\n", d[i], dt[i]);

      svd_fit(d,dt,npt,aa,ncoef);
  
      time_coef.beg_systime = smin;
      time_coef.end_systime = smax;
      time_coef.ncoef = ncoef;
      for (i=0; i<ncoef; i++) time_coef.coef[i] = aa[i+1];

      time_drift = (aa[2] + 2.0*aa[3]*d[1]) / D;
      printf("%s ", station);
      time_asc1(tmasc, smin); printf("[%f,19%.17s] to ",smin, tmasc);
      time_asc1(tmasc, smax); printf("[%f,19%.17s]\n",smax, tmasc);
      printf("  coefs: ");
      for (i=0; i<time_coef.ncoef; i++) printf("%.10E ", time_coef.coef[i]);
      printf("\n");
      printf("  drift: %E\n", time_drift);

      time_asc1(tmasc, smin);
      delta = calc_delta_t(smin, time_coef);
      printf("  check: 19%.17s (%.3f) -> dt=%.4f\n", tmasc, smin, delta);



/* Write fitted delta_time to time file */

      sprintf(fname, "%d.%s.fit",rah, station);
      fcreat_append(fname, &fp3);

      time_asc1(tmasc, smin); fprintf(fp3,"[%f,19%.17s] ", smin,tmasc);
      time_asc1(tmasc, smax); fprintf(fp3,"[%f,19%.17s] ", smax,tmasc);
      time_asc1(tmasc, smax); fprintf(fp3,"%d\n",ncoef);
      for (i=0; i<time_coef.ncoef; i++) fprintf(fp3,"%.10E ",time_coef.coef[i]);
      fprintf(fp3,"\n");
      fclose(fp3);

      m_free(d);
      m_free(dt);
      fscanf(Fp,"%d %s %d",&rah,date,&nentries);
    }
    fclose(Fp);
    Fp = NULL;
}


/*===================================================================*/
void fit(x, y, ndata, a, b)
float *x;
float *y;
float *a, *b;
int   ndata;
{
int i;
float wt, t, sxoss;
float sx = 0.0, sy = 0.0, st2 = 0.0;
float ss;

    *b = 0.0;
    for (i=0; i<ndata; i++)
    {
        sx += x[i];
        sy += y[i];
    }
    ss = ndata;
    sxoss = sx / ss;
    for (i=0; i<ndata; i++)
    {
        t = x[i] - sxoss;
        st2 += t * t;
        *b += t * y[i];
    }
    *b /= st2;
    *a = (sy - sx * (*b)) / ss;
}



/*
   Convert back time scale from days to seconds since
   1970 Jan 1st in time polynomial

    s = -d[0]*D;
    AA[1] = aa[1] + aa[2]*s/D  + aa[3]*s*s/(D*D); 
    AA[2] = aa[2]/D + 2.0*aa[3]*s/(D*D);
    AA[3] = aa[3]/(D*D); 
    time_drift = AA[2] + 2.0*AA[3]*d[0]*D;


    printf("%s ", station);
    time_asc1(tmasc, smin); printf("19%.17s to ", tmasc);
    time_asc1(tmasc, smax); printf("19%.17s\n", tmasc);
    printf("    coefs: %E %E %E  drift: %E\n",
             AA[1],AA[2],AA[3], time_drift);

 Verification: compute dt

    s = d[0]*D;
    time_asc1(tmasc, s);
    printf("    check: 19%.17s (%.3f) -> dt=%.4f\n",
           tmasc, s, AA[1]+AA[2]*s+AA[3]*s*s);
*/




/*==========================================================
    svdlib.c         July 1997

 *========================================================*/

#ifndef min
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))
#endif
#define SIGN(a,b) ((b) >= 0.0 ? fabs(a) : -fabs(a))

int svdfit       (float *, float *, float *,
                  int, float*, int, float*,
                  float*, int(*)(), float [][MAXPOL], float [][MAXPOL]);
int svdcmp       (float [][MAXPOL], int, int, float*, float [][MAXPOL]);
int svbksb       (float [][MAXPOL], float*, float [][MAXPOL],
                  int, int, float*, float*);
int fpoly        ();
float *vector    (int,int);
float **matrix   (int,int,int,int);
void free_vector (float*, int);
float pythag ( float a, float b );

float w[MAXPOL];

/*=======================================================*/
int svd_fit(x,y,ndt,c,nc)
float *x;
float *y;
float c[];
int ndt,nc;
{
float chisq;
int i, j;
static float *u;
static float *v;
static float *sig;

    u =   (float *) m_alloc((ndt+1)*(MAXPOL)*sizeof(float));
    v =   (float *) m_alloc((ndt+1)*(MAXPOL)*sizeof(float));
    sig = (float *) m_alloc((ndt+1)*sizeof(float));

    for (i=1; i<=ndt; i++) sig[i] = 1.0;

    svdfit(x, y, sig, ndt, c, nc, w, &chisq, fpoly, u,v);

    m_free(u);
    m_free(v);
    m_free(sig);
    return 0;
}

/*=======================================================*/
int svdfit(x, y, sig, ndata, a, ma, w, chisq, funcs, u,v)
float *x;
float *y;
float *sig;
int ndata;
float *a;
int ma;
float *w;
float *chisq;
int (*funcs)();
float u[][MAXPOL];
float v[][MAXPOL];
{
int i, j;
float wmax, *b, *afunc;
float thresh, tmp, sum;

if (0) fprintf(stderr,"RUNNING svdfit\n");

if (0) for (i=1; i<=ndata; ++i)
    printf("x[%d] %f  y[%d] %f  sig[%d] %f\n", i,x[i], i,y[i], i,sig[i]);

    b = vector(1,ndata+1);
    afunc = vector(1,ndata+1);

    for (i=1; i<=ndata; ++i)
    {
        (*funcs)(x[i], afunc, ma);
        tmp = 1. / sig[i];
        for (j=1; j<=ma; ++j)
        {
            u[i][j] = afunc[j] * tmp;
if (0) printf("u[%d][%d] %f\n", i,j,u[i][j]);
        }
        b[i] = y[i] * tmp;
if (0) printf("x[%d] %f  b[%d] %f\n", i,x[i],i,b[i]);
    }

    svdcmp(u, ndata, ma, w, v);

    wmax = 0.;
    for (j=1; j<=ma; ++j)   if (w[j] > wmax) wmax = w[j];
    thresh = wmax * (float)1e-5;
    for (j=1; j<=ma; ++j)   if (w[j] < thresh) w[j] = 0.;

    svbksb(u, w, v, ndata, ma, b, a);

    *chisq = 0.;
    for (i=1; i<=ndata; ++i)
    {
        (*funcs)(x[i], afunc, ma);
        sum = 0.;
        for (j=1; j<=ma; ++j) sum += a[j] * afunc[j];
        *chisq += (tmp=(y[i] - sum)/sig[i], tmp*tmp);
    }
    free_vector(afunc,1);
    free_vector(b,1);
    return 0;
}


/*=========================================================*/
int fpoly(x, p, np)
float x, p[];
int np;
{
int j;

    p[1] = (float)1.;
    for (j=2; j<=np; ++j) p[j] = p[j-1]*x;
    return 0;
}


/*======================================================*/
int svdcmp(uu, m, n, w, vv)
float uu[][MAXPOL];
int m, n;
float *w;
float vv[][MAXPOL];
{
float c, f, g, h;
int flag, i, j, k, l;
float s, scale, x, y, z, anorm;
int jj, nm;
float rv1[500];
int its;

if (0) printf("RUNNING svdcmp\n");
    g = 0.;
    scale = 0.;
    anorm = 0.;

    for (i=1; i<=n; ++i)
    {
        l = i + 1;
        rv1[i-1] = scale * g;
        g = s = scale = 0.;
        if (i <= m)
        {
            for (k=i; k<=m; ++k) scale += fabs(uu[k][i]);
            if (scale != (float)0.)
            {
                for (k=i; k<=m; ++k)
                {
                    uu[k][i] /= scale;
                    s += uu[k][i] * uu[k][i];
                }
                f = uu[i][i];
                g = -SIGN(sqrt(s), f);
                h = f*g - s;
                uu[i][i] = f - g;
                for (j=l; j<=n; ++j)
                {
                    s = 0.;
                    for (s=0.0,k=i; k<=m; ++k) s += uu[k][i] * uu[k][j];
                    f = s / h;
                    for (k=i; k<=m; ++k) uu[k][j] += f * uu[k][i];
                }
                for (k=i; k<=m; ++k) uu[k][i] *= scale;
            }
        }
        w[i] = scale * g;
        g = s = scale = 0.0;
        if (i <= m && i != n)
        {
            for (k=l; k<=n; ++k) scale += fabs(uu[i][k]);
            if (scale != 0.0)
            {
                for (k=l; k<=n; ++k)
                {
                    uu[i][k] /= scale;
                    s += uu[i][k] * uu[i][k];
                }
                f = uu[i][l];
                g = -SIGN(sqrt(s), f);
                h = f*g - s;
                uu[i][l] = f - g;
                for (k=l; k<=n; ++k) rv1[k-1] = uu[i][k] / h;
                for (j=l; j<=m; ++j)
                {
                    for (s=0.0,k=l; k<=n; ++k) s += uu[j][k] * uu[i][k];
                    for (k=l; k<=n; ++k) uu[j][k] += s * rv1[k-1];
                }
                for (k=l; k<=n; ++k) uu[i][k] *= scale;
            }
        }
        anorm = max(anorm, (fabs(w[i]) +  fabs(rv1[i-1])));
    }
    for (i=n; i>=1; --i)
    {
        if (i < n)
        {
            if (g != 0.0)
            {
                for (j=l; j<=n; ++j)
                    vv[j][i] = (uu[i][j] / uu[i][l]) / g;
                for (j=l; j<=n; ++j)
                {
                    for (s=0.0,k=l; k<=n; ++k) s += uu[i][k] * vv[k][j];
                    for (k=l; k<=n; ++k)
                    {
                         vv[k][j] += s * vv[k][i];
                    }
                }
            }
            for (j=l; j<=n; ++j) vv[i][j] = vv[j][i] = 0.0;
        }
        vv[i][i] = 1.0;
        g = rv1[i-1];
        l = i;
    }
    for (i = min(m,n); i >= 1; --i)
    {
      l = i + 1;
      g = w[i];
      for (j=l; j<=n; ++j) uu[i][j] = 0.0;
      if (g != 0.0)
      {
         g = 1.0 / g;
         for (j=l; j<=n; ++j)
         {
          for (s=0.0,k=l; k<=m; ++k) s += uu[k][i] * uu[k][j];
          f = s / uu[i][i] * g;
          for (k=i; k<=m; ++k) uu[k][j] += f * uu[k][i];
         }
         for (j=i; j<=m; ++j) uu[j][i] *= g;
      }
      else
      {
         for (j=i; j<=m; ++j) uu[j][i] = 0.0;
      }
      uu[i][i] += 1.0;
    }
/* Diagonalization of the bidiagonal form */
    for (k=n; k>=1; --k)
    {
      for (its=1; its<=30; ++its)
      {
         for (l=k; l>=1; --l)
         {
             nm = l - 1;
             if (fabs(rv1[l-1]) + anorm == anorm)
             {
                 goto L2;
             }
             if ((fabs(w[nm]) + anorm) == anorm)
             {
                 goto L1;
             }
         }

L1:
         c = 0.0;
         s = 1.0;
         for (i=l; i<=k; ++i)
         {
             f = s * rv1[i-1];
             if ((fabs(f) + anorm) ==  anorm)
             {
                 goto L2;
             }
             g = w[i];
             h = pythag(f, g);
             w[i] = h;
             h = 1.0 / h;
             c = g * h;
             s = -(f * h);
             for (j=1; j<=m; ++j)
             {
                 y = uu[j][nm];
                 z = uu[j][i];
                 uu[j][nm] = y * c + z * s;
                 uu[j][i] = -(y * s) + z * c;
             }
         }
L2:
         z = w[k];
         if (l == k)
         {
             if (z < 0.0)
             {
                 w[k] = -z;
                 for (j=1; j<=n; ++j) vv[j][k] = -vv[j][k];
             }
             goto L3;
         }
         if (its == 30)
         {
             fprintf(stderr,"no convergence in svdcmp\n");
             exit(1);
         }
         x = w[l];
         nm = k - 1;
         y = w[nm];
         g = rv1[nm-1];
         h = rv1[k-1];
         f = ((y - z) * (y + z) + (g - h) * (g + h)) / (h * 2.0 * y);
         g = pythag(f, 1.0);
         f = ((x - z) * (x + z) + h * (y / (f + SIGN(g, f)) - h)) / x;
         c = 1.0;
         s = (float)1.;
         for (j=l; j<=nm; ++j)
         {
             i = j + 1;
             g = rv1[i-1];
             y = w[i];
             h = s * g;
             g = c * g;
             z = pythag(f, h);
             rv1[j-1] = z;
             c = f / z;
             s = h / z;
             f = x * c + g * s;
             g = -(x * s) + g * c;
             h = y * s;
             y *= c;
             for (jj=1; jj<=n; ++jj)
             {
                 x = vv[jj][j];
                 z = vv[jj][i];
                 vv[jj][j] = x * c + z * s;
                 vv[jj][i] = -(x * s) + z * c;
             }
             z = pythag(f, h);
             w[j] = z;
             if (z != 0.0)
             {
                 z = (float)1. / z;
                 c = f * z;
                 s = h * z;
             }
             f = c * g + s * y;
             x = -(s * g) + c * y;
             for (jj=1; jj<=m; ++jj)
             {
                 y = uu[jj][j];
                 z = uu[jj][i];
                 uu[jj][j] = y * c + z * s;
                 uu[jj][i] = -(y * s) + z * c;
             }
         }
         rv1[l-1] = 0.0;
         rv1[k-1] = f;
         w[k] = x;
      }
L3:
     ;
    }
    return 0;
}


/*=======================================================*/
int svbksb(uu, w, vv, ndata, ma, b, a)
float uu[][MAXPOL];
float *w;
float vv[][MAXPOL];
int ndata, ma;
float *b, *a;
{
int jj, i, j;
float s, *tmp;

    tmp = vector(1,ma+1);
    for (j=1; j<=ma; ++j)
    {
        s = 0.;
        if (w[j] != 0.)
        {
            for (i=1; i<=ndata; ++i) s += uu[i][j]*b[i];
            s /= w[j];
        }
        tmp[j] = s;
    }
    for (j=1; j<=ma; ++j)
    {
        s = 0.;
        for (jj=1; jj<=ma; ++jj)
        {
            s += vv[j][jj]*tmp[jj];
        }
        a[j] = s;
    }
    free_vector(tmp,1);
    return 0;
}



/*=============================================================*/
float pythag(float a, float b)
{
float at,bt,ct;

    return ((at=fabs(a)) > (bt=fabs(b))) ?
                           (ct = bt/at, at*sqrt(1.+ct*ct)) :
        ((bt==0.0) ? 0.0 : (ct = at/bt, bt*sqrt(1.+ct*ct)));
}


/*==============================================================*/
float *vector(nl,nh)
int nl,nh;
{ 
float *v;

   v=(float*)malloc((unsigned) (nh-nl+1)*sizeof(float));
   if (!v) fprintf(stderr,"vector: memory allocation failure");
   return (v-nl);
}

/*==============================================================*/
float **matrix(nrl,nrh,ncl,nch)
int nrl,nrh,ncl,nch;
{
int i;
float **m;

    m=(float**)malloc((unsigned) (nrh-nrl+1)*sizeof(float));
    if (!m) fprintf(stderr,"matrix: memory allocation failure");
    m -=nrl;

    for(i=nrl;i<=nrh;i++)
    {
        m[i]=(float*)malloc((unsigned) (nch-ncl+1)*sizeof(float));
        if (!m[i])fprintf(stderr,"matrix: memory allocation failure");
        m[i] -=ncl;
    }
    return m;
}

/*==============================================================*/
void free_matrix(m,nrl,nrh,ncl)
float **m;
int nrl,nrh,ncl;
{
int i;
    for(i=nrh;i>nrl;i--) free((char *) (m[i]+ncl));
    free((char *) (m+nrl));
}

/*==============================================================*/
void free_vector(v,nl)
float *v;
int nl;
{ free((char *) (v+nl)); }


/*==============================================================*/
prvec(v,n)
float *v;
int n;
{
int i;
    for (i=1;i<=n; i++) printf("%.6e  ", v[i]);
    printf("\n");
    return 0;
}

/*==============================================================*/
prmat(v,nr,nc)
float **v;
int nr, nc;
{
int i,j;
    for (j=1;j<=nr;j++)
    {
        for (i=1;i<=nc; i++) printf("%.6e  ", v[i][j]);
        printf("\n");
    }
    return 0;
}

/*==============================================================*/
float **matrix2(nrh,nch)
int nrh,nch;
{
int i;
float **m;

    m=(float**)malloc((unsigned) (nrh)*sizeof(float));
    if (!m) fprintf(stderr,"matrix: memory allocation failure");

    for(i=0;i<nrh;i++)
    {
        m[i]=(float*)malloc((unsigned) (nch)*sizeof(float));
        if (!m[i])fprintf(stderr,"matrix: memory allocation failure");
    }
    return m;
}

/*==============================================================*/
prmat2(v,nr,nc)
float **v;
int nr, nc;
{
int i,j;
    for (j=0;j<nr;j++)
    {
        for (i=0;i<nc; i++) printf("%.6e  ", v[i][j]);
        printf("\n");
    }
    return 0;
}

