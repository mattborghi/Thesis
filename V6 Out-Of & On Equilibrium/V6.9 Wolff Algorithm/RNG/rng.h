// Random Number Generators from "Numerical Recipes"

#ifndef RNG_H_DEFINED
#define RNG_H_DEFINED

// Quick and dirty random number generator
// returns a uniform deviate in the interval [0,1)
// To seed the generator, reset qadseed
unsigned long qadseed = 123456789;
inline double qadran ( ) {
    qadseed = qadseed*1664525L + 1013904223L;
    return qadseed/4294967296.0;
}

// Long period >2x10^18 random number generator of L'Ecuyer with 
// Bays-Durham shuffle. Return a uniform deviate in the interval (0,1). 
// Call with int variable as argument set to a negative value, and
// don't this variable variable unless you want to reseed the generator

double ran2 (int& idum) {
    const int IM1 = 2147483563, IM2 = 2147483399;
    const double AM=(1.0/IM1);
    const int IMM1 = IM1-1;
    const int IA1 = 40014, IA2 = 40692, IQ1 = 53668, IQ2 = 52774;
    const int IR1 = 12211, IR2 = 3791, NTAB = 32;
    const int NDIV = 1+IMM1/NTAB;
    const double EPS = 3.0e-16, RNMX = 1.0-EPS;

    int j, k;
    static int idum2=123456789, iy = 0;
    static int iv[NTAB];
    double temp;

    if (idum <= 0) {
        idum = (idum == 0 ? 1 : -idum);
        idum2=idum;
        for (j=NTAB+7;j>=0;j--) {
            k=idum/IQ1;
            idum=IA1*(idum-k*IQ1)-k*IR1;
            if (idum < 0) idum += IM1;
            if (j < NTAB) iv[j] = idum;
        }
        iy=iv[0];
    }
    k=idum/IQ1;
    idum=IA1*(idum-k*IQ1)-k*IR1;
    if (idum < 0) idum += IM1;
    k=idum2/IQ2;
    idum2=IA2*(idum2-k*IQ2)-k*IR2;
    if (idum2 < 0) idum2 += IM2;
    j=iy/NDIV;
    iy=iv[j]-idum2;
    iv[j] = idum;
    if (iy < 1) iy += IMM1;
    if ((temp=AM*iy) > RNMX) return RNMX;
    else return temp;
}

#include <cmath>

// Returns a normally distributed deviate with zero mean and unit variance

double gasdev (int& idum) {
     static int iset = 0;
     static double gset;
     double fac, rsq, v1, v2;
     if (idum < 0) iset = 0;
     if (iset == 0) {
          do {
               v1 = 2.0*ran2(idum)-1.0;
               v2 = 2.0*ran2(idum)-1.0;
               rsq = v1*v1 + v2*v2;
          } while (rsq >= 1.0 || rsq == 0.0);
          fac = std::sqrt(-2.0*std::log(rsq)/rsq);
          gset = v1*fac;
          iset = 1;
          return v2*fac;
     } else {
          iset = 0;
          return gset;
     }
}

#endif /* RNG_H_DEFINED */

