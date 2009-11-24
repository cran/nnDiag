#include <iostream>
#include <iomanip>
#include <string>
#include <fstream>
#include <R.h>
#include <Rinternals.h>
using namespace std;

#include "annImpute.h"
#include "knnvarfun.h"

//#define MATHLIB_STANDALONE
//#include <Rmath.h>

extern "C"{
  SEXP knnvarfun_wrap(SEXP k_r, SEXP ref_r, SEXP refDim1_r, SEXP refDim2_r, SEXP refdata_r, SEXP target_r, SEXP tarDim1_r, SEXP tarDim2_r, SEXP RMtrx_r, SEXP ytilde_r, SEXP varyhat_r, SEXP varmuhat_r, SEXP varterm2_r) {

    //PROTECT
    int junk = knnvarfun(INTEGER(k_r)[0], REAL(ref_r), INTEGER(refDim1_r)[0], INTEGER(refDim2_r)[0], REAL(refdata_r), REAL(target_r), INTEGER(tarDim1_r)[0], INTEGER(tarDim2_r)[0], REAL(RMtrx_r), REAL(ytilde_r), REAL(varyhat_r), REAL(varmuhat_r), REAL(varterm2_r));  




    // UNPROTECT(1);
    return(k_r);
  }
}
