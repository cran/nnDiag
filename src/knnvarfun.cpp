#include <iostream>
#include <iomanip>
#include <string>
#include <fstream>
#include <R.h>
#include <Rinternals.h>
using namespace std;

#include "annImpute.h"

//#define MATHLIB_STANDALONE
//#include <Rmath.h>


int knnvarfun(int k, double *ref, int refDim1, int refDim2, double *refdata, double *target,int tarDim1, int tarDim2, double *RMtrx, double *ytilde, double *varyhat, double *varmuhat, double *varterm2){

  int i,j,r,s,ind1,ind2,ind3;
  double sum1,sum2,sum3,tempsum1,tempsum2;
  int *knnIndxMtrx = new int[tarDim1*k];
  double *knnDistMtrx = new double[tarDim1*k];
  double *ypred = new double[tarDim1*k]; 
  //double *ytilde = new double[tarDim1];
  double *sigma = new double[tarDim1];
  //double *varmuhat = new double[tarDim1];
  //double *varyhat = new double[tarDim1];
  double Ybar, VarYm1, VarYm2;  
  
  ann(k, ref, refDim1, refDim2, target, tarDim1, tarDim2, knnIndxMtrx, knnDistMtrx);
  
  for(i=0; i<tarDim1; i++ ){
    sum1 = 0;
    for(j=0; j<k; j++){
      ind1 = knnIndxMtrx[j*tarDim1+i];
      ypred[j*tarDim1+i] = refdata[ind1-1];
      sum1 += ypred[j*tarDim1+i]; 
    }
    ytilde[i] = sum1/k;
  }

  for(i=0; i<tarDim1; i++){
    sum1 = 0; sum2 = 0; sum3 = 0;

    for(j=0; j<k; j++){
      for (r=0; r<k; r++){
	ind1 = knnIndxMtrx[j*tarDim1 + i]; ind2 = knnIndxMtrx[r*tarDim1 + i];
        sum1 += RMtrx[(ind1-1)*(refDim1 + tarDim1) + ind2-1];
      }
      sum2 += RMtrx[(ind1-1)*(refDim1 + tarDim1) + refDim1 + i];
      sum3 += pow(ytilde[i]-ypred[j*tarDim1 + i],2);  
    }

    sigma[i] = sqrt(sum3/(k - sum1/k));
    varmuhat[i] = pow(sigma[i],2)/pow(k,2)*sum1;
    varyhat[i] = pow(sigma[i],2)/pow(k,2)*(sum1 - 2*k*sum2 + pow(k,2));
    varterm2[i] = -2*k*sum2;
  }

  sum1 = 0;
  for(i=0; i<tarDim1; i++){
    sum1 += ytilde[i];
  }
  Ybar = sum1/tarDim1;
  
  tempsum1 = 0; tempsum2 = 0;
  for(i=0; i<tarDim1; i++){
    for(j=0; j<tarDim1; j++){
      sum1 = 0; sum2 = 0; sum3 = 0;
      for(r=0; r<k; r++){
	for(s=0; s<k; s++){
	  ind1 = knnIndxMtrx[r*tarDim1 + i];
	  ind2 = knnIndxMtrx[s*tarDim1 + j];
	  ind3 = knnIndxMtrx[r*tarDim1 + j];
	  sum1 += RMtrx[(ind1-1)*(refDim1 + tarDim1) + ind2 - 1];
	}
	sum2 += RMtrx[(ind1-1)*(refDim1 + tarDim1) + refDim1 + j];
	sum3 += RMtrx[(ind3-1)*(refDim1 + tarDim1) + refDim1 + i];
      }
      tempsum1 += sigma[i] * sigma[j]/ pow(k,2) * sum1;
      tempsum2 += sigma[i] * sigma[j]/ pow(k,2) * (sum1 - k*sum2 - k*sum3 + pow(k,2));
    }
  }
  VarYm1 = tempsum1/ pow(tarDim1,2); VarYm2 = tempsum2/ pow(tarDim1,2);
  //cout<<Ybar<<endl;
  //cout<<VarYm1<<endl;
  //cout<<VarYm2<<endl;
	    
  return(0);
}






