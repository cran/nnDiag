#include <iostream>
#include <iomanip>
#include <string>
#include <fstream>
#include <R.h>
#include <Rinternals.h>
using namespace std;

#include "annImpute.h"
#include "ANN/ANN.h"

int ann(int k, double *ref, int refDim1, int refDim2, double *target, int tarDim1, int tarDim2, int *knnIndxMtrx, double *knnDistMtrx){
  
  int i,j;
  int cnt = 0;
  int rcnt = 0;
  double eps = 0; 
  
  int bucketSize = 1;
  ANNsplitRule splitRule = (ANNsplitRule) 3;
  ANNshrinkRule shrinkRule = (ANNshrinkRule) 0;
  
  /**************************
    ref and tree structure
  ***************************/
  ANNpointArray dataPts;
  ANNkd_tree* kdTree = NULL;    
  
  dataPts = annAllocPts(refDim1, refDim2);
  
  //read in pts 
  for(i = 0; i < refDim1; i++){
    for(j = 0; j < refDim2; j++){
      (dataPts[i])[j] = ref[j*refDim1+i];
    }
  }
  
  kdTree = new ANNkd_tree(dataPts, refDim1, refDim2, bucketSize, splitRule);
  
  /**************************
    target and search
  ***************************/
  ANNpoint queryPt;
  queryPt = annAllocPt(refDim2);
  
  ANNidxArray nnIdx = new ANNidx[k];
  ANNdistArray dists = new ANNdist[k];
  
  for(i = 0; i < tarDim1; i++){
    
    //mk query pt
    for(j = 0; j < tarDim2; j++){
      queryPt[j] = target[j*tarDim1+i];
    }
       
    kdTree->annkSearch(queryPt, k, nnIdx, dists, eps);
    
    //write nnIdx and dist to return mtrx
    for(j = 0; j < k; j++){
      knnIndxMtrx[j*tarDim1+i] = ++nnIdx[j];
      knnDistMtrx[j*tarDim1+i] = dists[j];
    }
  }
  
//    for(i=0; i<20; i++){
//      for(j = 0; j < k; j++){
//        cout<<knnIndxMtrx[j*tarDim1+i]<<"\t";
//      }
//      cout<<endl;
//    }
//    cout<<endl;

  /**************************
            clean-up
  ***************************/
  delete kdTree;
  delete [] nnIdx;
  delete [] dists;
  annDeallocPts(dataPts);
  annClose();
  
  return(0);
}
