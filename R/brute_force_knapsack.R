#' brute_force_knapsack
#' 
#' Given data frame \code{x}, max weight \code{W}, give the best solution with brutal force of O(2^n)
#' 
#' @param x, a data frame of items with different values and weights
#' @param W, the capacity of knapsack
#' @param fast, logical signal when Rcpp function activated
#' @return a list with best value and selected elements 
#' @examples
#' set.seed(42)
#' n<- 2000
#' knapsack_objects <-data.frame(
#'  w=sample(1:4000, size = n, replace = TRUE),
#'  v=runif(n = n, 0, 10000))
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' 

brute_force_knapsack<-function(x, W,fast=FALSE){
if(!is.data.frame(x)) stop("First argumet should be a data frame!")
else
  {if(!(grep("w",names(x))&grep("v",names(x)))) stop("There should be columns 'w' and 'v' in the data frame!")
   if(length(grep(TRUE,x<0))>0) stop("There is negative value in data frame!")}
if(!((is.numeric(W))&(W>0))) stop("The second argument should be positive number!")
  
  library(binaryLogic)
  library(Rcpp)
  n<-nrow(x)
  zhi<-0;yuan<-NULL
  cppFunction('int D2B(int deci, int nuo) {
                              int test,re;
                              test=deci>>nuo;
                              if (test&1) re=1;
                              else re=0;
                              return re;
                              }')
                              
  
  len<-2^n-1
  for(i in 1:len)
  { if(fast==TRUE) {te<-rep(0,n)
                   for(j in n:1) te[n-j+1]=D2B(i,j-1)
                   te<-as.logical(te)}
    else te<-as.logical(as.binary(i,n=n))
  
   if(sum(x$w[which(te==TRUE)])>W) next
   else if(sum(x$v[which(te==TRUE)])>zhi)
    {zhi<-sum(x$v[which(te==TRUE)])
     yuan<-which(te==TRUE)}}
   
  return (list(value=zhi,elements=yuan))
}