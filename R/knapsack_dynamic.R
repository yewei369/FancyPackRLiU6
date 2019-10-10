#' knapsack_dynamic
#' 
#' Given data frame \code{x}, max weight \code{W}, give the best solution with dynamic programing of O(n*W)
#' 
#' @param x, a data frame of items with different values and weights
#' @param W, the capacity of knapsack
#' @return a list with best value and selected elements 
#' @examples
#' set.seed(42)
#' n<- 1000000
#' knapsack_objects <-data.frame(
#'  w=sample(1:4000, size = n, replace = TRUE),
#'  v=runif(n = n, 0, 10000))
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#' 

knapsack_dynamic<-function(x, W){
  if(!is.data.frame(x)) stop("First argumet should be a data frame!")
  else
  {if(!(grep("w",names(x))&grep("v",names(x)))) stop("There should be columns 'w' and 'v' in the data frame!")
    if(length(grep(TRUE,x<0))>0) stop("There is negative value in data frame!")}
  if(!((is.numeric(W))&(W>0))) stop("The second argument should be positive number!")

  n<-nrow(x)
  m<-matrix(0,n+1,W+1)
  
  for(i in 2:n+1)
    for(j in 1:W+1)
      {if(x$w[i-1]>j) m[i,j]<-m[i-1,j]
      else m[i,j]<-max(m[i,j],m[i-1,j-x$w[i-1]]+x$v[i-1])}
  
  zhi<-max(m[,W+1]);yuan<-NULL
  a<-n+1;b<-W+1
  while(a>2&b>2){
    if(m[a,b]>m[a-1,b]) {yuan<-c(yuan,a-1);b<-b-x$w[a-1]}
    a<-a-1}
  
  yuan<-rev(yuan)
  
  return (list(value=zhi,elements=yuan))
  
}