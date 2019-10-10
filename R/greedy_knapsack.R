#' greedy_knapsack
#' 
#' Given data frame \code{x}, max weight \code{W}, give the approximate solution
#' 
#' @param x, a data frame of items with different values and weights
#' @param W, the capacity of knapsack
#' @return a list with approximate solution with value and selected elements 
#' @examples
#' set.seed(42)
#' n<- 1000000
#' knapsack_objects <-data.frame(
#'  w=sample(1:4000, size = n, replace = TRUE),
#'  v=runif(n = n, 0, 10000))
#' greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' 

greedy_knapsack<-function(x, W){
  if(!is.data.frame(x)) stop("First argumet should be a data frame!")
  else
  {if(!(grep("w",names(x))&grep("v",names(x)))) stop("There should be columns 'w' and 'v' in the data frame!")
    if(length(grep(TRUE,x<0))>0) stop("There is negative value in data frame!")}
  if(!((is.numeric(W))&(W>0))) stop("The second argument should be positive number!")
  
  new<-cbind(x,p=x$v/x$w)
  xu<-order(new$p,decreasing=TRUE)
  zhi=0;yuan<-NULL;zhong<-0
  len<-length(xu)
  for(i in 1:len)
    {te<-zhong+new$w[xu[i]]
    if(te<W) {zhi<-zhi+new$v[xu[i]];yuan<-c(yuan,xu[i]);zhong<-te}}
  
  return (list(value=zhi,elements=yuan))  
}