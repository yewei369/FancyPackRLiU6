\name{FancyPackRLiU6-package}
\alias{FancyPackRLiU6-package}
\alias{FancyPackRLiU6}
\docType{package}
\title{
\packageTitle{FancyPackRLiU6}
}
\description{
\packageDescription{FancyPackRLiU6}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{FancyPackRLiU6}
\packageIndices{FancyPackRLiU6}
The package presents three typical ways of resolving Knapsack problem, with brutal force, dynamic programing and approximate solutions. By means of exploring different computing complexities, a better understanding of fast code has been reached.
brute_force_knapsack(x,W)
knapsack_dynamic(x,W)
greedy_knapsack(x,W)
}
\author{
\packageAuthor{FancyPackRLiU6}

Maintainer: \packageMaintainer{FancyPackRLiU6}
}
\references{
https://en.wikipedia.org/wiki/Knapsack_problem
}

\keyword{ package }
\seealso{


}
\examples{
set.seed(42)
n<- 2000
knapsack_objects<-data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000))
system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500))[2]
system.time(knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500))[2]

}
