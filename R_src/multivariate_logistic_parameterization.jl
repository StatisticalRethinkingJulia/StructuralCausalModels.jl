#=
#' Multivariate logistic parametrization
#' 
#' Find matrices \code{C} and \code{M} of e binary multivariate logistic
#' parameterization.
#' 
#' The power set is in the order of dimensions of the sets.
#' 
#' @param d A positive integer, the number of binary responses.
#' @param P A list of vectors of integers specifying margins. For instance
#' \code{list(1, 2, c(1,2))}. Default: the power set of \code{1:d}.
#' @return \item{C}{A contrast matrix.} \item{L}{A marginalization matrix.}
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{binomial}, \link{marg.param}}
#' @references Glonek, G. J. N. and McCullagh, P. (1995). Multivariate logistic
#' models. Journal of the Royal Statistical Society, Ser. B 57, 533-546.
#' @keywords logistic model
#' @examples
#'  
#' mat.mlogit(2)
#' 
`mat.mlogit` <- function(d, P = powerset(1:d)) {
## Find matrices C and M of binary mlogit parameterization  for a table 2^d. 
## The output will be in the ordering of P.
## Here for 3 variables is: 1 2 3 12 13 23 123.  

`margmat` <- function(bi, mar){
### Defines the marginalization matrix
    if(mar ==  FALSE){
      matrix(1, 1, bi)
    }
    else {
      diag(bi)
    }
  }

`contrmat` <- function(bi, i){
### Contrast matrix
    if(i == FALSE){
      1
    }
    else{
      cbind(-1, diag(bi-1))
    }
  }
  V <- 1:d

  C <- matrix(0,0,0)
  L <- c()

  for(mar in P){
    K <- 1
    H <- 1
    for(i in V){
      w <- is.element(i, mar)
      K <- contrmat(2, w) %x% K
      H <- margmat(2, w)  %x% H
    }
    C <- blkdiag(C, K)
    L <- rbind(L, H)
  }
  list(C=C, L=L)
}   
=#


