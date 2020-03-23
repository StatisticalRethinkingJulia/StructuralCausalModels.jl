#=
#' Link function of marginal log-linear parameterization
#' 
#' Provides the contrast and marginalization matrices for the marginal
#' parametrization of a probability vector.
#' 
#' See Bartolucci, Colombi and Forcina (2007).
#' 
#' @param lev Integer vector containing the number of levels of each variable.
#' @param type A character vector with elements \code{"l"}, \code{"g"},
#' \code{"c"}, or \code{"r"} indicating the type of logit. The meaning is as
#' follows: \code{"g"} for global, \code{"c"} for continuation, \code{"r"} for
#' reverse continuation and \code{"l"} for local.
#' @return \item{C}{Matrix of constrasts (the first \code{sum(lev)-length(r)}
#' elements are referred to univariate logits)} \item{M}{Marginalization matrix
#' with elements 0 and 1.} \item{G}{Corresponding design matrix for the
#' corresponding log-linear model.}
#' @note Assumes that the vector of probabilities is in inv lex order.  The
#' interactions are returned in order of dimension, like e.g., 1, 2, 3, 12, 13,
#' 23, 123.
#' @author Francesco Bartolucci, Antonio Forcina, Giovanni M. Marchetti
#' @seealso \code{\link{mat.mlogit}}
#' @references Bartolucci, F., Colombi, R. and Forcina, A. (2007). An extended
#' class of marginal link functions for modelling contingency tables by
#' equality and inequality constraints. Statist. Sinica 17, 691-711.
#' @keywords logistic models ordinal models
#' @examples
#'     
#' marg.param(c(3,3), c("l", "g"))
#' 
`marg.param` = function(lev,type) 
# Creates matrices C and M for the marginal parametrization
# of the probability vector for a vector of categorical variables.
# INPUT:
# lev:  vector containing the number of levels of each variable
# type: vector with elements 'l', 'g', 'c', 'r' indicating the type of logit
#       'g' for global, 
#       'c' for continuation,
#       'r' for reverse continuation, 
#       'l' for local.
# OUTPUT:
# C:    matrix of constrats (the first sum(lev)-length(r) elements are
#       referred to univariate logits)
# M:    marginalization matrix with elements 0 and 1
# G:    corresponding design matrix for the corresponding log-linear model   
# Translated from a Matlab function by Bartolucci and Forcina.
# NOTE: assumes that the vector of probabilities is in inv lex order. 
#       The interactions are returned in order of dimension, like e.g.,  1 2 3 12 13 23 123. 
{
# preliminaries

`powset` <- function(d)
# Power set P(d).
{      
  P = expand.grid(rep(list(1:2), d))
  P[order(apply(P, 1, sum)),]-1
}

  r = length(lev)
# create sets of parameters
 S = powset(r)
 S = S[-1,]  # drop the empty set
  C = c(); M = c(); G = c()
  for (i in 1:nrow(S)){
    si = S[i,] 
    Ci = 1  # to update matrix C
    for (h in 1:r){
      if(si[h]==1){
        I = diag(lev[h] - 1)
        Ci = cbind(-I, I) %x% Ci 
      }
    }
    C = blkdiag(C, Ci)
    Mi = 1  # to update matrix M
    for (h in 1:r) { 
    lh = lev[h]-1 
      if(si[h]==1) {
        I = diag(lh)   
        T = 0 + lower.tri(matrix(1, lh,lh), diag=TRUE)
        ze = matrix(0, lh, 1)
        Mi = switch(type[h], 
          l = rbind(cbind(I, ze), cbind(ze, I)) %x%  Mi,
          g = rbind(cbind(T, ze), cbind(ze, t(T))) %x%  Mi,
          c = rbind(cbind(I, ze), cbind(ze, t(T))) %x%  Mi,
          r = rbind(cbind(T, ze), cbind(ze, I)) %x%  Mi)
      }        
      else {
        Mi = matrix(1, 1,lev[h])  %x%  Mi
      } 
    }    
    M = rbind(M, Mi)  
    
    Gi = 1  # for the design matrix
    for (h in 1:r) { 
    lh = lev[h] 
      if(si[h]==1) {                          
       T = 0 + lower.tri(matrix(1, lh,lh), diag=TRUE); T = T[,-1]  
        Gi = T %x% Gi   
      }
      else{
        Gi =  matrix(1, lh, 1) %x% Gi
      }
    }
    G = cbind(G, Gi)
  }   
list(C = C, M = M, G = G)
}
=#


