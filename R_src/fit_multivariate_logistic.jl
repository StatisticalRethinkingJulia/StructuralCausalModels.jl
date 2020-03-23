#=
## Fit multivariate logistic model with individual covariates
#############################################################


#' Inverts a marginal log-linear parametrization
#' 
#' Inverts a marginal log-linear parametrization.
#' 
#' A marginal log-linear link is defined by \eqn{\eta = C (M \log p)}. See
#' Bartolucci et al. (2007).
#' 
#' @param eta a vector of dimension \code{t-1} where \code{t} is the number of
#' cells of a contingency table.
#' @param C A contrast matrix.
#' @param M A marginalization matrix.
#' @param G G is the model matrix of the loglinear parameterization with no
#' constant term.
#' @param maxit an integer, specifying the maximum number of iterations.
#' Default 500.
#' @param print a logical value: if \code{TRUE}, prints the criterion after
#' each cycle.
#' @param tol A small value specifying the tolerance for the convergence
#' criterion. Default: \code{1e-10}.
#' @return A vector of probabilities \code{p}.
#' @note From a Matlab function by A. Forcina, University of Perugia, Italy.
#' @author Antonio Forcina, Giovanni M. Marchetti
#' @seealso \code{\link{mat.mlogit}}
#' @references Bartolucci, F., Colombi, R. and Forcina, A. (2007). An extended
#' class of marginal link functions for modelling contingency tables by
#' equality and inequality constraints. Statist. Sinica 17, 691-711.
#' @keywords marginal log-linear models discrete data
`binve`  <- function(eta, C, M, G, maxit=500, print=FALSE, tol = 1e-10){
# Inverts a marginal loglinear parameterization.
# eta  has dimension t-1, 
# G is the model matrix of the loglinear parameterization with no intercept.
# C and M are the matrices of the link. 
# From a Matlab function by A. Forcina, University of Perugia, Italy.
    
                                                           
    ## starting values

    k <- nrow(C)
    pmin <- 1e-100
    kG  <-  ncol(G)
    err <- 0
    th0  <-  matrix(1, k, 1) / 100

    ## prepare to iterate
    it <- 0
    mit <- 500          
    p <- exp(G %*% th0)
    p <-  p/sum(p)
    t <- M %*% p        
    d <- eta - C %*% log(t)
    div0 <- crossprod(d)
    hh <- 0
    s <- c(.1, .6, 1.2)
    div <- div0 + 1            
    while((it < maxit) & (div > tol)){
        R0 <- C %*% diagv(1/t, M) %*% diagv(p,G)
        rco <- rcond(R0) > tol
        ub <- 0
        while(rco==FALSE){
            cat("Rank: ", qr(R0)$rank, "\n")  
            R0 <- R0 + diag(k)
            rco <- rcond(R0) > tol
        }
        de  <-  solve(R0) %*% d
        dem  <-  max(abs(de))
        de  <-  (dem <= 4)*de + (dem>4)*4*de/dem
        th <- th0 + de
        p <- exp(G %*% th)
        p <- p/sum(p)
        p <- p+pmin*(p<pmin)
        p <- p/sum(p)
        t <- M %*% p
        d <- eta-C %*% log(t)
        div <- crossprod(d)
        rid <- (.01+div0)/(.01+div)
        iw <- 0
        while( (rid <0.96) & (iw<10)){
            th  <- th0 + de  %*% (rid^(iw+1))
            p <- exp(G %*% th)
            p <- p/sum(p)
            p <- p + pmin*(p<pmin)
            p <- p/sum(p)
            t <- M %*% p
            d <- (eta-C %*% log(M %*% p))
            div <- crossprod(d)
            rid <- (.01 + div)/(.01 + div0)
            iw <- iw+1;  it <- it+1
        }
        if( rid < 0.96){
            it <- mit
        }
        else{
            it <- it+1
            th0 <- th
        }
    }
   if (div > tol) {
        warning("div > tolerance.", call. =FALSE)
    }
    if (any(is.nan(p))){
        warning("Some NaN in the vector of probabilities.", call. =FALSE)
    }
    if (it > maxit){
        warning("Maximum number of iteration reached.", call. = FALSE)
    }
    if(print){
       cat("Iterations: ", it, ", Div = ", div, ".\n")
    }
    as.vector(p)
}       
=#
             

