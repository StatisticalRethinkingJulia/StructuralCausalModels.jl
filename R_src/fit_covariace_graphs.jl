#=
#' Fitting of Gaussian covariance graph models
#' 
#' Fits a Gaussian covariance graph model by maximum likelihood.
#' 
#' A covariance graph is an undirected graph in which the variables associated
#' to two non-adjacent nodes are marginally independent. The edges of these
#' models are represented by bi-directed edges (Drton and Richardson, 2003) or
#' by dashed lines (Cox and Wermuth, 1996).
#' 
#' By default, this function gives the ML estimates in the covariance graph
#' model, by iterative conditional fitting (Drton and Richardson, 2003).
#' Otherwise, the estimates from a ``dual likelihood'' estimator can be
#' obtained (Kauermann, 1996; Edwards, 2000, section 7.4).
#' 
#' @param amat A symmetric Booloean matrix with dimnames representing the
#' adjacency matrix of an UG.
#' @param S A symmetric positive definite matrix with dimnames, the sample
#' covariance matrix.
#' @param n A positive integer, the sample size.
#' @param alg A character string, the algorithm used.  If \code{alg="icf"} (the
#' default) the algorithm is based on iterative conditional fitting (see Drton
#' and Richardson, 2003). In this case the ML estimates are returned.  If
#' \code{alg="dual"} the algorithm is based on the dual likelihood (see
#' Kauermann, 1996). The fitted values are an approximation of the ML
#' estimates.
#' @param dual.alg And integer equal to 1 or 2. It is used if
#' \code{alg="dual"}. In this case a concentration graph model is fitted to the
#' inverse of the sample covariance matrix, and \code{dual.alg} is passed to
#' \code{fitConGraph} to specify the algorithm used in \code{fitConGraph}.
#' @param start.icf A symmetric matrix used as starting value of the algorithm.
#' If \code{start=NULL} the starting value is a diagonal matrix with diagonal
#' entries equal to sample variances.
#' @param tol A small positive number indicating the tolerance used in
#' convergence tests.
#' @return \item{Shat}{the fitted covariance matrix.} \item{dev}{the `deviance'
#' of the model.} \item{df}{the degrees of freedom.} \item{it}{the iterations.}
#' @author Mathias Drton
#' @seealso \code{\link{fitConGraph}}, \code{\link{icf}}
#' @references Cox, D. R. and Wermuth, N. (1996). \emph{Multivariate
#' dependencies}. London: Chapman \& Hall.
#' 
#' Drton, M. and Richardson, T. S. (2003). A new algorithm for maximum
#' likelihood estimation in Gaussian graphical models for marginal
#' independence. \emph{Proceedings of the Nineteenth Conference on Uncertainty
#' in Artificial Intelligence}, 184--191.
#' 
#' Kauermann, G. (1996). On a dualization of graphical Gaussian models.
#' \emph{Scandinavian Journal of Statistics}.  23, 105--116.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## Correlations among four strategies to cope with stress for 
#' ## 72 students. Cox & Wermuth (1996), p. 73.
#' 
#' data(stress)
#' 
#' ## A chordless 4-cycle covariance graph
#' G <- UG(~ Y*X + X*U + U*V + V*Y)
#' 
#' fitCovGraph(G, S = stress, n=72)
#' fitCovGraph(G, S = stress, n=72, alg="dual")
#' 
`fitCovGraph` <-
  function (amat, S, n, alg="icf", dual.alg=2, start.icf=NULL, tol = 1e-06){
### Fits a Covariance Graph. Mathias Drton, 2003
### amat: adjacency matrix; S: covariance matrix; n: sample size.
    amat <- In(amat) # Forces the ones in a bidirected graph defined with makeMG
    nam <- rownames(S)
    nod <- rownames(amat)
    if(is.null(nod)){
      stop("The adjacency matrix has no labels.")
    }
    if(!all(is.element(nod, nam)))
      stop("The nodes of the graph do not match the names of the variables.")
    else
      sek <- intersect(nam, nod)
    S <- S[sek,sek, drop=FALSE]              # Resizes eventually S
    amat <- amat[sek,sek, drop=FALSE]        # and reorders amat
    
    if(alg=="icf"){
      temp <- icf(amat, S, start.icf, tol)
    }
    else{
      if(alg == "dual"){
        Sinv <- solve(S)
        temp <- fitConGraph(amat, Sinv, n, pri=FALSE, alg=dual.alg, tol = 1e-06)
        temp <- list(Sigmahat=zapsmall(solve(temp$Shat)), iterations=temp$it)
      }
      else{
        stop("Algorithm misspecified!")
      }
    }

    df <- sum(amat[upper.tri(amat)] == 0) # Degrees of freedom
    k <- ncol(S)
    dev <- likGau(solve(temp$Sigmahat), S, n, k) 
    return(list(Shat=temp$Sigmahat, dev = dev, df = df, it=temp$iterations))
}
=#


