#=
#' Fitting of Gaussian DAG models
#' 
#' Fits linear recursive regressions with independent residuals specified by a
#' DAG.
#' 
#' \code{fitDag} checks if the order of the nodes in adjacency matrix is the
#' same of \code{S} and if not it reorders the adjacency matrix to match the
#' order of the variables in \code{S}. The nodes of the adjacency matrix may
#' form a subset of the variables in \code{S}.
#' 
#' @param amat a square matrix with dimnames representing the adjacency matrix
#' of the DAG
#' @param S a symmetric positive definite matrix, the sample covariance matrix
#' @param n an integer > 0, the sample size
#' @return \item{Shat}{the fitted covariance matrix.} \item{Ahat}{a square
#' matrix of the fitted regression coefficients. The entry \code{Ahat[i,j]} is
#' minus the regression coefficient of variable \code{i} in the regression
#' equation \code{j}. Thus there is a non zero partial regression coefficient
#' \code{Ahat[i,j]} corresponding to each non zero value \code{amat[j,i]} in
#' the adjacency matrix.} \item{Dhat}{a vector containing the partial variances
#' of each variable given the parents.} \item{dev}{the `deviance' of the
#' model.} \item{df}{the degrees of freedom.}
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{DAG}}, \code{\link{swp}}.
#' @references Cox, D. R. \& Wermuth, N. (1996). \emph{Multivariate
#' dependencies}. London: Chapman \& Hall.
#' @keywords graphs models multivariate
#' @examples
#' 
#' dag <- DAG(y ~ x+u, x ~ z, z ~ u)
#' "S" <- structure(c(2.93, -1.7, 0.76, -0.06,
#'                    -1.7, 1.64, -0.78, 0.1,
#'                     0.76, -0.78, 1.66, -0.78,
#'                     -0.06, 0.1, -0.78, 0.81), .Dim = c(4,4),
#'          .Dimnames = list(c("y", "x", "z", "u"), c("y", "x", "z", "u")))
#' fitDag(dag, S, 200)
#' 
"fitDag" <-
function (amat, S, n)
{
### Fits linear recursive regressions with independent residuals. 
### amat: the adjacency matrix of the DAG. S: cov matrix. n: sample size.
  if(missing(amat)){ # saturated model
    amat <-  lower.tri(diag(ncol(S)), diag=FALSE) * 1
    dimnames(amat) <- dimnames(S)
  }
  nam <- rownames(S)
  nod <- rownames(amat)
  if(is.null(nod))
    stop("The adjacency matrix has no labels.")
  if(!all(is.element(nod, nam)))
    stop("The nodes of the graph do not match the names of the variables.")
  else
    sek <- intersect(nam, nod) 
  S <- S[sek,sek]              # Resizes eventually S 
  amat <- amat[sek,sek]        # and reorders amat
  Delta <- rep(length(sek),0)
  
  emat <- edgematrix(amat) 
  A <- emat
  p <- ncol(S)
  ip <- 1:p
  for(i in 1:p) {
    u <- emat[i,]
    v <- ip[u == 1 & ip != i]
    M <- swp(S, v)[i,]
    A[i, ] <- - A[i, ] * M
    A[i,i] <- 1
    k <- sum(A[i,])
    Delta[i] <- M[i]
  }
  names(Delta) <- sek
  B <- solve(A)
  Shat <- B %*% diag(Delta) %*% t(B)
  dimnames(Shat) <- dimnames(S)
  Khat <- solve(Shat)
  H <- S %*% Khat
  trace <- function(A) sum(diag(A))
  dev <- (trace(H) - log(det(H)) - p) * n
  df <- p*(p+1)/2 - sum(amat==1) - p
  list(Shat=Shat, Ahat = A, Dhat=Delta, dev=dev, df=df)
}
=#


