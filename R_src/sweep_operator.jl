#=
#' Sweep operator
#' 
#' Sweeps a covariance matrix with respect to a subset of indices.
#' 
#' The sweep operator has been introduced by Beaton (1964) as a tool for
#' inverting symmetric matrices (see Dempster, 1969).
#' 
#' @param V a symmetric positive definite matrix, the covariance matrix.
#' @param b a subset of indices of the columns of \code{V}.
#' @return a square matrix \code{U} of the same order as \code{V}. If \code{a}
#' is the complement of \code{b}, then \code{U[a,b]} is the matrix of
#' regression coefficients of \code{a} given \code{b} and \code{U[a,a]} is the
#' corresponding covariance matrix of the residuals.
#' 
#' If \code{b} is empty the function returns \code{V}.
#' 
#' If \code{b} is the vector \code{1:nrow(V)} (or its permutation) then the
#' function returns the opposite of the inverse of \code{V}.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{fitDag}}
#' @references Beaton, A.E. (1964). \emph{The use of special matrix operators
#' in statistical calculus}. Ed.D. thesis, Harvard University. Reprinted as
#' Educational Testing Service Research Bulletin 64-51. Princeton.
#' 
#' Dempster, A.P. (1969). \emph{Elements of continuous multivariate analysis}.
#' Reading: Addison-Wesley.
#' @keywords array algebra models multivariate
#' @examples
#' 
#' ## A very simple example
#' V <- matrix(c(10, 1, 1, 2), 2, 2)
#' swp(V, 2)
#' 
"swp" <-
function (V, b) 
{
### SWP operator. V is the covariance matrix, b  is a  subset of indices.
  p <- ncol(V)
  u <- is.na(match(1:p, b))
  a <- (1:p)[u]
  out <- 0 * V
  dimnames(out) <- dimnames(V)
  if (length(a) == 0) 
    return(-solve(V))
  else if (length(a) == p) 
    return(V)
  else{
    Saa <- V[a, a, drop = FALSE]
    Sab <- V[a, b, drop = FALSE]
    Sbb <- V[b, b, drop = FALSE]
    B <- Sab %*% solve(Sbb)
    out[a, a] <- Saa - B %*% t(Sab)
    out[a, b] <- B
    out[b, a] <- t(B)
    out[b, b] <- -solve(Sbb)
    return(out)
  }
  ## list(swept = out, coef = out[a, b], rss = out[a, a, drop = F])
}

"topOrder" <-
function (amat) 
{
### Return the nodes in topological order (parents before children).
### Translated from: Kevin Murphy's BNT.
  if(!isAcyclic(amat)) stop("The graph is not acyclic!")
  n <- nrow(amat)
  nod <- 1:n
  indeg <- rep(0, n)
  up <- !amat[lower.tri(amat)]
  if(all(up))
    return(nod)
  zero.indeg <- c() #  a stack of nodes with no parents
  for(i in nod) {
    indeg[i] <- sum(amat[,i])
    if(indeg[i] == 0)
      zero.indeg <- c(i,  zero.indeg)
  }
  s <- 1
  ord <- rep(0, n)
  while(length(zero.indeg) > 0){
    v <- zero.indeg[1]  #  pop v
    zero.indeg <- zero.indeg[-1]
    ord[s] <- v
    s <- s + 1
    cs <- nod[amat[v,]==1]
    if(length(cs) == 0) next
    for(j in 1:length(cs)){
      k <- cs[j]
      indeg[k] <- indeg[k] - 1
      if(indeg[k] == 0)
        zero.indeg <- c(k,  zero.indeg) # push k    
    }
  }
  ord
}
=#


