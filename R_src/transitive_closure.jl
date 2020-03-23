#=
#' Transitive closure of a graph
#' 
#' Computes the transitive closure of a graph (undirected or directed acyclic).
#' 
#' The transitive closure of a directed graph with adjacency matrix \eqn{A} is
#' a graph with adjacency matrix \eqn{A^*} such that \eqn{A^*_{i,j} = 1} if
#' there is a directed path from \eqn{i} to \eqn{j}. The transitive closure of
#' an undirected graph is defined similarly (by substituting path to directed
#' path).
#' 
#' @param amat a Boolean matrix with dimnames representing the adjacency matrix
#' of a graph.
#' @return \item{A}{The adjacency matrix of the transitive closure.}
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{DAG}}, \code{\link{UG}}
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## Closure of a DAG
#' d <- DAG(y ~ x, x ~ z)
#' transClos(d)
#' 
#' ## Closure of an UG
#' g <- UG(~ x*y*z+z*u+u*v)
#' transClos(g)
#' 
`transClos` <-
function (amat) 
{
### Transitive closure of the relation with adjacency matrix amat.
  if (nrow(amat) == 1) 
    return(amat)
  A <- amat
  diag(A) <- 1
  repeat {
    B <- sign(A %*% A)
    if (all(B == A))
      break
    else A <- B
  }
  diag(A) <- 0
  A
}
=#


