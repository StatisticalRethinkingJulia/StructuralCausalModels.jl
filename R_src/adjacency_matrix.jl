#=
#' Adjacency matrix of a graph
#' 
#' Transforms the ``edge matrix'' of a graph into the adjacency matrix.
#' 
#' Given the edge matrix \eqn{A} of a graph, this can be transformed into an
#' adjacency matrix \eqn{E} with the formula \eqn{E = (A-I)^T}.
#' 
#' @param A a square matrix representing the edge matrix of a graph.
#' @return \item{E}{the adjacency matrix of the graph.}
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{edgematrix}}
#' @keywords array algebra graphs multivariate
#' @examples
#' 
#' amat <- DAG(y ~ x+z, z~u+v)
#' E <- edgematrix(amat)
#' adjMatrix(E)
#' 
"adjMatrix" <-
function (A) 
{
### From the edge matrix to the adjacency matrix
  E <- t(A)
  diag(E) <- 0
  E
}
=#


