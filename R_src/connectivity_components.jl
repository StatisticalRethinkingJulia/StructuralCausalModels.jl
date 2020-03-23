#=
#' Connectivity components
#' 
#' Finds the connectivity components of a graph.
#' 
#' 
#' @param amat a square matrix with dimnames, the adjacency matrix of an UG.
#' @param method an integer 1 or 2 to choose the method used to find the
#' components. Method 2 is more efficient for large graphs.
#' @return an integer vector representing a partition of the set of nodes.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{UG}}
#' @references Lauritzen, S. (1996). \emph{Graphical models}. Oxford: Clarendon
#' Press.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## three connected components
#' conComp(UG(~a*c+c*d+e+g*o*u))
#' ## a connected graph
#' conComp(UG(~ a*b+b*c+c*d+d*a))
#' 
`conComp` <-  function (amat, method = 1) 
### Finds the connected components of an UG graph from its adjacency matrix amat. 
{
    if (!all(amat == t(amat))) 
       stop("Not an undirected graph.")
  if(method == 2){
      u <- clusters(graph.adjacency(amat, mode="undirected"))$membership + 1
      return(u)
    }
    else if (method == 1){
      A <- transClos(amat)
      diag(A) <- 1
      n <- nrow(A)
      A <- sign(A + t(A))
      u <- A %*% 2^((n - 1):0)
    return(match(u, unique(u)))
  }
  else{ stop("Wrong method.")}
}
=#


