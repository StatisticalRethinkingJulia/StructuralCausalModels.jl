#=
#' Fundamental cycles
#' 
#' Finds the matrix of fundamental cycles of a connected undirected graph.
#' 
#' All the cycles in an UG can be obtained from combination (ring sum) of the
#' set of fundamental cycles. The matrix of fundamental cycles is a Boolean
#' matrix having as rows the fundamental cycles and as columns the edges of the
#' graph. If an entry is one then the edge associated to that column belongs to
#' the cycle associated to the row.
#' 
#' @param amat a symmetric matrix with dimnames denoting the adjacency matrix
#' of the undirected graph. The graph must be connected, otherwise the function
#' returns an error message.
#' @return a Boolean matrix of the fundamental cycles of the undirected graph.
#' If there is no cycle the function returns \code{NULL}.
#' @note This function is used by \code{isGident}. The row sum of the matrix
#' gives the length of the cycles.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{UG}}, \code{\link{findPath}}, \code{\link{fundCycles}},
#' \code{\link{isGident}}, \code{\link{bfsearch}}
#' @references Thulasiraman, K. \& Swamy, M.N.S. (1992). \emph{Graphs: theory
#' and algorithms}. New York: Wiley.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## Three cycles
#' cycleMatrix(UG(~a*b*d+d*e+e*a*f))
#' ## No cycle
#'  cycleMatrix(UG(~a*b))
#' ## two cycles: the first is even and the second is odd
#' cm <- cycleMatrix(UG(~a*b+b*c+c*d+d*a+a*u*v))
#' apply(cm, 1, sum)
#' 
"cycleMatrix" <-
function(amat){
### Fundamental Cycle matrix of the UG amat.
    fc <- fundCycles(amat)  # Edges of the fundamental cycles
    E <- allEdges(amat)     # All the edges of the graph
    n <- nrow(E)            # Number of edges
    k <- length(fc)         # Number of FC
    if(k == 0) return(NULL)
    cmat <- matrix(0, k, n)
    for(cy in 1:k) {
      M <- fc[[cy]]         # Edges in cycle cy
      for(j in 1:nrow(M)) {
        e <- sort(M[j,])   
        for(i in 1:n){          
          cmat[cy, i] <- cmat[cy, i] | all(E[i,] == e)
        }
      }
    }
    dimnames(cmat) <- list(1:k, paste(E[,1], E[,2]))
    cmat       
  }
=#


