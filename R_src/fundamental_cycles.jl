#=
#' Fundamental cycles
#' 
#' Finds the list of fundamental cycles of a connected undirected graph.
#' 
#' All the cycles in an UG can be obtained from combination (ring sum) of the
#' set of fundamental cycles.
#' 
#' @param amat a symmetric matrix with dimnames denoting the adjacency matrix
#' of the undirected graph. The graph must be connected, otherwise the function
#' returns an error message.
#' @return a list of matrices with two columns. Every component of the list is
#' associated to a cycle. The cycle is described by a \eqn{k \times 2} matrix
#' whose rows are the edges of the cycle. If there is no cycle the function
#' returns \code{NULL}.
#' @note This function is used by \code{cycleMatrix} and \code{isGident}.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{UG}},\code{\link{findPath}}, \code{\link{cycleMatrix}},
#' \code{\link{isGident}},\code{\link{bfsearch}}
#' @references Thulasiraman, K. \& Swamy, M.N.S. (1992). \emph{Graphs: theory
#' and algorithms}. New York: Wiley.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## Three fundamental cycles
#' fundCycles(UG(~a*b*d + d*e + e*a*f))
#' 
"fundCycles" <-
function(amat){
### Finds a set of fundamental cycles for an UG with adj. matrix amat.
    fc <- c()
    tr <- bfsearch(amat) # Spanning tree
    if(is.null(tr)) return(NULL)
    if(is.null(tr$chords)) return(NULL)
    co <- tr$chords # edges of the cospanning tree
    for(i in 1:nrow(co)) {
      e <- co[i,] 
      g <- tr$tree # edge matrix of the spanning tree
      cy <- findPath(g, st=e[1], en=e[2])
       splitCycle <- function(v){
         ## Splits a cycle v into a matrix of edges.
         v <- c(v, v[1])
         cbind(v[-length(v)], v[-1])
       }
      cy <- splitCycle(cy)
      fc <- c(fc, list(cy))
    }
    fc 
  }
=#


