#=
#' All edges of a graph
#' 
#' Finds the set of edges of a graph. That is the set of undirected edges if
#' the graph is undirected and the set of arrows if the graph is directed.
#' 
#' 
#' @param amat a square Boolean matrix, with dimnames, the adjacency matrix of
#' a graph.
#' @return a matrix with two columns. Each row of the matrix is a pair of
#' indices indicating an edge of the graph. If the graph is undirected, then
#' only one of the pairs \eqn{(i,j), (j,i)} is reported.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{cycleMatrix}}
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## A UG graph
#' allEdges(UG(~ y*v*k +v*k*d+y*d))
#' 
#' ## A DAG
#' allEdges(DAG(u~h+o+p, h~o, o~p))
#' 
"allEdges" <-
function(amat){
### Finds all the edges of a graph with edge matrix amat.
    nn <- 1:nrow(amat)
    E <- c()
    if(all(amat == t(amat))) { 
      amat[lower.tri(amat)] <- 0
    }
    for(i in nn) {
      e <- nn[amat[i,] == 1]
      if(length(e) == 0) next
      li <- cbind(i,  e)
      dimnames(li) <- list(rep("", length(e)), rep("", 2))
      E <- rbind(E, li) 
    }
    E
  }
=#


