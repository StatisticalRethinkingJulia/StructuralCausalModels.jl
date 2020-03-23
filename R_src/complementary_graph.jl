#=
#' The complementary graph
#' 
#' Finds the complementary graph of an undirected graph.
#' 
#' The complementary graph of an UG is the graph that has the same set of nodes
#' and an undirected edge connecting \eqn{i} and \eqn{j} whenever there is not
#' an \eqn{(i,j)} edge in the original UG.
#' 
#' @param amat the adjacency matrix of an undirected graph
#' @return the edge matrix of the complementary graph.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{UG}}, \code{\link{DAG}}
#' @references Lauritzen, S. (1996). \emph{Graphical models}. Oxford: Clarendon
#' Press.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## A chordless four-cycle
#' four <- UG(~ a*b + b*d + d*e + e*a)
#' four
#' cmpGraph(four)
#' 
"cmpGraph" <-
function(amat){
### Adjacency matrix of the complementary graph
    g <- 1*!amat
    diag(g) <- 0
    g
  }
=#


