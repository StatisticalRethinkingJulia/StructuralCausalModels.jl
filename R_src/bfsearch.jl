#=
#' Breadth first search
#' 
#' Breadth-first search of a connected undirected graph.
#' 
#' Breadth-first search is a systematic method for exploring a graph.  The
#' algorithm is taken from Aho, Hopcroft \& Ullman (1983).
#' 
#' @param amat a symmetric matrix with dimnames specifying the adjacency matrix
#' of the undirected graph
#' @param v an integer, indicating the starting node of the search. Defaults to
#' the first node.
#' @return \item{tree}{the edge matrix of the resulting spanning tree}
#' \item{branches}{a matrix with two columns, giving the indices of the
#' branches of the spanning tree} \item{chords}{a matrix with two columns,
#' giving the indices of the chords of the spanning tree}
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{UG}}, \code{\link{findPath}}, \code{\link{cycleMatrix}}
#' @references Aho, A.V., Hopcrtoft, J.E. \& Ullman, J.D. (1983). \emph{Data
#' structures and algorithms.} Reading: Addison-Wesley.
#' 
#' Thulasiraman, K. \& Swamy, M.N.S. (1992). \emph{Graphs: theory and
#' algorithms}. New York: Wiley.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## Finding a spanning tree of the butterfly graph
#' bfsearch(UG(~ a*b*o + o*u*j))
#' ## Starting from another node
#' bfsearch(UG(~ a*b*o + o*u*j), v=3)
#' 
"bfsearch" <-
function(amat, v=1) {
### Breadth-first search of a connected UG with adjacency matrix amat.
    n <- nrow(amat)
    indices <- 1:n
    if(n==1) return(NULL)
    visited <- rep(0, n)
    Q <- c()
    tree <- matrix(0, n,n)
    dimnames(tree) <- dimnames(amat)
    E <- c()
    visited[v] <- 1
    Q <- c(v, Q)
    while(!all(visited==1)){
      x <- Q[1]
      Q <- Q[-1]
     ## b <- bd(x, amat)
      b <- indices[amat[x,]==1] # Boundary of x. Assumes that amat is symmetric
      for(y in b){
        if(visited[y] == 0){
          visited[y] <- 1
          Q <- c(Q, y)
          tree[x,y]<- 1 ; tree[y,x] <- 1
          E <- rbind(E, c(x,y))
        }
      }
    }
    cross <- amat - tree
    V <- allEdges(cross)
    dimnames(E) <- list(rep("", nrow(E)), rep("", 2))
    list(tree = tree, branches = E,chords = V ) 
  }
=#


