#=
#' Finding paths
#' 
#' Finds one path between two nodes of a graph.
#' 
#' 
#' @param amat a square Boolean matrix with dimnames, the adjacency matrix of a
#' graph.
#' @param st an integer, the starting node.
#' @param en an integer, the ending node.
#' @param path a vector of integers, used in recursive calls. At the beginning
#' is \code{NULL}. It should not be modified by the user.
#' @return a vector of integers, the sequence of nodes of a path, starting from
#' \code{st} to \code{en}. In some graphs (spanning trees) there is only one
#' path between two nodes.
#' @note This function is not intended to be directly called by the user.
#' @author Giovanni M. Marchetti, translating the original \pkg{Python} code
#' (see references).
#' @seealso \code{\link{fundCycles}}
#' @references Python Softftware Foundation (2003). Python Patterns ---
#' Implementing Graphs. \url{http://www.python.org/doc/essays/graphs/}.
#' @keywords graphs
#' @examples
#' 
#' ## A (single) path on a spanning tree
#' findPath(bfsearch(UG(~ a*b*c + b*d + d*e+ e*c))$tree, st=1, en=5)
#' 
"findPath" <-
function (amat, st, en, path = c()) 
{
### Find a path between nodes st and en in a UG with adjacency mat. amat.
  indices <- 1:nrow(amat)
  if(st == en) # st is 'node' in recursive calls
    return(c(path, st))
  if(sum(amat[st,]) == 0 ) 
    return(NULL)
  ## ne <- bd(st,amat)
  ne <- indices[amat[st,]==1] # Boundary of x. Assumes that amat is symmetric
  for(node in ne){
    if(!is.element(node, c(path, st))){
      newpath <- findPath(amat, node, en, c(path, st))
      if(!is.null(newpath))
        return(newpath)
    }
  }
}
=#



