#=
#' Graph queries
#' 
#' Checks if a given graph is acyclic.
#' 
#' 
#' @param amat a square Boolean matrix with dimnames, the adjacency matrix of a
#' graph.
#' @param method an integer 1 or 2 specifying the method used. If
#' \code{method=1} the function calls the function \code{clusters} in package
#' \code{igraph} to find the strong components: two nodes v and w are in the
#' same strong component iff there are directed paths from v to w and from w to
#' v. If \code{method=2} the function uses the \code{ggm} function
#' \code{transClos}. Method 1 is faster.
#' @return a logical value, \code{TRUE} if the graph is acyclic and
#' \code{FALSE} otherwise.
#' @author David Edwards, Giovanni M. Marchetti
#' @references Aho, A.V., Hopcroft, J.E. \& Ullman, J.D. (1983). \emph{Data
#' structures and algorithms.} Reading: Addison-Wesley.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## A cyclic graph
#' d <- matrix(0,3,3)
#' rownames(d) <- colnames(d) <- c("x", "y", "z")
#' d["x","y"] <- d["y", "z"] <- d["z", "x"] <- 1
#' ## Test if the graph is acyclic
#' isAcyclic(d)
#' isAcyclic(d, method = 1)
#' 
`isAcyclic` <-
function (amat, method = 2) 
{
### Tests if the graph is acyclic.
  if(method ==1){
    G <- graph.adjacency(amat)
    return(max(clusters(G, mode = "strong")$csize) == 1)
  }
  else if(method ==2){
  B <- transClos(amat)
  l <- B[lower.tri(B)]
  u <- t(B)[lower.tri(t(B))]
  com <- (l&u)
  return(all(!com))
  }
  else{
    stop("Wrong method.")
  }
}
=#


