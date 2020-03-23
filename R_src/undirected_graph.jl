#=
#' Defining an undirected graph (UG)
#' 
#' A simple way to define an undirected graph by means of a single model
#' formula.
#' 
#' The undirected graph \eqn{G = (V, E)} is defined by a set of nodes \eqn{V}
#' and a set of pairs \eqn{E}. The set of pairs is defined by the set of
#' interactions in the formula. Interactions define complete subgraphs (not
#' necessarily maximal) of the UG.  The best way is to specify interactions
#' that match the cliques of the undirected graph. This is the standard way to
#' define graphical models for contingency tables. Remember that some
#' hierarchical models are not graphical, but they imply the same graph.
#' 
#' The function returns the edge matrix of the graph, i.e.  a square Boolean
#' matrix of order equal to the number of nodes of the graph and a one in
#' position \eqn{(i,j)} if there is an arrow from \eqn{j} to \eqn{i} and zero
#' otherwise. By default this matrix has ones along the main diagonal. For UGs
#' this matrix is symmetric.  The dimnames of the edge matrix are the nodes of
#' the UG.
#' 
#' @param f a single model formula without response
#' @return a Boolean matrix with dimnames, the adjacency matrix of the
#' undirected graph.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{fitConGraph}}, \code{\link{fitCovGraph}},
#' \code{\link{DAG}}
#' @references Lauritzen, S. (1996). \emph{Graphical models}. Oxford: Clarendon
#' Press.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## X independent of Y given Z
#' UG(~ X*Z + Y*Z)
#' 
#' # The saturated model
#' UG(~ X*Y*Z)
#' 
#' ## The model without three-way interactions has the same graph
#' UG(~ X*Y + Y*Z + Z*X)
#' UG(~ (X + Y + Z)^2)
#' 
#' ## Butterfly model defined from the cliques
#' UG(~ mec*vec*alg + alg*ana*sta)
#' 
#' ## Some isolated nodes
#' UG(~x*y*z + a + b) 
#' 
"UG" <-
function (f) 
{
### Defines an UG from a model formula. Returns the adj. matrix.  
  tt <- terms(f)
  if (attr(tt, "response") == 1)
    stop("You should not specify a response!")
  nod <- dimnames(attr(tt, "factors"))[[1]]
  
  N <- unique(nod) # set of nodes
  dN <- length(N)  # number of nodes
  amat <- matrix(0, dN, dN)
  o <- attr(tt, "order") <= 2
  v <- attr(tt, "factors")[, o, drop = FALSE]
  m <- match(dimnames(v)[[1]], N)
  for (i in 1:sum(o)) {
    ij <- m[v[, i] == 1]
    amat[ij[1], ij[2]] <- 1
    amat[ij[2], ij[1]] <- 1
  }
  dimnames(amat) <- list(N, N)
  amat
}
=#


