#=
#' d-separation
#' 
#' Determines if in a directed acyclic graph two set of nodes a d-separated by
#' a third set of nodes.
#' 
#' d-separation is a fundamental concept introduced by Pearl (1988).
#' 
#' @param amat a Boolean matrix with dimnames, representing the adjacency
#' matrix of a directed acyclic graph. The function does not check if this is
#' the case. See the function \code{isAcyclic}.
#' @param first a vector representing a subset of nodes of the DAG.  The vector
#' should be a character vector of the names of the variables matching the
#' names of the nodes in \code{rownames(A)}. It can be also a numeric vector of
#' indices.
#' @param second a vector representing another subset of nodes of the DAG.  The
#' set \code{second} must be disjoint from \code{first}.  The mode of
#' \code{second} must match the mode of \code{first}.
#' @param cond a vector representing a conditioning subset of nodes.  The set
#' \code{cond} must be disjoint from the other two sets and must share the same
#' mode.
#' @return a logical value. \code{TRUE} if \code{first} and \code{second} are
#' d-separated by \code{cond}.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{DAG}}, \code{\link{shipley.test}},
#' \code{\link{inducedCovGraph}}
#' @references Pearl, J. (1988). \emph{Probabilistic reasoning in intelligent
#' systems.} San Mateo: Morgan Kaufmann.
#' 
#' Lauritzen, S. (1996). \emph{Graphical models}. Oxford: Clarendon Press.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## Conditioning on a transition node
#' dSep(DAG(y ~ x, x ~ z), first="y", second="z", cond = "x")
#' ## Conditioning on a collision node (collider)
#' dSep(DAG(y ~ x, y ~ z), first="x", second="z", cond = "y")
#' ## Conditioning on a source node
#' dSep(DAG(y ~ x, z ~ x), first="y", second="z", cond = "x")
#' ## Marginal independence
#' dSep(DAG(y ~ x, y ~ z), first="x", second="z", cond = NULL)
#' ## The DAG defined on p.~47 of Lauritzen (1996)
#' dag <- DAG(g ~ x, h ~ x+f, f ~ b, x ~ l+d, d ~ c, c ~ a, l ~ y, y ~ b)
#' dSep(dag, first="a", second="b", cond=c("x", "y"))
#' dSep(dag, first="a", second=c("b", "d"), cond=c("x", "y"))
#' 
"dSep" <-
function(amat, first, second, cond) {
### Are first and second d-Separated by cond in a DAG? 
    e <- inducedCovGraph(amat, sel=c(first,second), cond=cond)
    all(e[first,second] == 0)
  }
=#


