#=
#' Directed graphs
#' 
#' Defines the adjacency of a directed graph.
#' 
#' The directed graph is defined by a sequence of models formulae.  For each
#' formula the response defines a node of the graph and its parents. The graph
#' contains no loops.
#' 
#' @param \dots a sequence of model formulae
#' @return the adjacency matrix of the directed graph, i.e., a square Boolean
#' matrix of order equal to the number of nodes of the graph and a one in
#' position \eqn{(i,j)} if there is an arrow from \eqn{i} to \eqn{j} and zero
#' otherwise.  The dimnames of the adjacency matrix are the labels for the
#' nodes of the graph.
#' @author G. M. Marchetti
#' @seealso \code{\link{DAG}}, \code{\link{UG}}
#' @references Lauritzen, S. (1996). \emph{Graphical models}. Oxford: Clarendon
#' Press.
#' @keywords graphs directed graph models multivariate
#' @examples
#' 
#' ## A DAG
#' DG(y ~ x, x ~ z, z ~ u)
#' 
#' ## A cyclic directed graph
#' DG(y ~ x, x ~ z, z ~ y)
#' 
#' ## A graph with two arrows between two nodes
#' DG(y ~ x, x ~ y)
#' 
#' ## There can be isolated nodes
#' DG(y ~ x, x ~ x)
#' 
`DG` <- function (...) 
{
    f <- list(...)
    nb <- length(f)
    nod <- c()
    for (k in 1:nb) {
        tt <- terms(f[[k]], specials = "I")
        vars <- dimnames(attr(tt, "factors"))[[1]]
        skip <- attr(tt, "specials")$I
        if (!is.null(skip)) 
            vars <- vars[-skip]
        nod <- c(nod, vars)
    }
    N <- unique(nod)
    dN <- length(N)
    amat <- matrix(0, dN, dN)
    for (k in 1:nb) {
        tt <- terms(f[[k]], specials = "I")
        vars <- dimnames(attr(tt, "factors"))[[1]]
        if (attr(tt, "response") == 1) {
            j <- match(vars[1], N)
            i <- match(vars[-1], N)
            amat[i, j] <- 1
        }
        else if (attr(tt, "response") == 0) 
            stop("Some equations have no response")
    }
    dimnames(amat) <- list(N, N)
    amat
}    
=#


