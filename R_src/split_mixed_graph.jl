#=
#' Loopless mixed graphs components
#' 
#' Splits the adjacency matrix of a loopless mixed graph into three components:
#' directed, undirected and bi-directed.
#' 
#' The matrices \code{ug}, and \code{bg} are just symmetric Boolean matrices.
#' 
#' @param amat a square matrix, with dimnames, representing a loopless mixed
#' graph. The matrix consists of 4 different integers as an \eqn{ij}-element: 0
#' for a missing edge between \eqn{i} and \eqn{j}, 1 for an arrow from \eqn{i}
#' to \eqn{j}, 10 for a full line between \eqn{i} and \eqn{j}, and 100 for a
#' bi-directed arrow between \eqn{i} and \eqn{j}. These numbers are added to be
#' associated with multiple edges of different types. The matrix is symmetric
#' w.r.t full lines and bi-directed arrows.
#' @return It is the inverse of \code{makeAG}. It returns the following
#' components.  \item{dg}{the adjacency matrix of the directed edges.}
#' \item{ug}{the adjacency matrix of the undirected edges.} \item{bg}{the
#' adjacency matrix of the bi-directed edges.}
#' @author Mathias Drton, Giovanni M. Marchetti
#' @seealso \code{\link{makeMG}}
#' @keywords graphs ancestral graph mixed graph models multivariate
#' @examples
#' 
#' ag <- makeMG(ug=UG(~y0*y1), dg=DAG(y4~y2, y2~y1), bg=UG(~y2*y3+y3*y4))  
#' isAG(ag)
#' unmakeMG(ag)
#' 
`unmakeMG` <- function(amat){
    ### Returns a list with the three components of a loopless MG.
    d <- nrow(amat)
    ug <- dg <- bg <- amat
    M <- expand.grid(dg = 0:1,ug = 0:1,bg = 0:1)
    i <- strtoi(as.character(amat), 2)
    GG <- M[i+1,]
    ug[,] <- GG[,2] 
    dg[,] <- GG[,1]
    bg[,] <- GG[,3]
    if(any(ug!=t(ug))) stop("Undirected edges are wrongly coded.")
    if(any(bg!=t(bg))) stop("Undirected edges are wrongly coded.")
    return(list(dg = dg, ug = ug, bg = bg))   
}  
=#


