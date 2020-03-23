#=
#' G-identifiability of an UG
#' 
#' Tests if an undirected graph is G-identifiable.
#' 
#' An undirected graph is said G-identifiable if every connected component of
#' the complementary graph contains an odd cycle (Stanghellini and Wermuth,
#' 2005). See also Tarantola and Vicard (2002).
#' 
#' @param amat a symmetric matrix with dimnames representing the adjacency
#' matrix of an undirected graph
#' @return a logical value, \code{TRUE} if the graph is G-identifiable and
#' \code{FALSE} if it is not.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{UG}}, \code{\link{cmpGraph}}, \code{\link{cycleMatrix}}
#' @references Stanghellini, E. \& Wermuth, N. (2005). On the identification of
#' path-analysis models with one hidden variable. \emph{Biometrika}, 92(2),
#' 337-350.
#' 
#' Stanghellini, E. (1997). Identification of a single-factor model using
#' graphical Gaussian rules. \emph{Biometrika}, 84, 241--244.
#' 
#' Tarantola, C. \& Vicard, P. (2002). Spanning trees and identifiability of a
#' single-factor model. \emph{Statistical Methods \& Applications}, 11,
#' 139--152.
#' 
#' Vicard, P. (2000). On the identification of a single-factor model with
#' correlated residuals. \emph{Biometrika}, 87, 199--205.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## A not G-identifiable UG
#' G1 <- UG(~ a*b + u*v)
#' isGident(G1)
#' ## G-identifiable UG
#' G2 <- UG(~ a + b + u*v)
#' isGident(G2)
#' ## G-identifiable UG
#' G3 <- cmpGraph(UG(~a*b*c+x*y*z))
#' isGident(G3)
#' 
"isGident" <-
function(amat){
### Is the UG with adjacency matrix amat G-identifiable?
    is.odd <- function(x) (x %% 2) == 1
    cmpGraph <- function(amat){
      ## Adjacency matrix of the complementary graph.
      g <- 0+ (!amat)
      diag(g) <- 0
      g
    }
    cgr <- cmpGraph(amat) 
    cc <- conComp(cgr) 
    l <- unique(cc)
    k <- length(l)
    g <- rep(k, 0)
    for(i in 1:k){
      subg <- cgr[cc==i, cc==i, drop=FALSE]
      m <- cycleMatrix(subg)
      if(is.null(m))
        rt <- 0
      else        
        rt <- apply(m, 1, sum)
      g[i] <- any(is.odd(rt))
    }
    all(g)
  }



"pa" <-
function (nn, amat) 
{
### List of the parents of nodes nn for a given with adjacency matrix amat.
  nod <- rownames(amat)
  if(is.null(nod)) stop("The adjacency matrix must have dimnames!")
  if(!all(is.element(nn, nod))) stop("Some of the nodes are not among the vertices.")
  k <- length(nn)
  p <- vector(k, mode="list")
  A <- 0 + ((amat != t(amat)) & (amat == 1)) # Select the directed edges
  for(i in 1:k) {
    p[[i]] <- nod[A[,nn[i]]==1 ]
  }
  setdiff(unique(unlist(p)), nn)
}
=#


