#=
#' Basis set of a DAG
#' 
#' Finds a basis set for the conditional independencies implied by a directed
#' acyclic graph, that is a minimal set of independencies that imply all the
#' other ones.
#' 
#' Given a DAG and a pair of non adjacent nodes \eqn{(i,j)} such that \eqn{j}
#' has higher causal order than \eqn{i}, the set of independency statements
#' \eqn{i} independent of \eqn{j} given the union of the parents of both
#' \eqn{i} and \eqn{j} is a basis set (see Shipley, 2000). This basis set has
#' the property to lead to independent test statistics.
#' 
#' @param amat a square matrix with dimnames representing the adjacency matrix
#' of a DAG.
#' @return a list of vectors representing several conditional independence
#' statements. Each vector contains the names of two non adjacent nodes
#' followed by the names of nodes in the conditioning set (which may be empty).
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{shipley.test}}, \code{\link{dSep}}, \code{\link{DAG}}
#' @references Shipley, B. (2000). A new inferential test for path models based
#' on directed acyclic graphs. \emph{Structural Equation Modeling}, 7(2),
#' 206--218.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## See Shipley (2000), Figure 2, p. 213
#' A <- DAG(x5~ x3+x4, x3~ x2, x4~x2, x2~ x1)
#' basiSet(A)
#' 
"basiSet" <-
function(amat){
### Basis set of a DAG with adjacency matrix amat.
    amat <- topSort(amat)
    nod <- rownames(amat)
    dv <- length(nod)
    ind <- NULL
    ## NOTE. This is correct if the adj mat is upper triangular.
    for(r in 1:dv){
      for(s in r:dv) {
        if((amat[r,s] != 0) | (s==r))
          next
        else{
          ed <- nod[c(r,s)]
          pa.r <- nod[amat[,r] == 1]
          pa.s <- nod[amat[,s] == 1] 
          dsep <- union(pa.r, pa.s) 
          dsep <- setdiff(dsep, ed)
          b <- list(c(ed, dsep))
          ind <- c(ind, b)
        }
      }
    }
    ##      ind <- lapply(ind, function(x) nn[x])
    ind
  }
=#

