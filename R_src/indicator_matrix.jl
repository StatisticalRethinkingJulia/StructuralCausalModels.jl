#=
#' Indicator matrix
#' 
#' Finds the indicator matrix of the zeros of a matrix.
#' 
#' The indicator matrix is a matrix of zeros and ones which has a zero element
#' iff the corresponding element of \code{A} is (exactly) zero.
#' 
#' @param A a matrix.
#' @return a matrix of the same dimensions as \code{A}.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{DAG}}, \code{\link{inducedCovGraph}},
#' \code{\link{inducedConGraph}}
#' @references Wermuth, N. \& Cox, D.R. (2004). Joint response graphs and
#' separation induced by triangular systems. \emph{J.R. Statist. Soc. B}, 66,
#' Part 3, 687-717.
#' @keywords array algebra graphs multivariate
#' @examples
#' 
#' ## A simple way to find the overall induced concentration graph
#' ## The DAG on p. 198 of Cox & Wermuth (1996)
#' amat <- DAG(y1 ~ y2 + y3, y3 ~ y5, y4 ~ y5)
#' A <- edgematrix(amat)
#' In(crossprod(A))
#' 
`In` <-
function (A) 
{
### Indicator matrix of structural zeros.
  abs(sign(A)) 
}

"inducedChainGraph" <-
function(amat, cc=rownames(amat), cond=NULL, type="LWF"){
### Induced chain graph with chain components cc.
    inducedBlockGraph <- function(amat, sel, cond){
      S <- inducedConGraph(amat, sel=union(cond, sel), cond=NULL)
      In(S[cond, sel, drop=FALSE])
    }
    nod <- rownames(amat)
    nam <- c(list(cond), cc)
    if(!all(unlist(nam) %in% nod))
      stop("The chain components or the conditioning set are wrong.")
    for(h in nam){
      for(k in nam){
        h <- unlist(h)
        k <- unlist(k)
        if (setequal(h,k))
          next
        if(length(intersect(h,k) > 0))
          stop("The sets are not disjoint!")
      }
    }
    nam <- unlist(nam)
    cg <- matrix(0, length(nam),length(nam))
    dimnames(cg) <- list(nam,nam)
    kc <- length(cc)
    if(type=="AMP"){
      for(i in 1:kc){
        past <-  unlist(cc[0:(i-1)]) 
        Past <- union(cond,past)
        g <- cc[[i]]
        
        Sgg.r <- inducedConGraph(amat, sel=g, cond=Past)
        cg[g, g] <- Sgg.r
        if(length(past) !=0){
          Pgr <- inducedRegGraph(amat, sel=g, cond=Past)
          cg[Past, g] <- Pgr
        }
      }
    }
    else if(type=="LWF"){
      for(i in 1:kc){
        past <-  unlist(cc[0:(i-1)]) 
        Past <- union(cond,past)
        g <- cc[[i]]
        
        Sgg.r <- inducedConGraph(amat,sel=g, cond=Past)
        cg[g, g] <- Sgg.r
        if(length(past) !=0){
          Cgr <- inducedBlockGraph(amat, sel=g, cond=Past)
          cg[Past, g] <- Cgr
        }
      }
    }
    else if(type=="MRG"){
      for(i in 1:kc){
        past <-  unlist(cc[0:(i-1)]) 
        Past <- union(cond,past)
        g <- cc[[i]]
        
        Sgg.r <- inducedCovGraph(amat, sel=g, cond=Past)
        cg[g, g] <- Sgg.r
        if(length(past) != 0){
          Pgr <- inducedRegGraph(amat, sel=g, cond=Past)
          cg[Past, g] <- Pgr
        }
      }
    }
    else
      stop("Wrong type.")
    n <- unlist(cc)
    cg[n,n, drop=FALSE]
  }

"inducedConGraph" <-
function(amat, sel=rownames(amat), cond=NULL){
### Induced concentration graph for a set of nodes given a conditioning set.
    ancGraph <- function(A) {
      ## Edge matrix of the overall ancestor graph.
      if(sum(dim(A)) == 0)
        return(A)
      else
        return(In(solve(2*diag(nrow(A)) - A)))
    }
   
   trclos <- function(M) {
      ## Transitive closure of an UG with edge matrix M. See Wermuth and Cox (2004). 
     edgematrix(transClos(adjMatrix(M)))
    }

    A <- edgematrix(amat) # From the adjacency matrix to edge matrix    
    nod <- rownames(A)
      if(!all(cond %in% nod))
        stop("Conditioning nodes are not among the vertices.")
    if(!all(sel %in% nod))
      stop("Selected nodes are not among the vertices.")
    
    if(length(intersect(sel,cond) > 0))
      stop("The sets are not disjoint!")

    l <- setdiff(nod, union(sel, cond))  # Marginal nodes
    g <- sel
    r <- cond
    L <- union(l,g)
    R <- union(g,r)
    
    Al <- ancGraph(A[l,l,drop=FALSE])
    ARR.l <- In(A[R,R, drop=FALSE] +
                A[R,l, drop=FALSE]%*% Al %*% A[l,R, drop=FALSE])
    TRl <- In(A[R,l,drop=FALSE] %*% Al)
    DRl <- In(diag(length(R)) + TRl %*% t(TRl))
    out <- In(t(ARR.l) %*% trclos(DRl) %*% ARR.l)
    out <- out[g,g, drop=FALSE]
    adjMatrix(out)*10
  }

"inducedCovGraph" <-
function(amat, sel=rownames(amat), cond=NULL){
### Induced covariance graph for a set of nodes given a conditioning set.
    ancGraph <- function(A) {
      ## Edge matrix of the overall ancestor graph.
      if(sum(dim(A)) == 0)
        return(A)
      else
        return(In(solve(2*diag(nrow(A)) - A)))
    }
     trclos <- function(M) {
      ## Transitive closure of an UG with edge matrix M. See Wermuth and Cox (2004). 
     edgematrix(transClos(adjMatrix(M)))
    }
    A <- edgematrix(amat) # From the adjacency matrix to edge matrix
    nod <- rownames(A)
    if(!all(cond %in% nod))
      stop("Conditioning nodes are not among the vertices.")
    if(!all(sel %in% nod))
      stop("Selected nodes are not among the vertices.")
    if(length(intersect(sel,cond) > 0))
      stop("The sets are not disjoint!")
    l <- setdiff(nod, union(sel, cond))  # Marginalized over nodes
    g <- sel
    r <- cond
    L <- union(l,g)
    R <- union(g,r)
    
    AL <-  ancGraph(A[L,L,drop=FALSE]) # In(solve(2*diag(length(L)) - A[L,L]))
    TrL <- In(A[r,L,drop=FALSE] %*% AL)
    DLr <- In(diag(length(L)) + t(TrL) %*% TrL)
    cl <- trclos(DLr)
    out <- In(AL %*% cl %*% t(AL))
    out <- out[g,g, drop=FALSE]
    adjMatrix(out)*100
  }

"inducedDAG" <-
function(amat, order, cond=NULL){
### Induced DAG in a new ordering.
    cc=as.list(order)
    inducedChainGraph(amat, cc=cc, cond=cond)
  }

"inducedRegGraph" <-
function(amat, sel=rownames(amat), cond=NULL){
### Induced regression graph for a set of nodes given a conditioning set.
    ancGraph <- function(A) {
      ## Edge matrix of the overall ancestor graph.
      if(sum(dim(A)) == 0)
        return(A)
      else
        return(In(solve(2*diag(nrow(A)) - A)))
    }
    trclos <- function(M) {
      ## Transitive closure of an UG with edge matrix M. See Wermuth and Cox (2004). 
     edgematrix(transClos(adjMatrix(M)))
    }
    A <- edgematrix(amat) # From the adjacency matrix to edge matrix
    nod <- rownames(A)
    if(!all(cond %in% nod))
      stop("Conditioning nodes are not among the vertices.")
    if(!all(sel %in% nod))
      stop("Selected nodes are not among the vertices.")
    if(length(intersect(sel,cond) > 0))
      stop("The sets are not disjoint!")
    l <- setdiff(nod, union(sel, cond))  # Nodes marginalized over
    g <- sel
    r <- cond
    L <- union(l,g)
    R <- union(g,r)
    
    AL <-  ancGraph(A[L,L,drop=FALSE])          # A^{LL} 
    TrL <- In(A[r,L,drop=FALSE] %*% AL)         # T_{rL}
    DrL <- In(diag(length(r)) + TrL %*% t(TrL)) # D_{rr-L}
    Arr.L <-  In(A[r,r, drop=FALSE] + A[r,L, drop=FALSE] %*% AL 
                 %*% A[L,r, drop=FALSE])        # A_{rr.L}
    FLr <- In(AL %*% A[L, r, drop=FALSE])       # F_{Lr} 
    out <- In(AL %*% t(TrL) %*% trclos(DrL) %*% Arr.L + FLr)
    t(out[g,r, drop=FALSE])
  }
=#


