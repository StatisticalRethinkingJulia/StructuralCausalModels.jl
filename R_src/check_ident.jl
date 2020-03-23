#=
#' Identifiability of a model with one latent variable
#' 
#' Checks four sufficient conditions for identifiability of a Gaussian DAG
#' model with one latent variable.
#' 
#' Stanghellini and Wermuth (2005) give some sufficient conditions for checking
#' if a Gaussian model that factorizes according to a DAG is identified when
#' there is one hidden node over which we marginalize.  Specifically, the
#' function checks the conditions of Theorem 1, (i) and (ii) and of Theorem 2
#' (i) and (ii).
#' 
#' @param amat a square matrix with dimnames, representing the adjacency matrix
#' of a DAG.
#' @param latent an integer representing the latent variables among the nodes,
#' or the name of the node.
#' @return a vector of length four, indicating if the model is identified
#' according to the conditions of theorems 1 and 2 in Stanghellini \& Wermuth
#' (2005). The answer is \code{TRUE} if the condition holds and thus the model
#' is globally identified or \code{FALSE} if the condition fails, and thus we
#' do not know if the model is identifiable.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{isGident}}, \code{\link{InducedGraphs}}
#' @references Stanghellini, E. \& Wermuth, N. (2005). On the identification of
#' path-analysis models with one hidden variable. \emph{Biometrika}, 92(2),
#' 337-350.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## See DAG in Figure 4 (a) in Stanghellini & Wermuth (2005)
#' d <- DAG(y1 ~ y3, y2 ~ y3 + y5, y3 ~ y4 + y5, y4 ~ y6)
#' checkIdent(d, "y3")  # Identifiable
#' checkIdent(d, "y4")  # Not identifiable?
#' 
#' ## See DAG in Figure 5 (a) in Stanghellini & Wermuth (2005)
#' d <- DAG(y1 ~ y5+y4, y2 ~ y5+y4, y3 ~ y5+y4)
#' checkIdent(d, "y4")  # Identifiable
#' checkIdent(d, "y5")  # Identifiable
#' 
#' ## A simple function to check identifiability for each node
#' 
#' is.ident <- function(amat){
#' ### Check suff. conditions on each node of a DAG.
#'    p <- nrow(amat)
#'    ## Degrees of freedom
#'      df <- p*(p+1)/2 - p  - sum(amat==1) - p + 1
#'    if(df <= 0)
#'        warning(paste("The degrees of freedom are ", df))
#'     a <- rownames(amat)
#'     for(i in a) {
#'       b <- checkIdent(amat, latent=i)
#'       if(TRUE %in% b)
#'         cat("Node", i, names(b)[!is.na(b)], "\n")
#'       else
#'         cat("Unknown.\n")
#'     }
#'   }
#' 
`checkIdent` <- function(amat, latent) {
### Checks SW sufficient conditions for identifiability of a DAG
### with adjacency matrix edge amat and one latent variable.
   "allSubsets" <-
     function (n) 
       {
         ## Returns all subsets of n
         p <- length(n)
         H <- data.matrix(expand.grid(rep(list(1:2), p))) - 1
         H <- split(H==1, row(H))
         lapply(H, function(i) n[i])
       }
   
    nod <- rownames(amat)
    if(is.null(nod)) stop("The adjacency matrix must have dimnames.")
    gcov <- inducedCovGraph(amat, sel=nod, cond=NULL); gcov <- sign(gcov)
    L <- latent
    if(length(L) > 1)
      stop("I have an answer only for one latent variable.")
    O <- setdiff(nod, L)
    m <-  bd(L, gcov)
    ## Theorem 1
    if(length(m) > 2){ 
      G <- inducedCovGraph(amat, sel=O, cond=L); G <- sign(G)
      cond.i <- isGident(G[m,m,drop=FALSE])
    }
    else
      cond.i <- FALSE
    gcon <- inducedConGraph(amat, sel=nod, cond=NULL) ; gcon <- sign(gcon)
    cc <- bd(L, gcon)
    if(length(cc) > 2) {
      cond.ii <- isGident(gcon[cc, cc, drop=FALSE])
    } 
    else  
      cond.ii <- FALSE
    ## Theorem 2 (revised)
    a <- union(pa(L, amat), ch(L, amat))
    if(length(a) > 2){
      Oa <- setdiff(O, a)   # O \ a
      S.a <- inducedCovGraph(amat, sel=union(Oa, L), cond=a); S.a <- sign(S.a)
      first <- S.a[Oa, L]
      first <- Oa[first==0]  # variables that satisfy condition (i) of thm. 2.
      if(length(first)==0){
        cond.iii <- FALSE
      }
      else {
        cond.iii <- FALSE
        H <- allSubsets(first) # in all subsets of these variables
        for(h in H){           # look for a G-identifiable cov. or con. graph
          isgid <- isGident(sign(inducedCovGraph(amat, sel=a, cond=union(L, h))))
          if(isgid){
            cond.iii <- TRUE
            break
          }
          else{
            isgid <- isGident(sign(inducedConGraph(amat, sel=a, cond=union(L, h))))
            if(isgid){
              cond.iii <- TRUE
              break
            }
          }
        }
      }
      second <- setdiff(O,m) # variables that satisfy condition (ii) of thm. 2.
      if(length(second)==0){
        cond.iv <- FALSE
      }
      else {
        cond.iv <- FALSE
        H <- allSubsets(second)  # in all subsets of these variables
        for(h in H){             # look for a G-identifiable cov. or con. graph
          isgid <- isGident(sign(inducedCovGraph(amat, sel=a, cond=union(L, h))))
          if(isgid){
            cond.iv <- TRUE
            break
          }
          else{
            isgid <- isGident(sign(inducedConGraph(amat, sel=a, cond=union(L, h))))
            if(isgid){
              cond.iv <- TRUE
              break
            }
          }
        }
      }  
    }
    else{
      cond.iii <- FALSE
      cond.iv <- FALSE
    }
   c(T1.i = cond.i, T1.ii = cond.ii,
     T2.i = cond.iii, T2.ii = cond.iv)
 }
=#



