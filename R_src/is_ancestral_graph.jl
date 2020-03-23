#=
#' Ancestral graph
#' 
#' Check if it is an adjacency matrix of an ancestral graph
#' 
#' Checks if the following conditions must hold: (i) no undirected edge meets
#' an arrowhead; (ii) no directed cycles; (iii) spouses cannot be ancestors.
#' For details see Richardson and Spirtes (2002).
#' 
#' @param amat %% ~~Describe \code{amat} here~~
#' @return A logical value, \code{TRUE} if it is an ancestral graph and
#' \code{FALSE} otherwise.
#' @author Giovanni M. Marchetti, Mathias Drton
#' @seealso \code{\link{makeMG}}, \code{\link{isADMG}}
#' @references Richardson, T. S. and Spirtes, P. (2002). Ancestral Graph Markov
#' Models. \emph{Annals of Statistics}, 30(4), 962--1030.
#' @keywords graphs ancestral graph mixed graph models multivariate
#' @examples
#' 
#'  ## Examples from Richardson and Spirtes (2002)
#'  a1 <- makeMG(dg=DAG(a~b, b~d, d~c), bg=UG(~a*c))  
#'  isAG(a1)    # Not an AG. (a2) p.969    
#'  a2 <- makeMG(dg=DAG(b ~ a, d~c), bg=UG(~a*c+c*b+b*d))           # Fig. 3 (b1) p.969  
#'  isAG(a2)
#' 
`isAG` <- function(amat) {
### checks if a graph is an ancestral graph
    comp <- unmakeMG(amat)
    ug <- comp$ug; dag = comp$dg; bg = comp$bg  
    out <- TRUE

    if(any(amat > 100)){
       warning("There are double edges.")
       out <- FALSE
       }
    anteriorGraph <- function (amat) 
      {
        ## Adjacency matrix of the anterior graph from an AG.
        A <- 0 + ((amat == 1) |(amat == 10)) # Select the directed and undirected edges
        transClos(A)
      }
    if(any(apply(dag, 2, sum) & apply(ug, 2, sum))){
      warning("Undirected edges meeting a directed edge.")
      out <- FALSE
      }
    if(any(apply(bg, 2, sum) & apply(ug, 2, sum))){
      warning("Undirected edges meeting a bidirected edge.")
      out <- FALSE
    }    
    H <- anteriorGraph(amat)
 
    if(any((H==1) & (amat == 100))){
      warning("Spouses cannot be ancestors.")
      out <- FALSE
    }  
    out
  }      
=#


