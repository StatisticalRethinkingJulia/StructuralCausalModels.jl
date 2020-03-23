#=
#' Acyclic directed mixed graphs
#' 
#' Check if it is an adjacency matrix of an ADMG
#' 
#' Checks if the following conditions must hold: (i) no undirected edge meets
#' an arrowhead; (ii) no directed cycles;
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
#'  isADMG(a1)    # Not an AG. (a2) p.969    
#'  a2 <- makeMG(dg=DAG(b ~ a, d~c), bg=UG(~a*c+c*b+b*d))           # Fig. 3 (b1) p.969  
#'  isADMG(a2)
#' 
`isADMG`<- function(amat){
  ### check is if a graph is an ADMG
  comp <- unmakeMG(amat)
  ug <- comp$ug; dag <- comp$dg; bg <- comp$bg  
  out <- TRUE
  if(any(amat > 100)){  
    warning("There are double edges.")
    out <- FALSE
  }
  if(!isAcyclic(dag)){
    warning("Not acyclic.")
    out <- FALSE
  }
  out 
}       
=#


