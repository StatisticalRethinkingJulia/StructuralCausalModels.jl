#=
#' Essential graph
#' 
#' Find the essential graph from a given directed acyclic graph.
#' 
#' Converts a DAG into the Essential Graph.  Is implemented by the algorithm by
#' D.M.Chickering (1995).
#' 
#' @param dagx a square binary matrix, the adjacency matrix of a directed
#' acyclic graph. The names of rows and of the columns are the nodes of the
#' DAG.
#' @return returns the adjacency matrix of the essential graph.
#' @author Giovanni M. Marchetti, translating a MATLAB function by Tomas Kocka,
#' AAU
#' @seealso \code{\link{DAG}}, \code{\link{InducedGraphs}}
#' @references Chickering, D.M. (1995). A transformational characterization of
#' equivalent Bayesian network structures. \emph{Proceedings of Eleventh
#' Conference on Uncertainty in Artificial Intelligence}, Montreal, QU, 87-98.
#' Morgan Kaufmann.
#' 
#' \url{http://research.microsoft.com/~dmax/publications/uai95.pdf}
#' @keywords graphs models multivariate
#' @examples
#' 
#' dag = DAG(U ~ Y+Z, Y~X, Z~X)
#' essentialGraph(dag)
#' 
"essentialGraph" <-
function(dagx){
### Converts a DAG into Essential Graph. 
### Is implemented by the algorithm by D.M.Chickering (1995).
### A transformational characterization of equivalent Bayesian network
### structures. Proceedings of Eleventh Conference on Uncertainty in
### Artificial Intelligence, Montreal, QU, pages 87-98. Morgan Kaufmann 
### http://research.microsoft.com/~dmax/publications/uai95.pdf 
### Implemented in Matlab by Tomas Kocka, AAU.
### Translated in R by Giovanni Marchetti, University of Florence.
  
  ord <- topOrder(dagx);      # get the topological order of nodes 
  n <- nrow(dagx)             # gets the number of nodes
  i <- expand.grid(1:n, 1:n)  # finds all nonzero elements in the adj matrix
  IJ <- i[dagx==1,]           # sort the arcs from lowest possible y
  I <- IJ[, 1]; J <- IJ[, 2]  # and highest possible x, arcs are x->y
  e <- 1
  for(y in 1:n){
    for(x in n:1){
      if(dagx[ord[x], ord[y]] == 1) { 
        I[e] <- ord[x]
        J[e] <- ord[y]
        e <- e + 1
      }
    }
  }
  ## Now we have to decide which arcs are part of the essential graph and
  ## which are undirected edges in the essential graph.
  ## Undecided arc in the DAG are 1, directed in EG are 2 and undirected in EG are 3.
  
  for(e in 1:length(I)){
    if(dagx[I[e],J[e]] == 1){
      cont <- TRUE
      for(w in 1:n){ 
        if(dagx[w,I[e]] == 2){
          if(dagx[w,J[e]] != 0)
            dagx[w,J[e]] <- 2
          else {
            for(ww in 1:n){
              if(dagx[ww,J[e]] != 0)
                      dagx[ww,J[e]] <- 2
            } # skip the rest and start with another arc from the list
            w <- n
            cont <- FALSE
          }
        }
      }
      if(cont){
        exists <- FALSE
        for(z in 1:n){
          if((dagx[z,J[e]] != 0) & (z != I[e]) & (dagx[z,I[e]] == 0)){
            exists <- TRUE
            for(ww in 1:n){
              if(dagx[ww,J[e]] == 1){
                dagx[ww,J[e]] <- 2
              }
            }
          }
        }
        if(!exists){
          for(ww in 1:n){
            if(dagx[ww,J[e]] == 1){
              dagx[ww,J[e]] <- 3
            }
          } 
        }
      }
    }          
  }
  (dagx==2) + (dagx==3) + t(dagx==3)
}
=#


