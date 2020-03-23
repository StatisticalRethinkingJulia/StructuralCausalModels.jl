#+
"bd" <-
function (nn, amat) 
{
### Boundary of the nodes nn for a graph with adjacency matrix amat.
  nod <- rownames(amat)
  if(is.null(nod)) stop("The edge matrix must have dimnames!")
  if(!all(is.element(nn, nod))) stop("Some of the nodes are not among the vertices.")
  b <- vector(length(nn), mode="list")
  diag(amat) <- 0  # As you do not want the node itself in the list
  k <- length(nn)
  for(i in 1:k) {
    b[[i]] <- c(  nod[amat[nn[i], ]==1 ],nod[amat[,nn[i]]==1 ] )
  }
  b <- unique(unlist(b))
  setdiff(b, nn)
}
=#

