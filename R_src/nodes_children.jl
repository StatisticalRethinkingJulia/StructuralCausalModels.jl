#=
"ch" <-
function (nn, amat) 
{
### List of the children of nodes nn for a given with adjacency matrix amat.
  nod <- rownames(amat)
  if(is.null(nod)) stop("The adjacency matrix must have dimnames!")
  if(!all(is.element(nn, nod))) stop("Some of the nodes are not among the vertices.")
  k <- length(nn)
  p <- vector(k, mode="list")
  A <- 0 + ((amat != t(amat)) & (amat == 1)) # Select the directed edges
  for(i in 1:k) {
    p[[i]] <- nod[A[nn[i],]==1 ]
  }
  setdiff(unique(unlist(p)), nn)
}
=#


