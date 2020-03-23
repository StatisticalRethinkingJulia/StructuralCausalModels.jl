#=
#' Block diagonal matrix
#' 
#' Split a vector x into a block diagonal matrix.
#' 
#' 
#' @param x A vector of length \code{n}.
#' @param blo A vector of positive integers such that \code{sum(blo) == n}.
#' @return A block-diagonal matrix with as many row as elements of \code{blo}
#' and \code{n} columns. The vector \code{x} is split into \code{length(blo)}
#' sub-vectors and these are the blocks of the resulting matrix.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{blkdiag}}, \code{\link{diag}}
#' @keywords matrix
#' @examples
#' 
#' blodiag(1:10, blo = c(2, 3, 5)) 
#' blodiag(1:10, blo = c(3,4,0,1))
#' 
`blodiag` = function(x, blo){
# Split a vector x into a block diagonal matrix bith components blo.
# Used by fitmlogit.
k = length(blo) 
 B = matrix(0, k, sum(blo))
 u = cumsum(c(1, blo))
 for(i in 1:k){       
    sub = u[i]:(u[i+1]-1)
      B[i,sub] = x[sub] 
 }   
B
}
=#


