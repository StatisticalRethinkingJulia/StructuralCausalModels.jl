#=
#' Matrix product with a diagonal matrix
#' 
#' Computes faster the product of a diagonal matrix times a full matrix.
#' 
#' Computes \eqn{N = D_v M} where \eqn{D_v} is diagonal avoiding the
#' \code{diag} operator.
#' 
#' @param v A numeric vector specifying the elements on the diagonal of a
#' matrix.
#' @param M A numeric matrix compatible with the product \eqn{D_v M}.
#' @return A matrix \code{N}.
#' @seealso \code{\link{diag}}
#' @keywords matrix
#' @examples
#' 
#' v <- 1:1000
#' M <- matrix(runif(3000), 1000, 3)
#' dim(diagv(v, M))
#' 
`diagv` <-     function(v,M){
# Computes N = diag(v) %*% M avoiding the diag operator.
    as.vector(v) * M
}
=#         


