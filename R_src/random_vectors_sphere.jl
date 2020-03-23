#=
#' Random vectors on a sphere
#' 
#' Generates a sample of points uniformly distributed on the surface of a
#' sphere in d-space.
#' 
#' The algorithm is based on normalizing to length 1 each d-vector of a sample
#' from a multivariate normal \eqn{N(0, I)}.
#' 
#' @param n an integer, the sample size.
#' @param d an integer, the dimension of the space. For example, a circle is
#' defined in 2D-space, a sphere in 3D-space.
#' @return a matrix of \code{n} rows and \code{d} columns.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{rnorm}}, \code{\link{rcorr}}
#' @keywords distribution multivariate
#' @examples
#' 
#' ## 100 points on circle
#' z <- rsphere(100,2)
#' plot(z)
#' 
#' ## 100 points on a sphere
#' z <- rsphere(100, 3)
#' pairs(z)
#' 
"rsphere" <-
function(n, d)
{
## Generates n random vectors uniformly dist. on the
## surface of a sphere, in d dimensions.
  X <- matrix(rnorm(n*d),n,d)
  d <- apply(X, 1, function(x) sqrt(sum(x*x)))
  sweep(X, 1, d, "/")
}
=#


