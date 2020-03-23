#=
#' Partial correlations
#' 
#' Finds the matrix of the partial correlations between pairs of variables
#' given the rest.
#' 
#' The algorithm computes \eqn{- \sigma^{rs}/(\sigma^{rr} \sigma^{ss})^{1/2}}
#' where the \eqn{\sigma^{rs}} are concentrations, i.e. elements of the inverse
#' covariance matrix.
#' 
#' @param S a symmetric positive definite matrix, representing a covariance
#' matrix.
#' @return A symmetric matrix with ones along the diagonal and in position
#' \eqn{(r,s)} the partial correlation between variables \eqn{r} and \eqn{s}
#' given all the remaining variables.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{var}}, \code{\link{cor}}, \code{\link{correlations}}
#' @references Cox, D. R. \& Wermuth, N. (1996). \emph{Multivariate
#' dependencies}. London: Chapman \& Hall.
#' @keywords array graphs models multivariate
#' @examples
#' 
#' ### Partial correlations for the mathematics marks data
#' data(marks)
#' S <- var(marks)
#' parcor(S)
#' 
"parcor" <-
function (S)
{
### Finds the partial correlation matrix of the variables given the rest.
### S is the covariance matrix.  
  p <- ncol(S)
  K <- solve(S)
  a <- 1/sqrt(diag(K))
  K <- K * outer(a, a)
  out <- 2 * diag(p) - K
  dimnames(out) <- dimnames(S)
  out
}
=#


