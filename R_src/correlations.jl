#=
#' Marginal and partial correlations
#' 
#' Computes a correlation matrix with ones along the diagonal, marginal
#' correlations in the lower triangle and partial correlations given all
#' remaining variables in the upper triangle.
#' 
#' 
#' @param x a square symmetric matrix, a covariance matrix, or a data.frame for
#' n observations and p variables.
#' @return a square correlation matrix with marginal correlations (lower
#' triangle) and partial correlations (upper triangle).
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{parcor}}, \code{\link{cor}}
#' @references Cox, D. R. \& Wermuth, N. (1996). \emph{Multivariate
#' dependencies}. London: Chapman \& Hall.
#' @keywords array graphs models multivariate
#' @examples
#' 
#' ## See Table 6.1 in Cox & Wermuth (1996)
#' data(glucose)
#' correlations(glucose)
#' 
"correlations" <-
function (x)
{
### Marginal correlations (lower half) and
### partial correlations given all remaining variables (upper half).
  
  if(is.data.frame(x))
    r <- cor(x)
  else  { # Recomputes the corr matrix
    Dg <- 1/sqrt(diag(x))
    r <- x * outer(Dg, Dg)
  }
  rp <- parcor(r)
  r[upper.tri(r)] <- rp[upper.tri(rp)]
  r
}
=#


