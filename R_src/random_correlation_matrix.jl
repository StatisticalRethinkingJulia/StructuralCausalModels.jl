#=
#' Random correlation matrix
#' 
#' Generates a random correlation matrix with the method of Marsaglia and Olkin
#' (1984).
#' 
#' The algorithm uses \code{\link{rsphere}} to generate \eqn{d} vectors on a
#' sphere in \eqn{d}-space. If \eqn{Z} is a matrix with such vectors as rows,
#' then the random correlation matrix is \eqn{ZZ'}.
#' 
#' @param d an integer > 0, the order of the correlation matrix.
#' @return a correlation matrix of order \code{d}.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{rsphere}}
#' @references Marshall, G.\& Olkin, I. (1984).Generating correlation matrices.
#' \emph{SIAM J. Sci. Stat. Comput.}, 5, 2, 470--475.
#' @keywords distribution multivariate
#' @examples
#' 
#' ## A random correlation matrix of order 3
#' rcorr(3)
#' ## A random correlation matrix of order 5
#' rcorr(5)
#' 
"rcorr" <-
function(d)
{
# Generates a random correlation matrix of dimension d
# with the method of Marsaglia and Olkin (1984).
 h<-rsphere(d,d)
 h %*% t(h)
}
=#


