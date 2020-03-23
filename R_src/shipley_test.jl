#=
#' Test of all independencies implied by a given DAG
#' 
#' Computes a simultaneous test of all independence relationships implied by a
#' given Gaussian model defined according to a directed acyclic graph, based on
#' the sample covariance matrix.
#' 
#' The test statistic is \eqn{C = -2 \sum \ln p_j} where \eqn{p_j} are the
#' p-values of tests of conditional independence in the basis set computed by
#' \code{basiSet(A)}. The p-values are independent uniform variables on
#' \eqn{(0,1)} and the statistic has exactly a chi square distribution on
#' \eqn{2k} degrees of freedom where \eqn{k} is the number of elements of the
#' basis set.  Shipley (2002) calls this test Fisher's C test.
#' 
#' @param amat a square Boolean matrix, of the same dimension as \code{S},
#' representing the adjacency matrix of a DAG.
#' @param S a symmetric positive definite matrix, the sample covariance matrix.
#' @param n a positive integer, the sample size.
#' @return \item{ctest}{Test statistic \eqn{C}.} \item{df}{Degrees of freedom.}
#' \item{pvalue}{The P-value of the test, assuming a two-sided alternative.}
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{basiSet}}, \code{\link{pcor.test}}
#' @references Shipley, B. (2000). A new inferential test for path models based
#' on directed acyclic graphs. \emph{Structural Equation Modeling}, 7(2),
#' 206--218.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## A decomposable model for the mathematics marks data
#' data(marks)
#' dag <- DAG(mechanics ~ vectors+algebra, vectors ~ algebra, 
#' statistics ~ algebra+analysis, analysis ~ algebra)
#' shipley.test(dag, cov(marks), n=88)
#' 
"shipley.test" <-
function (amat, S, n) 
{
### Overall d-separation test. See Shipley (2000).
### amat: adjacency matrix; S: covariance matrix;  n: observations.
  pval <- function(r, q, n){
    ## See pcor
    df = n - 2 - q
    tval <- r * sqrt(df)/sqrt(1-r*r)
    2 * pt(-abs(tval), df)
  }
  l <- basiSet(amat)
  k <- length(l)
  p <- rep(0, k)
  for(i in 1:k){
    r <- pcor(l[[i]], S)
    q <- length(l[[i]]) - 2
    p[i] <- pval(r, q, n)
  }
  ctest <- -2 * sum(log(p))
  df <- 2*k
  pv <- 1 - pchisq(ctest, df)
  list(ctest=ctest, df=df, pvalue=pv)
}
=#


