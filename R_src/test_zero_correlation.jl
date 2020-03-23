#=
#' Test for zero partial association
#' 
#' Test for conditional independence between two variables, given the other
#' ones, assuming a multivariate normal distribution.
#' 
#' 
#' @param r a partial correlation coefficient, computed by \code{\link{pcor}}.
#' @param q the number of variables in the conditioning set.
#' @param n integer > 0, the sample size.
#' @return \item{tval}{The Student's t-test statistic.} \item{df}{The degrees
#' of freedom} \item{pvalue}{The P-value, assuming a two-sided alternative.}
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{pcor}}, \code{\link{shipley.test}}
#' @keywords htest multivariate
#' @examples
#' 
#' ## Are 2,3 independent given 1?
#' data(marks)
#' pcor.test(pcor(c(2,3,1), var(marks)), 1, n=88)
#' 
"pcor.test" <-
function(r, q, n){
                df = n - 2 - q
                tval <- r * sqrt(df)/sqrt(1-r*r)
                pv <- 2 * pt(-abs(tval), df)
  list(tval = tval, df = df, pvalue = pv)

}
=#


