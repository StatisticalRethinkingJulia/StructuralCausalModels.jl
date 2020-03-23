#=
#' Null space of a matrix
#' 
#' Given a matrix \code{M} find a matrix \code{N} such that \eqn{N^T M} is
#' zero.
#' 
#' 
#' @param M A matrix.
#' @return The matrix \code{N} with the basis for the null space, or an empty
#' vector if the matrix \code{M} is square and of maximal rank.
#' @seealso \code{\link{Null}}, ~~~
#' @keywords matrix
#' @examples
#' 
#'  null(c(1,1,1))
#' 
`null` <- function (M) 
{
    tmp <- qr(M)
    set <- if (tmp$rank == 0L) 
        1L:ncol(M)
    else -(1L:tmp$rank)
    qr.Q(tmp, complete = TRUE)[, set, drop = FALSE]
}
=#
     

