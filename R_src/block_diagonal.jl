#=
#' Block diagonal matrix
#' 
#' Block diagonal concatenation of input arguments.
#' 
#' 
#' @param \dots Variable number of matrices \code{M1, M2, ...}.
#' @return A block diagonal matrix \code{diag(M1, M2, ...)}.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{diag}}
#' @keywords matrix
#' @examples
#' 
#' X <- c(1,1,2,2); Z <- c(10, 20, 30, 40); A <- factor(c(1,2,2,2))
#' blkdiag(model.matrix(~X+Z), model.matrix(~A))
#' 
 `blkdiag` <- function(...){
### Block diagonal concatenation of input arguments.
    a <- list(...)
    Y <- matrix(0,0,0);
    for(M in a){
        if(is.null(M))
            M <- matrix(0,0,0)
        M <- as.matrix(M)
        dY <- dim(Y); dM <- dim(M)
        zeros1 <- matrix(0, dY[1], dM[2])
        zeros2 <- matrix(0, dM[1], dY[2])
        Y <- rbind(cbind(Y, zeros1), cbind(zeros2, M))
    }
    Y
}
=#


