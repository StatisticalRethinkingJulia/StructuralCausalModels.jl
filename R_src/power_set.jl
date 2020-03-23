#=
#' Power set
#' 
#' Finds the list of all subsets of a set.
#' 
#' If \code{sort == FALSE} the sets are in inverse lexicographical order.
#' 
#' @param set A numeric or character vector.
#' @param sort Logical value. If \code{TRUE} the subsets are sorted according
#' to dimension.  Default is \code{TRUE}.
#' @param nonempty Logical value. If \code{TRUE} the empty set is omitted.
#' Default is \code{TRUE}.
#' @return A list of all subsets of \code{set}.
#' @author Giovanni M. Marchetti
#' @keywords sets
#' @examples
#' 
#' powerset(c("A", "B", "C"), nonempty = FALSE)  
#' powerset(1:3, sort = FALSE, nonempty = TRUE)
#' 
`powerset` <- function(set, sort = TRUE, nonempty=TRUE){
## Power set P(set). If nonempty = TRUE, the empty set is excluded.
    d <- length(set)
    if(d == 0){
        if(nonempty){
            stop("The set is empty.")
        }
        return(list(c()))
    }

    out <- expand.grid(rep(list(c(FALSE, TRUE)),d))
    out <- as.matrix(out)
    out <- apply(out, 1, function(x) set[x])
    if(nonempty){
        out <- out[-1]
    }
    if(sort){
        i <- order(unlist(lapply(out, length)))
    }
    else{
      i <- 1:length(out)
      }
    names(out) <- NULL
    out[i]
}        
=#


