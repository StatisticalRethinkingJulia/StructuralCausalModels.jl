#=
#' Fitting a Gaussian concentration graph model
#' 
#' Fits a concentration graph (a covariance selection model).
#' 
#' The algorithms for fitting concentration graph models by maximum likelihood
#' are discussed in Speed and Kiiveri (1986).  If the cliques are known the
#' function uses the iterative proportional fitting algorithm described by
#' Whittaker (1990, p. 184).  If the cliques are not specified the function
#' uses the algorithm by Hastie et al. (2009, p. 631ff).
#' 
#' @param amat a square Boolean matrix representing the adjacency matrix of an
#' UG
#' @param S the sample covariance matrix
#' @param n an integer denoting the sample size
#' @param cli a list containing the cliques of the graph. The components of the
#' list are character vectors containing the names of the nodes in the cliques.
#' The names must match the names of the vertices. The knowledge of the cliques
#' is not needed. If the cliques are not specified the function uses the
#' algorithm by Hastie et al. (2009, p. 446).
#' @param alg The algorithm used.
#' @param pri If TRUE is verbose
#' @param tol a small positive number indicating the tolerance used in
#' convergence tests.
#' @return \item{Shat}{the fitted covariance matrix.} \item{dev}{the `deviance'
#' of the model.} \item{df}{the degrees of freedom.} \item{it}{the iterations.}
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{UG}}, \code{\link{fitDag}}, \code{\link{marks}}
#' @references Cox, D. R. and Wermuth, N. (1996). \emph{Multivariate
#' dependencies}. London: Chapman \& Hall.
#' 
#' Hastie, T., Tibshirani, R. and Friedman, J. (2009).  \emph{The elements of
#' statistical learning.} Springer Verlag: New York.
#' 
#' Speed, T.P. and Kiiveri, H (1986). Gaussian Markov distributions over finite
#' graphs. \emph{Annals of Statistics}, 14, 138--150.
#' 
#' Whittaker, J. (1990). \emph{Graphical models in applied multivariate
#' statistics}. Chichester: Wiley.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## A model for the mathematics marks (Whittaker, 1990)
#' data(marks)
#' ## A butterfly concentration graph  
#' G <- UG(~ mechanics*vectors*algebra + algebra*analysis*statistics)
#' fitConGraph(G, cov(marks), nrow(marks))   
#' ## Using the cliques
#' 
#' cl = list(c("mechanics", "vectors",   "algebra"), c("algebra", "analysis" ,  "statistics")) 
#' fitConGraph(G, S = cov(marks), n = nrow(marks), cli = cl) 
#' 
`fitConGraph` <- function (amat, S, n, cli=NULL, alg = 3,  pri = FALSE, tol = 1e-06)
{
### Fits a concentration graph G.  
### Now it does not compute the cliques of the graph.

  nam <- rownames(S)
  nod <- rownames(amat)
  if(is.null(nod)){
    stop("The adjacency matrix has no labels.")
  }
  if(!all(is.element(nod, nam)))
    stop("The nodes of the graph do not match the names of the variables.")
  else
  sek <- intersect(nam, nod)
  S <- S[sek,sek, drop=FALSE]              # Resizes eventually S
  amat <- amat[sek,sek, drop=FALSE]        # and reorders amat
  nod <- rownames(amat)           
  if (all(amat==0)){
    alg <-  2
    cli = as.list(nod)
    } 
  if(is.null(cli)){
  alg <- 3  
  }
  else {
  alg <- 2   
  nc <- length(cli) 
   if(nc==1) {
      return(list(Shat = S, dev = 0, df = 0, it=1))
    }
   }  

  k <- ncol(S)

  if(alg == 1){     # First algorithm by Whittaker (needs the cliques)
  it <- 0
    W <-   diag(diag(S))  # Starting value
    dimnames(W) <- dimnames(S)
    repeat {
      W.old <- W
      it <- it+1
      for (i in 1:nc) {
        a <- cli[[i]]
        b <- setdiff(nod, a)
        Saa <- S[a, a]
        Waa <- W[a, a]
        Wba <- W[b, a]
        Wbb <- W[b, b]
        B <- Wba %*% solve(Waa)
        Spar <- Wbb - B %*% Waa %*% t(B)
        BV <- B %*% Saa
        W[b, a] <- BV
        W[a, b] <- t(BV)
        W[a, a] <- Saa
        W[b, b] <- Spar + BV %*% t(B)
      }
      if(sum(abs(W-W.old)) < tol) break
    }
  }
  else if(alg==2) {    # Second algorithm by Whittaker  (needs the cliques)
  it = 0
     K <-   solve(diag(diag(S)))  # Starting value
    dimnames(K) <- dimnames(S)
    repeat {
      K.old <- K
      it <- it+1
      for (i in 1:nc) {
        a <- cli[[i]]
        b <- setdiff(nod, a)
        K[a,a] <- solve(S[a,a]) + K[a,b] %*% solve(K[b,b]) %*% K[b,a] 
        if(pri) {
          dev <- likGau(K, S, n, k)
          cat(dev, "\n")
        }
      }
      if(sum(abs(K-K.old)) < tol) break
    }
     W <- solve(K)
  }              
  else if(alg==3){   # Hastie Friedman Tibshirani p. 791
  W0 <- S ; W <- S
  it <- 0
  converge = FALSE
  while( !converge ) {
          it <- it+1
          for (j in 1:k){   
            W11 <- W[-j,-j,drop=FALSE]     
            w12 <- W[-j,j]     
            s12 <- S[-j,j, drop=FALSE]
            paj <- amat[j,] == 1; # neighbors
            paj <- paj[-j]
           beta <- rep(0, k-1)
            if (all(!paj)){
            w <- rep(0, k-1)  
            }
            else{
              beta[paj] <- solve(W11[paj, paj], s12[paj, ])
              w <- W11 %*% beta
            }
              W[-j, j] <- w
              W[j, -j] <- w
          }
          di <- norm(W0-W)      
          if(pri) {
            cat(di, "\n")
          }
          if (di < tol){
            converge <- TRUE
          }
          else {
            W0 <- W 
          }
  }   
        
      } 
  df <- (sum(1-amat) - k)/2
  Kh <- solve(W)  
  dev <- likGau(Kh, S, n, k) 
  list(Shat = W, dev = dev, df = df, it=it)
}
=#


