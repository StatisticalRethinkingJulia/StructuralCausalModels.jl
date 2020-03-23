#=
#' Fitting Gaussian DAG models with one latent variable
#' 
#' Fits by maximum likelihood a Gaussian DAG model where one of the nodes of
#' the graph is latent and it is marginalised over.
#' 
#' For the EM algorithm used see Kiiveri (1987).
#' 
#' @param amat a square matrix with dimnames representing the adjacency matrix
#' of the DAG.
#' @param Syy a symmetric positive definite matrix, with dimnames, the sample
#' covariance matrix of the observed variables.  The set of the observed nodes
#' of the graph must be a subset of the set of the names of the variables in
#' \code{Syy}.
#' @param n a positive integer, the sample size.
#' @param latent the name of the latent variable.
#' @param norm an integer, the kind of normalization of the latent variable.
#' If \code{norm=1}, the latent is scaled to have unit variance. If
#' \code{norm=2}, the latent is scaled to have unit partial variance given its
#' parents.
#' @param seed an integer, used by \code{set.seed} to specify a random starting
#' point of the EM algorithm.
#' @param maxit an integer denoting the maximum number of iterations allowed
#' for the EM algorithm. If the convergence criterion is not satisfied within
#' maxit iterations the algorithms stops and a warning message is returned.
#' @param tol a small real value, denoting the tolerance used in testing
#' convergence.
#' @param pri logical, if \code{pri=TRUE} then the value of the deviance at
#' each iteration is printed.
#' @return \item{Shat}{ the fitted covariance matrix of all the variables
#' including the latent one. The latent variable is the last.  If \code{norm=1}
#' then the variance of the latent variable is constrained to 1.  }
#' \item{Ahat}{ a square matrix of the fitted regression coefficients. The
#' entry \code{Ahat[i,j]} is minus the regression coefficient of variable
#' \code{i} in the regression equation \code{j}. Thus there is a non zero
#' partial regression coefficient \code{Ahat[i,j]} corresponding to each non
#' zero value \code{amat[j,i]} in the adjacency matrix.  } \item{Dhat}{ a
#' vector containing the partial variances of each variable given the parents.
#' If \code{norm=2} then the partial variance of the latent variable is
#' constrained to 1.  } \item{dev}{ the `deviance' of the model.  } \item{df}{
#' the degrees of freedom of the model.  } \item{it}{ the number of EM
#' algorithm iterations at convergence.  }
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{fitDag}}, \code{\link{checkIdent}}
#' @references Kiiveri,H. T. (1987). An incomplete data approach to the
#' analysis of covariance structures. \emph{Psychometrika}, 52, 4, 539--554.
#' 
#' Joreskog, K.G. and Goldberger, A.S. (1975). Estimation of a model with
#' multiple indicators and multiple causes of a single latent variable.
#' \emph{Journal of the American Statistical Association}, 10, 631--639.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## data from Joreskog and Goldberger (1975)
#' V <- matrix(c(1,     0.36,   0.21,  0.10,  0.156, 0.158,
#'               0.36,  1,      0.265, 0.284, 0.192, 0.324,
#'               0.210, 0.265,  1,     0.176, 0.136, 0.226,
#'               0.1,   0.284,  0.176, 1,     0.304, 0.305, 
#'               0.156, 0.192,  0.136, 0.304, 1,     0.344,
#'               0.158, 0.324,  0.226, 0.305, 0.344, 1),     6,6)
#' nod <- c("y1", "y2", "y3", "x1", "x2", "x3")
#' dimnames(V) <- list(nod,nod)
#' dag <- DAG(y1 ~ z, y2 ~ z, y3 ~ z, z ~ x1 + x2 + x3, x1~x2+x3, x2~x3) 
#' fitDagLatent(dag, V, n=530, latent="z", seed=4564)
#' fitDagLatent(dag, V, n=530, latent="z", norm=2, seed=145)
#' 
"fitDagLatent" <-
function (amat, Syy, n, latent, norm = 1,  seed, maxit=9000, tol=1e-6, pri=FALSE) 
{
### Fits linear recursive regressions with independent residuals and one latent variable.
### Syy: covariance matrix, n: sample size, amat: adjacency matrix.
### NOTE: both amat and Syy must have rownames.
### latent is the "name" of the latent in the rownames of Syy. norm = normalisation type.

  ## Local functions
  setvar1 <- function (V, z, paz, norm)
    ## paz are the parents of the latent (needed if norm=2)
    {
      ## Normalizes V
      if(norm == 1){
        ## Rescales V forcing V[z,z] = 1
        a <- 1 / sqrt(V[z,z])
      }
      else if(norm== 2) {
        ## Rescales V forcing Delta[z,z] = 1
        if(sum(paz) > 0)
          sig <- V[z,z] - V[z, paz] %*% solve(V[paz,paz]) %*% V[paz,z]
        else
          sig <- V[z,z]
        a <- 1/sqrt(sig)
      }
      V[z,] <- V[z,] * a
      V[,z] <- V[,z] * a
      V
    }

  cmqi <- function (Syy, Sigma, z) 
    {
      ## Computes the matrix C(M | Q) by Kiiveri (1987), Psychometrika.
      ## It is a slight generalization in which Z is not the last element.
      ## z is a Boolean vector indicating the position of the latent variable in X.
      y <- ! z
      Q <- solve(Sigma)
      Qzz <- Q[z,z]
      Qzy <- Q[z,y]
      B <- - solve(Qzz) %*% Qzy
      BSyy <- B %*% Syy
      E <- Sigma*0 
      E[y,y] <- Syy
      E[y,z] <- t(BSyy)
      E[z,y] <- BSyy
      E[z,z] <- BSyy %*% t(B) + solve(Qzz)
      dimnames(E) <- dimnames(Sigma) 
      E
    }
  fitdag <- function (amat, S,n, constr=NULL)
    {
      ## Fits linear recursive regressions with independent residuals (fast version).
      ## NOTE. amat and S must have the same size and variables. constr is a matrix
      ## indicating the edges that must be constrained to 1.
      emat <- edgematrix(amat)
      A <- emat
      p <- ncol(S)
      Delta <- rep(p,0)
      ip <- 1:p
      for(i in ip) {
        u <- emat[i,]
        v <- ip[(u == 1) & (ip != i)]           # Parents 
        if(length(v) == 0){                     # If pa is empty
          Delta[i] <- S[i,i]
          next
        }
        M <- lmfit(S, y=i, x=v, z=(constr[i, ] == 1))
        A[i, ] <- - A[i, ] * M
        A[i,i] <- 1
        k <- sum(A[i,])
        Delta[i] <- M[i]
      }
      Khat <- t(A) %*% diag(1/Delta) %*% A
      Shat <- solve(Khat)
      list(A = A, Delta=Delta, Shat=Shat, Khat=Khat)
    }
  lmfit <- function(S, y, x, z=NULL){
    ## Regression coefficients of y given x eventually with z constrained to 1.
    ## Residual variance in position y.
    Sxy <- S[x, y, drop=FALSE] - apply(S[x, z, drop=FALSE], 1, sum) 
    Sxx <- S[x, x, drop=FALSE]
    bxy <- solve(Sxx,Sxy)
    out<- rep(0, nrow(S))
    out[x] <- bxy
    out[z] <- 1
    names(out) <- rownames(S)
    xz <- c(x,z)
    b <- out[xz, drop=FALSE]
    res <- S[y,y] + t(b) %*% (S[xz, xz, drop=FALSE] %*% b - 2 * S[xz,y, drop=FALSE])
    out[y] <- res
    out
  }
  lik <-  function(K, S, n, p){
    ## Computes the deviance
    trace <- function(a) sum(diag(a)) 
    SK <- S %*% K 
    (trace(SK) - log(det(SK)) - p) * n
  }
  ## Beginning  of the main function
  nam <- rownames(Syy)        # Names of the variables (they can be more than the nodes)
  nod <- rownames(amat)       # Names of the nodes of the DAG (that contains the latent)
 
  if(is.null(nod))
    stop("The adjacency matrix has no labels.")
  if(!all(is.element(setdiff(nod, latent), nam)))
    stop("The observed nodes of the graph do not match the names of the variables.")
  else
    sek <- intersect(nam, nod)
  Syy <- Syy[sek,sek, drop=FALSE]          # Resizes eventually S
  sek <- c(sek, latent)
  amat <- amat[sek,sek, drop=FALSE]        # and reorders amat
  nod <- rownames(amat)
  paz <- pa(latent, amat)                  # Parents of the latent
  paz <- is.element(nod, paz)
  dn <- list(nod,nod)
  wherez <- is.element(nod, latent)        # Index of the latent
  if(is.null(wherez))
    stop("Wrong name of the latent variable!")
  wherey <- ! wherez
  p <- ncol(Syy)
  df <- p*(p+1)/2  - sum(amat==1) - p      # Degrees of freedom 
  
  if(df <= 0)
    warning(paste("The degrees of freedom are ", df))
  if(!missing(seed))  set.seed(seed)     # For random starting value
  Sigma.old <- rcorr(p+1)
  Sigma.old <- setvar1(Sigma.old, wherez, paz, norm=norm)
  dimnames(Sigma.old) <- dn    
  it <- 0
  repeat{
    it <- it+1
    if(it > maxit){
      warning("Maximum number of iterations reached.")
      break
    }
    Q <- cmqi(Syy, Sigma.old, wherez) # E-step. See Kiiveri, 1987                                        
    fit <- fitdag(amat, Q, n)         # M-step
    Sigma.new <- fit$Shat
                                       
    if(pri) {
      dev <-  lik(fit$Khat, Q, n, (p+1)) # Monitoring progress of iterations
      cat(dev, "\n")
    }
    else{
      if(0==(it %% 80))
        cat("\n")
      else
        cat(".")
    }                                     
    if(sum(abs(Sigma.new - Sigma.old)) < tol) break  # Test convergence
    Sigma.old <- Sigma.new
  }
  cat("\n")
  dev <-  lik(fit$Khat, Q, n, (p+1))
  Shat <- setvar1(Sigma.new, wherez, paz, norm=norm) # Normalize Shat
  dimnames(Shat) <- dn
  Khat <- solve(Shat)
  fit <- fitDag(amat, Shat, n)
  list(Shat=Shat, Ahat=fit$Ahat, Dhat=fit$Dhat, dev=dev, df=df, it=it) 
}
=#


