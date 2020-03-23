#=
#' Iterative conditional fitting
#' 
#' Main algorithm for MLE fitting of Gaussian Covariance Graphs and Gaussian
#' Ancestral models.
#' 
#' These functions are not intended to be called directly by the user.
#' 
#' @aliases icf icfmag
#' @param bi.graph a symmetric matrix with dimnames representing the adjacency
#' matrix of an undirected graph.
#' @param mag a square matrix representing the adjacency matrix of an ancestral
#' graph (for example returned by \code{makeAG}).
#' @param S a symmetric positive definite matrix, the sample covariance matrix.
#' The order of the variables must be the same of the order of vertices in the
#' adjacency matrix.
#' @param start a symmetric matrix used as starting value of the algorithm. If
#' \code{start=NULL} the starting value is a diagonal matrix.
#' @param tol A small positive number indicating the tolerance used in
#' convergence tests.
#' @return \item{Sigmahat}{the fitted covariance matrix.} \item{Bhat}{matrix of
#' the fitted regression coefficients associated to the directed edges.}
#' \item{Omegahat}{matrix of the partial covariances of the residuals between
#' regression equations.} \item{iterations}{the number of iterations.}
#' @author Mathias Drton
#' @seealso \code{\link{fitCovGraph}}, \code{\link{fitAncestralGraph}}
#' @references Drton, M. \& Richardson, T. S. (2003). A new algorithm for
#' maximum likelihood estimation in Gaussian graphical models for marginal
#' independence. \emph{Proceedings of the Ninetheen Conference on Uncertainty
#' in Artificial Intelligence}, 184--191.
#' 
#' Drton, M. \& Richardson, T. S. (2004). Iterative Conditional Fitting for
#' Gaussian Ancestral Graph Models.  Proceedings of the 20th Conference on
#' Uncertainty in Artificial Intelligence, Department of Statistics, 130--137.
#' @keywords internal
"icf" <-
function(bi.graph, S, start=NULL, tol = 1e-06){
### Iterative conditional fitting for bidirected graphs. Mathias Drton, 2003
    if(!is.matrix(S)){
      stop("Second argument is not a matrix!")
    }
    if(dim(S)[1]!=dim(S)[2]){
      stop("Second argument is not a square matrix!")
    }
    if(min(eigen(S)[[1]])<=0){
      stop("Second argument is not a positive definite matrix!")
    }

    p <- nrow(S)
    i <- 0

    ## prep spouses and non-spouses
    
    pa.each.node <-function(amat){
      ## List of the parents of each node.
      ## If amat is symmetric it returns the boundaries.
      p <- nrow(amat)
      b <- vector(p, mode="list")
      ip <- 1:p
      for(i in 1:p)
        b[[i]] <- ip[amat[,i]==1]
      b 
    }
    spo <- pa.each.node(bi.graph)
    nsp <- pa.each.node(cmpGraph(bi.graph))
    number.spouses <- unlist(lapply(spo, length))
    no.spouses <- (1:p)[number.spouses==0]
    all.spouses <- (1:p)[number.spouses==(p-1)]
    nontrivial.vertices <- setdiff((1:p), no.spouses)

    if(length(nontrivial.vertices)==0){
      if(p==1){
        Sigma <- S
      }
      else{
        Sigma <- diag(diag(S))
        dimnames(Sigma) <- dimnames(S)
      }
      return(list(Sigmahat=Sigma, iterations=1))
    }

    if(is.null(start)){
      Sigma <- as.matrix(diag(diag(S))) # starting value
    }
    else{
      temp <- diag(start)
      start[bi.graph==0] <- 0
      diag(start) <- temp
      diag(start)[no.spouses] <- diag(S)[no.spouses]
      Sigma <- as.matrix(start)
      if(min(eigen(Sigma)$values) <= 0){
        stop("Starting value is not feasible!")
      }
    }
    
    repeat{
      i <- i+1
      Sigma.old <- Sigma
      for(v in nontrivial.vertices){
        if(is.element(v, all.spouses)){
          B <- S[v,-v]%*%solve(S[-v,-v])
          lambda <- S[v,v]-B%*%S[-v,v]
        }
        else{
          B.spo.nsp <-
            Sigma[spo[[v]],nsp[[v]]]%*%solve(Sigma[nsp[[v]],nsp[[v]]])
          YZ <- S[v,spo[[v]]]-S[v,nsp[[v]]]%*%t(B.spo.nsp) 
          B.spo <- 
            YZ %*%
              solve( S[spo[[v]],spo[[v]]]
                    -S[spo[[v]],nsp[[v]]]%*%t(B.spo.nsp)
                    -B.spo.nsp%*%S[nsp[[v]],spo[[v]]]
                    +B.spo.nsp%*%S[nsp[[v]],nsp[[v]]]%*%t(B.spo.nsp) )
          lambda <- S[v,v]-B.spo%*%t(YZ)
          B.nsp <- -B.spo%*%B.spo.nsp
          B <- rep(0, p)
          B[spo[[v]]] <- B.spo
          B[nsp[[v]]] <- B.nsp
          B <- B[-v]
        }
        ## here I can improve by only using B[spo[[v]]]!
        Sigma[v,-v] <- B%*%Sigma[-v,-v]
        Sigma[v,nsp[[v]]] <- 0
        Sigma[-v,v] <- t(Sigma[v,-v])
        Sigma[v,v] <- lambda + B%*%Sigma[-v,v]
      }
      if(sum(abs(Sigma.old-Sigma)) < tol) break
    }
    dimnames(Sigma) <- dimnames(S)
    return(list(Sigmahat=Sigma, iterations=i))
  }

`icfmag` <-
function(mag, S, tol = 1e-06){
    ## Iterative conditional fitting for ancestral and mixed graphs. Mathias Drton, 2003, 2009.
    if(!is.matrix(S)){
      stop("Second argument is not a matrix!")
    }
    if(dim(S)[1]!=dim(S)[2]){
      stop("Second argument is not a square matrix!")
    }
    if(min(eigen(S)[[1]])<=0){
      stop("Second argument is not a positive definite matrix!")
    }
    p <- nrow(S)  # Dimensionality
    temp <- unmakeMG(mag)
    mag.ug <- temp$ug
    mag.dag <- temp$dg  
    mag.bg <- temp$bg
    
    ## Catch trivial case
    if(p==1){
      return(list(Sigmahat=S, Omegahat=S, Bhat=NULL, Lambdahat=NULL, iterations=1))
    }
    
    ## Starting value
    Omega <- diag(diag(S))
    dimnames(Omega) <- dimnames(S)
    B <- diag(p)
    dimnames(B) <- dimnames(S)

    ## IPS for UG
    
    UG.part <- (1:p)[0==apply(mag.dag + mag.bg,2,sum)] 
    if(length(UG.part)> 0){
      Lambda.inv <-
        fitConGraph(mag.ug[UG.part,UG.part, drop=FALSE],
                    S[UG.part,UG.part, drop=FALSE], p+1,tol = tol)$Shat
      Omega[UG.part,UG.part] <- Lambda.inv
    }

    ## Prepare list of spouses, parents, etc.
    pa.each.node <-function(amat){
      ## List of the parents of each node.
      ## If the adjacency matrix is symmetric it gives the boundary.
      p <- nrow(amat)
      b <- vector(p, mode="list")
      ip <- 1:p
      for(i in 1:p)
        b[[i]] <- ip[amat[,i]==1]
      b 
    }
    spo <- pa.each.node(mag.bg)
    nsp <- pa.each.node(cmpGraph(mag.bg))
    pars <- pa.each.node(mag.dag) 
    
    i <- 0
    repeat{
      i <- i+1
      Omega.old <- Omega
      B.old <- B
      for(v in setdiff(1:p, UG.part)){
        parv <- pars[[v]]
        spov <- spo[[v]]
        if(length(spov)==0){
          if(length(parv)!=0){
            if(i == 1){ # do it only once
              ## attention: B = - beta
              B[v,parv] <- -S[v,parv]%*%solve(S[parv,parv])
              Omega[v,v] <- S[v,v]+B[v,parv]%*%S[parv,v]
            }
          }
        }
        else{
          if(length(parv)!=0){
            O.inv <- matrix(0, p,p)
            O.inv[-v,-v] <- solve(Omega[-v,-v])
            Z <- O.inv[spov,-v] %*%B[-v,]
            lpa <- length(parv)
            lspo <- length(spov)
            XX <- matrix(0, lpa+lspo, lpa+lspo)
            XX[1:lpa, 1:lpa] <- S[parv,parv]
            XX[1:lpa,(lpa+1):(lpa+lspo)] <- S[parv,]%*%t(Z)
            XX[(lpa+1):(lpa+lspo),1:lpa] <- t(XX[1:lpa,(lpa+1):(lpa+lspo)])
            XX[(lpa+1):(lpa+lspo),(lpa+1):(lpa+lspo)] <- Z%*%S%*%t(Z)
            YX <- c(S[v,parv], S[v,]%*%t(Z))
            temp <- YX %*% solve(XX)
            B[v,parv] <- -temp[1:lpa]
            Omega[v,spov] <- temp[(lpa+1):(lpa+lspo)]
            Omega[spov,v] <- Omega[v,spov]
            
            temp.var <- S[v,v] - temp %*% YX
            Omega[v,v] <- temp.var +
              Omega[v,spov] %*% O.inv[spov,spov] %*% Omega[spov,v]
          }
          else{
            O.inv <- matrix(0, p,p)
            O.inv[-v,-v] <- solve(Omega[-v,-v])
            Z <- O.inv[spov,-v] %*%B[-v,]
            XX <- Z%*%S%*%t(Z)
            YX <- c(S[v,]%*%t(Z))
            Omega[v,spov] <- YX %*% solve(XX)
            Omega[spov,v] <- Omega[v,spov]
            
            temp.var <- S[v,v] -  Omega[v,spov] %*% YX
            Omega[v,v] <- temp.var +
              Omega[v,spov] %*% O.inv[spov,spov] %*%
                Omega[spov,v]
          }
        }
      }
      if(sum(abs(Omega.old-Omega)) + sum(abs(B.old-B)) < tol) break
    }
    Sigma <- solve(B)%*%Omega%*%solve(t(B))
  ##  Corrections by Thomas Richardson of the following:
  ##  Lambda <- Omega
  ##  Lambda[-UG.part,-UG.part] <- 0
    Lambda <- matrix(0, p, p)
    if(length(UG.part) > 0){  
      Lambda[-UG.part, -UG.part] <- Omega[-UG.part, -UG.part]
  }   
    
    Omega[UG.part,UG.part] <- 0
    return(list(Sigmahat=Sigma, Bhat=B, Omegahat=Omega, Lambdahat=Lambda,
                iterations=i))
  }
=#


