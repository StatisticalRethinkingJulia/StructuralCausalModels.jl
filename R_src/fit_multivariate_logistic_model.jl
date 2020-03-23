#=
##### The main function fitmlogit  ##########

#' Multivariate logistic models
#' 
#' Fits a logistic regression model to multivariate binary responses.
#' 
#' See Evans and Forcina (2011).
#' 
#' @param \dots Model formulae of marginal logistic models for each response
#' and for each association parameters (log-odds ratios).
#' @param C Matrix of equality constraints.
#' @param D Matrix of inequality cosntraints.
#' @param data A data frame containing the responses and the explanatory
#' variables.
#' @param mit A positive integer: maximum number of iterations.  Default:
#' \code{100}.
#' @param ep A tolerance used in the algorithm: default \code{1e-80}.
#' @param acc A tolerance used in the algorithm: default \code{1e-4}.
#' @return \item{LL}{The maximized log-likelihood.} \item{be}{The vector of the
#' Maximum likelihood estimates of the parameters.} \item{S}{The estimated
#' asymptotic covariance matrix.} \item{P}{The estimated cell probabilities for
#' each individual.}
#' @author Antonio Forcina, Giovanni M. Marchetti
#' @seealso \code{\link{glm}}
#' @references Evans, R.J. and Forcina, A. (2013). Two algorithms for fitting
#' constrained marginal models. \emph{Computational Statistics and Data
#' Analysis}, 66, 1-7.
#' @keywords multivariate logistic model
#' @examples
#'     
#' data(surdata)                     
#' out1 <- fitmlogit(A ~X, B ~ Z, cbind(A, B) ~ X*Z, data = surdata)     
#' out1$beta
#' out2 <- fitmlogit(A ~X, B ~ Z, cbind(A, B) ~ 1, data = surdata)        
#' out2$beta
#' 
`fitmlogit` <- function(..., C = c(), D = c(), data, mit = 100, ep = 1e-80, acc = 1e-4) {
# Fits a logistic regression model to multivariate binary responseses.

# Preliminaries

loglin2 <- function(d){
# Finds the matrix G for a set o d binary variables in inv lex order.

    G <- 1
    K <- matrix(c(1,1,0,1), 2, 2)
        
    for(i in 1:d){
      G <- G %x% K
    }
    G[,-1]    
}    


mods = list(...)
# mods should have 2^q - 1 components  
nm = length(mods)  

be = c()
# Starting values 
resp = c()        
Xbig = c()    
blo = c()
for (k in 1:nm){   
  mf = model.frame(mods[[k]], data = data)  
  res = model.response(mf)  
  Xsmall = model.matrix(mods[[k]], data = data)  
  Xbig = cbind(Xbig, Xsmall)      
  blo = c(blo, ncol(Xsmall))
  nr = 1   
    if(is.vector(res)){
    b = glm(mods[[k]], family = binomial, data = data)
      be = c(be, coef(b))    
  }
   else { 
        be2 = rep(0.1, ncol(Xsmall))
      be = c(be, be2)
      nc = ncol(res)
      
      if(nc > nr){    
      nr = nc
      Y = res  
          }
   }
} 

q  = nr        # number of responses   

b = rep(2, q)       # Assuming all binary variables
             

# Transforms the binary observation into a cell number 
   y = 1 + (Y %*% 2^(0:(q-1))) 

# Finds the matrices C, M and G

  mml = mat.mlogit(q)
  Co = mml$C; Ma = mml$L; Co = as.matrix(Co)
  G = loglin2(q)


b0 = be
n = length(y) # le righe di y sono le unita'
t = max(y)  #  Questo e' semplicemente 2^q 


k = length(be)   # number of parameters
rc = nrow(C); cc = ncol(C)
rd = nrow(D); cd = ncol(D)

# if (k != cc){ 
#     warning('check col C') 
# }
# if( k != cd){ 
#     warning('check col D') 
# }
 
if (! is.null(C)){ # se C non ha zero righe trova il null space di C
     U = null(C) 
 }
 
 seta = nrow(Co)  # e' la dimensione di eta
 mg = t(G) %*% matrix(1/t, t, t)    # NB troppi t!
  
 H = solve(crossprod(G) - mg %*% G) %*% (t(G)-mg)

# initialize
            


 P = matrix(0,t,n) 
 cat('Initial probabilities\n')
 
 for (iu in 1:n){ #  initialize P iu = index of a unit
#   X = .bdiag(lapply(mods, function(x) model.matrix(x, data = data[iu,])))    ### Change this   
#   X = as.matrix(X)
   X = blodiag(Xbig[iu,], blo)
   eta = X %*% be     
   eta = as.matrix(eta)
   p = binve(eta, Co,Ma,G)
   p = pmax(p,ep); p=p/sum(p)  
   P[,iu] = p           
 }
      
 # Iterate

 it=0; test=0;
 diss=1;  LL0=0; dis=1; dm=1;
 while (it < mit &&  (dis + diss) > acc){
   LL = 0; s = matrix(0, k,1); S = matrix(0, k, k); dis = 0
   for (iu in 1:n) {  
     # X = .bdiag(lapply(mods, function(x) model.matrix(x, data = data[iu,])))   
     # X = as.matrix(X)
     X = blodiag(Xbig[iu,], blo)      
     p = P[,iu]     
  
     if (it > 0){
       Op = diag(p) - p %*% t(p)  

       R = Co %*% diagv(1/(Ma %*% p),Ma) %*% Op %*% G # This is the inverse Jacobian 
                    
       while (rcond(R) < 1e-12){
           R = R + diag(seta)       
       }  
    
      R = solve(R) 
      delta = X %*% be - Co %*% log(Ma %*% p)
      th = H %*% log(p) + R %*% delta
      dm = max(th) - min(th)
      p = exp(G %*% th);  p=p/sum(p) 
      p = pmax(p,ep);     p=p/sum(p)  
      P[,iu] = p     
     }              

      LL = LL + log(p[y[iu]])
     
      Op = diag(as.vector(p)) - p %*% t(p)   

      R = Co %*% diagv(1/(Ma %*% p),Ma) %*% Op %*% G     # Check 

      while (rcond(R)<1e-12){
         R = R + diag(seta)
      }
      R = solve(R) 
      eta = Co %*% log(Ma %*% p)
      delta = X %*% be - eta
      dis = dis + sum(abs(delta))
      A = G %*% R %*% X  
      B = t(R) %*% t(G) %*% Op %*% A 
      S = S + t(B) %*% X     
  
      #    attivare una delle due 

      s = s + (t(A[y[iu],, drop = FALSE]) - t(A) %*% p) + t(B) %*% eta   # versione 1
#     s = s +( t(A[y[iu],]) - t(A)%*% p)             # versione 2

   }

   while(rcond(S) < 1e-10){
     S = S + mean(abs(diag(S))) * diag(k)
   }
  #  attivare 1 delle due     
 
    b0 = be;  v = solve(S, s) #  versione 1
#    b0=be; v = b0 + solve(S) %*% s # versione 2
      
    if(is.null(rc)  & is.null(rd)){
         de = v - b0
     }
    else if(is.null(rc)) { # only inequalities
      Si = solve(S) 
      Li = t(chol(Si)) 
      Di = D %*% Li
      de = NULL
      # de = v - b0 + Li %*% ldp(Di,-D %*% v) # Needs ldp
    } 
    else if (is.null(rd)){  # only equalities
      Ai = solve(t(U) %*% S %*% U)
      de = U %*% Ai %*% t(U) %*% S %*% v - b0
    }
   else {             # both  equalities and inequalities
      Ai = solve(t(U) %*% S %*% U)
      Li = t(chol(Ai)) 
      Dz = D %*% U 
      ta = Ai %*% t(U) %*% S %*% v
      #de = U %*% (ta + Li %*% ldp(Dz %*% Li, -Dz %*% ta)) - b0 # Needs ldp
      de = NULL
   }
   
   dm0 =dm; dm = max(de) - min(de);   # shorten step  

   dd = (dm > 1.5); de = de/(1 + dd*(dm^(.85)))
   be = b0 + de   
   diss = sum(abs(de))
   LL0 = LL  
   it = it+1     
   cat(c(it, LL/100, dis/n, diss), "\n")
#   cat(t(be), "\n")

}   

list(LL=LL, beta=be, S=solve(S), P=P)
}               
=#


