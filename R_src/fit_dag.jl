
#=
`fitDAG` <- function (..., data)
{
### Fits linear recursive regressions with independent residuals. 
#   ...,  a list of models
#   data, a data frame.
 
  mo = list(...) 
  p = length(mo)      
  D = DAG(...)
 
nam = rownames(D)          

  data = data[, nam]  
## Existing responses
   
   resp = c()
for( i in 1:p) {
    te = as.character(mo[[i]])
    resp = c(resp, te[2])
    }  
   newresp = setdiff(nam, resp) 
  for(k in 1:length(newresp)){          
         newmo = formula(paste(newresp[k], "~ 1"))  
         mo = c(mo, newmo)  
      }          
   to = topOrder(D)
   o = match(nam[to], c(resp, newresp))
   mo = mo[o]
   data = data[,to]
  

  beta = vector(p, mode = "list")
  delta = rep(0,p)    
  n = nrow(data) 
  lik = 0 
  df = 0     
  nomi = colnames(data)
  for(i in 1:length(mo)) {           
  moi = mo[[i]]
  te = as.character(moi)
  if(te[2] == te[3]){ 
     lik = lik + n * log(2*pi * var(data[, te[2]])) + (n-1)  
  
     df  = df + (n-1)
     next 
  }   
  else {
     m = lm(moi, data = data[1:i])  
     mq = summary(m)   
     beta[[i]] = mq$coefficients[,1]
     delta[[i]] = (mq$sigma)^2  
  #     Yh[, te[2]] = fitted(m)             
     d = n - length(beta[i])
     lik  = lik + n * log(2*pi * delta[i]) + d
       df = df + d     
       nm = paste(nomi[1:i], collapse = ",")
       cat(paste("\nModel:", te[2], te[1], te[3], " Margin: ",nm ,"\n"))
       
       print(mq$coefficients, digits = 4)
  }
}

 # Shat <- cov(Yh)
 # Khat <- solve(Shat)
#  H <- S %*% Khat
#  trace <- function(A) sum(diag(A))
#  dev <- (trace(H) - log(det(H)) - p) * n
#  df <- p*(p+1)/2 - sum(amat==1) - p
  list( bhat = beta, dhat=delta, dev=lik, df=df, lik = 2*lik)
}
=#


