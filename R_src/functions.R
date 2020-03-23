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



#' Adjacency matrix of a graph
#' 
#' Transforms the ``edge matrix'' of a graph into the adjacency matrix.
#' 
#' Given the edge matrix \eqn{A} of a graph, this can be transformed into an
#' adjacency matrix \eqn{E} with the formula \eqn{E = (A-I)^T}.
#' 
#' @param A a square matrix representing the edge matrix of a graph.
#' @return \item{E}{the adjacency matrix of the graph.}
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{edgematrix}}
#' @keywords array algebra graphs multivariate
#' @examples
#' 
#' amat <- DAG(y ~ x+z, z~u+v)
#' E <- edgematrix(amat)
#' adjMatrix(E)
#' 
"adjMatrix" <-
function (A) 
{
### From the edge matrix to the adjacency matrix
  E <- t(A)
  diag(E) <- 0
  E
}



#' All edges of a graph
#' 
#' Finds the set of edges of a graph. That is the set of undirected edges if
#' the graph is undirected and the set of arrows if the graph is directed.
#' 
#' 
#' @param amat a square Boolean matrix, with dimnames, the adjacency matrix of
#' a graph.
#' @return a matrix with two columns. Each row of the matrix is a pair of
#' indices indicating an edge of the graph. If the graph is undirected, then
#' only one of the pairs \eqn{(i,j), (j,i)} is reported.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{cycleMatrix}}
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## A UG graph
#' allEdges(UG(~ y*v*k +v*k*d+y*d))
#' 
#' ## A DAG
#' allEdges(DAG(u~h+o+p, h~o, o~p))
#' 
"allEdges" <-
function(amat){
### Finds all the edges of a graph with edge matrix amat.
    nn <- 1:nrow(amat)
    E <- c()
    if(all(amat == t(amat))) { 
      amat[lower.tri(amat)] <- 0
    }
    for(i in nn) {
      e <- nn[amat[i,] == 1]
      if(length(e) == 0) next
      li <- cbind(i,  e)
      dimnames(li) <- list(rep("", length(e)), rep("", 2))
      E <- rbind(E, li) 
    }
    E
  }



#' Basis set of a DAG
#' 
#' Finds a basis set for the conditional independencies implied by a directed
#' acyclic graph, that is a minimal set of independencies that imply all the
#' other ones.
#' 
#' Given a DAG and a pair of non adjacent nodes \eqn{(i,j)} such that \eqn{j}
#' has higher causal order than \eqn{i}, the set of independency statements
#' \eqn{i} independent of \eqn{j} given the union of the parents of both
#' \eqn{i} and \eqn{j} is a basis set (see Shipley, 2000). This basis set has
#' the property to lead to independent test statistics.
#' 
#' @param amat a square matrix with dimnames representing the adjacency matrix
#' of a DAG.
#' @return a list of vectors representing several conditional independence
#' statements. Each vector contains the names of two non adjacent nodes
#' followed by the names of nodes in the conditioning set (which may be empty).
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{shipley.test}}, \code{\link{dSep}}, \code{\link{DAG}}
#' @references Shipley, B. (2000). A new inferential test for path models based
#' on directed acyclic graphs. \emph{Structural Equation Modeling}, 7(2),
#' 206--218.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## See Shipley (2000), Figure 2, p. 213
#' A <- DAG(x5~ x3+x4, x3~ x2, x4~x2, x2~ x1)
#' basiSet(A)
#' 
"basiSet" <-
function(amat){
### Basis set of a DAG with adjacency matrix amat.
    amat <- topSort(amat)
    nod <- rownames(amat)
    dv <- length(nod)
    ind <- NULL
    ## NOTE. This is correct if the adj mat is upper triangular.
    for(r in 1:dv){
      for(s in r:dv) {
        if((amat[r,s] != 0) | (s==r))
          next
        else{
          ed <- nod[c(r,s)]
          pa.r <- nod[amat[,r] == 1]
          pa.s <- nod[amat[,s] == 1] 
          dsep <- union(pa.r, pa.s) 
          dsep <- setdiff(dsep, ed)
          b <- list(c(ed, dsep))
          ind <- c(ind, b)
        }
      }
    }
    ##      ind <- lapply(ind, function(x) nn[x])
    ind
  }

"bd" <-
function (nn, amat) 
{
### Boundary of the nodes nn for a graph with adjacency matrix amat.
  nod <- rownames(amat)
  if(is.null(nod)) stop("The edge matrix must have dimnames!")
  if(!all(is.element(nn, nod))) stop("Some of the nodes are not among the vertices.")
  b <- vector(length(nn), mode="list")
  diag(amat) <- 0  # As you do not want the node itself in the list
  k <- length(nn)
  for(i in 1:k) {
    b[[i]] <- c(  nod[amat[nn[i], ]==1 ],nod[amat[,nn[i]]==1 ] )
  }
  b <- unique(unlist(b))
  setdiff(b, nn)
}



#' Breadth first search
#' 
#' Breadth-first search of a connected undirected graph.
#' 
#' Breadth-first search is a systematic method for exploring a graph.  The
#' algorithm is taken from Aho, Hopcroft \& Ullman (1983).
#' 
#' @param amat a symmetric matrix with dimnames specifying the adjacency matrix
#' of the undirected graph
#' @param v an integer, indicating the starting node of the search. Defaults to
#' the first node.
#' @return \item{tree}{the edge matrix of the resulting spanning tree}
#' \item{branches}{a matrix with two columns, giving the indices of the
#' branches of the spanning tree} \item{chords}{a matrix with two columns,
#' giving the indices of the chords of the spanning tree}
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{UG}}, \code{\link{findPath}}, \code{\link{cycleMatrix}}
#' @references Aho, A.V., Hopcrtoft, J.E. \& Ullman, J.D. (1983). \emph{Data
#' structures and algorithms.} Reading: Addison-Wesley.
#' 
#' Thulasiraman, K. \& Swamy, M.N.S. (1992). \emph{Graphs: theory and
#' algorithms}. New York: Wiley.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## Finding a spanning tree of the butterfly graph
#' bfsearch(UG(~ a*b*o + o*u*j))
#' ## Starting from another node
#' bfsearch(UG(~ a*b*o + o*u*j), v=3)
#' 
"bfsearch" <-
function(amat, v=1) {
### Breadth-first search of a connected UG with adjacency matrix amat.
    n <- nrow(amat)
    indices <- 1:n
    if(n==1) return(NULL)
    visited <- rep(0, n)
    Q <- c()
    tree <- matrix(0, n,n)
    dimnames(tree) <- dimnames(amat)
    E <- c()
    visited[v] <- 1
    Q <- c(v, Q)
    while(!all(visited==1)){
      x <- Q[1]
      Q <- Q[-1]
     ## b <- bd(x, amat)
      b <- indices[amat[x,]==1] # Boundary of x. Assumes that amat is symmetric
      for(y in b){
        if(visited[y] == 0){
          visited[y] <- 1
          Q <- c(Q, y)
          tree[x,y]<- 1 ; tree[y,x] <- 1
          E <- rbind(E, c(x,y))
        }
      }
    }
    cross <- amat - tree
    V <- allEdges(cross)
    dimnames(E) <- list(rep("", nrow(E)), rep("", 2))
    list(tree = tree, branches = E,chords = V ) 
  }

"ch" <-
function (nn, amat) 
{
### List of the children of nodes nn for a given with adjacency matrix amat.
  nod <- rownames(amat)
  if(is.null(nod)) stop("The adjacency matrix must have dimnames!")
  if(!all(is.element(nn, nod))) stop("Some of the nodes are not among the vertices.")
  k <- length(nn)
  p <- vector(k, mode="list")
  A <- 0 + ((amat != t(amat)) & (amat == 1)) # Select the directed edges
  for(i in 1:k) {
    p[[i]] <- nod[A[nn[i],]==1 ]
  }
  setdiff(unique(unlist(p)), nn)
}



#' Identifiability of a model with one latent variable
#' 
#' Checks four sufficient conditions for identifiability of a Gaussian DAG
#' model with one latent variable.
#' 
#' Stanghellini and Wermuth (2005) give some sufficient conditions for checking
#' if a Gaussian model that factorizes according to a DAG is identified when
#' there is one hidden node over which we marginalize.  Specifically, the
#' function checks the conditions of Theorem 1, (i) and (ii) and of Theorem 2
#' (i) and (ii).
#' 
#' @param amat a square matrix with dimnames, representing the adjacency matrix
#' of a DAG.
#' @param latent an integer representing the latent variables among the nodes,
#' or the name of the node.
#' @return a vector of length four, indicating if the model is identified
#' according to the conditions of theorems 1 and 2 in Stanghellini \& Wermuth
#' (2005). The answer is \code{TRUE} if the condition holds and thus the model
#' is globally identified or \code{FALSE} if the condition fails, and thus we
#' do not know if the model is identifiable.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{isGident}}, \code{\link{InducedGraphs}}
#' @references Stanghellini, E. \& Wermuth, N. (2005). On the identification of
#' path-analysis models with one hidden variable. \emph{Biometrika}, 92(2),
#' 337-350.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## See DAG in Figure 4 (a) in Stanghellini & Wermuth (2005)
#' d <- DAG(y1 ~ y3, y2 ~ y3 + y5, y3 ~ y4 + y5, y4 ~ y6)
#' checkIdent(d, "y3")  # Identifiable
#' checkIdent(d, "y4")  # Not identifiable?
#' 
#' ## See DAG in Figure 5 (a) in Stanghellini & Wermuth (2005)
#' d <- DAG(y1 ~ y5+y4, y2 ~ y5+y4, y3 ~ y5+y4)
#' checkIdent(d, "y4")  # Identifiable
#' checkIdent(d, "y5")  # Identifiable
#' 
#' ## A simple function to check identifiability for each node
#' 
#' is.ident <- function(amat){
#' ### Check suff. conditions on each node of a DAG.
#'    p <- nrow(amat)
#'    ## Degrees of freedom
#'      df <- p*(p+1)/2 - p  - sum(amat==1) - p + 1
#'    if(df <= 0)
#'        warning(paste("The degrees of freedom are ", df))
#'     a <- rownames(amat)
#'     for(i in a) {
#'       b <- checkIdent(amat, latent=i)
#'       if(TRUE %in% b)
#'         cat("Node", i, names(b)[!is.na(b)], "\n")
#'       else
#'         cat("Unknown.\n")
#'     }
#'   }
#' 
`checkIdent` <- function(amat, latent) {
### Checks SW sufficient conditions for identifiability of a DAG
### with adjacency matrix edge amat and one latent variable.
   "allSubsets" <-
     function (n) 
       {
         ## Returns all subsets of n
         p <- length(n)
         H <- data.matrix(expand.grid(rep(list(1:2), p))) - 1
         H <- split(H==1, row(H))
         lapply(H, function(i) n[i])
       }
   
    nod <- rownames(amat)
    if(is.null(nod)) stop("The adjacency matrix must have dimnames.")
    gcov <- inducedCovGraph(amat, sel=nod, cond=NULL); gcov <- sign(gcov)
    L <- latent
    if(length(L) > 1)
      stop("I have an answer only for one latent variable.")
    O <- setdiff(nod, L)
    m <-  bd(L, gcov)
    ## Theorem 1
    if(length(m) > 2){ 
      G <- inducedCovGraph(amat, sel=O, cond=L); G <- sign(G)
      cond.i <- isGident(G[m,m,drop=FALSE])
    }
    else
      cond.i <- FALSE
    gcon <- inducedConGraph(amat, sel=nod, cond=NULL) ; gcon <- sign(gcon)
    cc <- bd(L, gcon)
    if(length(cc) > 2) {
      cond.ii <- isGident(gcon[cc, cc, drop=FALSE])
    } 
    else  
      cond.ii <- FALSE
    ## Theorem 2 (revised)
    a <- union(pa(L, amat), ch(L, amat))
    if(length(a) > 2){
      Oa <- setdiff(O, a)   # O \ a
      S.a <- inducedCovGraph(amat, sel=union(Oa, L), cond=a); S.a <- sign(S.a)
      first <- S.a[Oa, L]
      first <- Oa[first==0]  # variables that satisfy condition (i) of thm. 2.
      if(length(first)==0){
        cond.iii <- FALSE
      }
      else {
        cond.iii <- FALSE
        H <- allSubsets(first) # in all subsets of these variables
        for(h in H){           # look for a G-identifiable cov. or con. graph
          isgid <- isGident(sign(inducedCovGraph(amat, sel=a, cond=union(L, h))))
          if(isgid){
            cond.iii <- TRUE
            break
          }
          else{
            isgid <- isGident(sign(inducedConGraph(amat, sel=a, cond=union(L, h))))
            if(isgid){
              cond.iii <- TRUE
              break
            }
          }
        }
      }
      second <- setdiff(O,m) # variables that satisfy condition (ii) of thm. 2.
      if(length(second)==0){
        cond.iv <- FALSE
      }
      else {
        cond.iv <- FALSE
        H <- allSubsets(second)  # in all subsets of these variables
        for(h in H){             # look for a G-identifiable cov. or con. graph
          isgid <- isGident(sign(inducedCovGraph(amat, sel=a, cond=union(L, h))))
          if(isgid){
            cond.iv <- TRUE
            break
          }
          else{
            isgid <- isGident(sign(inducedConGraph(amat, sel=a, cond=union(L, h))))
            if(isgid){
              cond.iv <- TRUE
              break
            }
          }
        }
      }  
    }
    else{
      cond.iii <- FALSE
      cond.iv <- FALSE
    }
   c(T1.i = cond.i, T1.ii = cond.ii,
     T2.i = cond.iii, T2.ii = cond.iv)
 }




#' The complementary graph
#' 
#' Finds the complementary graph of an undirected graph.
#' 
#' The complementary graph of an UG is the graph that has the same set of nodes
#' and an undirected edge connecting \eqn{i} and \eqn{j} whenever there is not
#' an \eqn{(i,j)} edge in the original UG.
#' 
#' @param amat the adjacency matrix of an undirected graph
#' @return the edge matrix of the complementary graph.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{UG}}, \code{\link{DAG}}
#' @references Lauritzen, S. (1996). \emph{Graphical models}. Oxford: Clarendon
#' Press.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## A chordless four-cycle
#' four <- UG(~ a*b + b*d + d*e + e*a)
#' four
#' cmpGraph(four)
#' 
"cmpGraph" <-
function(amat){
### Adjacency matrix of the complementary graph
    g <- 1*!amat
    diag(g) <- 0
    g
  }



#' Connectivity components
#' 
#' Finds the connectivity components of a graph.
#' 
#' 
#' @param amat a square matrix with dimnames, the adjacency matrix of an UG.
#' @param method an integer 1 or 2 to choose the method used to find the
#' components. Method 2 is more efficient for large graphs.
#' @return an integer vector representing a partition of the set of nodes.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{UG}}
#' @references Lauritzen, S. (1996). \emph{Graphical models}. Oxford: Clarendon
#' Press.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## three connected components
#' conComp(UG(~a*c+c*d+e+g*o*u))
#' ## a connected graph
#' conComp(UG(~ a*b+b*c+c*d+d*a))
#' 
`conComp` <-  function (amat, method = 1) 
### Finds the connected components of an UG graph from its adjacency matrix amat. 
{
    if (!all(amat == t(amat))) 
       stop("Not an undirected graph.")
  if(method == 2){
  		u <- clusters(graph.adjacency(amat, mode="undirected"))$membership + 1
    	return(u)
  	}
  	else if (method == 1){
    	A <- transClos(amat)
    	diag(A) <- 1
    	n <- nrow(A)
    	A <- sign(A + t(A))
    	u <- A %*% 2^((n - 1):0)
	 	return(match(u, unique(u)))
	}
	else{ stop("Wrong method.")}
}



#' Marginal and partial correlations
#' 
#' Computes a correlation matrix with ones along the diagonal, marginal
#' correlations in the lower triangle and partial correlations given all
#' remaining variables in the upper triangle.
#' 
#' 
#' @param x a square symmetric matrix, a covariance matrix, or a data.frame for
#' n observations and p variables.
#' @return a square correlation matrix with marginal correlations (lower
#' triangle) and partial correlations (upper triangle).
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{parcor}}, \code{\link{cor}}
#' @references Cox, D. R. \& Wermuth, N. (1996). \emph{Multivariate
#' dependencies}. London: Chapman \& Hall.
#' @keywords array graphs models multivariate
#' @examples
#' 
#' ## See Table 6.1 in Cox & Wermuth (1996)
#' data(glucose)
#' correlations(glucose)
#' 
"correlations" <-
function (x)
{
### Marginal correlations (lower half) and
### partial correlations given all remaining variables (upper half).
  
  if(is.data.frame(x))
    r <- cor(x)
  else  { # Recomputes the corr matrix
    Dg <- 1/sqrt(diag(x))
    r <- x * outer(Dg, Dg)
  }
  rp <- parcor(r)
  r[upper.tri(r)] <- rp[upper.tri(rp)]
  r
}



#' Fundamental cycles
#' 
#' Finds the matrix of fundamental cycles of a connected undirected graph.
#' 
#' All the cycles in an UG can be obtained from combination (ring sum) of the
#' set of fundamental cycles. The matrix of fundamental cycles is a Boolean
#' matrix having as rows the fundamental cycles and as columns the edges of the
#' graph. If an entry is one then the edge associated to that column belongs to
#' the cycle associated to the row.
#' 
#' @param amat a symmetric matrix with dimnames denoting the adjacency matrix
#' of the undirected graph. The graph must be connected, otherwise the function
#' returns an error message.
#' @return a Boolean matrix of the fundamental cycles of the undirected graph.
#' If there is no cycle the function returns \code{NULL}.
#' @note This function is used by \code{isGident}. The row sum of the matrix
#' gives the length of the cycles.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{UG}}, \code{\link{findPath}}, \code{\link{fundCycles}},
#' \code{\link{isGident}}, \code{\link{bfsearch}}
#' @references Thulasiraman, K. \& Swamy, M.N.S. (1992). \emph{Graphs: theory
#' and algorithms}. New York: Wiley.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## Three cycles
#' cycleMatrix(UG(~a*b*d+d*e+e*a*f))
#' ## No cycle
#'  cycleMatrix(UG(~a*b))
#' ## two cycles: the first is even and the second is odd
#' cm <- cycleMatrix(UG(~a*b+b*c+c*d+d*a+a*u*v))
#' apply(cm, 1, sum)
#' 
"cycleMatrix" <-
function(amat){
### Fundamental Cycle matrix of the UG amat.
    fc <- fundCycles(amat)  # Edges of the fundamental cycles
    E <- allEdges(amat)     # All the edges of the graph
    n <- nrow(E)            # Number of edges
    k <- length(fc)         # Number of FC
    if(k == 0) return(NULL)
    cmat <- matrix(0, k, n)
    for(cy in 1:k) {
      M <- fc[[cy]]         # Edges in cycle cy
      for(j in 1:nrow(M)) {
        e <- sort(M[j,])   
        for(i in 1:n){          
          cmat[cy, i] <- cmat[cy, i] | all(E[i,] == e)
        }
      }
    }
    dimnames(cmat) <- list(1:k, paste(E[,1], E[,2]))
    cmat       
  }



#' Directed acyclic graphs (DAGs)
#' 
#' A simple way to define a DAG by means of regression model formulae.
#' 
#' The DAG is defined by a sequence of recursive regression models.  Each
#' regression is defined by a model formula.  For each formula the response
#' defines a node of the graph and the explanatory variables the parents of
#' that node. If the regressions are not recursive the function returns an
#' error message.
#' 
#' Some authors prefer the terminology acyclic directed graphs (ADG).
#' 
#' @param \dots a sequence of model formulae
#' @param order logical, defaulting to \code{FALSE}. If \code{TRUE} the nodes
#' of the DAG are permuted according to the topological order. If \code{FALSE}
#' the nodes are in the order they first appear in the model formulae (from
#' left to right).
#' @return the adjacency matrix of the DAG, i.e.  a square Boolean matrix of
#' order equal to the number of nodes of the graph and a one in position
#' \eqn{(i,j)} if there is an arrow from \eqn{i} to \eqn{j} and zero otherwise.
#' The rownames of the adjacency matrix are the nodes of the DAG.
#' 
#' If \code{order = TRUE} the adjacency matrix is permuted to have parents
#' before children.  This can always be done (in more than one way) for DAGs.
#' The resulting adjacency matrix is upper triangular.
#' @note The model formulae may contain interactions, but they are ignored in
#' the graph.
#' @author G. M. Marchetti
#' @seealso \code{\link{UG}}, \code{\link{topSort}}, \code{\link{edgematrix}},
#' \code{\link{fitDag}}
#' @references Lauritzen, S. (1996). \emph{Graphical models}. Oxford: Clarendon
#' Press.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## A Markov chain
#' DAG(y ~ x, x ~ z, z ~ u)
#' 
#' ## Another DAG
#' DAG(y ~ x + z + u, x ~ u, z ~ u)
#' 
#' ## A DAG with an isolated node
#' DAG(v ~ v, y ~ x + z, z ~ w + u)
#' 
#' ## There can be repetitions
#' DAG(y ~ x + u + v, y ~ z, u ~ v + z)
#' 
#' ## Interactions are ignored
#' DAG(y ~ x*z + z*v, x ~ z)
#' 
#' ## A cyclic graph returns an error!
#' \dontrun{DAG(y ~ x, x ~ z, z ~ y)}
#' 
#' ## The order can be changed
#' DAG(y ~ z, y ~ x + u + v,  u ~ v + z)
#' 
#' ## If you want to order the nodes (topological sort of the DAG)
#' DAG(y ~ z, y ~ x + u + v,  u ~ v + z, order=TRUE)
#' 
"DAG" <-
function (...,order=FALSE) 
{
### Defines a DAG from a set of equations (defined with model formulae).
  f <- list(...)
  nb <- length(f)  # nb is the number of model formulae (of blocks)
  nod <- c()       # Counts the number of nodes
  for (k in 1:nb) {
    tt <- terms(f[[k]], specials="I")
    vars <- dimnames(attr(tt, "factors"))[[1]]
    skip <-  attr(tt, "specials")$I 
    if(!is.null(skip))
         vars <- vars[-skip]
    nod <- c(nod, vars)
  }
  N <- unique(nod) # set of nodes
  dN <- length(N)  # number of nodes
  amat <- matrix(0,dN,dN)
  for (k in 1:nb) {
    tt <- terms(f[[k]], specials = "I")      
    vars <- dimnames(attr(tt, "factors"))[[1]]   
    if (attr(tt, "response") == 1) {
      j <- match(vars[1], N)
      i <- match(vars[-1], N)
      amat[i, j] <- 1
    }
    else if (attr(tt, "response") == 0) 
      stop("Some equations have no response")
  }
    if(!isAcyclic(amat))
      warning("The graph contains directed cycles!")
  dimnames(amat) <- list(N, N)
  if(order){
    amat <- topSort(amat)
  }
  amat
}



#' Drawing a graph with a simple point and click interface.
#' 
#' Draw a graph from its adjacency matrix representation.
#' 
#' The function is a very simple tool useful for displaying small graphs, with
#' a rudimentary interface for moving nodes and edges of a given graph and
#' adjusting the final plot. For better displays use \pkg{dynamicGraph} or
#' \pkg{Rgraphviz} package in Bioconductor project.
#' 
#' @param amat the adjacency matrix representation of the graph. This can be an
#' undirected graph, a directed acyclic graph or a mixed graph with at most a
#' summary graph structure. See also \code{\link{plotGraph}}
#' @param coor an optional matrix of dimensions \code{p} x 2 where \eqn{p} is
#' the number of vertices of the graph. If \code{coor=NULL} then the function
#' chooses a default position for the nodes.
#' @param adjust a logical value, defaults to \code{FALSE}. If \code{TRUE} the
#' graph is plotted and the system waits until the mouse button is pressed
#' (same behaviour of \code{locator} function.
#' @param alpha a positive value between controlling the distance from the end
#' of the edges to the nodes of the graph.
#' @param beta a positive value controlling the distance of the labels of the
#' variables from the nodes.
#' @param lwd line width of the edges (default: 1).
#' @param ecol color of the edges (default: "blue").
#' @param bda bidirected edge arrow length (default: 0.1).
#' @param layout The name of a function used to compute the (initial) layout of
#' the graph. The default is \code{layout.auto}. This can be further adjusted
#' if \code{adjust} is \code{TRUE}.
#' @return The function plots the graph with a initial positioning of the
#' nodes, as specified by \code{coor} and remains in a waiting state.  The
#' position of each node can be shifted by pointing and clicking (with the
#' first mouse button) close to the node.  When the mouse button is pressed the
#' node which is closer to the selected point is moved to that position.  Thus,
#' one must be careful to click closer to the selected node than to any other
#' node.  The nodes can be moved to any position by repeating the previous
#' operation.  The adjustment process is terminated by pressing any mouse
#' button other than the first.
#' 
#' At the end of the process, the function returns invisibly the coordinates of
#' the nodes. The coordinates may be used later to redisplay the graph.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{UG}}, \code{\link{DAG}}, \code{\link{makeMG}},
#' \code{\link{plotGraph}}
#' @references \pkg{dynamicGraph}, \pkg{Rgraphwiz},
#' \url{http://www.bioconductor.org}.
#' 
#' GraphViz, Graph Visualization Project. AT\&T Research.
#' \url{http://www.graphviz.org}.
#' @keywords graphs hplot iplot
#' @examples
#' 
#' ## A directed acyclic graph
#' d <- DAG(y1 ~ y2+y6, y2 ~ y3, y3 ~ y5+y6, y4 ~ y5+y6)
#' \dontrun{drawGraph(d)}
#' 
#' ## An undirected graph
#' g <- UG(~giova*anto*armo + anto*arj*sara) 
#' \dontrun{drawGraph(d)}
#' 
#' ## An ancestral graph
#' ag <- makeMG(ug=UG(~y0*y1), dg=DAG(y4~y2, y2~y1), bg=UG(~y2*y3+y3*y4))
#' drawGraph(ag, adjust = FALSE)
#' drawGraph(ag, adjust = FALSE)
#' 
#' ## A more complex example with coordinates: the UNIX evolution
#' xy <-
#' structure(c(5, 15, 23, 25, 26, 17, 8, 6, 6, 7, 39, 33, 23, 49, 
#' 19, 34, 13, 29, 50, 68, 70, 86, 89, 64, 81, 45, 64, 49, 64, 87, 
#' 65, 65, 44, 37, 64, 68, 73, 85, 83, 95, 84, 0, 7, 15, 27, 44, 
#' 37, 36, 20, 51, 65, 44, 64, 59, 73, 69, 78, 81, 90, 97, 89, 72, 
#' 85, 74, 62, 68, 59, 52, 48, 43, 50, 34, 21, 18, 5, 1, 10, 2, 
#' 11, 2, 1, 44), .Dim = c(41, 2), .Dimnames = list(NULL, c("x", 
#' "y")))
#' Unix <- DAG(
#'                 SystemV.3 ~ SystemV.2,
#'                 SystemV.2 ~ SystemV.0,
#'                 SystemV.0 ~ TS4.0,
#'                 TS4.0 ~ Unix.TS3.0 + Unix.TS.PP + CB.Unix.3,
#'                 PDP11.SysV ~ CB.Unix.3,
#'                 CB.Unix.3 ~ CB.Unix.2,
#'                 CB.Unix.2 ~ CB.Unix.1,
#'                 Unix.TS.PP ~ CB.Unix.3,
#'                 Unix.TS3.0 ~ Unix.TS1.0 + PWB2.0 + USG3.0 + Interdata,
#'                 USG3.0 ~ USG2.0,
#'                 PWB2.0 ~ Interdata + PWB1.2,
#'                 USG2.0 ~ USG1.0,
#'                 CB.Unix.1 ~ USG1.0,
#'                 PWB1.2 ~ PWB1.0,
#'                 USG1.0 ~ PWB1.0,
#'                 PWB1.0 ~ FifthEd,
#'                 SixthEd ~ FifthEd,
#'                 LSX ~ SixthEd,
#'                 MiniUnix ~ SixthEd,
#'                 Interdata ~ SixthEd,
#'                 Wollongong ~ SixthEd,
#'                 SeventhEd ~ Interdata,
#'                 BSD1 ~ SixthEd,
#'                 Xenix ~ SeventhEd,
#'                 V32 ~ SeventhEd,
#'                 Uniplus ~ SeventhEd,
#'                 BSD3 ~ V32,
#'                 BSD2 ~ BSD1,
#'                 BSD4 ~ BSD3,
#'                 BSD4.1 ~ BSD4,
#'                 EigthEd ~ SeventhEd + BSD4.1,
#'                 NinethEd ~ EigthEd,
#'                 Ultrix32 ~ BSD4.2,
#'                 BSD4.2 ~ BSD4.1,
#'                 BSD4.3 ~ BSD4.2,
#'                 BSD2.8 ~ BSD4.1 + BSD2,
#'                 BSD2.9 ~ BSD2.8,
#'                 Ultrix11 ~ BSD2.8 + V7M + SeventhEd,
#'                 V7M ~ SeventhEd
#'                 )
#' drawGraph(Unix, coor=xy, adjust=FALSE)
#' # dev.print(file="unix.fig", device=xfig) # Edit the graph with Xfig
#' 
`drawGraph` <- function (amat, coor = NULL, adjust = FALSE, alpha = 1.5, beta = 3, 
    lwd = 1, ecol = "blue", bda = 0.1, layout = layout.auto) 
{
    if (is.null(dimnames(amat))) {
        rownames(a) <- 1:ncol(amat)
        colnames(a) <- 1:ncol(amat)
    }
    if (all(amat == t(amat)) & all(amat[amat != 0] == 1)) {
        amat <- amat * 10
    }
    `lay` = function(a, directed  = TRUE, start = layout){
        if (class(a)[1] == "igraph" || class(a)[1] == "graphNEL" || class(a)[1] == 
                "character") {
            a <- grMAT(a)
        }
        if (is(a,"matrix")) {
            if (nrow(a) == ncol(a)) {
                if (length(rownames(a)) != ncol(a)) {
                    rownames(a) <- 1:ncol(a)
                    colnames(a) <- 1:ncol(a)
                }
                if (!directed) {
                    if (all(a == t(a)) & all(a[a != 0] == 1)) {
                        a <- a * 10
                    }
                }
                l1 <- c()
                l2 <- c()
                for (i in 1:nrow(a)) {
                    for (j in i:nrow(a)) {
                        if (a[i, j] == 1) {
                            l1 <- c(l1, i, j)
                            l2 <- c(l2, 2)
                        }
                        if (a[j, i]%%10 == 1) {
                            l1 <- c(l1, j, i)
                            l2 <- c(l2, 2)
                        }
                        if (a[i, j] == 10) {
                            l1 <- c(l1, i, j)
                            l2 <- c(l2, 0)
                        }
                        if (a[i, j] == 11) {
                            l1 <- c(l1, i, j, i, j)
                            l2 <- c(l2, 2, 0)
                        }
                        if (a[i, j] == 100) {
                            l1 <- c(l1, i, j)
                            l2 <- c(l2, 3)
                        }
                        if (a[i, j] == 101) {
                            l1 <- c(l1, i, j, i, j)
                            l2 <- c(l2, 2, 3)
                        }
                        if (a[i, j] == 110) {
                            l1 <- c(l1, i, j, i, j)
                            l2 <- c(l2, 0, 3)
                        }
                        if (a[i, j] == 111) {
                            l1 <- c(l1, i, j, i, j, i, j)
                            l2 <- c(l2, 2, 0, 3)
                        }
                    }
                }
            }
            else {
                stop("'object' is not in a valid adjacency matrix form")
            }
            if (length(l1) > 0) {
                ## l1 <- l1 - 1   # igraph0
                agr <- graph(l1, n = nrow(a), directed = TRUE)
            }
        }
        else {
            stop("'object' is not in a valid format")
        }
        x = start(agr)
        x[,1] =  10 + 80 * (x[,1] - min(x[,1])) / (max(x[,1]) - min(x[,1]))
        x[,2] = 10 + 80 * (x[,2] - min(x[,2])) / (max(x[,2]) - min(x[,2]))
        x
    }
    plot.dots <- function(xy, v, dottype, n, beta) {
        for (i in 1:n) {
            if (dottype[i] == 1) {
                points(xy[i, 1], xy[i, 2], pch = 1, cex = 1.2, 
                  lwd = lwd)
            }
            else if (dottype[i] == 2) {
                points(xy[i, 1], xy[i, 2], pch = 16, cex = 1.2)
            }
        }
        text(xy[, 1] - beta, xy[, 2] + 2*beta, v, cex = 1.2)
    }
    angle <- function(v, alpha = pi/2) {
        theta <- Arg(complex(real = v[1], imaginary = v[2]))
        z <- complex(argument = theta + alpha)
        c(Re(z), Im(z))
    }
    double.edges <- function(x1, x2, y1, y2, lwd, ecol) {
        d <- 50
        n <- 30
        dd <- 2
        k <- length(x1)
        if (is.na(x1)) 
            return()
        for (i in 1:k) {
            x <- c(x1[i], x2[i])
            y <- c(y1[i], y2[i])
            m <- (x + y)/2
            cen <- m + d * angle(y - x)
            xm <- x - cen
            ym <- y - cen
            thetax <- Arg(complex(real = xm[1], imaginary = xm[2]))
            thetay <- Arg(complex(real = ym[1], imaginary = ym[2]))
            theta <- seq(thetax, thetay, len = n)
            l <- crossprod(y - m)
            delta <- sqrt(d^2 + l)
            lx <- cen[1] + delta * cos(theta)
            ly <- cen[2] + delta * sin(theta)
            lines(lx, ly, lty = 2, col = ecol, lwd = lwd)
            vy <- angle(y - cen)
            vx <- angle(x - cen)
            vx1 <- x + dd * angle(vx, alpha = pi/12)
            vx2 <- x + dd * angle(vx, alpha = -pi/12)
            vy1 <- y + dd * angle(vy, alpha = 11 * pi/12)
            vy2 <- y + dd * angle(vy, alpha = -11 * pi/12)
            segments(x[1], x[2], vx1[1], vx1[2], col = ecol, 
                lwd = lwd)
            segments(x[1], x[2], vx2[1], vx2[2], col = ecol, 
                lwd = lwd)
            segments(y[1], y[2], vy1[1], vy1[2], col = ecol, 
                lwd = lwd)
            segments(y[1], y[2], vy2[1], vy2[2], col = ecol, 
                lwd = lwd)
            ex = x + 0.05 * (y - x)
            ey = x + 0.95 * (y - x)
            arrows(ex[1], ex[2], ey[1], ey[2], lty = 1, code = 1, 
                angle = 20, length = 0.1, lwd = lwd, col = ecol)
        }
    }
    draw.edges <- function(coor, u, alpha, type, lwd, ecol, bda) {
        for (k in 1:nrow(u)) {
            a <- coor[u[k, 1], ]
            b <- coor[u[k, 2], ]
            ba <- b - a
            ba <- ba/sqrt(sum(ba * ba))
            x <- a + ba * alpha
            y <- b - ba * alpha
            switch(type + 1, segments(x[1], x[2], y[1], y[2], 
                lty = 1, lwd = lwd, col = ecol), arrows(x[1], 
                x[2], y[1], y[2], code = 2, angle = 20, length = 0.1, 
                lty = 1, lwd = lwd, col = ecol), arrows(x[1], 
                x[2], y[1], y[2], code = 3, angle = 20, length = bda, 
                lty = 5, lwd = lwd, col = ecol), double.edges(x[1], 
                x[2], y[1], y[2], lwd = lwd, ecol))
        }
    }
#     def.coor <- function(ce, k, h, w) {
#         if (k == 1) 
#             return(ce)
#         else if (k == 2) {
#             r1 <- c(ce[1], ce[1])
#             r2 <- c(ce[2] + h * 0.3, ce[2] - h * 0.3)
#         }
#         else if (k == 3) {
#             r1 <- c(ce[1], ce[1], ce[1])
#             r2 <- c(ce[2] + h * 0.25, ce[2], ce[2] - h * 0.25)
#         }
#         else if (k == 4) {
#             r1 <- c(ce[1] - w * 0.3, ce[1] + w * 0.3, ce[1] + 
#                 w * 0.3, ce[1] - w * 0.3)
#             r2 <- c(ce[2] - h * 0.3, ce[2] - h * 0.3, ce[2] + 
#                 h * 0.3, ce[2] + h * 0.3)
#         }
#         else {
#             a <- 1
#             z <- seq(a, a + 2 * pi, len = k + 1)
#             z <- z[-1]
#             r1 <- ce[1] + w/2.5 * cos(z)
#             r2 <- ce[2] + h/2.5 * sin(z)
#         }
#         cbind(r1, r2)
#     }
#     def.coor.dag <- function(amat, w, h, left) {
#         nod <- rownames(amat)
#         o <- topOrder(amat)
#         if (left) 
#             o <- rev(o)
#         k <- length(nod)
#         x <- seq(0, 100, len = k)
#         y <- rep(c(20, 40, 60, 80), ceiling(k/4))[1:k]
#         xy <- cbind(x, y)
#         rownames(xy) <- nod[o]
#         xy[nod, ]
#     }
    v <- parse(text = rownames(amat))
    n <- length(v)
    dottype <- rep(1, n)
    old <- par(mar = c(0, 0, 0, 0))
    on.exit(par(old))
    plot(c(0, 100), c(0, 100), type = "n", axes = FALSE, xlab = "", 
        ylab = "")
    center <- matrix(c(50, 50), ncol = 2)
    if (is.null(coor)) {
        coor <- lay(amat)
#         isdag <- isAcyclic(amat)
#         if (isdag) 
#             coor <- def.coor.dag(amat, 100, 100, left = left)
#         else coor <- def.coor(center, n, 100, 100)
    }
    g0 <- amat * ((amat == 10) & (t(amat) == 10))
    g0[lower.tri(g0)] <- 0
    g1 <- amat * ((amat == 1) & !((amat > 0) & (t(amat) > 0)))
    g2 <- amat * ((amat == 100) & (t(amat) == 100))
    g2[lower.tri(g2)] <- 0
    g3 <- (amat == 101) + 0
    i <- expand.grid(1:n, 1:n)
    u0 <- i[g0 > 0, ]
    u1 <- i[g1 > 0, ]
    u2 <- i[g2 > 0, ]
    u3 <- i[g3 > 0, ]
    if (nrow(coor) != length(v)) 
        stop("Wrong coordinates of the vertices.")
    plot.dots(coor, v, dottype, n, beta)
    draw.edges(coor, u0, alpha, type = 0, lwd = lwd, ecol, bda)
    draw.edges(coor, u1, alpha, type = 1, lwd = lwd, ecol, bda)
    draw.edges(coor, u2, alpha, type = 2, lwd = lwd, ecol, bda)
    draw.edges(coor, u3, alpha, type = 3, lwd = lwd, ecol, bda)
    if (adjust) {
        repeat {
            xnew <- unlist(locator(1))
            if (length(xnew) == 0) {
                break
            }
            d2 <- (xnew[1] - coor[, 1])^2 + (xnew[2] - coor[, 
                2])^2
            i <- (1:n)[d2 == min(d2)]
            coor[i, 1] <- xnew[1]
            coor[i, 2] <- xnew[2]
            plot(c(0, 100), c(0, 100), type = "n", axes = FALSE, 
                xlab = "", ylab = "")
            plot.dots(coor, v, dottype, n, beta)
            draw.edges(coor, u0, alpha, type = 0, lwd = lwd, 
                ecol, bda)
            draw.edges(coor, u1, alpha, type = 1, lwd = lwd, 
                ecol, bda)
            draw.edges(coor, u2, alpha, type = 2, lwd = lwd, 
                ecol, bda)
            draw.edges(coor, u3, alpha, type = 3, lwd = lwd, 
                ecol, bda)
        }
    }
    colnames(coor) <- c("x", "y")
    return(invisible(coor))
}



#' d-separation
#' 
#' Determines if in a directed acyclic graph two set of nodes a d-separated by
#' a third set of nodes.
#' 
#' d-separation is a fundamental concept introduced by Pearl (1988).
#' 
#' @param amat a Boolean matrix with dimnames, representing the adjacency
#' matrix of a directed acyclic graph. The function does not check if this is
#' the case. See the function \code{isAcyclic}.
#' @param first a vector representing a subset of nodes of the DAG.  The vector
#' should be a character vector of the names of the variables matching the
#' names of the nodes in \code{rownames(A)}. It can be also a numeric vector of
#' indices.
#' @param second a vector representing another subset of nodes of the DAG.  The
#' set \code{second} must be disjoint from \code{first}.  The mode of
#' \code{second} must match the mode of \code{first}.
#' @param cond a vector representing a conditioning subset of nodes.  The set
#' \code{cond} must be disjoint from the other two sets and must share the same
#' mode.
#' @return a logical value. \code{TRUE} if \code{first} and \code{second} are
#' d-separated by \code{cond}.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{DAG}}, \code{\link{shipley.test}},
#' \code{\link{inducedCovGraph}}
#' @references Pearl, J. (1988). \emph{Probabilistic reasoning in intelligent
#' systems.} San Mateo: Morgan Kaufmann.
#' 
#' Lauritzen, S. (1996). \emph{Graphical models}. Oxford: Clarendon Press.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## Conditioning on a transition node
#' dSep(DAG(y ~ x, x ~ z), first="y", second="z", cond = "x")
#' ## Conditioning on a collision node (collider)
#' dSep(DAG(y ~ x, y ~ z), first="x", second="z", cond = "y")
#' ## Conditioning on a source node
#' dSep(DAG(y ~ x, z ~ x), first="y", second="z", cond = "x")
#' ## Marginal independence
#' dSep(DAG(y ~ x, y ~ z), first="x", second="z", cond = NULL)
#' ## The DAG defined on p.~47 of Lauritzen (1996)
#' dag <- DAG(g ~ x, h ~ x+f, f ~ b, x ~ l+d, d ~ c, c ~ a, l ~ y, y ~ b)
#' dSep(dag, first="a", second="b", cond=c("x", "y"))
#' dSep(dag, first="a", second=c("b", "d"), cond=c("x", "y"))
#' 
"dSep" <-
function(amat, first, second, cond) {
### Are first and second d-Separated by cond in a DAG? 
    e <- inducedCovGraph(amat, sel=c(first,second), cond=cond)
    all(e[first,second] == 0)
  }



#' Edge matrix of a graph
#' 
#' Transforms the adjacency matrix of a graph into an ``edge matrix''.
#' 
#' In some matrix computations for graph objects the adjacency matrix of the
#' graph is transformed into an ``edge matrix''. Briefly, if \eqn{E} is the
#' adjacency matrix of the graph, the edge matrix is \eqn{A =
#' sign(E+I)^T=[a_{ij}]}.  Thus, \eqn{A} has ones along the diagonal and if the
#' graph has no edge between nodes \eqn{i} and \eqn{j} the entries
#' \eqn{a_{i,j}} and \eqn{a_{j,i}} are both zero.  If there is an arrow from
#' \eqn{j} to \eqn{i} \eqn{a_{i,j}=1} and \eqn{a_{j,i} = 0}. If there is an
#' undirected edge, both \eqn{a_{i,j}=a_{j,i}=1}.
#' 
#' @param E a square matrix, representing the adjacency matrix of a graph.
#' @param inv a logical value.
#' @return \item{A}{the edge matrix of the graph.  If \code{TRUE} the nodes are
#' sorted in inverted topological order and the edge matrix is upper
#' triangular.}
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{adjMatrix}}
#' @references Wermuth, N. (2003). Analysing social science data with graphical
#' Markov models. In: \emph{Highly Structured Stochastic Systems.} P. Green, N.
#' Hjort \& T. Richardson (eds.), 47--52. Oxford: Oxford University Press.
#' @keywords array algebra graphs multivariate
#' @examples
#' 
#' amat <- DAG(y ~ x+z, z~u+v)
#' amat
#' edgematrix(amat)
#' edgematrix(amat, inv=TRUE)
#' 
"edgematrix" <-
function (E, inv=FALSE) 
{
### From the adjacency matrix to the edge matrix
  E <- sign(E)
  if(inv){
    ord <- topOrder(E)
    ord <- rev(ord) # Inverse topological order: Nanny ordering.
    E <- E[ord, ord]
  }
  A <- t(E)
  diag(A) <- 1
  A
}



#' Essential graph
#' 
#' Find the essential graph from a given directed acyclic graph.
#' 
#' Converts a DAG into the Essential Graph.  Is implemented by the algorithm by
#' D.M.Chickering (1995).
#' 
#' @param dagx a square binary matrix, the adjacency matrix of a directed
#' acyclic graph. The names of rows and of the columns are the nodes of the
#' DAG.
#' @return returns the adjacency matrix of the essential graph.
#' @author Giovanni M. Marchetti, translating a MATLAB function by Tomas Kocka,
#' AAU
#' @seealso \code{\link{DAG}}, \code{\link{InducedGraphs}}
#' @references Chickering, D.M. (1995). A transformational characterization of
#' equivalent Bayesian network structures. \emph{Proceedings of Eleventh
#' Conference on Uncertainty in Artificial Intelligence}, Montreal, QU, 87-98.
#' Morgan Kaufmann.
#' 
#' \url{http://research.microsoft.com/~dmax/publications/uai95.pdf}
#' @keywords graphs models multivariate
#' @examples
#' 
#' dag = DAG(U ~ Y+Z, Y~X, Z~X)
#' essentialGraph(dag)
#' 
"essentialGraph" <-
function(dagx){
### Converts a DAG into Essential Graph. 
### Is implemented by the algorithm by D.M.Chickering (1995).
### A transformational characterization of equivalent Bayesian network
### structures. Proceedings of Eleventh Conference on Uncertainty in
### Artificial Intelligence, Montreal, QU, pages 87-98. Morgan Kaufmann 
### http://research.microsoft.com/~dmax/publications/uai95.pdf 
### Implemented in Matlab by Tomas Kocka, AAU.
### Translated in R by Giovanni Marchetti, University of Florence.
  
  ord <- topOrder(dagx);      # get the topological order of nodes 
  n <- nrow(dagx)             # gets the number of nodes
  i <- expand.grid(1:n, 1:n)  # finds all nonzero elements in the adj matrix
  IJ <- i[dagx==1,]           # sort the arcs from lowest possible y
  I <- IJ[, 1]; J <- IJ[, 2]  # and highest possible x, arcs are x->y
  e <- 1
  for(y in 1:n){
    for(x in n:1){
      if(dagx[ord[x], ord[y]] == 1) { 
        I[e] <- ord[x]
        J[e] <- ord[y]
        e <- e + 1
      }
    }
  }
  ## Now we have to decide which arcs are part of the essential graph and
  ## which are undirected edges in the essential graph.
  ## Undecided arc in the DAG are 1, directed in EG are 2 and undirected in EG are 3.
  
  for(e in 1:length(I)){
    if(dagx[I[e],J[e]] == 1){
      cont <- TRUE
      for(w in 1:n){ 
        if(dagx[w,I[e]] == 2){
          if(dagx[w,J[e]] != 0)
            dagx[w,J[e]] <- 2
          else {
            for(ww in 1:n){
              if(dagx[ww,J[e]] != 0)
                      dagx[ww,J[e]] <- 2
            } # skip the rest and start with another arc from the list
            w <- n
            cont <- FALSE
          }
        }
      }
      if(cont){
        exists <- FALSE
        for(z in 1:n){
          if((dagx[z,J[e]] != 0) & (z != I[e]) & (dagx[z,I[e]] == 0)){
            exists <- TRUE
            for(ww in 1:n){
              if(dagx[ww,J[e]] == 1){
                dagx[ww,J[e]] <- 2
              }
            }
          }
        }
        if(!exists){
          for(ww in 1:n){
            if(dagx[ww,J[e]] == 1){
              dagx[ww,J[e]] <- 3
            }
          } 
        }
      }
    }          
  }
  (dagx==2) + (dagx==3) + t(dagx==3)
}



#' Finding paths
#' 
#' Finds one path between two nodes of a graph.
#' 
#' 
#' @param amat a square Boolean matrix with dimnames, the adjacency matrix of a
#' graph.
#' @param st an integer, the starting node.
#' @param en an integer, the ending node.
#' @param path a vector of integers, used in recursive calls. At the beginning
#' is \code{NULL}. It should not be modified by the user.
#' @return a vector of integers, the sequence of nodes of a path, starting from
#' \code{st} to \code{en}. In some graphs (spanning trees) there is only one
#' path between two nodes.
#' @note This function is not intended to be directly called by the user.
#' @author Giovanni M. Marchetti, translating the original \pkg{Python} code
#' (see references).
#' @seealso \code{\link{fundCycles}}
#' @references Python Softftware Foundation (2003). Python Patterns ---
#' Implementing Graphs. \url{http://www.python.org/doc/essays/graphs/}.
#' @keywords graphs
#' @examples
#' 
#' ## A (single) path on a spanning tree
#' findPath(bfsearch(UG(~ a*b*c + b*d + d*e+ e*c))$tree, st=1, en=5)
#' 
"findPath" <-
function (amat, st, en, path = c()) 
{
### Find a path between nodes st and en in a UG with adjacency mat. amat.
  indices <- 1:nrow(amat)
  if(st == en) # st is 'node' in recursive calls
    return(c(path, st))
  if(sum(amat[st,]) == 0 ) 
    return(NULL)
  ## ne <- bd(st,amat)
  ne <- indices[amat[st,]==1] # Boundary of x. Assumes that amat is symmetric
  for(node in ne){
    if(!is.element(node, c(path, st))){
      newpath <- findPath(amat, node, en, c(path, st))
      if(!is.null(newpath))
        return(newpath)
    }
  }
}



#' Fitting of Gaussian Ancestral Graph Models
#' 
#' Iterative conditional fitting of Gaussian Ancestral Graph Models.
#' 
#' In the Gaussian case, the models can be parameterized using precision
#' parameters, regression coefficients, and error covariances (compare
#' Richardson and Spirtes, 2002, Section 8). This function finds the MLE
#' \eqn{\hat \Lambda}{L} of the precision parameters by fitting a concentration
#' graph model. The MLE \eqn{\hat B}{B} of the regression coefficients and the
#' MLE \eqn{\hat\Omega}{O} of the error covariances are obtained by iterative
#' conditional fitting (Drton and Richardson, 2003, 2004). The three sets of
#' parameters are combined to the MLE \eqn{\hat\Sigma}{S} of the covariance
#' matrix by matrix multiplication: \deqn{\hat\Sigma = \hat B^{-1}(\hat
#' \Lambda+\hat\Omega)\hat }{S = B^(-1) (L+O) B^(-t).}\deqn{ B^{-T}.}{S =
#' B^(-1) (L+O) B^(-t).} Note that in Richardson and Spirtes (2002), the
#' matrices \eqn{\Lambda}{L} and \eqn{\Omega}{O} are defined as submatrices.
#' 
#' @param amat a square matrix, representing the adjacency matrix of an
#' ancestral graph.
#' @param S a symmetric positive definite matrix with row and col names, the
#' sample covariance matrix.
#' @param n the sample size, a positive integer.
#' @param tol a small positive number indicating the tolerance used in
#' convergence checks.
#' @return \item{Shat}{the fitted covariance matrix.} \item{Lhat}{matrix of the
#' fitted precisions associated with undirected edges and vertices that do not
#' have an arrowhead pointing at them.} \item{Bhat}{matrix of the fitted
#' regression coefficients associated to the directed edges.  Precisely said
#' \code{Bhat} contains ones on the diagonal and the off-diagonal entry
#' \eqn{(i,j)}{(i,j)} equals the \emph{negated} MLE of the regression
#' coefficient for variable \eqn{j}{j} in the regression of variable \eqn{i}{i}
#' on its parents. Note that this \eqn{(i,j)}{(i,j)} entry in \code{Bhat}
#' corresponds to a directed edge \eqn{j \to i}{j -> i}, and thus to a one as
#' \eqn{(j,i)}{(j,i)} entry in the adjacency matrix.} \item{Ohat}{matrix of the
#' error covariances and variances of the residuals between regression
#' equations associated with bi-directed edges and vertices with an arrowhead
#' pointing at them.} \item{dev}{the `deviance' of the model.} \item{df}{the
#' degrees of freedom.} \item{it}{the iterations.}
#' @author Mathias Drton
#' @seealso \code{\link{fitCovGraph}}, \code{\link{icf}}, \code{\link{makeMG}},
#' \code{\link{fitDag}}
#' @references Drton, M. and Richardson, T. S. (2003). A new algorithm for
#' maximum likelihood estimation in Gaussian graphical models for marginal
#' independence. \emph{Proceedings of the Nineteenth Conference on Uncertainty
#' in Artificial Intelligence}, 184-191.
#' 
#' Drton, M. and Richardson, T. S. (2004). Iterative Conditional Fitting for
#' Gaussian Ancestral Graph Models.  Proceedings of the 20th Conference on
#' Uncertainty in Artificial Intelligence, Department of Statistics, 130-137.
#' 
#' Richardson, T. S. and Spirtes, P. (2002). Ancestral Graph Markov Models.
#' \emph{Annals of Statistics}. 30(4), 962-1030.
#' @keywords graphs models ancestral graph multivariate
#' @examples
#' 
#' ## A covariance matrix
#' "S" <- structure(c(2.93, -1.7, 0.76, -0.06,
#'                   -1.7, 1.64, -0.78, 0.1,
#'                    0.76, -0.78, 1.66, -0.78,
#'                   -0.06, 0.1, -0.78, 0.81), .Dim = c(4,4),
#'                  .Dimnames = list(c("y", "x", "z", "u"), c("y", "x", "z", "u")))
#' ## The following should give the same fit.   
#' ## Fit an ancestral graph y -> x <-> z <- u
#' fitAncestralGraph(ag1 <- makeMG(dg=DAG(x~y,z~u), bg = UG(~x*z)), S, n=100)
#' 
#' ## Fit an ancestral graph y <-> x <-> z <-> u
#' fitAncestralGraph(ag2 <- makeMG(bg= UG(~y*x+x*z+z*u)), S, n=100)
#' 
#' ## Fit the same graph with fitCovGraph
#' fitCovGraph(ag2, S, n=100)    
#' 
#' ## Another example for the mathematics marks data
#' 
#' data(marks)
#' S <- var(marks)
#' mag1 <- makeMG(bg=UG(~mechanics*vectors*algebra+algebra*analysis*statistics))
#' fitAncestralGraph(mag1, S, n=88)
#' 
#' mag2 <- makeMG(ug=UG(~mechanics*vectors+analysis*statistics),
#'                dg=DAG(algebra~mechanics+vectors+analysis+statistics))
#' fitAncestralGraph(mag2, S, n=88) # Same fit as above
#' 
`fitAncestralGraph` <-
function (amat, S, n, tol = 1e-06){
### Fit Ancestral Graphs. Mathias Drton, 2003 2009. It works for ADMGs 
    nam <- rownames(S)
    nod <- rownames(amat)
    ## permute graph to have same layout as S
    if(is.null(nod)){
      stop("The adjacency matrix has no labels!")
    }
    if(!all(is.element(nod, nam)))
      stop("The nodes of the graph do not match the names of the variables")
    else
      sek <- intersect(nam, nod)
    S <- S[sek,sek, drop=FALSE]              # Resizes eventually S
    amat <- amat[sek,sek, drop=FALSE]        # and reorders amat
    
    temp <- icfmag(amat, S, tol)
    p <- ncol(S)
    df <- p*(p-1)/2 - sum(In(amat+t(amat)))/2   # Degrees of freedom 
    dev <- likGau(solve(temp$Sigmahat), S, n, p)
    if(is.null(temp$Bhat)){
      Beta <- NULL
    }
    else{
      ## Beta <- diag(1,p)-temp$Bhat
      Beta <- temp$Bhat
    }
    return(list(Shat=temp$Sigmahat, Lhat=temp$Lambdahat, Bhat=Beta,
                Ohat=temp$Omegahat, dev = dev, df = df, it=temp$iterations))
  }
               


`likGau` = function(K, S, n, k){
# deviance of the Gaussian model.
SK = S %*% K
tr = function(A) sum(diag(A))
(tr(SK) - log(det(SK)) - k) * n
}





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


#' Fitting of Gaussian covariance graph models
#' 
#' Fits a Gaussian covariance graph model by maximum likelihood.
#' 
#' A covariance graph is an undirected graph in which the variables associated
#' to two non-adjacent nodes are marginally independent. The edges of these
#' models are represented by bi-directed edges (Drton and Richardson, 2003) or
#' by dashed lines (Cox and Wermuth, 1996).
#' 
#' By default, this function gives the ML estimates in the covariance graph
#' model, by iterative conditional fitting (Drton and Richardson, 2003).
#' Otherwise, the estimates from a ``dual likelihood'' estimator can be
#' obtained (Kauermann, 1996; Edwards, 2000, section 7.4).
#' 
#' @param amat A symmetric Booloean matrix with dimnames representing the
#' adjacency matrix of an UG.
#' @param S A symmetric positive definite matrix with dimnames, the sample
#' covariance matrix.
#' @param n A positive integer, the sample size.
#' @param alg A character string, the algorithm used.  If \code{alg="icf"} (the
#' default) the algorithm is based on iterative conditional fitting (see Drton
#' and Richardson, 2003). In this case the ML estimates are returned.  If
#' \code{alg="dual"} the algorithm is based on the dual likelihood (see
#' Kauermann, 1996). The fitted values are an approximation of the ML
#' estimates.
#' @param dual.alg And integer equal to 1 or 2. It is used if
#' \code{alg="dual"}. In this case a concentration graph model is fitted to the
#' inverse of the sample covariance matrix, and \code{dual.alg} is passed to
#' \code{fitConGraph} to specify the algorithm used in \code{fitConGraph}.
#' @param start.icf A symmetric matrix used as starting value of the algorithm.
#' If \code{start=NULL} the starting value is a diagonal matrix with diagonal
#' entries equal to sample variances.
#' @param tol A small positive number indicating the tolerance used in
#' convergence tests.
#' @return \item{Shat}{the fitted covariance matrix.} \item{dev}{the `deviance'
#' of the model.} \item{df}{the degrees of freedom.} \item{it}{the iterations.}
#' @author Mathias Drton
#' @seealso \code{\link{fitConGraph}}, \code{\link{icf}}
#' @references Cox, D. R. and Wermuth, N. (1996). \emph{Multivariate
#' dependencies}. London: Chapman \& Hall.
#' 
#' Drton, M. and Richardson, T. S. (2003). A new algorithm for maximum
#' likelihood estimation in Gaussian graphical models for marginal
#' independence. \emph{Proceedings of the Nineteenth Conference on Uncertainty
#' in Artificial Intelligence}, 184--191.
#' 
#' Kauermann, G. (1996). On a dualization of graphical Gaussian models.
#' \emph{Scandinavian Journal of Statistics}.  23, 105--116.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## Correlations among four strategies to cope with stress for 
#' ## 72 students. Cox & Wermuth (1996), p. 73.
#' 
#' data(stress)
#' 
#' ## A chordless 4-cycle covariance graph
#' G <- UG(~ Y*X + X*U + U*V + V*Y)
#' 
#' fitCovGraph(G, S = stress, n=72)
#' fitCovGraph(G, S = stress, n=72, alg="dual")
#' 
`fitCovGraph` <-
  function (amat, S, n, alg="icf", dual.alg=2, start.icf=NULL, tol = 1e-06){
### Fits a Covariance Graph. Mathias Drton, 2003
### amat: adjacency matrix; S: covariance matrix; n: sample size.
    amat <- In(amat) # Forces the ones in a bidirected graph defined with makeMG
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
    
    if(alg=="icf"){
      temp <- icf(amat, S, start.icf, tol)
    }
    else{
      if(alg == "dual"){
        Sinv <- solve(S)
        temp <- fitConGraph(amat, Sinv, n, pri=FALSE, alg=dual.alg, tol = 1e-06)
        temp <- list(Sigmahat=zapsmall(solve(temp$Shat)), iterations=temp$it)
      }
      else{
        stop("Algorithm misspecified!")
      }
    }

    df <- sum(amat[upper.tri(amat)] == 0) # Degrees of freedom
    k <- ncol(S)
    dev <- likGau(solve(temp$Sigmahat), S, n, k) 
    return(list(Shat=temp$Sigmahat, dev = dev, df = df, it=temp$iterations))
}



#' Fitting of Gaussian DAG models
#' 
#' Fits linear recursive regressions with independent residuals specified by a
#' DAG.
#' 
#' \code{fitDag} checks if the order of the nodes in adjacency matrix is the
#' same of \code{S} and if not it reorders the adjacency matrix to match the
#' order of the variables in \code{S}. The nodes of the adjacency matrix may
#' form a subset of the variables in \code{S}.
#' 
#' @param amat a square matrix with dimnames representing the adjacency matrix
#' of the DAG
#' @param S a symmetric positive definite matrix, the sample covariance matrix
#' @param n an integer > 0, the sample size
#' @return \item{Shat}{the fitted covariance matrix.} \item{Ahat}{a square
#' matrix of the fitted regression coefficients. The entry \code{Ahat[i,j]} is
#' minus the regression coefficient of variable \code{i} in the regression
#' equation \code{j}. Thus there is a non zero partial regression coefficient
#' \code{Ahat[i,j]} corresponding to each non zero value \code{amat[j,i]} in
#' the adjacency matrix.} \item{Dhat}{a vector containing the partial variances
#' of each variable given the parents.} \item{dev}{the `deviance' of the
#' model.} \item{df}{the degrees of freedom.}
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{DAG}}, \code{\link{swp}}.
#' @references Cox, D. R. \& Wermuth, N. (1996). \emph{Multivariate
#' dependencies}. London: Chapman \& Hall.
#' @keywords graphs models multivariate
#' @examples
#' 
#' dag <- DAG(y ~ x+u, x ~ z, z ~ u)
#' "S" <- structure(c(2.93, -1.7, 0.76, -0.06,
#'                    -1.7, 1.64, -0.78, 0.1,
#'                     0.76, -0.78, 1.66, -0.78,
#'                     -0.06, 0.1, -0.78, 0.81), .Dim = c(4,4),
#'          .Dimnames = list(c("y", "x", "z", "u"), c("y", "x", "z", "u")))
#' fitDag(dag, S, 200)
#' 
"fitDag" <-
function (amat, S, n)
{
### Fits linear recursive regressions with independent residuals. 
### amat: the adjacency matrix of the DAG. S: cov matrix. n: sample size.
  if(missing(amat)){ # saturated model
    amat <-  lower.tri(diag(ncol(S)), diag=FALSE) * 1
    dimnames(amat) <- dimnames(S)
  }
  nam <- rownames(S)
  nod <- rownames(amat)
  if(is.null(nod))
    stop("The adjacency matrix has no labels.")
  if(!all(is.element(nod, nam)))
    stop("The nodes of the graph do not match the names of the variables.")
  else
    sek <- intersect(nam, nod) 
  S <- S[sek,sek]              # Resizes eventually S 
  amat <- amat[sek,sek]        # and reorders amat
  Delta <- rep(length(sek),0)
  
  emat <- edgematrix(amat) 
  A <- emat
  p <- ncol(S)
  ip <- 1:p
  for(i in 1:p) {
    u <- emat[i,]
    v <- ip[u == 1 & ip != i]
    M <- swp(S, v)[i,]
    A[i, ] <- - A[i, ] * M
    A[i,i] <- 1
    k <- sum(A[i,])
    Delta[i] <- M[i]
  }
  names(Delta) <- sek
  B <- solve(A)
  Shat <- B %*% diag(Delta) %*% t(B)
  dimnames(Shat) <- dimnames(S)
  Khat <- solve(Shat)
  H <- S %*% Khat
  trace <- function(A) sum(diag(A))
  dev <- (trace(H) - log(det(H)) - p) * n
  df <- p*(p+1)/2 - sum(amat==1) - p
  list(Shat=Shat, Ahat = A, Dhat=Delta, dev=dev, df=df)
}



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



#' Fundamental cycles
#' 
#' Finds the list of fundamental cycles of a connected undirected graph.
#' 
#' All the cycles in an UG can be obtained from combination (ring sum) of the
#' set of fundamental cycles.
#' 
#' @param amat a symmetric matrix with dimnames denoting the adjacency matrix
#' of the undirected graph. The graph must be connected, otherwise the function
#' returns an error message.
#' @return a list of matrices with two columns. Every component of the list is
#' associated to a cycle. The cycle is described by a \eqn{k \times 2} matrix
#' whose rows are the edges of the cycle. If there is no cycle the function
#' returns \code{NULL}.
#' @note This function is used by \code{cycleMatrix} and \code{isGident}.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{UG}},\code{\link{findPath}}, \code{\link{cycleMatrix}},
#' \code{\link{isGident}},\code{\link{bfsearch}}
#' @references Thulasiraman, K. \& Swamy, M.N.S. (1992). \emph{Graphs: theory
#' and algorithms}. New York: Wiley.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## Three fundamental cycles
#' fundCycles(UG(~a*b*d + d*e + e*a*f))
#' 
"fundCycles" <-
function(amat){
### Finds a set of fundamental cycles for an UG with adj. matrix amat.
    fc <- c()
    tr <- bfsearch(amat) # Spanning tree
    if(is.null(tr)) return(NULL)
    if(is.null(tr$chords)) return(NULL)
    co <- tr$chords # edges of the cospanning tree
    for(i in 1:nrow(co)) {
      e <- co[i,] 
      g <- tr$tree # edge matrix of the spanning tree
      cy <- findPath(g, st=e[1], en=e[2])
       splitCycle <- function(v){
         ## Splits a cycle v into a matrix of edges.
         v <- c(v, v[1])
         cbind(v[-length(v)], v[-1])
       }
      cy <- splitCycle(cy)
      fc <- c(fc, list(cy))
    }
    fc 
  }



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



#' Indicator matrix
#' 
#' Finds the indicator matrix of the zeros of a matrix.
#' 
#' The indicator matrix is a matrix of zeros and ones which has a zero element
#' iff the corresponding element of \code{A} is (exactly) zero.
#' 
#' @param A a matrix.
#' @return a matrix of the same dimensions as \code{A}.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{DAG}}, \code{\link{inducedCovGraph}},
#' \code{\link{inducedConGraph}}
#' @references Wermuth, N. \& Cox, D.R. (2004). Joint response graphs and
#' separation induced by triangular systems. \emph{J.R. Statist. Soc. B}, 66,
#' Part 3, 687-717.
#' @keywords array algebra graphs multivariate
#' @examples
#' 
#' ## A simple way to find the overall induced concentration graph
#' ## The DAG on p. 198 of Cox & Wermuth (1996)
#' amat <- DAG(y1 ~ y2 + y3, y3 ~ y5, y4 ~ y5)
#' A <- edgematrix(amat)
#' In(crossprod(A))
#' 
`In` <-
function (A) 
{
### Indicator matrix of structural zeros.
  abs(sign(A)) 
}

"inducedChainGraph" <-
function(amat, cc=rownames(amat), cond=NULL, type="LWF"){
### Induced chain graph with chain components cc.
    inducedBlockGraph <- function(amat, sel, cond){
      S <- inducedConGraph(amat, sel=union(cond, sel), cond=NULL)
      In(S[cond, sel, drop=FALSE])
    }
    nod <- rownames(amat)
    nam <- c(list(cond), cc)
    if(!all(unlist(nam) %in% nod))
      stop("The chain components or the conditioning set are wrong.")
    for(h in nam){
      for(k in nam){
        h <- unlist(h)
        k <- unlist(k)
        if (setequal(h,k))
          next
        if(length(intersect(h,k) > 0))
          stop("The sets are not disjoint!")
      }
    }
    nam <- unlist(nam)
    cg <- matrix(0, length(nam),length(nam))
    dimnames(cg) <- list(nam,nam)
    kc <- length(cc)
    if(type=="AMP"){
      for(i in 1:kc){
        past <-  unlist(cc[0:(i-1)]) 
        Past <- union(cond,past)
        g <- cc[[i]]
        
        Sgg.r <- inducedConGraph(amat, sel=g, cond=Past)
        cg[g, g] <- Sgg.r
        if(length(past) !=0){
          Pgr <- inducedRegGraph(amat, sel=g, cond=Past)
          cg[Past, g] <- Pgr
        }
      }
    }
    else if(type=="LWF"){
      for(i in 1:kc){
        past <-  unlist(cc[0:(i-1)]) 
        Past <- union(cond,past)
        g <- cc[[i]]
        
        Sgg.r <- inducedConGraph(amat,sel=g, cond=Past)
        cg[g, g] <- Sgg.r
        if(length(past) !=0){
          Cgr <- inducedBlockGraph(amat, sel=g, cond=Past)
          cg[Past, g] <- Cgr
        }
      }
    }
    else if(type=="MRG"){
      for(i in 1:kc){
        past <-  unlist(cc[0:(i-1)]) 
        Past <- union(cond,past)
        g <- cc[[i]]
        
        Sgg.r <- inducedCovGraph(amat, sel=g, cond=Past)
        cg[g, g] <- Sgg.r
        if(length(past) != 0){
          Pgr <- inducedRegGraph(amat, sel=g, cond=Past)
          cg[Past, g] <- Pgr
        }
      }
    }
    else
      stop("Wrong type.")
    n <- unlist(cc)
    cg[n,n, drop=FALSE]
  }

"inducedConGraph" <-
function(amat, sel=rownames(amat), cond=NULL){
### Induced concentration graph for a set of nodes given a conditioning set.
    ancGraph <- function(A) {
      ## Edge matrix of the overall ancestor graph.
      if(sum(dim(A)) == 0)
        return(A)
      else
        return(In(solve(2*diag(nrow(A)) - A)))
    }
   
   trclos <- function(M) {
      ## Transitive closure of an UG with edge matrix M. See Wermuth and Cox (2004). 
     edgematrix(transClos(adjMatrix(M)))
    }

    A <- edgematrix(amat) # From the adjacency matrix to edge matrix    
    nod <- rownames(A)
      if(!all(cond %in% nod))
        stop("Conditioning nodes are not among the vertices.")
    if(!all(sel %in% nod))
      stop("Selected nodes are not among the vertices.")
    
    if(length(intersect(sel,cond) > 0))
      stop("The sets are not disjoint!")

    l <- setdiff(nod, union(sel, cond))  # Marginal nodes
    g <- sel
    r <- cond
    L <- union(l,g)
    R <- union(g,r)
    
    Al <- ancGraph(A[l,l,drop=FALSE])
    ARR.l <- In(A[R,R, drop=FALSE] +
                A[R,l, drop=FALSE]%*% Al %*% A[l,R, drop=FALSE])
    TRl <- In(A[R,l,drop=FALSE] %*% Al)
    DRl <- In(diag(length(R)) + TRl %*% t(TRl))
    out <- In(t(ARR.l) %*% trclos(DRl) %*% ARR.l)
    out <- out[g,g, drop=FALSE]
    adjMatrix(out)*10
  }

"inducedCovGraph" <-
function(amat, sel=rownames(amat), cond=NULL){
### Induced covariance graph for a set of nodes given a conditioning set.
    ancGraph <- function(A) {
      ## Edge matrix of the overall ancestor graph.
      if(sum(dim(A)) == 0)
        return(A)
      else
        return(In(solve(2*diag(nrow(A)) - A)))
    }
     trclos <- function(M) {
      ## Transitive closure of an UG with edge matrix M. See Wermuth and Cox (2004). 
     edgematrix(transClos(adjMatrix(M)))
    }
    A <- edgematrix(amat) # From the adjacency matrix to edge matrix
    nod <- rownames(A)
    if(!all(cond %in% nod))
      stop("Conditioning nodes are not among the vertices.")
    if(!all(sel %in% nod))
      stop("Selected nodes are not among the vertices.")
    if(length(intersect(sel,cond) > 0))
      stop("The sets are not disjoint!")
    l <- setdiff(nod, union(sel, cond))  # Marginalized over nodes
    g <- sel
    r <- cond
    L <- union(l,g)
    R <- union(g,r)
    
    AL <-  ancGraph(A[L,L,drop=FALSE]) # In(solve(2*diag(length(L)) - A[L,L]))
    TrL <- In(A[r,L,drop=FALSE] %*% AL)
    DLr <- In(diag(length(L)) + t(TrL) %*% TrL)
    cl <- trclos(DLr)
    out <- In(AL %*% cl %*% t(AL))
    out <- out[g,g, drop=FALSE]
    adjMatrix(out)*100
  }

"inducedDAG" <-
function(amat, order, cond=NULL){
### Induced DAG in a new ordering.
    cc=as.list(order)
    inducedChainGraph(amat, cc=cc, cond=cond)
  }

"inducedRegGraph" <-
function(amat, sel=rownames(amat), cond=NULL){
### Induced regression graph for a set of nodes given a conditioning set.
    ancGraph <- function(A) {
      ## Edge matrix of the overall ancestor graph.
      if(sum(dim(A)) == 0)
        return(A)
      else
        return(In(solve(2*diag(nrow(A)) - A)))
    }
    trclos <- function(M) {
      ## Transitive closure of an UG with edge matrix M. See Wermuth and Cox (2004). 
     edgematrix(transClos(adjMatrix(M)))
    }
    A <- edgematrix(amat) # From the adjacency matrix to edge matrix
    nod <- rownames(A)
    if(!all(cond %in% nod))
      stop("Conditioning nodes are not among the vertices.")
    if(!all(sel %in% nod))
      stop("Selected nodes are not among the vertices.")
    if(length(intersect(sel,cond) > 0))
      stop("The sets are not disjoint!")
    l <- setdiff(nod, union(sel, cond))  # Nodes marginalized over
    g <- sel
    r <- cond
    L <- union(l,g)
    R <- union(g,r)
    
    AL <-  ancGraph(A[L,L,drop=FALSE])          # A^{LL} 
    TrL <- In(A[r,L,drop=FALSE] %*% AL)         # T_{rL}
    DrL <- In(diag(length(r)) + TrL %*% t(TrL)) # D_{rr-L}
    Arr.L <-  In(A[r,r, drop=FALSE] + A[r,L, drop=FALSE] %*% AL 
                 %*% A[L,r, drop=FALSE])        # A_{rr.L}
    FLr <- In(AL %*% A[L, r, drop=FALSE])       # F_{Lr} 
    out <- In(AL %*% t(TrL) %*% trclos(DrL) %*% Arr.L + FLr)
    t(out[g,r, drop=FALSE])
  }



#' Graph queries
#' 
#' Checks if a given graph is acyclic.
#' 
#' 
#' @param amat a square Boolean matrix with dimnames, the adjacency matrix of a
#' graph.
#' @param method an integer 1 or 2 specifying the method used. If
#' \code{method=1} the function calls the function \code{clusters} in package
#' \code{igraph} to find the strong components: two nodes v and w are in the
#' same strong component iff there are directed paths from v to w and from w to
#' v. If \code{method=2} the function uses the \code{ggm} function
#' \code{transClos}. Method 1 is faster.
#' @return a logical value, \code{TRUE} if the graph is acyclic and
#' \code{FALSE} otherwise.
#' @author David Edwards, Giovanni M. Marchetti
#' @references Aho, A.V., Hopcroft, J.E. \& Ullman, J.D. (1983). \emph{Data
#' structures and algorithms.} Reading: Addison-Wesley.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## A cyclic graph
#' d <- matrix(0,3,3)
#' rownames(d) <- colnames(d) <- c("x", "y", "z")
#' d["x","y"] <- d["y", "z"] <- d["z", "x"] <- 1
#' ## Test if the graph is acyclic
#' isAcyclic(d)
#' isAcyclic(d, method = 1)
#' 
`isAcyclic` <-
function (amat, method = 2) 
{
### Tests if the graph is acyclic.
  if(method ==1){
    G <- graph.adjacency(amat)
    return(max(clusters(G, mode = "strong")$csize) == 1)
  }
  else if(method ==2){
  B <- transClos(amat)
  l <- B[lower.tri(B)]
  u <- t(B)[lower.tri(t(B))]
  com <- (l&u)
  return(all(!com))
  }
  else{
    stop("Wrong method.")
  }
}



#' G-identifiability of an UG
#' 
#' Tests if an undirected graph is G-identifiable.
#' 
#' An undirected graph is said G-identifiable if every connected component of
#' the complementary graph contains an odd cycle (Stanghellini and Wermuth,
#' 2005). See also Tarantola and Vicard (2002).
#' 
#' @param amat a symmetric matrix with dimnames representing the adjacency
#' matrix of an undirected graph
#' @return a logical value, \code{TRUE} if the graph is G-identifiable and
#' \code{FALSE} if it is not.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{UG}}, \code{\link{cmpGraph}}, \code{\link{cycleMatrix}}
#' @references Stanghellini, E. \& Wermuth, N. (2005). On the identification of
#' path-analysis models with one hidden variable. \emph{Biometrika}, 92(2),
#' 337-350.
#' 
#' Stanghellini, E. (1997). Identification of a single-factor model using
#' graphical Gaussian rules. \emph{Biometrika}, 84, 241--244.
#' 
#' Tarantola, C. \& Vicard, P. (2002). Spanning trees and identifiability of a
#' single-factor model. \emph{Statistical Methods \& Applications}, 11,
#' 139--152.
#' 
#' Vicard, P. (2000). On the identification of a single-factor model with
#' correlated residuals. \emph{Biometrika}, 87, 199--205.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## A not G-identifiable UG
#' G1 <- UG(~ a*b + u*v)
#' isGident(G1)
#' ## G-identifiable UG
#' G2 <- UG(~ a + b + u*v)
#' isGident(G2)
#' ## G-identifiable UG
#' G3 <- cmpGraph(UG(~a*b*c+x*y*z))
#' isGident(G3)
#' 
"isGident" <-
function(amat){
### Is the UG with adjacency matrix amat G-identifiable?
    is.odd <- function(x) (x %% 2) == 1
    cmpGraph <- function(amat){
      ## Adjacency matrix of the complementary graph.
      g <- 0+ (!amat)
      diag(g) <- 0
      g
    }
    cgr <- cmpGraph(amat) 
    cc <- conComp(cgr) 
    l <- unique(cc)
    k <- length(l)
    g <- rep(k, 0)
    for(i in 1:k){
      subg <- cgr[cc==i, cc==i, drop=FALSE]
      m <- cycleMatrix(subg)
      if(is.null(m))
        rt <- 0
      else        
        rt <- apply(m, 1, sum)
      g[i] <- any(is.odd(rt))
    }
    all(g)
  }



"pa" <-
function (nn, amat) 
{
### List of the parents of nodes nn for a given with adjacency matrix amat.
  nod <- rownames(amat)
  if(is.null(nod)) stop("The adjacency matrix must have dimnames!")
  if(!all(is.element(nn, nod))) stop("Some of the nodes are not among the vertices.")
  k <- length(nn)
  p <- vector(k, mode="list")
  A <- 0 + ((amat != t(amat)) & (amat == 1)) # Select the directed edges
  for(i in 1:k) {
    p[[i]] <- nod[A[,nn[i]]==1 ]
  }
  setdiff(unique(unlist(p)), nn)
}



#' Partial correlations
#' 
#' Finds the matrix of the partial correlations between pairs of variables
#' given the rest.
#' 
#' The algorithm computes \eqn{- \sigma^{rs}/(\sigma^{rr} \sigma^{ss})^{1/2}}
#' where the \eqn{\sigma^{rs}} are concentrations, i.e. elements of the inverse
#' covariance matrix.
#' 
#' @param S a symmetric positive definite matrix, representing a covariance
#' matrix.
#' @return A symmetric matrix with ones along the diagonal and in position
#' \eqn{(r,s)} the partial correlation between variables \eqn{r} and \eqn{s}
#' given all the remaining variables.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{var}}, \code{\link{cor}}, \code{\link{correlations}}
#' @references Cox, D. R. \& Wermuth, N. (1996). \emph{Multivariate
#' dependencies}. London: Chapman \& Hall.
#' @keywords array graphs models multivariate
#' @examples
#' 
#' ### Partial correlations for the mathematics marks data
#' data(marks)
#' S <- var(marks)
#' parcor(S)
#' 
"parcor" <-
function (S)
{
### Finds the partial correlation matrix of the variables given the rest.
### S is the covariance matrix.  
  p <- ncol(S)
  K <- solve(S)
  a <- 1/sqrt(diag(K))
  K <- K * outer(a, a)
  out <- 2 * diag(p) - K
  dimnames(out) <- dimnames(S)
  out
}



#' Partial correlation
#' 
#' Computes the partial correlation between two variables given a set of other
#' variables.
#' 
#' 
#' @param u a vector of integers of length > 1. The first two integers are the
#' indices of variables the correlation of which must be computed. The rest of
#' the vector is the conditioning set.
#' @param S a symmetric positive definite matrix, a sample covariance matrix.
#' @return a scalar, the partial correlation matrix between variables
#' \code{u[1]} and \code{u[2]} given \code{u[-c(1,2)]}.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{cor}}, \code{\link{parcor}}, \code{\link{correlations}}
#' @keywords models multivariate
#' @examples
#' 
#' data(marks)
#' ## The correlation between vectors and algebra given analysis and statistics
#'  pcor(c("vectors", "algebra", "analysis", "statistics"), var(marks))
#' ## The same
#' pcor(c(2,3,4,5), var(marks))
#' ## The correlation between vectors and algebra given statistics
#'  pcor(c("vectors", "algebra", "statistics"), var(marks))
#' ## The marginal correlation between analysis and statistics 
#' pcor(c("analysis","statistics"), var(marks))
#' 
"pcor" <-
function (u, S) 
{
### Partial correlation between u[1:2], given th rest of u. S: cov matrix.
  k <- solve(S[u,u])
  -k[1,2]/sqrt(k[1,1]*k[2,2])
}



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



#' Random correlation matrix
#' 
#' Generates a random correlation matrix with the method of Marsaglia and Olkin
#' (1984).
#' 
#' The algorithm uses \code{\link{rsphere}} to generate \eqn{d} vectors on a
#' sphere in \eqn{d}-space. If \eqn{Z} is a matrix with such vectors as rows,
#' then the random correlation matrix is \eqn{ZZ'}.
#' 
#' @param d an integer > 0, the order of the correlation matrix.
#' @return a correlation matrix of order \code{d}.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{rsphere}}
#' @references Marshall, G.\& Olkin, I. (1984).Generating correlation matrices.
#' \emph{SIAM J. Sci. Stat. Comput.}, 5, 2, 470--475.
#' @keywords distribution multivariate
#' @examples
#' 
#' ## A random correlation matrix of order 3
#' rcorr(3)
#' ## A random correlation matrix of order 5
#' rcorr(5)
#' 
"rcorr" <-
function(d)
{
# Generates a random correlation matrix of dimension d
# with the method of Marsaglia and Olkin (1984).
 h<-rsphere(d,d)
 h %*% t(h)
}



#' Random sample from a decomposable Gaussian model
#' 
#' Generates a sample from a mean centered multivariate normal distribution
#' whose covariance matrix has a given triangular decomposition.
#' 
#' The value in position \eqn{(i,j)} of \code{A} (with \eqn{i < j}) is a
#' regression coefficient (with sign changed) in the regression of variable
#' \eqn{i} on variables \eqn{i+1, \dots, d}.
#' 
#' The value in position \eqn{i} of \code{Delta} is the residual variance in
#' the above regression.
#' 
#' @param n an integer > 0, the sample size.
#' @param A a square, upper triangular matrix with ones along the diagonal. It
#' defines, together with \code{Delta}, the concentration matrix (and also the
#' covariance matrix) of the multivariate normal. The order of \code{A} is the
#' number of components of the normal.
#' @param Delta a numeric vector of length equal to the number of columns of
#' \code{A}.
#' @return a matrix with \code{n} rows and \code{nrow(A)} columns, a sample
#' from a multivariate normal distribution with mean zero and covariance matrix
#' \code{S = solve(A) %*% diag(Delta) %*% t(solve(A))}.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{triDec}}, \code{\link{fitDag}}
#' @references Cox, D. R. \& Wermuth, N. (1996). \emph{Multivariate
#' dependencies}. London: Chapman \& Hall.
#' @keywords distribution multivariate
#' @examples
#' 
#' ## Generate a sample of 100 observation from a multivariate normal
#' ## The matrix of the path coefficients 
#' A <- matrix(
#' c(1, -2, -3,  0, 0,  0,  0,
#'   0,  1,  0, -4, 0,  0,  0,
#'   0,  0,  1,  2, 0,  0,  0,
#'   0,  0,  0,  1, 1, -5,  0,
#'   0,  0,  0,  0, 1,  0,  3,
#'   0,  0,  0,  0, 0,  1, -4,
#'   0,  0,  0,  0, 0,  0,  1), 7, 7, byrow=TRUE)
#' D <- rep(1, 7)
#' X <- rnormDag(100, A, D)
#' 
#' ## The true covariance matrix
#' solve(A) %*% diag(D) %*% t(solve(A))
#' 
#' ## Triangular decomposition of the sample covariance matrix
#' triDec(cov(X))$A
#' 
"rnormDag" <-
function (n, A, Delta) 
{
### Generates n observations from a multivariate normal with mean 0
### and a covariance matrix A^-1 Delta (A^-1)'.
  p <- length(Delta)
  E <- matrix(0, n, p)
  for(j in 1:p) { 
    E[,j] <- rnorm(n, 0, sqrt(Delta[j]))
  }
  B <- solve(A)
  Y <- E %*% t(B) 
  colnames(Y) <- colnames(A)
  Y
}



#' Random vectors on a sphere
#' 
#' Generates a sample of points uniformly distributed on the surface of a
#' sphere in d-space.
#' 
#' The algorithm is based on normalizing to length 1 each d-vector of a sample
#' from a multivariate normal \eqn{N(0, I)}.
#' 
#' @param n an integer, the sample size.
#' @param d an integer, the dimension of the space. For example, a circle is
#' defined in 2D-space, a sphere in 3D-space.
#' @return a matrix of \code{n} rows and \code{d} columns.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{rnorm}}, \code{\link{rcorr}}
#' @keywords distribution multivariate
#' @examples
#' 
#' ## 100 points on circle
#' z <- rsphere(100,2)
#' plot(z)
#' 
#' ## 100 points on a sphere
#' z <- rsphere(100, 3)
#' pairs(z)
#' 
"rsphere" <-
function(n, d)
{
## Generates n random vectors uniformly dist. on the
## surface of a sphere, in d dimensions.
  X <- matrix(rnorm(n*d),n,d)
  d <- apply(X, 1, function(x) sqrt(sum(x*x)))
  sweep(X, 1, d, "/")
}



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



#' Sweep operator
#' 
#' Sweeps a covariance matrix with respect to a subset of indices.
#' 
#' The sweep operator has been introduced by Beaton (1964) as a tool for
#' inverting symmetric matrices (see Dempster, 1969).
#' 
#' @param V a symmetric positive definite matrix, the covariance matrix.
#' @param b a subset of indices of the columns of \code{V}.
#' @return a square matrix \code{U} of the same order as \code{V}. If \code{a}
#' is the complement of \code{b}, then \code{U[a,b]} is the matrix of
#' regression coefficients of \code{a} given \code{b} and \code{U[a,a]} is the
#' corresponding covariance matrix of the residuals.
#' 
#' If \code{b} is empty the function returns \code{V}.
#' 
#' If \code{b} is the vector \code{1:nrow(V)} (or its permutation) then the
#' function returns the opposite of the inverse of \code{V}.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{fitDag}}
#' @references Beaton, A.E. (1964). \emph{The use of special matrix operators
#' in statistical calculus}. Ed.D. thesis, Harvard University. Reprinted as
#' Educational Testing Service Research Bulletin 64-51. Princeton.
#' 
#' Dempster, A.P. (1969). \emph{Elements of continuous multivariate analysis}.
#' Reading: Addison-Wesley.
#' @keywords array algebra models multivariate
#' @examples
#' 
#' ## A very simple example
#' V <- matrix(c(10, 1, 1, 2), 2, 2)
#' swp(V, 2)
#' 
"swp" <-
function (V, b) 
{
### SWP operator. V is the covariance matrix, b  is a  subset of indices.
  p <- ncol(V)
  u <- is.na(match(1:p, b))
  a <- (1:p)[u]
  out <- 0 * V
  dimnames(out) <- dimnames(V)
  if (length(a) == 0) 
    return(-solve(V))
  else if (length(a) == p) 
    return(V)
  else{
    Saa <- V[a, a, drop = FALSE]
    Sab <- V[a, b, drop = FALSE]
    Sbb <- V[b, b, drop = FALSE]
    B <- Sab %*% solve(Sbb)
    out[a, a] <- Saa - B %*% t(Sab)
    out[a, b] <- B
    out[b, a] <- t(B)
    out[b, b] <- -solve(Sbb)
    return(out)
  }
  ## list(swept = out, coef = out[a, b], rss = out[a, a, drop = F])
}

"topOrder" <-
function (amat) 
{
### Return the nodes in topological order (parents before children).
### Translated from: Kevin Murphy's BNT.
  if(!isAcyclic(amat)) stop("The graph is not acyclic!")
  n <- nrow(amat)
  nod <- 1:n
  indeg <- rep(0, n)
  up <- !amat[lower.tri(amat)]
  if(all(up))
    return(nod)
  zero.indeg <- c() #  a stack of nodes with no parents
  for(i in nod) {
    indeg[i] <- sum(amat[,i])
    if(indeg[i] == 0)
      zero.indeg <- c(i,  zero.indeg)
  }
  s <- 1
  ord <- rep(0, n)
  while(length(zero.indeg) > 0){
    v <- zero.indeg[1]  #  pop v
    zero.indeg <- zero.indeg[-1]
    ord[s] <- v
    s <- s + 1
    cs <- nod[amat[v,]==1]
    if(length(cs) == 0) next
    for(j in 1:length(cs)){
      k <- cs[j]
      indeg[k] <- indeg[k] - 1
      if(indeg[k] == 0)
        zero.indeg <- c(k,  zero.indeg) # push k    
    }
  }
  ord
}



#' Topological sort
#' 
#' \code{topOrder} returns the topological order of a directed acyclic graph
#' (parents, before children). \code{topSort} permutates the adjacency matrix
#' according to the topological order.
#' 
#' The topological order needs not to be unique.  After the permutation the
#' adjacency matrix of the graph is upper triangular. The function is a
#' translation of the Matlab function \code{topological_sort} in Toolbox
#' \pkg{BNT} written by Kevin P. Murphy.
#' 
#' @aliases topSort topOrder
#' @param amat a square Boolean matrix with dimnames, representing the
#' adjacency matrix of a directed acyclic graph.
#' @return \code{topOrder(amat)} returns a vector of integers representing the
#' permutation of the nodes. \code{topSort(amat)} returns the adjacency matrix
#' with rows and columns permutated.
#' @note The order of the nodes defined by \code{DAG} is that of their first
#' appearance in the model formulae (from left to right).
#' @author Kevin P. Murphy, Giovanni M. Marchetti
#' @seealso \code{\link{DAG}}, \code{\link{isAcyclic}}
#' @references Aho, A.V., Hopcrtoft, J.E. \& Ullman, J.D. (1983). \emph{Data
#' structures and algorithms.} Reading: Addison-Wesley.
#' 
#' Lauritzen, S. (1996). \emph{Graphical models}. Oxford: Clarendon Press.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## A simple example
#' dag <- DAG(a ~ b, c ~ a + b, d ~ c + b)
#' dag
#' topOrder(dag)
#' topSort(dag)
#' 
"topSort" <-
function (amat) {
### Topological sort of the DAG with adjacency matrix amat.
    ord <- topOrder(amat)
    amat[ord, ord]
}



#' Transitive closure of a graph
#' 
#' Computes the transitive closure of a graph (undirected or directed acyclic).
#' 
#' The transitive closure of a directed graph with adjacency matrix \eqn{A} is
#' a graph with adjacency matrix \eqn{A^*} such that \eqn{A^*_{i,j} = 1} if
#' there is a directed path from \eqn{i} to \eqn{j}. The transitive closure of
#' an undirected graph is defined similarly (by substituting path to directed
#' path).
#' 
#' @param amat a Boolean matrix with dimnames representing the adjacency matrix
#' of a graph.
#' @return \item{A}{The adjacency matrix of the transitive closure.}
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{DAG}}, \code{\link{UG}}
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## Closure of a DAG
#' d <- DAG(y ~ x, x ~ z)
#' transClos(d)
#' 
#' ## Closure of an UG
#' g <- UG(~ x*y*z+z*u+u*v)
#' transClos(g)
#' 
`transClos` <-
function (amat) 
{
### Transitive closure of the relation with adjacency matrix amat.
  if (nrow(amat) == 1) 
    return(amat)
  A <- amat
  diag(A) <- 1
  repeat {
    B <- sign(A %*% A)
    if (all(B == A))
      break
    else A <- B
  }
  diag(A) <- 0
  A
}



#' Triangular decomposition of a covariance matrix
#' 
#' Decomposes a symmetric positive definite matrix with a variant of the
#' Cholesky decomposition.
#' 
#' Any symmetric positive definite matrix \eqn{\Sigma}{Sigma} can be decomposed
#' as \eqn{\Sigma = B \Delta B^T}{Sigma = B %*% Delta %*% t(B)} where \eqn{B}
#' is upper triangular with ones along the main diagonal and
#' \eqn{\Delta}{Delta} is diagonal. If \eqn{\Sigma}{Sigma} is a covariance
#' matrix, the concentration matrix is \eqn{\Sigma^{-1} = A^T \Delta^{-1} A}
#' where \eqn{A = B^{-1}} is the matrix of the regression coefficients (with
#' the sign changed) of a system of linear recursive regression equations with
#' independent residuals. In the equations each variable \eqn{i} is regressed
#' on the variables \eqn{i+1, \dots, d}.  The elements on the diagonal of
#' \eqn{\Delta} are the partial variances.
#' 
#' @param Sigma a symmetric positive definite matrix.
#' @return \item{A}{a square upper triangular matrix of the same order as
#' \code{Sigma} with ones on the diagonal.} \item{B}{the inverse of \code{A},
#' another triangular matrix with unit diagonal.} \item{Delta}{a vector
#' containing the diagonal values of \eqn{\Delta}.}
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{chol}}
#' @references Cox, D. R. \& Wermuth, N. (1996). \emph{Multivariate
#' dependencies}. London: Chapman \& Hall.
#' @keywords array algebra models multivariate
#' @examples
#' 
#' ## Triangular decomposition of a covariance matrix
#' B <- matrix(c(1,  -2, 0, 1,
#'               0,   1, 0, 1,
#'               0,   0, 1, 0,
#'               0,   0, 0, 1), 4, 4, byrow=TRUE)
#' B
#' D <- diag(c(3, 1, 2, 1))
#' S <- B %*% D %*% t(B)
#' triDec(S)
#' solve(B)
#' 
"triDec" <-
function(Sigma){
### Triangular decomposition of covariance matrix Sigma.  
  R = chol(solve(Sigma))
  dimnames(R) = dimnames(Sigma)
  D = diag(R)
  A = diag(1/D) %*% R
  dimnames(A) <- dimnames(Sigma)
  B = solve(A) 
  list(A = A, B = B, Delta = 1/(D^2))
}



#' Defining an undirected graph (UG)
#' 
#' A simple way to define an undirected graph by means of a single model
#' formula.
#' 
#' The undirected graph \eqn{G = (V, E)} is defined by a set of nodes \eqn{V}
#' and a set of pairs \eqn{E}. The set of pairs is defined by the set of
#' interactions in the formula. Interactions define complete subgraphs (not
#' necessarily maximal) of the UG.  The best way is to specify interactions
#' that match the cliques of the undirected graph. This is the standard way to
#' define graphical models for contingency tables. Remember that some
#' hierarchical models are not graphical, but they imply the same graph.
#' 
#' The function returns the edge matrix of the graph, i.e.  a square Boolean
#' matrix of order equal to the number of nodes of the graph and a one in
#' position \eqn{(i,j)} if there is an arrow from \eqn{j} to \eqn{i} and zero
#' otherwise. By default this matrix has ones along the main diagonal. For UGs
#' this matrix is symmetric.  The dimnames of the edge matrix are the nodes of
#' the UG.
#' 
#' @param f a single model formula without response
#' @return a Boolean matrix with dimnames, the adjacency matrix of the
#' undirected graph.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{fitConGraph}}, \code{\link{fitCovGraph}},
#' \code{\link{DAG}}
#' @references Lauritzen, S. (1996). \emph{Graphical models}. Oxford: Clarendon
#' Press.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## X independent of Y given Z
#' UG(~ X*Z + Y*Z)
#' 
#' # The saturated model
#' UG(~ X*Y*Z)
#' 
#' ## The model without three-way interactions has the same graph
#' UG(~ X*Y + Y*Z + Z*X)
#' UG(~ (X + Y + Z)^2)
#' 
#' ## Butterfly model defined from the cliques
#' UG(~ mec*vec*alg + alg*ana*sta)
#' 
#' ## Some isolated nodes
#' UG(~x*y*z + a + b) 
#' 
"UG" <-
function (f) 
{
### Defines an UG from a model formula. Returns the adj. matrix.  
  tt <- terms(f)
  if (attr(tt, "response") == 1)
    stop("You should not specify a response!")
  nod <- dimnames(attr(tt, "factors"))[[1]]
  
  N <- unique(nod) # set of nodes
  dN <- length(N)  # number of nodes
  amat <- matrix(0, dN, dN)
  o <- attr(tt, "order") <= 2
  v <- attr(tt, "factors")[, o, drop = FALSE]
  m <- match(dimnames(v)[[1]], N)
  for (i in 1:sum(o)) {
    ij <- m[v[, i] == 1]
    amat[ij[1], ij[2]] <- 1
    amat[ij[2], ij[1]] <- 1
  }
  dimnames(amat) <- list(N, N)
  amat
}



#' Mixed Graphs
#' 
#' Defines a loopless mixed graph from the directed, undirected and undirected
#' components.
#' 
#' A loopless mixed graph is a mixed graph with three types of edges:
#' undirected, directed and bi-directed edges.  Note that the three adjacency
#' matrices must have labels and may be defined using the functions \code{DG},
#' \code{DAG} or \code{UG}.  The adjacency matrices of the undirected graphs
#' may be just symmetric Boolean matrices.
#' 
#' @param dg the adjacency matrix of a directed graph specifying the arrows of
#' the mixed graph.
#' @param ug the adjacency matrix of an undirected graph specifying the lines
#' of the mixed graph.
#' @param bg the adjacency matrix of an undirected graph specifying the
#' bidirected edges of the mixed graph.
#' @return a square matrix obtained by combining the three graph components
#' into an adjacency matrix of a mixed graph. The matrix consists of 4
#' different integers as an \eqn{ij}-element: 0 for a missing edge between
#' \eqn{i} and \eqn{j}, 1 for an arrow from \eqn{i} to \eqn{j}, 10 for a full
#' line between \eqn{i} and \eqn{j}, and 100 for a bi-directed arrow between
#' \eqn{i} and \eqn{j}.  These numbers are added to be associated with multiple
#' edges of different types. The matrix is symmetric w.r.t full lines and
#' bi-directed arrows.
#' @author Giovanni M. Marchetti, Mathias Drton
#' @seealso \code{\link{UG}}, \code{\link{DAG}}
#' @references Richardson, T. S. and Spirtes, P. (2002). Ancestral Graph Markov
#' Models. \emph{Annals of Statistics}, 30(4), 962--1030.
#' @keywords graphs ancestral graph mixed graph models multivariate
#' @examples
#' 
#' ## Examples from Richardson and Spirtes (2002)
#' a1 <- makeMG(dg=DAG(a~b, b~d, d~c), bg=UG(~a*c))  
#' isAG(a1)    # Not an AG. (a2) p.969    
#' a2 <- makeMG(dg=DAG(b ~ a, d~c), bg=UG(~a*c+c*b+b*d))           # Fig. 3 (b1) p.969  
#' isAG(a1)
#' a3 <- makeMG(ug = UG(~ a*c), dg=DAG(b ~ a, d~c), bg=UG(~ b*d)) # Fig. 3 (b2) p.969
#' a5 <- makeMG(bg=UG(~alpha*beta+gamma*delta), dg=DAG(alpha~gamma,
#' delta~beta))  # Fig. 6 p. 973
#' ## Another Example
#' a4 <- makeMG(ug=UG(~y0*y1), dg=DAG(y4~y2, y2~y1), bg=UG(~y2*y3+y3*y4))  
#' ## A mixed graphs with double edges. 
#' mg <- makeMG(dg = DG(Y ~ X, Z~W, W~Z, Q~X), ug = UG(~X*Q), 
#' bg = UG(~ Y*X+X*Q+Q*W + Y*Z) )
#' ## Chronic pain data: a regression graph
#' chronic.pain <- makeMG(dg = DAG(Y ~ Za, Za ~ Zb + A, Xa ~ Xb, 
#' Xb ~ U+V, U ~ A + V, Zb ~ B, A ~ B), bg = UG(~Za*Xa + Zb*Xb))
#' 
`makeMG` <- function (dg = NULL, ug = NULL, bg = NULL) 
{
    dg.nodes <- rownames(dg)
    ug.nodes <- rownames(ug)
    bg.nodes <- rownames(bg)
    ver <- unique(c(dg.nodes, ug.nodes, bg.nodes))
    d <- length(ver)
    amat <- matrix(0, d, d)
    dimnames(amat) <- list(ver, ver)
    amat.dg <- amat
    amat.ug <- amat
    amat.bg <- amat
    if (!is.null(dg)) 
        amat.dg[dg.nodes, dg.nodes] <- dg
    if (!is.null(ug)) 
        amat.ug[ug.nodes, ug.nodes] <- ug * 10
    if (!is.null(bg)) 
        amat.bg[bg.nodes, bg.nodes] <- bg * 100
    amat.dg + amat.ug + amat.bg
}     


#' Loopless mixed graphs components
#' 
#' Splits the adjacency matrix of a loopless mixed graph into three components:
#' directed, undirected and bi-directed.
#' 
#' The matrices \code{ug}, and \code{bg} are just symmetric Boolean matrices.
#' 
#' @param amat a square matrix, with dimnames, representing a loopless mixed
#' graph. The matrix consists of 4 different integers as an \eqn{ij}-element: 0
#' for a missing edge between \eqn{i} and \eqn{j}, 1 for an arrow from \eqn{i}
#' to \eqn{j}, 10 for a full line between \eqn{i} and \eqn{j}, and 100 for a
#' bi-directed arrow between \eqn{i} and \eqn{j}. These numbers are added to be
#' associated with multiple edges of different types. The matrix is symmetric
#' w.r.t full lines and bi-directed arrows.
#' @return It is the inverse of \code{makeAG}. It returns the following
#' components.  \item{dg}{the adjacency matrix of the directed edges.}
#' \item{ug}{the adjacency matrix of the undirected edges.} \item{bg}{the
#' adjacency matrix of the bi-directed edges.}
#' @author Mathias Drton, Giovanni M. Marchetti
#' @seealso \code{\link{makeMG}}
#' @keywords graphs ancestral graph mixed graph models multivariate
#' @examples
#' 
#' ag <- makeMG(ug=UG(~y0*y1), dg=DAG(y4~y2, y2~y1), bg=UG(~y2*y3+y3*y4))  
#' isAG(ag)
#' unmakeMG(ag)
#' 
`unmakeMG` <- function(amat){
    ### Returns a list with the three components of a loopless MG.
    d <- nrow(amat)
    ug <- dg <- bg <- amat
    M <- expand.grid(dg = 0:1,ug = 0:1,bg = 0:1)
    i <- strtoi(as.character(amat), 2)
    GG <- M[i+1,]
    ug[,] <- GG[,2] 
    dg[,] <- GG[,1]
    bg[,] <- GG[,3]
    if(any(ug!=t(ug))) stop("Undirected edges are wrongly coded.")
    if(any(bg!=t(bg))) stop("Undirected edges are wrongly coded.")
    return(list(dg = dg, ug = ug, bg = bg))   
}  


#' Directed graphs
#' 
#' Defines the adjacency of a directed graph.
#' 
#' The directed graph is defined by a sequence of models formulae.  For each
#' formula the response defines a node of the graph and its parents. The graph
#' contains no loops.
#' 
#' @param \dots a sequence of model formulae
#' @return the adjacency matrix of the directed graph, i.e., a square Boolean
#' matrix of order equal to the number of nodes of the graph and a one in
#' position \eqn{(i,j)} if there is an arrow from \eqn{i} to \eqn{j} and zero
#' otherwise.  The dimnames of the adjacency matrix are the labels for the
#' nodes of the graph.
#' @author G. M. Marchetti
#' @seealso \code{\link{DAG}}, \code{\link{UG}}
#' @references Lauritzen, S. (1996). \emph{Graphical models}. Oxford: Clarendon
#' Press.
#' @keywords graphs directed graph models multivariate
#' @examples
#' 
#' ## A DAG
#' DG(y ~ x, x ~ z, z ~ u)
#' 
#' ## A cyclic directed graph
#' DG(y ~ x, x ~ z, z ~ y)
#' 
#' ## A graph with two arrows between two nodes
#' DG(y ~ x, x ~ y)
#' 
#' ## There can be isolated nodes
#' DG(y ~ x, x ~ x)
#' 
`DG` <- function (...) 
{
    f <- list(...)
    nb <- length(f)
    nod <- c()
    for (k in 1:nb) {
        tt <- terms(f[[k]], specials = "I")
        vars <- dimnames(attr(tt, "factors"))[[1]]
        skip <- attr(tt, "specials")$I
        if (!is.null(skip)) 
            vars <- vars[-skip]
        nod <- c(nod, vars)
    }
    N <- unique(nod)
    dN <- length(N)
    amat <- matrix(0, dN, dN)
    for (k in 1:nb) {
        tt <- terms(f[[k]], specials = "I")
        vars <- dimnames(attr(tt, "factors"))[[1]]
        if (attr(tt, "response") == 1) {
            j <- match(vars[1], N)
            i <- match(vars[-1], N)
            amat[i, j] <- 1
        }
        else if (attr(tt, "response") == 0) 
            stop("Some equations have no response")
    }
    dimnames(amat) <- list(N, N)
    amat
}    


#' Ancestral graph
#' 
#' Check if it is an adjacency matrix of an ancestral graph
#' 
#' Checks if the following conditions must hold: (i) no undirected edge meets
#' an arrowhead; (ii) no directed cycles; (iii) spouses cannot be ancestors.
#' For details see Richardson and Spirtes (2002).
#' 
#' @param amat %% ~~Describe \code{amat} here~~
#' @return A logical value, \code{TRUE} if it is an ancestral graph and
#' \code{FALSE} otherwise.
#' @author Giovanni M. Marchetti, Mathias Drton
#' @seealso \code{\link{makeMG}}, \code{\link{isADMG}}
#' @references Richardson, T. S. and Spirtes, P. (2002). Ancestral Graph Markov
#' Models. \emph{Annals of Statistics}, 30(4), 962--1030.
#' @keywords graphs ancestral graph mixed graph models multivariate
#' @examples
#' 
#' 	## Examples from Richardson and Spirtes (2002)
#' 	a1 <- makeMG(dg=DAG(a~b, b~d, d~c), bg=UG(~a*c))  
#' 	isAG(a1)    # Not an AG. (a2) p.969    
#' 	a2 <- makeMG(dg=DAG(b ~ a, d~c), bg=UG(~a*c+c*b+b*d))           # Fig. 3 (b1) p.969  
#' 	isAG(a2)
#' 
`isAG` <- function(amat) {
### checks if a graph is an ancestral graph
    comp <- unmakeMG(amat)
    ug <- comp$ug; dag = comp$dg; bg = comp$bg  
    out <- TRUE

    if(any(amat > 100)){
    	 warning("There are double edges.")
    	 out <- FALSE
    	 }
    anteriorGraph <- function (amat) 
      {
        ## Adjacency matrix of the anterior graph from an AG.
        A <- 0 + ((amat == 1) |(amat == 10)) # Select the directed and undirected edges
        transClos(A)
      }
    if(any(apply(dag, 2, sum) & apply(ug, 2, sum))){
    	warning("Undirected edges meeting a directed edge.")
    	out <- FALSE
    	}
    if(any(apply(bg, 2, sum) & apply(ug, 2, sum))){
      warning("Undirected edges meeting a bidirected edge.")
      out <- FALSE
    }    
    H <- anteriorGraph(amat)
 
    if(any((H==1) & (amat == 100))){
      warning("Spouses cannot be ancestors.")
      out <- FALSE
    }  
    out
  }      



#' Acyclic directed mixed graphs
#' 
#' Check if it is an adjacency matrix of an ADMG
#' 
#' Checks if the following conditions must hold: (i) no undirected edge meets
#' an arrowhead; (ii) no directed cycles;
#' 
#' @param amat %% ~~Describe \code{amat} here~~
#' @return A logical value, \code{TRUE} if it is an ancestral graph and
#' \code{FALSE} otherwise.
#' @author Giovanni M. Marchetti, Mathias Drton
#' @seealso \code{\link{makeMG}}, \code{\link{isADMG}}
#' @references Richardson, T. S. and Spirtes, P. (2002). Ancestral Graph Markov
#' Models. \emph{Annals of Statistics}, 30(4), 962--1030.
#' @keywords graphs ancestral graph mixed graph models multivariate
#' @examples
#' 
#' 	## Examples from Richardson and Spirtes (2002)
#' 	a1 <- makeMG(dg=DAG(a~b, b~d, d~c), bg=UG(~a*c))  
#' 	isADMG(a1)    # Not an AG. (a2) p.969    
#' 	a2 <- makeMG(dg=DAG(b ~ a, d~c), bg=UG(~a*c+c*b+b*d))           # Fig. 3 (b1) p.969  
#' 	isADMG(a2)
#' 
`isADMG`<- function(amat){
  ### check is if a graph is an ADMG
  comp <- unmakeMG(amat)
  ug <- comp$ug; dag <- comp$dg; bg <- comp$bg  
  out <- TRUE
  if(any(amat > 100)){  
    warning("There are double edges.")
    out <- FALSE
  }
  if(!isAcyclic(dag)){
    warning("Not acyclic.")
    out <- FALSE
  }
  out 
}       



#' Plot of a mixed graph
#' 
#' Plots a mixed graph from an adjacency matrix, a \code{graphNEL} object, an
#' \code{\link{igraph}} object, or a descriptive vector.
#' 
#' \code{plotGraph} uses \code{\link{plot}} and \code{\link{tkplot}} in
#' \pkg{\link{igraph}} package.
#' 
#' @param a An adjacency matrix: a matrix that consists of 4 different integers
#' as an \eqn{ij}-element: 0 for a missing edge between \eqn{i} and \eqn{j}, 1
#' for an arrow from \eqn{i} to \eqn{j}, 10 for a full line between \eqn{i} and
#' \eqn{j}, and 100 for a bi-directed arrow between \eqn{i} and \eqn{j}. These
#' numbers can be added to generate multiple edges of different types. The
#' matrix must be symmetric w.r.t full lines and bi-directed arrows.  Or a
#' graph that can be a \code{graphNEL} or an \code{\link{igraph}} object.Or a
#' vector of length \eqn{3e}, where \eqn{e} is the number of edges of the
#' graph, that is a sequence of triples (type,node1label,node2label). The type
#' of edge can be \code{"a"} (arrows from node1 to node2), \code{"b"} (arcs),
#' and \code{"l"} (lines).
#' @param dashed A logical value. If \code{TRUE} the bi-directed edges are
#' plotted as undirected dashed edges.
#' @param tcltk A logical value. If \code{TRUE} the function opens a tcltk
#' device to plot the graphs, allowing the interactive manimulation of the
#' graph. If \code{FALSE}the function opens a standard device without
#' interaction.
#' @param layout The name of a function used to compute the (initial) layout of
#' the graph. The default is \code{layout.auto}. This can be further adjusted
#' if \code{tcltk} is \code{TRUE}.
#' @param directed A logical value. If \code{FALSE} a symmetric adjacency
#' matrix with entries 1 is interpreted as an undirected graph. If \code{FALSE}
#' it is interpreted as a directed graph with double arrows. If \code{a} is not
#' an adjacency matrix, it is ignored.
#' @param noframe A logical value. If \code{TRUE}, then the nodes are not
#' circled.
#' @param nodesize An integer denoting the size of the nodes (default 15). It
#' can be increased to accommodate larger labels.
#' @param vld An integer defining the distance between a vertex and its label.
#' Defaults to 0.
#' @param vc Vertex color. Default is "gray".
#' @param vfc Vertex frame color. Default is "black".
#' @param colbid Color of the bi-directed edges. Default is "FireBrick3".
#' @param coloth Color of all the other edges. Default is "black".
#' @param cex An integer (defaults to 1) to adjust the scaling of the font of
#' the labels.
#' @param \dots Further arguments to be passed to \code{plot} or \code{tkplot}.
#' @return Plot of the associated graph and returns invisibly a list with two
#' slots: \code{tkp.id}, \code{graph}, the input graph as an \code{igraph}
#' object.  The id can be used to get the layout of the adjusted graph.  The
#' bi-directed edges are plotted in red.
#' @author Kayvan Sadeghi, Giovanni M. Marchetti
#' @seealso \code{\link{grMAT}}, \code{\link{tkplot}}, \code{\link{drawGraph}},
#' \code{\link{plot.igraph}}
#' @keywords graphs adjacency matrix mixed graphs plot
#' @examples
#' 
#' exvec<-c("b",1,2,"b",1,14,"a",9,8,"l",9,11,
#'          "a",10,8,"a",11,2,"a",11,9,"a",11,10,
#'          "a",12,1,"b",12,14,"a",13,10,"a",13,12)
#' plotGraph(exvec)
#' ############################################
#' amat<-matrix(c(0,11,0,0,10,0,100,0,0,100,0,1,0,0,1,0),4,4)
#' plotGraph(amat)     
#' plotGraph(makeMG(bg = UG(~a*b*c+ c*d), dg = DAG(a ~ x + z, b ~ z )))
#' plotGraph(makeMG(bg = UG(~a*b*c+ c*d), dg = DAG(a ~ x + z, b ~ z )), dashed = TRUE)    
#' # A graph with double and triple edges
#' G <-
#' structure(c(0, 101, 0, 0, 100, 0, 100, 100, 0, 100, 0, 100, 0, 
#' 111, 100, 0), .Dim = c(4L, 4L), .Dimnames = list(c("X", "Z", 
#' "Y", "W"), c("X", "Z", "Y", "W")))
#' plotGraph(G)      
#' # A regression chain graph with longer labels
#'  plotGraph(makeMG(bg = UG(~Love*Constraints+ Constraints*Reversal+ Abuse*Distress), 
#'    dg = DAG(Love ~ Abuse + Distress, Constraints ~ Distress, Reversal ~ Distress, 
#'    Abuse ~ Fstatus, Distress ~ Fstatus), 
#'    ug = UG(~Fstatus*Schooling+ Schooling*Age)), 
#'    dashed = TRUE, noframe = TRUE)    
#' # A graph with 4 edges between two nodes. 
#' G4 = matrix(0, 2, 2); G4[1,2] = 111; G4[2,1] = 111
#' plotGraph(G4)
#' 
`plotGraph` <- function (a, dashed = FALSE, tcltk = TRUE, layout = layout.auto, directed = FALSE, noframe = FALSE, nodesize = 15, vld = 0, vc = "gray", vfc = "black", colbid = "FireBrick3", coloth = "black", cex = 1.5, ...) 
{
  if (class(a)[1] == "igraph" || class(a)[1] == "graphNEL" || class(a)[1] == 
    "character") {
    a <- grMAT(a)
  }
  if (is(a,"matrix")) {
    if (nrow(a) == ncol(a)) {
      if (length(rownames(a)) != ncol(a)) {
        rownames(a) <- 1:ncol(a)
        colnames(a) <- 1:ncol(a)
      }
      if (!directed) {
        if (all(a == t(a)) & all(a[a != 0] == 1)) {
          a <- a * 10
        }
      }
      l1 <- c()
      l2 <- c()
      for (i in 1:nrow(a)) {
        for (j in i:nrow(a)) {
          if (a[i, j] == 1) {
            l1 <- c(l1, i, j)
            l2 <- c(l2, 2)
          }
          if (a[j, i]%%10 == 1) {
            l1 <- c(l1, j, i)
            l2 <- c(l2, 2)
          }
          if (a[i, j] == 10) {
            l1 <- c(l1, i, j)
            l2 <- c(l2, 0)
          }
          if (a[i, j] == 11) {
            l1 <- c(l1, i, j, i, j)
            l2 <- c(l2, 2, 0)
          }
          if (a[i, j] == 100) {
            l1 <- c(l1, i, j)
            l2 <- c(l2, 3)
          }
          if (a[i, j] == 101) {
            l1 <- c(l1, i, j, i, j)
            l2 <- c(l2, 2, 3)
          }
          if (a[i, j] == 110) {
            l1 <- c(l1, i, j, i, j)
            l2 <- c(l2, 0, 3)
          }
          if (a[i, j] == 111) {
            l1 <- c(l1, i, j, i, j, i, j)
            l2 <- c(l2, 2, 0, 3)
          }
        }
      }
    }
    else {
      stop("'object' is not in a valid adjacency matrix form")
    }
    if (length(l1) > 0) {
      ## l1 <- l1 - 1   # igraph0
      agr <- graph(l1, n = nrow(a), directed = TRUE)
    }
    if (length(l1) == 0) {
      agr <- graph.empty(n = nrow(a), directed = TRUE)
      return(tkplot(agr, vertex.label = rownames(a)))
    }
    ed0 <- get.edgelist(agr)
    ne <- nrow(ed0)
    ed <- apply(apply(ed0, 1, sort), 2, paste, collapse = "-")
    tb = table(ed)
    curve <- rep(0, ne)
    if (any(tb > 1)) {
      tb <- tb[tb > 1]
      for (i in 1:length(tb)) {
        reped <- names(tb[i]) == ed
        U = ed0[reped, ]
        if (sum(reped) == 2) {
          ed0[reped]
          if (all(is.element(c(0, 3), l2[reped]))) {
            curve[reped] <- c(0.9, -0.9)
          }
          if (all(U[1, ] == U[2, ])) {
            curve[reped] <- c(0.6, -0.6)
          }
          else {
            curve[reped] <- c(0.6, 0.6)
          }
        }
        if (sum(reped) == 3) {
          curve[(l2 == 3) & reped] <- 0.9
          curve[(l2 == 0) & reped] <- -0.9
        }
        if (sum(reped) == 4) {
          curve[(l2 == 3) & reped] <- 0.3
          curve[(l2 == 0) & reped] <- -0.3
          curve[(l2 == 1) & reped] <- 0.9
          curve[(l2 == 2) & reped] <- 0.9
        }
      }
    }
    col = rep(coloth, ne)
    col[l2 == 3] <- colbid
    if (dashed) {
      ety = rep(1, ne)
      ety[l2 == 3] <- 2
      l2[l2 == 3] <- 0
    }
    else {
      ety = rep(1, ne)
    }
    if (noframe) {
      vfc <- "white"
      vc <- "white"
    }
    if(tcltk == TRUE){
      id <- tkplot(agr, layout = layout, edge.curved = curve, 
                   vertex.label = rownames(a), edge.arrow.mode = l2, 
                   edge.color = col, edge.lty = ety, 
                   vertex.label.family = "sans", 
                   edge.width = 1.5, vertex.size = nodesize, 
                   vertex.frame.color = vfc, vertex.color = vc, 
                   vertex.label.cex = cex, edge.arrow.width = 1, 
                   edge.arrow.size = 1.2, vertex.label.dist = vld, ...)
      }
    else {
      id <- plot(agr, layout = layout, edge.curved = curve, 
                   vertex.label = rownames(a), edge.arrow.mode = l2, 
                   edge.color = col, edge.lty = ety, 
                   vertex.label.family = "sans", 
                   edge.width = 2, vertex.size = nodesize*1.5, 
                   vertex.frame.color = vfc, vertex.color = vc, 
                   vertex.label.cex = cex*0.8, edge.arrow.width = 2, 
                   edge.arrow.size = .5, vertex.label.dist = vld, ...)
      }
  V(agr)$name <- rownames(a)
  agr <- set.edge.attribute(agr, "edge.arrow.mode", index = E(agr), l2)
  return(invisible(list(tkp.id = id, igraph = agr)))
}
else {
  stop("'object' is not in a valid format")
}
}

## Fit multivariate logistic model with individual covariates
#############################################################
 




#' Inverts a marginal log-linear parametrization
#' 
#' Inverts a marginal log-linear parametrization.
#' 
#' A marginal log-linear link is defined by \eqn{\eta = C (M \log p)}. See
#' Bartolucci et al. (2007).
#' 
#' @param eta a vector of dimension \code{t-1} where \code{t} is the number of
#' cells of a contingency table.
#' @param C A contrast matrix.
#' @param M A marginalization matrix.
#' @param G G is the model matrix of the loglinear parameterization with no
#' constant term.
#' @param maxit an integer, specifying the maximum number of iterations.
#' Default 500.
#' @param print a logical value: if \code{TRUE}, prints the criterion after
#' each cycle.
#' @param tol A small value specifying the tolerance for the convergence
#' criterion. Default: \code{1e-10}.
#' @return A vector of probabilities \code{p}.
#' @note From a Matlab function by A. Forcina, University of Perugia, Italy.
#' @author Antonio Forcina, Giovanni M. Marchetti
#' @seealso \code{\link{mat.mlogit}}
#' @references Bartolucci, F., Colombi, R. and Forcina, A. (2007). An extended
#' class of marginal link functions for modelling contingency tables by
#' equality and inequality constraints. Statist. Sinica 17, 691-711.
#' @keywords marginal log-linear models discrete data
`binve`  <- function(eta, C, M, G, maxit=500, print=FALSE, tol = 1e-10){
# Inverts a marginal loglinear parameterization.
# eta  has dimension t-1, 
# G is the model matrix of the loglinear parameterization with no intercept.
# C and M are the matrices of the link. 
# From a Matlab function by A. Forcina, University of Perugia, Italy.
    
                                                           
    ## starting values

    k <- nrow(C)
    pmin <- 1e-100
    kG  <-  ncol(G)
    err <- 0
    th0  <-  matrix(1, k, 1) / 100

    ## prepare to iterate
    it <- 0
    mit <- 500          
    p <- exp(G %*% th0)
    p <-  p/sum(p)
    t <- M %*% p        
    d <- eta - C %*% log(t)
    div0 <- crossprod(d)
    hh <- 0
    s <- c(.1, .6, 1.2)
    div <- div0 + 1            
    while((it < maxit) & (div > tol)){
        R0 <- C %*% diagv(1/t, M) %*% diagv(p,G)
        rco <- rcond(R0) > tol
        ub <- 0
        while(rco==FALSE){
            cat("Rank: ", qr(R0)$rank, "\n")  
            R0 <- R0 + diag(k)
            rco <- rcond(R0) > tol
        }
        de  <-  solve(R0) %*% d
        dem  <-  max(abs(de))
        de  <-  (dem <= 4)*de + (dem>4)*4*de/dem
        th <- th0 + de
        p <- exp(G %*% th)
        p <- p/sum(p)
        p <- p+pmin*(p<pmin)
        p <- p/sum(p)
        t <- M %*% p
        d <- eta-C %*% log(t)
        div <- crossprod(d)
        rid <- (.01+div0)/(.01+div)
        iw <- 0
        while( (rid <0.96) & (iw<10)){
            th  <- th0 + de  %*% (rid^(iw+1))
            p <- exp(G %*% th)
            p <- p/sum(p)
            p <- p + pmin*(p<pmin)
            p <- p/sum(p)
            t <- M %*% p
            d <- (eta-C %*% log(M %*% p))
            div <- crossprod(d)
            rid <- (.01 + div)/(.01 + div0)
            iw <- iw+1;  it <- it+1
        }
        if( rid < 0.96){
            it <- mit
        }
        else{
            it <- it+1
            th0 <- th
        }
    }
   if (div > tol) {
        warning("div > tolerance.", call. =FALSE)
    }
    if (any(is.nan(p))){
        warning("Some NaN in the vector of probabilities.", call. =FALSE)
    }
    if (it > maxit){
        warning("Maximum number of iteration reached.", call. = FALSE)
    }
    if(print){
       cat("Iterations: ", it, ", Div = ", div, ".\n")
    }
    as.vector(p)
}       

             

# The following function has been generalized and called mlogit.param



#' Multivariate logistic parametrization
#' 
#' Find matrices \code{C} and \code{M} of e binary multivariate logistic
#' parameterization.
#' 
#' The power set is in the order of dimensions of the sets.
#' 
#' @param d A positive integer, the number of binary responses.
#' @param P A list of vectors of integers specifying margins. For instance
#' \code{list(1, 2, c(1,2))}. Default: the power set of \code{1:d}.
#' @return \item{C}{A contrast matrix.} \item{L}{A marginalization matrix.}
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{binomial}, \link{marg.param}}
#' @references Glonek, G. J. N. and McCullagh, P. (1995). Multivariate logistic
#' models. Journal of the Royal Statistical Society, Ser. B 57, 533-546.
#' @keywords logistic model
#' @examples
#'  
#' mat.mlogit(2)
#' 
`mat.mlogit` <- function(d, P = powerset(1:d)) {
## Find matrices C and M of binary mlogit parameterization  for a table 2^d. 
## The output will be in the ordering of P.
## Here for 3 variables is: 1 2 3 12 13 23 123.  

`margmat` <- function(bi, mar){
### Defines the marginalization matrix
    if(mar ==  FALSE){
      matrix(1, 1, bi)
    }
    else {
      diag(bi)
    }
  }

`contrmat` <- function(bi, i){
### Contrast matrix
    if(i == FALSE){
      1
    }
    else{
      cbind(-1, diag(bi-1))
    }
  }
  V <- 1:d

  C <- matrix(0,0,0)
  L <- c()

  for(mar in P){
    K <- 1
    H <- 1
    for(i in V){
      w <- is.element(i, mar)
      K <- contrmat(2, w) %x% K
      H <- margmat(2, w)  %x% H
    }
    C <- blkdiag(C, K)
    L <- rbind(L, H)
  }
  list(C=C, L=L)
}   



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

     


#' Matrix product with a diagonal matrix
#' 
#' Computes faster the product of a diagonal matrix times a full matrix.
#' 
#' Computes \eqn{N = D_v M} where \eqn{D_v} is diagonal avoiding the
#' \code{diag} operator.
#' 
#' @param v A numeric vector specifying the elements on the diagonal of a
#' matrix.
#' @param M A numeric matrix compatible with the product \eqn{D_v M}.
#' @return A matrix \code{N}.
#' @seealso \code{\link{diag}}
#' @keywords matrix
#' @examples
#' 
#' v <- 1:1000
#' M <- matrix(runif(3000), 1000, 3)
#' dim(diagv(v, M))
#' 
`diagv` <-     function(v,M){
# Computes N = diag(v) %*% M avoiding the diag operator.
    as.vector(v) * M
}
         


#' Block diagonal matrix
#' 
#' Split a vector x into a block diagonal matrix.
#' 
#' 
#' @param x A vector of length \code{n}.
#' @param blo A vector of positive integers such that \code{sum(blo) == n}.
#' @return A block-diagonal matrix with as many row as elements of \code{blo}
#' and \code{n} columns. The vector \code{x} is split into \code{length(blo)}
#' sub-vectors and these are the blocks of the resulting matrix.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{blkdiag}}, \code{\link{diag}}
#' @keywords matrix
#' @examples
#' 
#' blodiag(1:10, blo = c(2, 3, 5)) 
#' blodiag(1:10, blo = c(3,4,0,1))
#' 
`blodiag` = function(x, blo){
# Split a vector x into a block diagonal matrix bith components blo.
# Used by fitmlogit.
k = length(blo) 
 B = matrix(0, k, sum(blo))
 u = cumsum(c(1, blo))
 for(i in 1:k){       
	  sub = u[i]:(u[i+1]-1)
      B[i,sub] = x[sub]	
 }   
B
}


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



#' Link function of marginal log-linear parameterization
#' 
#' Provides the contrast and marginalization matrices for the marginal
#' parametrization of a probability vector.
#' 
#' See Bartolucci, Colombi and Forcina (2007).
#' 
#' @param lev Integer vector containing the number of levels of each variable.
#' @param type A character vector with elements \code{"l"}, \code{"g"},
#' \code{"c"}, or \code{"r"} indicating the type of logit. The meaning is as
#' follows: \code{"g"} for global, \code{"c"} for continuation, \code{"r"} for
#' reverse continuation and \code{"l"} for local.
#' @return \item{C}{Matrix of constrasts (the first \code{sum(lev)-length(r)}
#' elements are referred to univariate logits)} \item{M}{Marginalization matrix
#' with elements 0 and 1.} \item{G}{Corresponding design matrix for the
#' corresponding log-linear model.}
#' @note Assumes that the vector of probabilities is in inv lex order.  The
#' interactions are returned in order of dimension, like e.g., 1, 2, 3, 12, 13,
#' 23, 123.
#' @author Francesco Bartolucci, Antonio Forcina, Giovanni M. Marchetti
#' @seealso \code{\link{mat.mlogit}}
#' @references Bartolucci, F., Colombi, R. and Forcina, A. (2007). An extended
#' class of marginal link functions for modelling contingency tables by
#' equality and inequality constraints. Statist. Sinica 17, 691-711.
#' @keywords logistic models ordinal models
#' @examples
#'     
#' marg.param(c(3,3), c("l", "g"))
#' 
`marg.param` = function(lev,type) 
# Creates matrices C and M for the marginal parametrization
# of the probability vector for a vector of categorical variables.
# INPUT:
# lev:  vector containing the number of levels of each variable
# type: vector with elements 'l', 'g', 'c', 'r' indicating the type of logit
#       'g' for global, 
#       'c' for continuation,
#       'r' for reverse continuation, 
#       'l' for local.
# OUTPUT:
# C:    matrix of constrats (the first sum(lev)-length(r) elements are
#       referred to univariate logits)
# M:    marginalization matrix with elements 0 and 1
# G:    corresponding design matrix for the corresponding log-linear model   
# Translated from a Matlab function by Bartolucci and Forcina.
# NOTE: assumes that the vector of probabilities is in inv lex order. 
#       The interactions are returned in order of dimension, like e.g.,  1 2 3 12 13 23 123. 
{
# preliminaries

`powset` <- function(d)
# Power set P(d).
{      
	P = expand.grid(rep(list(1:2), d))
	P[order(apply(P, 1, sum)),]-1
}

  r = length(lev)
# create sets of parameters
 S = powset(r)
 S = S[-1,]  # drop the empty set
  C = c(); M = c(); G = c()
  for (i in 1:nrow(S)){
    si = S[i,] 
    Ci = 1  # to update matrix C
    for (h in 1:r){
      if(si[h]==1){
        I = diag(lev[h] - 1)
        Ci = cbind(-I, I) %x% Ci 
      }
    }
    C = blkdiag(C, Ci)
    Mi = 1  # to update matrix M
    for (h in 1:r) { 
	  lh = lev[h]-1 
      if(si[h]==1) {
        I = diag(lh)   
        T = 0 + lower.tri(matrix(1, lh,lh), diag=TRUE)
        ze = matrix(0, lh, 1)
        Mi = switch(type[h], 
          l = rbind(cbind(I, ze), cbind(ze, I)) %x%  Mi,
          g = rbind(cbind(T, ze), cbind(ze, t(T))) %x%  Mi,
          c = rbind(cbind(I, ze), cbind(ze, t(T))) %x%  Mi,
          r = rbind(cbind(T, ze), cbind(ze, I)) %x%  Mi)
      }        
      else {
        Mi = matrix(1, 1,lev[h])  %x%  Mi
      } 
    }    
    M = rbind(M, Mi)  
    
    Gi = 1  # for the design matrix
    for (h in 1:r) { 
	  lh = lev[h] 
      if(si[h]==1) {                          
	     T = 0 + lower.tri(matrix(1, lh,lh), diag=TRUE); T = T[,-1]  
        Gi = T %x% Gi   
      }
      else{
        Gi =  matrix(1, lh, 1) %x% Gi
      }
    }
    G = cbind(G, Gi)
  }   
list(C = C, M = M, G = G)
}



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

# source("~/Documents/R/graphical_models/fitmlogit.R")   
# fitmlogit(A ~X, B ~ Z, cbind(A, B) ~ 1, data = datisim)    
# source("~/Documents/R/graphical_models/ilaria/sim-blogit.R") 
