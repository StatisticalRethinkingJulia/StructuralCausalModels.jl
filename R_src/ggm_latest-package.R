

#' Anger data
#' 
#' Anger data
#' 
#' Trait variables are viewed as stable personality characteristics, and state
#' variables denote behaviour in specific situations.  See Cox and Wermuth
#' (1996).
#' 
#' @name anger
#' @docType data
#' @format A covariance matrix for 4 variables measured on 684 female students.
#' \describe{ \item{X}{anxiety state} \item{Y}{anger state} \item{Z}{anxiety
#' trait} \item{U}{anger trait} }
#' @references Cox, D. R. and Wermuth, N. (1996). \emph{Multivariate
#' dependencies}. London: Chapman and Hall.
#' 
#' Cox, D.R. and Wermuth, N. (1990). \emph{An approximation to maximum
#' likelihood estimates in reduced models}. 77(4), 747-761.
#' @keywords datasets
#' @examples
#'  
#' # Fit a chordless 4-cycle model 
#' data(anger) 
#' G = UG(~ Y*X + X*Z + Z*U + U*Y)
#' fitConGraph(G,anger, 684) 
#' 
NULL





#' Data on blood pressure body mass and age
#' 
#' Raw data on blood pressure, body mass and age on 44 female patients, and
#' covariance matrix for derived variables.
#' 
#' 
#' @name derived
#' @docType data
#' @format A list containing a dataframe \code{raw} with 44 lines and 5 columns
#' and a symmetric 4x4 covariance matrix \code{S}.
#' 
#' The following is the description of the variables in the dataframe
#' \code{raw} \describe{ \item{list("Sys")}{Systolic blood pressure, in mm Hg}
#' \item{list("Dia")}{Diastolic blood pressure, in mm Hg}
#' \item{list("Age")}{Age of the patient, in years} \item{list("Hei")}{Height,
#' in cm} \item{list("Wei")}{Weight, in kg} } The following is the description
#' of the variables for the covariance matrix \code{S}.  \describe{
#' \item{list("Y")}{Derived variable \code{Y=log(Sys/Dia)}}
#' \item{list("X")}{Derived variables \code{X=log(Dia)}} \item{list("Z")}{Body
#' mass index \code{Z=Wei/(Hei/100)^2}} \item{list("W")}{Age} }
#' @references Wermuth N. and Cox D.R. (1995). Derived variables calculated
#' from similar joint responses: some characteristics and examples.
#' \emph{Computational Statistics and Data Analysis}, 19, 223-234.
#' @keywords datasets
#' @examples
#' 
#' # A DAG model with a latent variable U
#' G = DAG(Y ~ Z + U, X ~ U + W, Z ~ W)
#' 
#' data(derived)
#' 
#' # The model fitted using the derived variables
#' out = fitDagLatent(G, derived$S, n = 44, latent = "U")
#' 
#' # An ancestral graph model marginalizing over U
#' H = AG(G, M = "U")
#' 
#' # The ancestral graph model fitted obtaining the 
#' # same result
#' out2 = fitAncestralGraph(H, derived$S, n = 44)
#' 
NULL





#' The package \code{ggm}: summary information
#' 
#' This package provides functions for defining, manipulating and fitting
#' graphical Markov models.
#' 
#' 
#' @section Functions: The main functions can be classified as follows.
#' \itemize{ \item Functions for defining graphs (undirected, directed acyclic,
#' ancestral graphs): \code{\link{UG}}, \code{\link{DAG}},
#' \code{\link{makeMG}}, \code{\link{grMAT}}; \item Functions for doing graph
#' operations (parents, boundary, cliques, connected components, fundamental
#' cycles, d-separation, m-separation): \code{\link{pa}}, \code{\link{bd}},
#' \code{\link{cliques}}, \code{\link{conComp}}, \code{\link{fundCycles}};
#' \item Functions for testing independence statements and generating maximal
#' graphs from non-maximal graphs: \code{\link{dSep}}, \code{\link{msep}},
#' \code{\link{Max}}; \item Function for finding covariance and concentration
#' graphs induced by marginalization and conditioning:
#' \code{\link{inducedCovGraph}}, \code{\link{inducedConGraph}}; \item
#' Functions for finding multivariate regression graphs and chain graphs
#' induced by marginalization and conditioning: \code{\link{inducedRegGraph}},
#' \code{\link{inducedChainGraph}}, \code{\link{inducedDAG}}; \item Functions
#' for finding stable mixed graphs (ancestral, summary and ribbonless) after
#' marginalization and conditioning: \code{\link{AG}}, \code{\link{SG}},
#' \code{\link{RG}}; \item Functions for fitting by ML Gaussian DAGs,
#' concentration graphs, covariance graphs and ancestral graphs:
#' \code{\link{fitDag}}, \code{\link{fitConGraph}}, \code{\link{fitCovGraph}},
#' \code{\link{fitAncestralGraph}}; \item Functions for testing several
#' conditional independences:\code{\link{shipley.test}}; \item Functions for
#' checking global identification of DAG Gaussian models with one latent
#' variable (Stanghellini-Vicard's condition for concentration graphs, new
#' sufficient conditions for DAGs): \code{\link{isGident}},
#' \code{\link{checkIdent}}; \item Functions for fitting Gaussian DAG models
#' with one latent variable: \code{\link{fitDagLatent}}; \item Functions for
#' testing Markov equivalences and generating Markov equivalent graphs of
#' specific types: \code{\link{MarkEqRcg}}, \code{\link{MarkEqMag}},
#' \code{\link{RepMarDAG}}, \code{\link{RepMarUG}}, \code{\link{RepMarBG}}. }
#' The package is intended as a contribution to the gR-project derscribed by
#' Lauritzen (2002).
#' @references Lauritzen, S. L. (2002). gRaphical Models in R. \emph{R News},
#' 3(2)39.
#' @keywords multivariate models graphs
NULL





#' Glucose control
#' 
#' Data on glucose control of diabetes patients.
#' 
#' Data on 68 patients with fewer than 25 years of diabetes. They were
#' collected at the University of Mainz to identify psychological and
#' socio-economic variables possibly important for glucose control, when
#' patients choose the appropriate dose of treatment depending on the level of
#' blood glucose measured several times per day.
#' 
#' The variable of primary interest is \code{Y}, glucose control, measured by
#' glycosylated haemoglobin. \code{X}, knowledge about the illness, is a
#' response of secondary interest. Variables \code{Z}, \code{U} and \code{V}
#' measure patients' type of attribution, called fatalistic externality, social
#' externality and internality. These are intermediate variables. Background
#' variables are \code{W}, the duration of the illness, \code{A} the duration
#' of formal schooling and \code{B}, gender. The background variables \code{A}
#' and \code{B} are binary variables with coding \code{-1}, \code{1}.
#' 
#' @name glucose
#' @docType data
#' @format A data frame with 68 observations on the following 8 variables.
#' \describe{ \item{Y}{a numeric vector, Glucose control (glycosylated
#' haemoglobin), values up to about 7 or 8 indicate good glucose control.}
#' \item{X}{a numeric vector, a score for knowledge about the illness.}
#' \item{Z}{a numeric vector, a score for fatalistic externality (mere chance
#' determines what occurs).} \item{U}{a numeric vector, a score for social
#' externality (powerful others are responsible).} \item{V}{a numeric vector, a
#' score for internality (the patient is him or herself responsible).}
#' \item{W}{a numeric vector, duration of the illness in years.} \item{A}{a
#' numeric vector, level of education, with levels \code{-1}: at least 13 years
#' of formal schooling, \code{1}: less then 13 years.} \item{B}{a numeric
#' vector, gender with levels \code{-1}: females, \code{1}: males.} }
#' @references Cox, D. R. \& Wermuth, N. (1996). \emph{Multivariate
#' dependencies}. London: Chapman \& Hall.
#' @source Cox & Wermuth (1996), p. 229.
#' @keywords datasets
#' @examples
#' 
#' data(glucose)
#' ## See Cox & Wermuth (1996), Figure 6.3 p. 140
#' coplot(Y ~ W | A, data=glucose)
#' 
NULL





#' Graphs induced by marginalization or conditioning
#' 
#' Functions to find induced graphs after conditioning on a set of variables
#' and marginalizing over another set.
#' 
#' Given a directed acyclic graph representing a set of conditional
#' independencies it is possible to obtain other graphs of conditional
#' independence implied after marginalizingover and conditionig on sets of
#' nodes. Such graphs are the covariance graph, the concentration graph, the
#' multivariate regression graph and the chain graph with different
#' interpretations (see Cox \& Wermuth, 1996, 2004).
#' 
#' @aliases inducedCovGraph inducedConGraph inducedRegGraph inducedChainGraph
#' inducedDAG InducedGraphs
#' @param amat a square Boolean matrix, the adjacency matrix of a directed
#' acyclic graph. The names of rows and of the columns are the nodes of the
#' DAG.
#' @param sel a character vector representing a subset of selected variables.
#' The elements of the vector must be a subset of the names of the nodes i.e.
#' of \code{rownames(A)}.  By default \code{sel} is the set of the nodes of the
#' DAG.
#' @param cond a character vector representing the variables on which you want
#' to condition. \code{cond} must be disjoint from \code{sel} and their union
#' must be a subset of the set of nodes. The set difference between the set of
#' nodes and the union of \code{sel} and \code{cond} are the variables over
#' which we marginalize.  \code{cond} may be the null vector (the default),
#' meaning that you want to condition on the empty set.
#' @param cc a list of character vectors specifying the chain components for
#' the chain graph.
#' @param type a string indicating the interpretation of the chain graph. It
#' can be either "LWF" (Lauritzen, Wermuth, Frydenberg interpretation), "AMP"
#' (Andersson, Madigan, Perlman interpretation) or "MRG" (Multivariate
#' regression graph interpretation).
#' @param order a character vector indicating the ordering of the vertices of a
#' DAG (left to right, past to future).
#' @return \code{inducedCovGraph} returns the adjacency matrix of the
#' covariance graph of the variables in set \code{sel} given the variables in
#' set \code{cond}, implied by the original directed acyclic graph with
#' adjacency matrix \code{amat}.
#' 
#' \code{inducedConGraph} returns the adjacency matrix of the concentration
#' graph of the variables in set \code{sel} given the variables in set
#' \code{cond}, implied by the original directed acyclic graph with adjacency
#' matrix \code{amat}.
#' 
#' \code{inducedRegGraph} returns the adjacency matrix of the multivariate
#' regression graph of the variables in set \code{sel} given the variables in
#' set \code{cond}, implied by the original directed acyclic graph with
#' adjacency matrix \code{amat}.
#' 
#' \code{inducedChainGraph} returns the adjacency matrix of the chain graph for
#' the variables in chain components \code{cc}, given the variables in set
#' \code{cond}, with interpretation specified by string \code{type}, implied by
#' the original directed acyclic graph with adjacency matrix \code{amat}.
#' 
#' \code{inducedDAG} returns the adjacency matrix of the DAG with the ordering
#' \code{order}, implied by the original directed acyclic graph with adjacency
#' matrix \code{amat}.
#' @note If \code{sel} is \code{NULL} the functions return the null matrix.  If
#' \code{cond} is \code{NULL}, the conditioning set is empty and the functions
#' \code{inducedConGraph} and \code{inducedCovGraph} return the overall induced
#' covariance or concentration matrices of the selected variables. If you do
#' not specify \code{sel} you cannot specify a non \code{NULL} value of
#' \code{cond}.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{DAG}}, \code{\link{UG}},\code{\link{isAcyclic}}
#' @references Cox, D. R. \& Wermuth, N. (1996). \emph{Multivariate
#' dependencies}. London: Chapman \& Hall.
#' 
#' Wermuth, N. \& Cox, D.R. (2004). Joint response graphs and separation
#' induced by triangular systems. \emph{J.R. Statist. Soc. B}, 66, Part 3,
#' 687-717.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## Define a DAG
#' dag <- DAG(a ~ x, c ~ b+d, d~ x)
#' dag
#' ## Induced covariance graph of a, b, d given the empty set.
#' inducedCovGraph(dag, sel=c("a", "b", "d"), cond=NULL)
#' 
#' ## Induced concentration graph of a, b, c given x
#' inducedConGraph(dag, sel=c("a", "b", "c"), cond="x")
#' 
#' ## Overall covariance graph
#' inducedCovGraph(dag)
#' 
#' ## Overall concentration graph
#' inducedConGraph(dag)
#' 
#' ## Induced covariance graph of x, b, d given c, x.
#' inducedCovGraph(dag, sel=c("a", "b", "d"), cond=c("c", "x"))
#' 
#' ## Induced concentration graph of a, x, c given d, b.
#' inducedConGraph(dag, sel=c("a", "x", "c"), cond=c("d", "b"))
#' 
#' ## The DAG on p. 198 of Cox & Wermuth (1996)
#' dag <- DAG(y1~ y2 + y3, y3 ~ y5, y4 ~ y5)
#' 
#' ## Cf. figure 8.7 p. 203 in Cox & Wermuth (1996)
#' inducedCovGraph(dag, sel=c("y2", "y3", "y4", "y5"), cond="y1")
#' inducedCovGraph(dag, sel=c("y1", "y2", "y4", "y5"), cond="y3")
#' inducedCovGraph(dag, sel=c("y1", "y2", "y3", "y4"), cond="y5")
#' 
#' ## Cf. figure 8.8 p. 203 in Cox & Wermuth (1996)
#' inducedConGraph(dag, sel=c("y2", "y3", "y4", "y5"), cond="y1")
#' inducedConGraph(dag, sel=c("y1", "y2", "y4", "y5"), cond="y3")
#' inducedConGraph(dag, sel=c("y1", "y2", "y3", "y4"), cond="y5")
#' 
#' ## Cf. figure 8.9 p. 204 in Cox & Wermuth (1996)
#' inducedCovGraph(dag, sel=c("y2", "y3", "y4", "y5"), cond=NULL)
#' inducedCovGraph(dag, sel=c("y1", "y2", "y4", "y5"), cond=NULL)
#' inducedCovGraph(dag, sel=c("y1", "y2", "y3", "y4"), cond=NULL)
#' 
#' ## Cf. figure 8.10 p. 204 in Cox & Wermuth (1996)
#' inducedConGraph(dag, sel=c("y2", "y3", "y4", "y5"), cond=NULL)
#' inducedConGraph(dag, sel=c("y1", "y2", "y4", "y5"), cond=NULL)
#' inducedConGraph(dag, sel=c("y1", "y2", "y3", "y4"), cond=NULL)
#' 
#' ## An induced regression graph
#' dag2 = DAG(Y ~ X+U, W ~ Z+U)
#' inducedRegGraph(dag2, sel="W",  cond=c("Y", "X", "Z"))
#' 
#' ## An induced DAG
#' inducedDAG(dag2, order=c("X","Y","Z","W"))
#' 
#' ## An induced multivariate regression graph
#' inducedRegGraph(dag2, sel=c("Y", "W"), cond=c("X", "Z"))
#' 
#' ## An induced chain graph with LWF interpretation
#' dag3 = DAG(X~W, W~Y, U~Y+Z)
#' cc = list(c("W", "U"), c("X", "Y", "Z"))
#' inducedChainGraph(dag3, cc=cc, type="LWF")
#' 
#' ## ... with AMP interpretation
#' inducedChainGraph(dag3, cc=cc, type="AMP")
#' 
#' ## ... with multivariate regression interpretation
#' cc= list(c("U"), c("Z", "Y"), c("X", "W"))
#' inducedChainGraph(dag3, cc=cc, type="MRG")
#' 
NULL





#' Mathematics marks
#' 
#' Examination marks of 88 students in five subjects.
#' 
#' Mechanics and Vectors were closed book examinations. Algebra, Analysis and
#' Statistics were open book examinations.
#' 
#' @name marks
#' @docType data
#' @format A data frame with 88 observations on the following 5 variables.
#' \describe{ \item{mechanics}{a numeric vector, mark in Mechanics}
#' \item{vectors}{a numeric vector, mark in Vectors} \item{algebra}{a numeric
#' vector, mark in Algebra} \item{analysis}{a numeric vector, mark in Analysis}
#' \item{statistics}{a numeric vector, mark in Statistics } }
#' @references Whittaker, J. (1990). \emph{Graphical models in applied
#' multivariate statistics}. Chichester: Wiley.
#' @source Mardia, K.V., Kent, J.T. and Bibby, (1979). \emph{Multivariate
#' analysis}. London: Academic Press.
#' @keywords datasets
#' @examples
#' 
#' data(marks)
#' pairs(marks)
#' 
NULL





#' Simple graph operations
#' 
#' Finds the boundary, children, parents of a subset of nodes of a graph.
#' 
#' For definitions of the operators see Lauritzen (1996).
#' 
#' @aliases bd ch pa
#' @param nn a vector of nodes. It may either a numeric vector, or a character
#' vector. If it is character vector must be a subset of the \code{rownames} of
#' the edge matrix.
#' @param amat a square matrix with dimnames specifying the adjacency matrix of
#' the graph
#' @return The operators return a character vector specifying the boundary or
#' the children or the parents of nodes \code{nn} in the graph.  This is a
#' numeric or a character vector depending on the mode of \code{nn}.
#' @author Giovanni M. Marchetti
#' @seealso \code{\link{UG}}, \code{\link{DAG}}
#' @references Lauritzen, S. (1996). \emph{Graphical models}. Oxford: Clarendon
#' Press.
#' @keywords graphs models multivariate
#' @examples
#' 
#' ## find boundary of a subset of nodes of a DAG
#' G <- DAG(y ~ x+b+a, b~a, x~a)
#' bd("b", G)
#' bd(c("b", "x"), G)
#' bd("x", G)
#' bd(c("x","b"), G)
#' ## find boundary of a subset of nodes of an UG
#' G <- UG(~ y*x*z + z*h*v)
#' bd("z", G)
#' bd(c("y", "x"), G)
#' bd("v", G)
#' bd(c("x","v"), G)
#' ## children of a subset of nodes of a DAG
#' G <- DAG(y ~ x+b+a, b~a, x~a)
#' ch("b", G)
#' ch(c("b", "x"), G)
#' ch("x", G)
#' ch(c("a","x"), G)
#' ## parents of a subset of nodes of a DAG
#' pa("b", G)
#' pa(c("b", "x"), G)
#' pa("x", G)
#' pa(c("x","b"), G)
#' 
NULL





#' Stress
#' 
#' Stress data
#' 
#' See Cox and Wermuth (1996).
#' 
#' @name stress
#' @docType data
#' @format A \eqn{4 \times 4} covariance matrix for the following variables.
#' \describe{ \item{Y}{} \item{V}{} \item{X}{} \item{U}{} }
#' @references Cox, D. R. \& Wermuth, N. (1996). \emph{Multivariate
#' dependencies}. London: Chapman \& Hall.
#' 
#' Slangen K., Kleemann P.P and Krohne H.W. (1993). Coping with surgical
#' stress. In: Krohne H. W. (ed.).  \emph{Attention and avoidance: Strategies
#' in coping with aversiveness}. New York, Heidelberg: Springer, 321-346.
#' @keywords datasets
#' @examples
#' 
#' data(stress)
#' G = UG(~ Y*X + X*V + V*U + U*Y)
#' fitConGraph(G, stress, 100)
#' 
NULL





#' A simulated data set
#' 
#' Simulated data following a seemingly unrelated regression model.
#' 
#' 
#' @name surdata
#' @docType data
#' @format A data frame with 600 observations on the following 4 variables.
#' \describe{ \item{list("A")}{a numeric response vector} \item{list("B")}{a
#' numeric response vector} \item{list("X")}{a numeric vector}
#' \item{list("Z")}{a numeric vector with codes \code{1} and \code{-1} for a
#' binary variables.} }
#' @keywords datasets
#' @examples
#' 
#' data(surdata)
#' pairs(surdata)
#' 
NULL





#' Utility functions
#' 
#' Functions used internally.
#' 
#' 
#' @aliases rem SPl RR likGau
#' @author Kayvan Sadeghi, Giovanni M. Marchetti
#' @seealso \code{unique},\code{setdiff}, \code{is.element}
#' @keywords utility
NULL



