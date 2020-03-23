#=
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
=#



