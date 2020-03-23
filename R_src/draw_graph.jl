#=
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
=#


