# Common plotting elements
# 
# Plotting layout for NetworkInference package.
# 
# @param mode What elements to return.
# 
# @return A ggplot object that can be added to a ggplot plot 
PLOT_THEME_ <- function(mode = NULL) {
    if(is.null(mode)) {
        out <- theme_bw() 
    } else if(mode == "color") {
        out <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                 "#0072B2", "#D55E00", "#CC79A7")
    }
    return(out)
}

#' Plot a cascade object
#' 
#' Allows plotting of one or multiple, labeled or unlabeled cascades.
#' 
#' The function returns a ggplot plot object (class \code{gg, ggplot}) which 
#' can be modified like any other ggplot. See the ggplot documentation and the 
#' examples below for more details.
#' 
#' @import ggplot2
#' @import ggrepel
#' @importFrom stats density
#' 
#' @param x object of class cascade to be plotted.
#' @param label_nodes logical, indicating if should the nodes in each cascade be 
#'     labeled. If the cascades are very dense setting this to \code{FALSE} is
#'     recommended.
#' @param selection a vector of cascade ids to plot.
#' @param ... additional arguments passed to plot.
#' 
#' @examples 
#' 
#' data(cascades)
#' plot(cascades, selection = names(cascades$cascade_nodes)[1:5])
#' plot(cascades, label_nodes = FALSE, selection = sample(1:54, 20))
#' 
#' # Modify resulting ggplot object
#' library(ggplot2) 
#' p <- plot(cascades, label_nodes = FALSE, selection = sample(1:54, 20))
#' ## Add a title
#' p <- p + ggtitle('Your Title')
#' p
#' ## Change Axis
#' p <- p + xlab("Your modified y axis label") #x and y labels are flipped here
#' p <- p + ylab("Your modified x axis label") #x and y labels are flipped here
#' p
#' 
#' @return A ggplot plot object.
#' @export
plot.cascade <- function(x, label_nodes = TRUE, selection = NULL, ...) {
    
    # Check inputs
    assert_that(inherits(x, "cascade"))
    assert_that(inherits(label_nodes, "logical"))
    pdat <- as.data.frame(x)
    
    # Select cascades
    if(!is.null(selection)) {
        # Check selection input
        assert_that(length(selection) >= 1) 
        assert_that(is.element(class(selection), c("character", "numeric", 
                                                   "integer", "factor")))
        chk <- is.element(selection, unique(pdat$cascade_id))
        if(!all(chk)) {
            msg <- paste("The following cascade id(s) provided in `selection` do",
                         "not exist in the cascade object:", 
                         paste0(selection[!chk], collapse = ","), "\n")
            stop(msg) 
        }
        selection <- as.character(selection)  
        # Slice data
        sel <- is.element(pdat$cascade_id, selection)
        pdat <- pdat[sel, ]
    }
    
    pdat$cascade_id <- as.factor(pdat$cascade_id)
    
    if(length(unique(pdat$cascade_id)) > 20 & label_nodes) {
        msg <- paste("Plotting more than 20 cascades with labels is not",
                     "recommended. Set label_nodes to FALSE or choose a subset",
                     "of cascades using the `selection` argument\n")
        warning(msg)
    }
    
    # Plot
    palette <-  PLOT_THEME_(mode = "color")
    
    ## Labeled Plot
    if(label_nodes) {
        p <- ggplot() +
            geom_line(aes_string(x = "event_time", y = "cascade_id"), 
                      color = "grey", linetype = 2, data = pdat) +
            geom_label_repel(aes_string(label = "node_name", color = "node_name", 
                                        x = "event_time", y = "cascade_id"), 
                             size = 2.5, data = pdat) +
            ylab("Cascade ID") + xlab("Time") +
            scale_color_discrete(guide = FALSE)
    ## Unlabeled plot
    } else {
        p <- ggplot(pdat, aes_string(x = "cascade_id", y = "event_time")) +
            geom_violin() + 
            geom_jitter(height = 0, width = 0.05, alpha = 0.6, size = 0.5) +
            xlab("Cascade ID") + ylab("Time") +
            coord_flip()
    }

    ## Layout
    p <- p + 
        PLOT_THEME_()
    return(p)
}

#' Visualize netinf output
#' 
#' Visualize the inferred diffusion network or the marginal gain in fit obtained
#' by addition of each edge.
#' 
#' If `type = improvement` a ggplot object is returned. It can be modified like
#' any other ggplot. See the ggplot documentation and the examples in 
#' \link{plot.cascade}.
#' 
#' @import ggplot2
#' 
#' @param x object of class diffnet to be plotted.
#' @param type character, one of \code{c("network", "improvement", "p-value")} 
#'     indicating if the inferred diffusion network, the 
#'     improvement for each edge or the p-value from the vuong test for each
#'     edge should be visualized .
#' @param ... additional arguments.
#' 
#' @examples 
#'
#' \dontrun{
#'  data(cascades)
#'  res <- netinf(cascades, quiet = TRUE)
#'  plot(res, type = "network")
#'  plot(res, type = "improvement")
#'  plot(res, type = "p-value")
#' }
#' 
#' @return A ggplot plot object if \code{type = "improvement"} otherwise an 
#'     igraph plot.
#' @export
plot.diffnet <- function(x, type = "network", ...) {
    # Check inputs
    type <- match.arg(type, c("network", "improvement", "p-value"))
    
    if(type == "network") {
        if (requireNamespace("igraph", quietly = TRUE)) {
            # Plot network
            g <- igraph::graph_from_data_frame(d = x[, 1:2])
            igraph::plot.igraph(g, edge.arrow.size=.3, vertex.color = "grey70")
        } else {
            stop("In order to use this functionality the `igraph` package needs to be installed. Run `install.packages('igraph')` and retry.")
        } 
    }
    else if(type == "improvement") {
        ggplot(x) + 
            geom_line(aes_string(x=c(1:nrow(x)), y = "improvement"), 
                      color = "grey80", size = 0.5) +
            geom_point(aes_string(x=c(1:nrow(x)), y = "improvement"), 
                       size = 0.5) + 
            xlab("Edge Number") + ylab("Improvement") +
            PLOT_THEME_()
    } else {
        ggplot(x) + 
            geom_line(aes_string(x=c(1:nrow(x)), y = "p_value"), 
                      color = "grey80", size = 0.5) +
            geom_point(aes_string(x=c(1:nrow(x)), y = "p_value"), 
                       size = 0.5) + 
            xlab("Edge Number") + ylab("P-Value") +
            PLOT_THEME_()
    }
}
