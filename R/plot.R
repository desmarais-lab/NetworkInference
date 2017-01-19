#' Common plotting elements
#' 
#' Plotting layout for NetworkInference package.
#' 
#' @param mode What elements to return.
#' 
#' @return A ggplot object that can be added to a ggplot plot 
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
#' @import ggplot2
#' @import ggrepel
#' @importFrom stats density
#' 
#' @param x Object of class cascade to be plottet
#' @param label_nodes Logical, indicating if should the nodes in each cascade be 
#'     labeled. If the cascades are very dense setting this to \code{FALSE} is
#'     recommended.
#' @param selection A vector of cascade ids to plot.
#' @param plot_elements Addtional ggplot plotting elements to be appended to the
#'     plot (e.g. axis labels etc.).
#' @param density_ draw density lines for each cascade.
#' @param ... additional arguments passed to plot
#' 
#' @examples 
#' 
#' data(cascades)
#' plot(cascades, selection = names(cascades$cascade_nodes)[1:5])
#' plot(cascades, label_nodes = FALSE)
#' 
#' @return A ggplot plot object
#' @export
plot.cascade <- function(x, label_nodes = TRUE, 
                         selection = NULL, plot_elements = NULL, 
                         density_ = FALSE, ...) {
    
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
    
    if(length(unique(pdat$cascade_id)) > 20 & label_nodes) {
        msg <- paste("Plotting more than 20 cascades with labels is not",
                     "recommended. Set label_nodes to FALSE or choose a subset",
                     "of cascades using the `selection` argument\n")
        warning(msg)
    }
    
    # Plot
    palette <-  PLOT_THEME_(mode = "color")
    
    ## Base 
    p <- ggplot() + 
        geom_line(aes_string(x = "event_time", y = "cascade_id"), 
                  color = "grey", linetype = 2, data = pdat)    
        
    ## Hazard lines
    if(density_) {
        n <- 512
        min_time <- min(pdat$event_time)
        max_time <- max(pdat$event_time)
        selected_cascades <- as.cascade(pdat, node_names = unique(pdat$node_name))
        densities <- lapply(selected_cascades$cascade_times, density, n = n)
        xs <- do.call(c, lapply(densities, function(x) x$x))
        ys <- do.call(c, lapply(densities, function(x) x$y))
        offset <- rep(c(1:length(unique(pdat$cascade_id))), each = n)
        # [TODO]: This is still a hack
        scaling_factor <- 5
        hdat <- data.frame("x" = xs, "y" = ys * scaling_factor + offset, 
                           id_ = rep(unique(pdat$cascade_id), each = n))
        # Remove 'extrapolation'
        hdat <- hdat[!(hdat$x < min_time | hdat$x > max_time), ]

        p <- p + geom_line(data = hdat, aes_string(x = "x", y = "y", 
                                                   group = "id_"), 
                           color = palette[3], size = 0.45, alpha = 0.6)
    }
   
    ## Labeled Plot
    if(label_nodes) {
        p <- p + 
            geom_label_repel(aes_string(label = "node_name", color = "node_name", 
                                        x = "event_time", y = "cascade_id"), 
                             size = 2.5, data = pdat) +
            scale_color_discrete(guide = FALSE)
    ## Unlabeled plot
    } else {
        p <- p + 
            geom_point(aes_string(x = "event_time", y = "cascade_id"), size = 1,
                       data = pdat) 
    }

    ## Layout
    p <- p + 
        ylab("Cascade ID") + xlab("Time") +
        PLOT_THEME_() + plot_elements
    return(p)
}



#' Plot a cascade object
#' 
#' @import ggplot2
#' 
#' @param x Object of class diffnet to be plotted.
#' @param type character, one of \code{c("network", "improvement") indicating if 
#'     the inferred diffusion network (\code{"network"}) or the improvement for each
#'     edge should be visualized (\code{"improvement"})}.
#' @param ... additional arguments.
#' 
#' @examples 
#'
#' \dontrun{
#'  data(cascades)
#'  res <- netinf(cascades, n_edges = 6, lambda = 1)
#'  plot(res, type = "network")
#'  plot(res, type = "improvement")
#' }
#' 
#' @return A ggplot plot object if \code{type = "improvement"} otherwise an 
#'     igraph plot.
#' @export
plot.diffnet <- function(x, type = "network", ...) {
    # Check inputs
    type <- match.arg(type, c("network", "improvement"))
    
    if(type == "network") {
        # Check if igraph is installed
        if(!is.element("igraph", rownames(installed.packages()))) {
            stop("In order to use this functionality the `igraph` package needs
                 to be installed. Run `install.packages('igraph')` and retry.")
        }
        
        # Plot network
        g <- igraph::graph_from_data_frame(d = x[, 1:2])
        plot(g, edge.arrow.size=.3, vertex.color = "grey70")
    }
    else{
        ggplot(x) + 
            geom_line(aes(x=c(1:nrow(x)), y = improvement), color = "grey80", 
                      size = 0.5) +
            geom_point(aes(x=c(1:nrow(x)), y = improvement)) + 
            xlab("Edge Number") + ylab("Improvement") +
            PLOT_THEME_()
    }
}