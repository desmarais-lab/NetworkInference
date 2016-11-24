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
#' 
#' @param cascades Object of class cascade to be plottet
#' @param label_nodes Logical, indicating if should the nodes in each cascade be 
#'     labeled. If the cascades are very dense setting this to \code{FALSE} is
#'     recommended.
#' @param selection A vector of cascade ids to plot.
#' @param plot_elements Addtional ggplot plotting elements to be appended to the
#'     plot (e.g. axis labels etc.).
#' 
#' @return A ggplot plot object
plot.cascade <- function(cascades, label_nodes = TRUE, selection = NULL,
                         plot_elements = NULL) {
    
    # Check inputs
    assert_that(inherits(cascades, "cascade"))
    assert_that(inherits(label_nodes, "logical"))
    pdat <- as.data.frame(cascades)
    
    # Select cascades
    if(!is.null(selection)) {
        # Check selection input
        assert_that(length(selection) >= 1) 
        assert_that(is.element(class(selection), c("character", "numeric", 
                                                   "integer", "factor")))
        selection <- as.character(selection)  
        # Slice data
        sel <- is.element(pdat$cascade_id, selection)
        pdat <- pdat[sel, ]
    }
    
    if(length(unique(pdat$cascade_id)) > 20 & label_nodes) {
        msg <- paste("Plotting more than 20 cascades with labels is not",
                     "recommended. Set label_nodes to FALSE or choose a subset",
                     "of cascades using the `selection` argument.")
        warning(msg)
    }
    
    # Plot
    
    ## Base
    p <- ggplot(aes_string(x = "event_time", y = "cascade_id"), data = pdat)
    
    ## Optional plotting elements 
    if(label_nodes) {
        p <- p + 
            geom_line(color = "grey", linetype = 2) +
            geom_label_repel(aes_string(label = "node_name", 
                                        color = "node_name"), 
                             size = 2.5) +
            scale_color_discrete(guide = FALSE)
    } else {
        p <- p + geom_point(size = 1)
    }

    ## Layout
    p <- p + 
        ylab("Cascade ID") + xlab("Time") +
        PLOT_THEME_() + plot_elements
    return(p)
}