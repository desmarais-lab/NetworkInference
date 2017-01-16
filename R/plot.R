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
#' @param y Generic method placeholder. Leave \code{NULL}
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
plot.cascade <- function(x, y = NULL, label_nodes = TRUE, 
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