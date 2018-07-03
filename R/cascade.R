# Functions related to the cascade class

#' Is the object of class cascade?
#' 
#' @param object the object to be tested.
#' 
#' @return \code{TRUE} if object is a cascade, \code{FALSE} otherwise.
#' 
#' @examples
#' 
#' data(cascades)
#' is.cascade(cascades)
#' # > TRUE
#' is.cascade(1) 
#' # > FALSE
#' @export
is.cascade <- function(object) {
    inherits(object, "cascade")
}


#' Transform long data to cascade 
#'
#' Create a cascade object from data in long format.
#' 
#' Each row of the data describes one event in the cascade. The data must 
#' contain at least three columns:
#' \enumerate{
#'    \item Cascade node name: The identifier of the node that experiences the 
#'        event.
#'    \item Event time: The time when the node experiences the event. Note that
#'        if the time column is of class date or any other special time class, 
#'        it will be converted to an integer with `as.numeric()`. 
#'    \item Cascade id: The identifier of the cascade that the event pertains to.
#' }
#' The default names for these columns are \code{node_name}, \code{event_time} 
#' and \code{cascade_id}. If other names are used in the \code{data} object the 
#' names have to be specified in the corresponding arguments (see argument 
#' documentation)
#' 
#' @import checkmate 
#' @import assertthat
#' 
#' @param data \link{data.frame}, containing the cascade data 
#'     with column names corresponding to the arguments provided to 
#'     \code{cascade_node_names}, \code{event_time} and \code{cascade_id}.
#' @param cascade_node_name character, column name of \code{data} that specifies 
#'     the node names in the cascade. 
#' @param event_time character, column name of \code{data} that specifies the 
#'     event times for each node involved in a cascade.
#' @param cascade_id character, column name of the cascade identifier.
#' @param node_names character, factor or numeric vector containing the names for each node. 
#'     Optional. If not provided, node names are inferred from the cascade data.
#'     
#' @return An object of class \code{cascade}. This is a list containing three
#'     (named) elements: 
#'     \enumerate{
#'         \item \code{"node_names"} A character vector of node names.
#'         \item \code{"cascade_nodes"} A list with one character vector per
#'             cascade containing the node names in order of the events.
#'         \item \code{"cascade_times"} A list with one element per cascade 
#'             containing the event times for the nodes in \code{"cascade_names"}.
#'     }
#' 
#' @examples 
#' 
#' df <- simulate_rnd_cascades(10, n_nodes = 20)
#' cascades <- as_cascade_long(df)
#' is.cascade(cascades)
#
#' @export
as_cascade_long <- function(data, cascade_node_name = "node_name", 
                             event_time = "event_time", 
                             cascade_id = "cascade_id", node_names = NULL) {
    # Check all inputs 
    data <- as.data.frame(data)
    if(is.null(node_names)) {
        node_names <- as.character(unique(data[, cascade_node_name]))
    }
    qassert(cascade_node_name, 'S1')
    qassert(event_time, 'S1')
    qassert(cascade_id, 'S1')
    assert_that(is.element(cascade_node_name, colnames(data)))
    assert_that(is.element(event_time, colnames(data)))
    assert_that(is.element(cascade_id, colnames(data)))
    assert_data_frame(data, min.rows = 1, min.cols = 3)
    
    # Transform the data  
    ## Transform cascade ids and node names to character to get consistency 
    ## down the line
    data[, cascade_node_name] <- as.character(data[, cascade_node_name])
    data[, cascade_id] <- as.character(data[, cascade_id])
    
    ## Transform to cascade data structure
    splt <- split(data, f = data[, cascade_id]) 
    cascade_nodes <- lapply(splt, function(x) x[, cascade_node_name])
    cascade_times <- lapply(splt, function(x) as.numeric(x[, event_time]))
    cascade_times <- lapply(cascade_times, as.numeric)
    names(cascade_nodes) <- names(splt)
    names(cascade_times) <- names(splt)
    
    # Check if data is consistent
    assert_cascade_consistency_(cascade_nodes, cascade_times, node_names)
    
    out <- list("cascade_nodes" = cascade_nodes, 
                "cascade_times" = cascade_times, 
                "node_names" = node_names)
    class(out) <- c("cascade", "list")
    
    out <- order_cascade_(out)
    
    return(out)
    
}


#' Transform wide data to cascade
#' 
#' Create a cascade object from data in wide format.
#' 
#' If data is in wide format, each row corresponds to a node and each column to
#' a cascade. Each cell indicates the event time for a node - cascade 
#' combination. If a node did not experience an event for a cascade (the node
#' is censored) the cell entry must be \code{NA}.
#' 
#' @import checkmate 
#' @import assertthat
#' 
#' @param data \link{data.frame} or \link{matrix}, rows corresponding to nodes, 
#'     columns to cascades. Matrix entries are the event times for each node, 
#'     cascade pair. Missing values indicate censored observations, that is, 
#'     nodes that did not have an event). Specify column and row names if 
#'     cascade and node ids other than integer sequences are  desired. Note that, 
#'     if the time column is of class date or any other special time class, it 
#'     will be converted to an integer with `as.numeric()`. 
#' @param node_names character, factor or numeric vector, containing names for each node. 
#'     Optional. If not provided, node names are inferred from the provided data.
#'     
#' @return An object of class \code{cascade}. This is a list containing three
#'     (named) elements: 
#'     \enumerate{
#'         \item \code{"node_names"} A character vector of node names.
#'         \item \code{"cascade_nodes"} A list with one character vector per
#'             cascade containing the node names in order of the events.
#'         \item \code{"cascade_times"} A list with one element per cascade 
#'             containing the event times for the nodes in \code{"cascade_names"}.
#'     }
#
#' @examples 
#' 
#' data("policies")
#' cascades <- as_cascade_long(policies, cascade_node_name = 'statenam', 
#'                             event_time = 'adopt_year', cascade_id = 'policy')
#' wide_policies = as.matrix(cascades)
#' cascades <- as_cascade_wide(wide_policies)
#' is.cascade(cascades)
#' 
#' @export
#' 
as_cascade_wide <- function(data, node_names = NULL) {
    
    # Check all inputs 
    if(is.null(node_names)) {
        # Get node names
        if(is.null(rownames(data))) {
            msg <- paste("No rownames provided for data matrix. Assigning integer",
                         "names to nodes.\n")
            warning(msg)
            node_names <- as.character(c(1:nrow(data)))
        } else {
            node_names <- rownames(data)
        }
    }
    
    assert(
        checkClass(data, "data.frame"),
        checkClass(data, "matrix")
    ) 
    data <- as.matrix(data)
    assert_matrix(data, all.missing = FALSE)
    assert_that(length(node_names) == nrow(data))
 
    # Transform the data  
    ## Get cascade ids
    if(is.null(colnames(data))) {
        msg <- paste("No column names provided for data. Assigning integer names",
                     "to cascades.\n")
        warning(msg)
        cascade_ids <- as.character(c(1:ncol(data)))
    } else {
        cascade_ids <- colnames(data)
    }
    
    ## Transform to cascade data structure
    nona_times <- apply(data, 2, clean_casc_vec_, mode = "times", data = data,
                        node_names = node_names)
    nona_nodes <- apply(data, 2, clean_casc_vec_, mode = "nodes", data = data,
                        node_names = node_names)
    # If dim(data)[2] = 1 apply returns vector, if > 1 it returns list. Generate
    # equivalent output in both cases:
    if(inherits(nona_times, "matrix")) {
        nona_times <- list(nona_times)
        names(cascade_times) <- colnames(nona_times)
        cascade_nodes <- list(as.character(nona_nodes))
        names(cascade_nodes) <- colnames(nona_nodes)
        
    } else { # already list
        cascade_nodes <- nona_nodes
    }
       
    # Check if data is consistent
    assert_cascade_consistency_(cascade_nodes, nona_times, node_names)
    
    out <- list("cascade_nodes" = cascade_nodes, 
                "cascade_times" = nona_times, 
                "node_names" = node_names)
    class(out) <- c("cascade", "list")
    out <- order_cascade_(out)
    
    return(out)   
}


# Clean cascade vector (remove nas and sort)
clean_casc_vec_ <- function(x, mode, data, node_names) {
    n <- node_names[!is.na(x)]
    x <- as.numeric(x[!is.na(x)])
    times <- sort(x, decreasing = TRUE)
    n <- n[order(x, decreasing = TRUE)]
    names(times) <- NULL
    names(n) <- NULL
    if(mode == "times") return(times)
    else return(n)
}

#' Convert a cascade object to a matrix
#' 
#' Generates a \code{\link{matrix}} containing the cascade information in the 
#' cascade object in wide format. Missing values are used for nodes that do not 
#' experience an event in a cascade.
#' 
#' @param x cascade object to convert.
#' @param ... additional arguments to be passed to or from methods. 
#'     (Currently not supported.)
#' 
#' @return A matrix containing all cascade information in wide format. That is,
#' each row of the matrix corresponds to a node and each column to a cascade. 
#' Cell entries are event times. Censored nodes have \code{NA} for their entry.
#'     
#' @examples
#' 
#' data(cascades) 
#' as.matrix(cascades)
#' 
#' @export 
as.matrix.cascade <- function(x, ...) {
    
    # Check inputs
    assert_that(inherits(x, "cascade"))
   
    cids <- names(x$cascade_times) 
    nids <- x$node_names
    
    time_lookup_ <- function(pair) {
        cid <- pair[1]
        nid <- pair[2]
        match <- which(x$cascade_nodes[[cid]] == nid)
        if(length(match) == 1) {
            return(x$cascade_times[[cid]][match])
        } else {
            return(NA)
        }
    }
    
    combos <- expand.grid(cids, nids) 
    times <- apply(combos, 1, time_lookup_)
    
    # Reshape to matrix
    out <- matrix(times, nrow = length(nids), ncol = length(cids), byrow = TRUE)
    rownames(out) <- nids
    colnames(out) <- cids
    
    return(out)    
}

#' Select a subset of cascades from cascade object
#' 
#' @param cascade cascade, object to select from
#' @param selection character or integer, vector of cascade_ids to select
#' 
#' @return An object of class cascade containing just the selected cascades
#' 
#' @examples
#' 
#' data(policies)
#' cascades <- as_cascade_long(policies, cascade_node_name = 'statenam', 
#'                             event_time = 'adopt_year', cascade_id = 'policy')
#' cascade_names <- names(cascades$cascade_times)
#' subset_cascade(cascades, selection = cascade_names[1:10])
#' 
#' @export
subset_cascade <- function(cascade, selection) {
    # Check inputs
    assert_that(inherits(cascade, 'cascade'))
    cascade_names <- names(cascade$cascade_times)
    assert_that(all(selection %in% cascade_names))
    
    cascade_times <- cascade$cascade_times[selection]
    cascade_nodes <- cascade$cascade_nodes[selection]
    #node_names <- unique(do.call(c, cascade_nodes))
    node_names <- cascade$node_names
    out <- list(cascade_nodes = cascade_nodes, cascade_times = cascade_times,
                node_names = node_names)
    class(out) <- c('cascade', 'list')
    return(out)
}

#' Drop nodes from a cascade object
#' 
#' @param cascades cascade, object to drop nodes from.
#' @param nodes character or integer, vector of node_ids to drop.
#' @param drop logical, Should empty cascades be dropped.
#' 
#' @return An object of class cascade containing the cascades without the 
#'     dropped nodes.
#' 
#' @examples
#' 
#' data(policies)
#' cascades <- as_cascade_long(policies, cascade_node_name = 'statenam', 
#'                             event_time = 'adopt_year', cascade_id = 'policy')
#' new_cascades <- drop_nodes(cascades, c("California", "New York"))
#' 
#' @export
drop_nodes <- function(cascades, nodes, drop = TRUE) {
    # Check inputs
    assert_that(inherits(cascades, 'cascade'))
    assert_that(all(nodes %in% cascades$node_names))
    c_length <- length(cascades$cascade_nodes)
    cascade_ids <- names(cascades$cascade_nodes)
    
    # Drop nodes from cascade_nodes, cascade_times and node_names 
    drop_idxs <- lapply(cascades$cascade_nodes, function(x) which(x %in% nodes))
    cascade_nodes <- lapply(1:c_length, function(i) {
       cascades$cascade_nodes[[i]][-drop_idxs[[i]]] 
    })
    names(cascade_nodes) <- cascade_ids
    cascade_times <- lapply(1:c_length, function(i) {
       cascades$cascade_times[[i]][-drop_idxs[[i]]] 
    })
    names(cascade_times) <- cascade_ids
    node_names <- cascades$node_names[!cascades$node_names %in% nodes]
    
    # Check for empty cascades
    if(drop) {
       cascade_nodes <- remove_zero_length_(cascade_nodes)
       cascade_times <- remove_zero_length_(cascade_times)
    }
    
    out <- list(cascade_nodes = cascade_nodes, cascade_times = cascade_times,
                node_names = node_names)
    class(out) <- c('cascade', 'list')
    return(out)
}



#' Subset a cascade object in time
#' 
#' Remove each all events occurring outside the desired subset for each cascade 
#' in a cascade object.
#' 
#' @param cascade cascade, object to subset.
#' @param start_time numeric, start time of the subset.
#' @param end_time numeric, end time of the subset.
#' @param drop logical, should empty sub-cascades be dropped?
#'
#' @return An object of class cascade, where only events are included that have 
#'     times \code{start_time} <= t < \code{end_time}.
#'     
#' @examples
#' 
#' data(cascades)
#' sub_cascades <- subset_cascade_time(cascades, 10, 20, drop=TRUE)
#' 
#' @export
subset_cascade_time <- function(cascade, start_time, end_time, drop=TRUE) {
   # Check inputs
   assert_that(inherits(cascade, 'cascade'))
   qassert(start_time, 'N1')
   qassert(end_time, 'N1')
   qassert(drop, 'B1')
    
   casc_length <- length(cascade$cascade_nodes)    
   subset_idxs <- lapply(cascade$cascade_times, function(x) {
       out <- which(x >= start_time & x < end_time)
       }) 
   subset_times <- lapply(1:casc_length, function(x) {
      return(cascade$cascade_times[[x]][subset_idxs[[x]]])
   }) 
   subset_nodes <- lapply(1:casc_length, function(x) {
      return(cascade$cascade_nodes[[x]][subset_idxs[[x]]])
   }) 
   names(subset_times) <- names(subset_nodes) <- names(cascade$cascade_times)
   if(drop) {
       subset_times <- remove_zero_length_(subset_times)
       subset_nodes <- remove_zero_length_(subset_nodes)
   }
   #subset_node_names <- unique(do.call(c, subset_nodes))
   subset_node_names <- cascade$node_names
   out <- list(cascade_nodes = subset_nodes, cascade_times = subset_times, 
               node_names = subset_node_names)
   class(out) <- c("cascade", "list")
   return(out)
}

# Remove vectors of length zero from a list of vectors
remove_zero_length_ <- function(x) {
    out <- lapply(x, function(y) if(length(y) == 0) return() else return(y))
    return(Filter(Negate(is.null), out))
}

#' Convert a cascade object to a data frame
#' 
#' Generates a data frame containing the cascade information in the cascade object.
#' 
#' @param x Cascade object to convert.
#' @param row.names	NULL or a character vector giving the row names for the data 
#'     frame. Missing values are not allowed.
#' @param optional logical. If TRUE, setting row names and converting column 
#'     names (to syntactic names: see make.names) is optional. (Not supported)
#' @param ... Additional arguments passed to \code{\link{data.frame}}.
#' 
#' @return A data frame with three columns. Containing 1) The names of 
#'     the nodes (\code{"node_name"}) that experience an event in each cascade, 
#'     2) the event time (\code{"event_time"}) of the corresponding node, 
#'     3) the cascade identifier \code{"cascade_id"}.
#'     
#' @examples
#' 
#' data(cascades)
#' as.data.frame(cascades)
#' 
#' @export 
as.data.frame.cascade <- function(x, row.names = NULL, optional = FALSE,
                                  ...) {
    # Check inputs
    assert_that(inherits(x, "cascade"))
    
    # Warning for zero length cascades that will be dropped
    zero_length <- sapply(x$cascade_nodes, function(y) length(y) == 0)
    if(any(zero_length)) {
        dropped <- names(x$cascade_nodes)[zero_length]
        msg <- paste("The following cascades have no events and will be dropped: ", 
                     paste(dropped, collapse = " "))
        warning(msg)        
    }

    # Convert
    cascade_nodes <- do.call(c, x$cascade_nodes)
    cascade_times <- do.call(c, x$cascade_times)
    smry <- summary(x, quiet = TRUE)
    ids <- apply(smry, 1, function(x) rep(x[1], each = x[2])) 
    # apply generates matrix if nrow(smry) = 1 and list if > 1, generate 
    # consistent output:
    if(inherits(ids, "matrix")) {
       cascade_ids <- as.character(ids) 
    } else {
       cascade_ids <- do.call(c, ids) 
    }
    out <- data.frame("node_name" = cascade_nodes, 'event_time' = cascade_times, 
                      "cascade_id" = cascade_ids, 
                      stringsAsFactors = FALSE, ...)
    if(!is.null(row.names)) {
        row.names(out) <- row.names
    } else {
        row.names(out) <- as.character(c(1:nrow(out)))
    }
    return(out) 
}

# Sort cascades by event time
# 
# @param cascades, object of class cascade
# 
# @return An object of class cascade with each cascade (ids and times) ordered
#     by event time
order_cascade_ <- function(cascades) {
    
    casc_names <- names(cascades$cascade_times)
   
    sort_times <- function(x) return(cascades$cascade_times[[x]][orderings[[x]]])
    sort_nodes <- function(x) return(cascades$cascade_nodes[[x]][orderings[[x]]])
    
    orderings <- lapply(cascades$cascade_times, order)
    times <- lapply(c(1:length(cascades$cascade_times)), sort_times)
    ids <- lapply(c(1:length(cascades$cascade_nodes)), sort_nodes)
    
    cascades$cascade_nodes <- ids    
    cascades$cascade_times <- times
    
    names(cascades$cascade_nodes) <- casc_names
    names(cascades$cascade_times) <- casc_names
    
    return(cascades)
}

assert_cascade_consistency_ <- function(cascade_nodes, cascade_times, 
                                        node_names) {

    # Check if containers for nodes and event times have same length (same number
    # of cascades)
    if(length(cascade_nodes) != length(cascade_times)) {
       stop("cascade_nodes is not the same length as cascade_times.", 
            call. = FALSE)
    }
    
    # Check if each cascade has same length in event time and node name container
    lens_ids <- sapply(cascade_nodes, length)
    lens_times <- sapply(cascade_times, length)
    if(any(lens_ids != lens_times)) {
       stop("Corresponding elements in cascade_nodes and cascade_times are not of 
       equal length.", call. = FALSE)
    }
    
    # Check that all cascade elements are of correct class 
    tids <- sapply(cascade_nodes, function(x) assert_that(is.element(class(x), 
                                                          c("numeric", "character",
                                                            "factor", "integer"))))
    ttimes <- sapply(cascade_times, function(x) assert_that(is.element(class(x), 
                                                          c("numeric", "integer"))))
    if(!all(tids)) {
      stop("At least one element of cascade_nodes is not of class numeric,
           integer, character or factor or contains missing values.", 
           call. = FALSE) 
    }
    if(!all(ttimes)) {
      stop("At least one element of cascade_times is not of class numeric 
           or contains missing values.", call. = FALSE) 
    }
                         
    # Check consistency between node names in cascades and provided node names
    unique_cascade_nodes <- unique(do.call(c, cascade_nodes))
    chk <- is.element(unique_cascade_nodes, node_names)
    if(!all(chk)) {
        msg <- paste0("The following node(s) that occur in the cascades are ",
                      "not contained in provided node_names:\n", 
                      unique_cascade_nodes[!chk], "\nPlease provide the full ", 
                      "list of node names.")
        stop(msg, .call = FALSE)
    }
}