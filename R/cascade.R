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

#' Create a cascade object from input data
#'
#' A generic function to transform input data into a cascade object to be used 
#' in other \code{NetworkInference} functions. The method invoked depends on the 
#' class of the first argument. See section Details for available methods.
#' 
#' The following methods are available:
#' \itemize{
#'     \item \link{as.cascade.data.frame}
#'     \item \link{as.cascade.matrix}
#' }
#'  
#' @param data cascades to be converted. See Details for supported classes.
#' @param ... additional arguments passed to dispatched method. See methods 
#'     linked in Details for more information.
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
#' # For data frames 
#' df <- simulate_rnd_cascades(10, n_nodes = 20)
#' cascades <- as.cascade(df)
#' is.cascade(cascades)
#' 
#' # For matrices
#' cascade_matrix <- as.matrix(cascades)
#' cascades <- as.cascade(cascade_matrix)
#' is.cascade(cascades)
#'     
#' @export
as.cascade <- function(data, ...) {
   UseMethod("as.cascade", data)
}

#' Transform data frame to cascade
#'
#' Create a cascade object from a \code{\link{data.frame}}.
#' 
#' @import checkmate 
#' @import assertthat
#' 
#' @param cascade_node_name character, column name of \code{data} that specifies 
#'     the node names in the cascade. 
#' @param event_time character, column name of \code{data} that specifies the 
#'     event times for each node involved in a cascade.
#' @param cascade_id character, column name of the cascade identifier.
#' @param data \link{data.frame}, containing the cascade data with column names 
#'     corresponding to the arguments provided to \code{cascade_node_names}, 
#'     \code{event_time} and \code{cascade_id}.
#' @param node_names character, factor or numeric vector containing the names for each node. 
#'     Optional. If not provided, node names are inferred from the cascade data.
#'     Note that in this case nodes that are not involved in any cascade (isolates)
#'     will be dropped (not recommended).
#' @param ..., additional arguments.
#'     
#' @return An object of class \code{cascade}. See \link{as.cascade} for details.
#' 
#' @examples 
#' 
#' # For data frames 
#' df <- simulate_rnd_cascades(10, n_nodes = 20)
#' cascades <- as.cascade(df)
#' is.cascade(cascades)
#
#' @export
as.cascade.data.frame <- function(data, cascade_node_name = "node_name", 
                                  event_time = "event_time", 
                                  cascade_id = "cascade_id", 
                                  node_names = NULL, ...) {
    
    # Check all inputs 
    if(is.null(node_names)) {
        msg <- paste("Argument node_names not provided. Inferring node names",
                     "from cascade data. Nodes not involved in any cascade will",
                     "be dropped.\n")
        warning(msg)
        node_names <- as.character(unique(data[, cascade_node_name]))
    }
    qassert(cascade_node_name, 'S1')
    qassert(event_time, 'S1')
    qassert(cascade_id, 'S1')
    assert_that(is.element(cascade_node_name, colnames(data)))
    assert_that(is.element(event_time, colnames(data)))
    assert_that(is.element(cascade_id, colnames(data)))
    assert_data_frame(data, min.rows = 1, min.cols = 3)
    data <- as.data.frame(data)

    # Transform the data  
    ## Transform cascade ids and node names to character to get consistency 
    ## down the line
    data[, cascade_node_name] <- as.character(data[, cascade_node_name])
    data[, cascade_id] <- as.character(data[, cascade_id])
    
    ## Transform to cascade data structure
    splt <- split(data, f = data[, cascade_id]) 
    cascade_nodes <- lapply(splt, function(x) x[, cascade_node_name])
    cascade_times <- lapply(splt, function(x) x[, event_time])
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

#' Transform matrix to cascade
#' 
#' Create a cascade object from a \code{\link{matrix}}.
#' 
#' @import checkmate 
#' @import assertthat
#' 
#' @param data \link{matrix}, rows corresponding to nodes, columns to cascades.
#'     Matrix entries are the event times for each node, cascade pair. 
#'     Missing values indicate censored observations, that is, nodes that did not
#'     have an event). Specify column 
#'     and row names if cascade and node ids other than integer sequences are 
#'     desired.
#' @param node_names character, factor or numeric vector, containing names for each node. 
#'     Optional. If not provided, node names are inferred from the provided data.
#'     Note that in this case nodes that are not involved in any cascade (isolates)
#'     will be dropped (not recommended).
#' @param ..., additional arguments.
#'     
#' @return An object of class \code{cascade}. See \link{as.cascade} for details.
#' 
#' @examples 
#' 
#' # For matrices
#' cascade_matrix <- as.matrix(cascades)
#' cascades <- as.cascade(cascade_matrix)
#' is.cascade(cascades)
#' 
#' @export
as.cascade.matrix <- function(data, node_names = NULL, ...) {
    
    # Check all inputs 
    if(is.null(node_names)) {
        msg <- paste("Argument node_names not provided. Inferring node names",
                     "from cascade data. Nodes not involved in any cascade will",
                     "be dropped.\n")
        warning(msg)
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
    
    assert_matrix(data, all.missing = FALSE)
   
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
    nona_times <- apply(data, 2, clean_casc_vec_, mode = "times", data = data)
    nona_nodes <- apply(data, 2, clean_casc_vec_, mode = "nodes", data = data)
    # If dim(data)[2] = 1 apply returns vector, if > 1 it returns list. Generate
    # equivalent output in both cases:
    if(inherits(nona_times, "matrix")) {
        cascade_times <- list(as.numeric(nona_times))    
        names(cascade_times) <- colnames(nona_times)
        cascade_nodes <- list(as.character(nona_nodes))
        names(cascade_nodes) <- colnames(nona_nodes)
        
    } else { # already list
        cascade_times <- lapply(nona_times, as.numeric)
        cascade_nodes <- nona_nodes
    }
       
    # Check if data is consistent
    assert_cascade_consistency_(cascade_nodes, cascade_times, node_names)
    
    out <- list("cascade_nodes" = cascade_nodes, 
                "cascade_times" = cascade_times, 
                "node_names" = node_names)
    class(out) <- c("cascade", "list")
    out <- order_cascade_(out)
    
    return(out)   
}

# Clean cascade vector (remove nas and sort)
clean_casc_vec_ <- function(x, mode, data) {
    n <- rownames(data)[!is.na(x)]
    x <- x[!is.na(x)]
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
#' cascade object. Missing values are used for nodes that do not experience an event in a cascade.
#' 
#' @param x cascade object to convert.
#' @param ... additional arguments to be passed to or from methods. 
#'     (Currently not supported.)
#' 
#' @return A matrix containing all cascade information. See section Details for
#'     more information.
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