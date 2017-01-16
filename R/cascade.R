# Functions related to the cascade class

#' Is the object a cascade?
#' 
#' @param object The object to be tested
#' @export
is.cascade <- function(object) {
    inherits(object, "cascade")
}

#' Create a cascade object from input data
#'
#' A generic function to transform input data into a cascade object to be used 
#' in other \code{NetworkInference} function. The method invoked depends on the 
#' \link{class} of the first argument. See section Details for available methods.
#' 
#' The following methods are available:
#' \itemize{
#'     \item \link{as.cascade.data.frame}
#'     \item \link{as.cascade.matrix}
#' }
#'  
#' @param data Cascades to be converted. See Details for supported classes.
#' @param ... additional arguments passed to dispatched method. See methods 
#'     linked in Details for more information.
#' 
#' @return An object of class \code{cascade}. This is a list containing three
#'     (named) elements: 
#'     \enumerate{
#'         \item \code{node_names} A character vector of node names.
#'         \item \code{cascade_nodes} A list with one character vector per
#'             cascade containing the node names in order of the events.
#'         \item \code{cascade_times} A list with one element per cascade 
#'             containing the event times for the nodes in \code{cascade_names}.
#'     }
#'     
#' @export
as.cascade <- function(data, ...) {
   UseMethod("as.cascade", data)
}

#' Create cascade object from data frame
#' 
#' @import checkmate 
#' @import assertthat
#' 
#' @param cascade_node_name Character, column name of \code{data} that specifies 
#'     the node names in the cascade. 
#' @param event_time Character, column name of \code{data} that specifies the 
#'     event times for each node involved in a cascade.
#' @param cascade_id Character, column name of the cascade identifier.
#' @param data \link{data.frame} containing the cascade data with column names 
#'     corresponding to the arguments provided to \code{cascade_node_names}, 
#'     \code{event_time} and \code{cascade_id}.
#' @param node_names Character, factor or numeric vector of names for each node. 
#'     Optional. If not provided, node names are inferred from the cascade data.
#'     Note that in this case nodes that are not involved in any cascade (isolates)
#'     will be dropped (not recommended).
#' @param ..., Additional arguments
#'     
#' @return An object of class \code{cascade}. See \link{as.cascade} for details.
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
    assert_data_frame(data, min.rows = 2, min.cols = 3)

    # Transform the data  
    ## Transform cascade ids and node names to character to get consistency 
    ## down the line
    data[, cascade_node_name] <- as.character(data[, cascade_node_name])
    data[, cascade_id] <- as.character(data[, cascade_id])
    
    ## Transform to cascade data structure
    splt <- split(data, f = data[, cascade_id]) 
    cascade_nodes <- lapply(splt, function(x) x[, cascade_node_name])
    cascade_times <- lapply(splt, function(x) x[, event_time])
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

#' Create cascade object from a matrix
#' 
#' @import checkmate 
#' @import assertthat
#' 
#' @param data \link{matrix} Rows corresponding to nodes, columns to cascades.
#'     Matrix entries are the event times for node, cascade pairs. Specify column 
#'     and row names if cascade and node ids other than integer sequences are 
#'     desired.
#' @param node_names Character, factor or numeric vector of names for each node. 
#'     Optional. If not provided, node names are inferred from the provided data.
#'     Note that in this case nodes that are not involved in any cascade (isolates)
#'     will be dropped (not recommended).
#' @param ..., Additional arguments
#'     
#' @return An object of class \code{cascade}. See \link{as.cascade} for details.
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
    clean_casc_vec <- function(x, mode) {
        n <- rownames(data)[!is.na(x)]
        x <- x[!is.na(x)]
        times <- sort(x, decreasing = TRUE)
        n <- n[order(x, decreasing = TRUE)]
        names(times) <- NULL
        names(n) <- NULL
        if(mode == "times") return(times)
        else return(n)
    }
    
    cascade_times <- apply(data, 2, clean_casc_vec, "times") 
    cascade_nodes <- apply(data, 2, clean_casc_vec, "nodes") 
       
    # Check if data is consistent
    assert_cascade_consistency_(cascade_nodes, cascade_times, node_names)
    
    out <- list("cascade_nodes" = cascade_nodes, 
                "cascade_times" = cascade_times, 
                "node_names" = node_names)
    class(out) <- c("cascade", "list")
    out <- order_cascade_(out)
    
    return(out)   
}


#' Convert a cascade object to a matrix
#' 
#' Generates a matrix containing the cascade information in the cascade object.
#' 
#' Missing values are used for nodes that do not experience an event in a cascade.
#' 
#' @param x cascade object to convert.
#' @param ... additional arguments to be passed to or from methods. 
#'     (Currently not supported.)
#' 
#' @return A matrix containing all cascade information. See section Details for
#'     more information.
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
#'     frame. Missing values are not allowed. (Not supported)
#' @param optional logical. If TRUE, setting row names and converting column 
#'     names (to syntactic names: see make.names) is optional. (Not supported)
#' @param ... Additional arguments passed to \code{\link{data.frame}}.
#' 
#' @return A data frame with three columns. Containing (in order) 1) The names of 
#'     the nodes that experience an event in each cascade, 2) the event time of the
#'     corresponding node, 3) the cascade identifier.
#' 
#' @export 
as.data.frame.cascade <- function(x, row.names = NULL, optional = FALSE,
                                  ...) {
    # Check inputs
    assert_that(inherits(x, "cascade"))
    
    # Convert
    cascade_nodes <- do.call(c, x$cascade_nodes)
    cascade_times <- do.call(c, x$cascade_times)
    smry <- summary(x)
    cascade_ids <- do.call(c, apply(smry, 1, function(x) rep(x[1], each = x[2])))
    out <- data.frame("node_name" = cascade_nodes, 'event_time' = cascade_times, 
                      "cascade_id" = cascade_ids, 
                      stringsAsFactors = FALSE, ...)
    row.names(out) <- as.character(c(1:nrow(out)))
    return(out) 
}

#' Sort cascades by event time
#' 
#' @param cascades, object of class cascade
#' 
#' @return An object of class cascade with each cascade (ids and times) ordered
#'     by event time
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

#' Assert cascade consistency
#' 
#' Assert that the cascade information provided by the user is consistent.
#' 
#' @param cascade_nodes List of vectors of integer ids in order of infection.
#' @param cascade_times List of vectors of infection times corresponding to 
#'     \code{cascade_nodes}.
#' @param node_names Full list of node names.
#' @import checkmate
assert_cascade_consistency_ <- function(cascade_nodes, cascade_times, 
                                        node_names) {

    # Check if containers for nodes and event times have same length (same number
    # of cascades)
    if(length(cascade_nodes) != length(cascade_times)) {
       stop("cascade_ids is not the same length as cascade_times.", 
            call. = FALSE)
    }
    
    # Check if each cascade has same length in event time and node name container
    lens_ids <- sapply(cascade_nodes, length)
    lens_times <- sapply(cascade_times, length)
    if(any(lens_ids != lens_times)) {
       stop("Corresponding elements in cascade_ids and cascade_times are not of 
       equal length.", call. = FALSE)
    }
    
    # Check that all cascade elements are of correct class 
    tids <- sapply(cascade_nodes, function(x) assert_that(is.element(class(x), 
                                                          c("numeric", "character",
                                                            "factor", "integer"))))
    ttimes <- sapply(cascade_times, qtest, rules = 'R+[0,)')
    if(!all(tids)) {
      stop("At least one element of cascade_ids is not of class numeric,
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

#' Simulate a set of cascades
#'
#' For testing purposes.   
#' 
#' @importFrom stats runif
#' 
#' @param n_cascades Number of cascades to generate 
#' @param id_class One of \code{c("character", "factor", "numeric")}. What class
#'     should the cascade_id indicator be. 
#'     
#' @return A data frame containing (in order of columns) infected node ids, 
#'     infection times and cascade identifiers for 20 distinct nodes.
#' @export
simulate_cascades_ <- function(n_cascades, id_class = "character") {
    qassert(n_cascades, "X1[1,)")
    id_class <- match.arg(arg = id_class, choices = c("character", "factor", 
                                                      "numeric"))
    make_cascade_ <- function(cid, id_class) {
        n <- as.integer(runif(1, 2, 20))
        ids <- sample(letters[0:20], n, replace = FALSE)
        times <- sort(runif(n, 0, 30), decreasing = TRUE)
        return(data.frame(ids, times, rep(cid, n), stringsAsFactors = FALSE))
    }
     
    if(id_class == "character"){
        ids <- as.character(outer(letters, letters, FUN = paste0))
        cascades <- do.call(rbind, lapply(sample(ids, n_cascades, 
                                                 replace = FALSE), 
                                          make_cascade_))
    } else if(id_class == "factor") {
        ids <- as.character(outer(letters, letters, FUN = paste0))
        cascades <- do.call(rbind, lapply(sample(ids, n_cascades, 
                                                 replace = FALSE), 
                                          make_cascade_))
        cascades[, 3] <- as.factor(cascades[, 3])
    } else if(id_class == "numeric") {
         cascades <- do.call(rbind, lapply(sample(c(1:n_cascades), n_cascades, 
                                                  replace = FALSE), 
                                          make_cascade_))
    }
    colnames(cascades) <- c("node_name", "event_time", "cascade_id")
    rownames(cascades) <- as.character(c(1:nrow(cascades)))
    return(cascades)
}

