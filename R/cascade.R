# Functions related to the cascade class

# TODO: 
# - allow factor for cascade names
# - assert consistency between provided node information and nodes used in the 
#   cascade input


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
#' }
#'  
#' @param dat Cascades to be converted. See Details for supported classes.
#' @param node_ids Vector of integer ids for each node.
#' @param node_names Character vector of names for each node. Optional. Must be 
#'     of same length and sorting as node_ids. If not provided node_ids are used
#'     as names.
#' 
#' @return An object of class \code{cascade}. This is a list containing four 
#'     (named) elements: 
#'     \enumerate{
#'         \item \code{node_ids} A numeric vector of integer node ids.
#'         \item \code{node_names} A character vector of node names.
#'         \item \code{cascade_ids} A list with one element per cascade containing
#'             the node ids of each cascade.
#'         \item \code{cascade_times} A list with one element per cascade 
#'             containing the infection times for the nodes in \code{cascade_ids}.
#'     }
#'     (\code{cascade_times}) and a Numeric Vector of node ids 
#'     
#' @export
as.cascade <- function(dat, node_ids, node_names = NULL) {
   UseMethod("as.cascade", dat)
}


#' Create cascade object from data frame
#' 
#' @param dat \link{data.frame} with three columns containing the cascade 
#'     infromation. The first column contains the node ids of the cascades, the 
#'     second column the infection times of the corresponding nodes and the third
#'     column contains a character cascade names. If no names are provided, an 
#'     integer sequence is used.
#' @param node_ids Vector of integer ids for each node.
#' @param node_names Character vector of names for each node. Optional. Must be 
#'     of same length and sorting as node_ids. If not provided node_ids are used
#'     as names.
#'  
#'  @import checkmate 
#'  @import assertthat
as.cascade.data.frame <- function(dat, node_ids, node_names = NULL) {
    
    # Check all inputs 
    assert_node_info_(node_ids, node_names)
    assert_data_frame(dat, min.rows = 2, min.cols = 2)
    unused_columns <- see_if(ncol(dat) > 3)
    if(unused_columns) {
        msg <- paste("dat has more than three columns. Additional columns are", 
                     "not used. Use ?as.cascade.data.frame for details on the",
                     "required structure of dat.")
        warning(msg)
    }
    qassert(dat[, 1], 'X>1[0,)', .var.name = "dat[, 1]: Node ids.")
    qassert(dat[, 2], 'R>1[0,)', .var.name = "dat[, 2]: Cascade times.")
    qassert(dat[, 3], 'S>1[0,)', .var.name = "dat[, 3]: Cascade names.")
     
    # Transform the data  
    splt <- split(dat, f = dat[, 3]) 
    ids = lapply(splt, function(x) x[, 1])
    names(ids) <- NULL
    times = lapply(splt, function(x) x[, 2])
    names(times) <- NULL
    
    # Check if data is consistent
    assert_cascade_consistency_(ids, times)
    
    out <- list("cascade_ids" = ids, "cascade_times" = times, "node_ids" = node_ids, 
                "node_names" = node_names)
    class(out) <- c("cascade", "list")
    
    return(out)
}


#' Assert cascade consistency
#' 
#' Assert that the cascade information provided by the user is consistent.
#' 
#' @param ids List of vectors of integer ids in order of infection.
#' @param times List of vectors of infection times corresponding to \code{ids}.
#' @import checkmate
assert_cascade_consistency_ <- function(ids, times) {
   if(length(ids) != length(times)) {
       stop("cascade_ids is not the same length as cascade_times.", 
            call. = FALSE)
   }
   lens_ids <- sapply(ids, length)
   
   lens_times <- sapply(times, length)
   if(any(lens_ids != lens_times)) {
       stop("Corresponding elements in cascade_ids and cascade_times are not of 
       equal length.", call. = FALSE)
   }
   tids <- sapply(ids, qtest, rules = 'X+[0,)')
   ttimes <- sapply(times, qtest, rules = 'R+[0,)')
   if(!all(tids)) {
      stop("At least element of cascade_ids is not of class numeric or integer.") 
   }
   if(!all(ttimes)) {
      stop("At least element of cascade_times is not of class double.") 
   }
}


#' Assert that node information is consistent
#' 
#' Used in all methods invoked by as.cascade to check if the node information 
#' (ids, times) passed into the funciton by the user are valid.
#' 
#' @param node_ids Integer node ids.
#' @param node_names Character node names.
#' 
#' @import checkmate
assert_node_info_ <- function(node_ids, node_names) {
    qassert(node_ids, 'X+[0,)', .var.name = "node_ids")     
    if(missing(node_names)) {
        qassert(node_names, 'S+', .var.name = "node_names")     
    } else {
        node_names <- as.character(node_ids)
    }
    return(TRUE)
}

#' Simulate a set of cascades
#' 
#' @param n_cascades Number of cascades to generate 
simulate_cascades_ <- function(n_cascades) {
    make_cascade_ <- function(cid) {
        n <- as.integer(runif(1, 2, 20))
        ids <- sample(c(0:20), n, replace = FALSE)
        times <- sort(runif(n, 0, 30), decreasing = TRUE)
        return(data.frame(ids, times, as.character(rep(cid, n)),
                          stringsAsFactors = FALSE))
    }
    cascades <- do.call(rbind, lapply(c(1:10), make_cascade_))
    return(cascades)
}