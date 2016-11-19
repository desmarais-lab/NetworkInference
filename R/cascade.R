# Functions related to the cascade class

# TODO: 
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
#' @param node_names Character, numeric or factor vector of names for each node. 
#'     Optional. Must be of same length and sorting as node_ids. If not provided 
#'     node_ids are used as names.
#' 
#' @return An object of class \code{cascade}. This is a list containing four 
#'     (named) elements: 
#'     \enumerate{
#'         \item \code{node_names} A character vector of node names.
#'         \item \code{cascade_ids} A list with one element per cascade containing
#'             the node ids of each cascade.
#'         \item \code{cascade_times} A list with one element per cascade 
#'             containing the infection times for the nodes in \code{cascade_ids}.
#'     }
#'     (\code{cascade_times}) and a Numeric Vector of node ids 
#'     
#' @export
as.cascade <- function(dat, node_names = NULL) {
   UseMethod("as.cascade", dat)
}

#' Create cascade object from data frame
#' 
#' @import checkmate 
#' @import assertthat' 
#' @param dat \link{data.frame} with three columns containing the cascade 
#'     infromation. The first column contains the node names of the cascades, the 
#'     second column the event times of the corresponding nodes and the third
#'     column contains a cascade identifier (can be \code{integer}, 
#'     \code{numeric}, \code{character} of \code{factor})
#' @param node_names Character, factor or numeric vector of names for each node. 
#'     Optional. If not provided, node names are inferred from the cascade data.
#'     Note that in this case nodes that are not involved in any cascade (isolates)
#'     will be dropped (not recommended).
#'     
#' @return An object of class \code{cascade}.
as.cascade.data.frame <- function(dat, node_names = NULL) {
    
    # Check all inputs 
    if(!is.null(node_names)) {
        assert_that(is.element(class(node_names), c("integer", "factor", 
                                                    "character", "numeric")))       
    } else {
        msg <- paste("Argument node_names not provided. Inferring node names",
                     "from cascade data. Nodes not involved in any cascade will",
                     "be dropped.")
        warning(msg)
        node_names <- as.character(unique(dat[, 1]))
    }
    assert_data_frame(dat, min.rows = 2, min.cols = 3)
    unused_columns <- assertthat::see_if(ncol(dat) > 3)
    if(unused_columns) {
        msg <- paste("dat has more than three columns. Additional columns are", 
                     "not used. Use ?as.cascade.data.frame for details on the",
                     "required structure of dat.")
        warning(msg)
    }
    qassert(dat[, 1], 'S>1[0,)', .var.name = "dat[, 1]: Node names.")
    qassert(dat[, 2], 'R>1[0,)', .var.name = "dat[, 2]: Cascade times.")
    assert_that(is.element(class(dat[, 3]), c("integer", "factor", "character",
                                              "numeric")))
   

    # Transform the data  
    ## Transform cascade ids to character to get consistency down the line
    dat[, 3] <- as.character(dat[, 3])
    
    ## Transform to cascade data structure
    splt <- split(dat, f = dat[, 3]) 
    ids <- lapply(splt, function(x) x[, 1])
    times <- lapply(splt, function(x) x[, 2])
    names(ids) <- names(splt)
    names(times) <- names(splt)
    
    # Check if data is consistent
    assert_cascade_consistency_(ids, times)
    
    out <- list("cascade_ids" = ids, "cascade_times" = times, 
                "node_names" = node_names)
    class(out) <- c("cascade", "list")
    
    return(out)
}


#' Convert a cascade object to a data frame
#' 
#' Generates a data frame containing the cascade information in the cascade object.
#' Node information is lost
#' 
#' @param x Cascade object to convert.
#' @param row.names	NULL or a character vector giving the row names for the data 
#'     frame. Missing values are not allowed. (Not supported)
#' @param optional logical. If TRUE, setting row names and converting column 
#'     names (to syntactic names: see make.names) is optional. (Not supported)
#' @param ... Additional arguments passed to \code{\link{data.frame}}.
#' 
#' @return A data frame with three columns. Containing (in order) 1) The ids of 
#'     the infected nodes in each cascade, 2) the infection time of the
#'     corresponding node, 3) the cascade identifier.
#' 
#' @export 
as.data.frame.cascade <- function(x, row.names = NULL, optional = FALSE,
                                  ...) {
    # Check inputs
    assert_that(class(x)[1] == "cascade")
    
    # Convert
    ids <- do.call(c, x$cascade_ids)
    times <- do.call(c, x$cascade_times)
    smry <- summary(x)
    cascade_ids <- do.call(c, apply(smry, 1, function(x) rep(x[1], each = x[2])))
    out <- data.frame("ids" = ids, 'time' = times, "cascade_id" = cascade_ids, 
                      stringsAsFactors = FALSE, ...)
    row.names(out) <- as.character(c(1:nrow(out)))
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
   tids <- sapply(ids, function(x) assert_that(is.element(class(x), 
                                                          c("numeric", "character",
                                                            "factor", "integer"))))
   ttimes <- sapply(times, qtest, rules = 'R+[0,)')
   if(!all(tids)) {
      stop("At least one element of cascade_ids is not of class numeric,
           integer, character or factor or contains missing values.") 
   }
   if(!all(ttimes)) {
      stop("At least one element of cascade_times is not of class numeric 
           or contains missing values.") 
   }
}

#' Simulate a set of cascades
#'
#' For testing purposes.   
#' 
#' @param n_cascades Number of cascades to generate 
#' @param id_class One of \code{c("character", "factor", "numeric")}. What class
#'     should the cascade_id indicator be. 
#'     
#' @return A data frame containing (in order of columns) infected node ids, 
#'     infection times and cascade identifiers for 20 distinct nodes.
 
simulate_cascades_ <- function(n_cascades, id_class = "character") {
    id_class <- match.arg(arg = id_class, choices = c("character", "factor", 
                                                      "numeric"))
    make_cascade_ <- function(cid, id_class) {
        n <- as.integer(runif(1, 2, 20))
        ids <- sample(letters[0:20], n, replace = FALSE)
        times <- sort(runif(n, 0, 30), decreasing = TRUE)
        return(data.frame(ids, times, rep(cid, n), stringsAsFactors = FALSE))
    }
    if(id_class == "character"){
        cascades <- do.call(rbind, lapply(sample(letters, 10, replace = FALSE), 
                                          make_cascade_))
    } else if(id_class == "factor") {
        cascades <- do.call(rbind, lapply(sample(letters, 10, replace = FALSE), 
                                          make_cascade_))
        cascades[, 3] <- as.factor(cascades[, 3])
    } else if(id_class == "numeric") {
         cascades <- do.call(rbind, lapply(sample(c(1:10), 10, replace = FALSE), 
                                          make_cascade_))
    }
    colnames(cascades) <- c("ids", "time", "cascade_id")
    rownames(cascades) <- as.character(c(1:nrow(cascades)))
    return(cascades)
}

