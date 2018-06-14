#' US State Policy Adoption (SPID)
#'
#' The SPID data includes information on the year of adoption for over 700 
#' policies in the American states. 
#' 
#' This version 1.0 of the database. For each policy we document the year of first 
#' adoption for each state. Adoption dates range from 1691 to 2017 and includes 
#' all fifty states. Policies are adopted by anywhere from 1 to 50 states, with 
#' an average of 24 adoptions. The data were assembled from a variety of sources, 
#' including academic publications and policy advocacy/information groups. 
#' Policies were coded according to the Policy Agendas Project major 
#' topic code. Additional information on policies is available at the source 
#' repository.
#' 
#' @name policies
#' 
#' @usage data(policies)  
#' @docType data
#' 
#' @format The data comes in two objects of class \code{data.frame}. The first
#'     object, named \code{policies} contains the adoption events. Each row 
#'     corresponds to an adoption event. Each adoption event is described by 
#'     the three columns: 
#' \itemize{
#'     \item \code{statenam}: Name of the adopting state.
#'     \item \code{policy}: Name of the policy.
#'     \item \code{adopt_year}: Year when the state adopted the policy.
#' }
#' The second object (\code{policies_metadata}) contains more details on each
#' of the policies. It contains these columns:
#' \itemize{
#'     \item \code{policy}: Name of the policy.
#'     \item \code{source}: Original source of the data.
#'     \item \code{first_year}: First year any state adopted this policy.
#'     \item \code{last_year}: Last year any state adopted this policy.
#'     \item \code{adopt_count}: Number of states that adopted this policy.
#'     \item \code{description}: Description of the policy.
#'     \item \code{majortopic}: Topic group the policy belongs to.
#' } 
#' Both \code{data.frame} objects can be joined (merged) on the common column
#' \code{policy} (see example code).
#'     
#' @source \url{https://doi.org/10.7910/DVN/CVYSR7}
#' 
#' @aliases policies policies_metadata
#' 
#' @references Boehmke, Frederick J.; Mark Brockway; Bruce A. Desmarais; 
#'     Jeffrey J. Harden; Scott LaCombe; Fridolin Linder; and 
#'     Hanna Wallach. 2018. "A New Database for Inferring Public Policy 
#'     Innovativeness and Diffusion Networks." Working paper.
#'     
#' @examples
#' 
#' data('policies')
#' 
#' # Join the adoption events with the metadata 
#' merged_policies <- merge(policies, policies_metadata, by = 'policy')
"policies"

#' Example cascades
#'
#' An example dataset of 31 nodes and 54 cascades. From the original netinf 
#' implementation in SNAP. 
#' 
#' @name cascades
#' 
#' @usage data(cascades)  
#' @docType data
#' 
#' @format An object of class \code{cascade} containing 4 objects
#' \describe{
#'   \item{node_names}{Character node names}
#'   \item{cascade_nodes}{A list of integer vectors. Each containing the names of the
#'       nodes infected in this cascades in the order of infection}
#'   \item{cascade_times}{A list of numeric vectors. Each containing the infection
#'       times for the corresponding nodes in cascade_nodes}
#' }
#' @source \url{https://github.com/snap-stanford/snap/blob/master/examples/netinf/example-cascades.txt}
"cascades"



#' Validation output from netinf source.
#' 
#' Contains output from original netinf C++ implementation, executed on 
#' \code{\link{cascades}}. For testing purposes.
#' 
#' @name validation
#' 
#' @usage data(validation)  
#' @docType data
#' 
#' @format An object of class \code{data.frame} with 6 columns, containing:
#' \describe{
#'   \item{origin_node}{Origin of diffusion edge.}
#'   \item{destination_node}{Destination node of diffusion edge.}
#'   \item{volume}{??}
#'   \item{marginal_gain}{Marginal gain from edge.}
#'   \item{median_time_difference}{Median time between events in origin and 
#'       destination}
#'   \item{mean_time_difference}{Mean time between events in origin and 
#'       destination}
#' }
#' 
#' @source Output from netinf example program (\url{https://github.com/snap-stanford/snap/tree/master/examples/netinf}).
"validation"

# # Code to generate validation dataset. Data is copied from netinf source example
# # ouput
#
# dat <- c(23,0,21,3049.810355,1.543899,2.136381,
# 9,5,21,3049.291245,1.574027,2.161101,
# 0,31,20,2905.326401,1.889040,2.099126,
# 5,14,16,2326.861114,1.732281,1.936626,
# 5,3,15,2181.930785,1.229308,1.903394)
#
# validation <- data.frame(matrix(dat, nc = 6, byrow = TRUE))
# validation <- validation[order(validation[, 1], validation[, 2]), ]
# rownames(validation) <- c(1:nrow(validation))
# validation[, 1] <- as.character(validation[, 1])
# validation[, 2] <- as.character(validation[, 2])
# colnames(validation) <- c("origin_node", "destination_node", "volume", 
#                          "marginal_gain", "median_time_difference",
#                          "mean_time_difference")
# save(validation, file = 'data/validation.RData')


#' Larger simulated validation network.
#' 
#' A network from simulated data. For testing purposes.
#' 
#' @name sim_validation
#' 
#' @usage data(sim_validation)  
#' @docType data
#' 
#' @format An object of class \code{data.frame} with 4 columns, containing:
#' \describe{
#'   \item{origin_node}{Origin of diffusion edge.}
#'   \item{destination_node}{Destination node of diffusion edge.}
#'   \item{improvement}{Improvement in score for the edge}
#'   \item{p-value}{p-value for vuong test}
#' }
#' 
#' @source See code below.
"sim_validation"

# # Code to generate validation dataset.
# set.seed(142857)
# df <- simulate_rnd_cascades(50, 50)
# cascades <- as_cascade_long(df)
# network <- netinf(cascades, n_edges = 0.05)
# sim_validation <- list("input" = cascades, "output" = network)
# save(sim_validation, file = 'data/sim_validation.RData')
 