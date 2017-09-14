#' NetworkInference: Inferring latent diffusion networks
#' 
#' This package provides an R implementation of the \code{netinf} algorithm 
#' created by Gomez Rodriguez, Leskovec, and  Krause (2010). Given a set of
#' events that spread between a set of nodes the algorithm infers the most likely
#' stable diffusion network that is underlying the diffusion process.
#' 
#' The package provides three groups of functions: 1) data preparation 
#' 2) estimation and 3) interpretation.
#' 
#' @section Data preparation:
#' 
#' The core estimation function \code{\link{netinf}} requires an object of class 
#' \code{cascade} (see \link{as_cascade_long} and \link{as_cascade_wide}). 
#' Cascade data contains information on the potential nodes in the network as 
#' well as on event times for each node in each cascade. 
#' 
#' @section Estimation:
#' 
#' Diffusion networks are estimated using the \code{\link{netinf}} function. It 
#' produces a diffusion network in form of an edgelist (of class 
#' \code{\link{data.frame}}).
#' 
#' @section Interpretation and Visualization:
#' 
#' Cascade data can be visualized with the \code{plot} method of the \code{cascade}
#' class (\code{diffnet, \link{plot.cascade}}). Results of the estimation process can 
#' be visualized using the plotting method of the \code{diffnet} class. 
#' 
#' @section Performance:
#' 
#' If higher performance is required and for very large data sets, a faster pure C++ 
#' implementation is available in the Stanford Network Analysis Project (SNAP). 
#' The software can be downloaded at \url{http://snap.stanford.edu/netinf/}.
#' 
#' @useDynLib NetworkInference, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @name NetworkInference
NULL