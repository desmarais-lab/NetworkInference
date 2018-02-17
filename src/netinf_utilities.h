#include <chrono>
using namespace Rcpp;

/**
 * Type definitions
 */
typedef std::chrono::system_clock Clock;
typedef std::chrono::system_clock::time_point time_point;
typedef std::chrono::duration<double, std::milli> time_duration;


/**
 * Find the position of an (first) integer in a vector
 * 
 * @param x Vector to search in.
 * @param val Value to search for.
 * 
 * @return Integer index of location of val in x or -1 if val not in x.
 */
int get_index(IntegerVector x, int val);

/**
 * Sum all elements of a vector, skipping na values
 * 
 * @param x Vector to sum over
 */
double sum_vector(NumericVector x);

/**
 * Copy a numeric vector
 */
NumericVector copy_vector(NumericVector x);

/**
 * Print the estimated estimation time in legible units
 * 
 * @param fp_ms duration of inference of a single edge in milliseconds.
 * @param auto_edges Does the algorithm run in auto mode (no fixed number of 
 *     edges).
 * @param n_edges Number of edges to infer (only relevant if auto_edges=false).
 */
void print_time_estimate(std::chrono::duration<double, std::milli> fp_ms,
                         bool auto_edges, int n_edges);


time_point print_timing(time_point start_time, std::string step);