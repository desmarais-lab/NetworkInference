using namespace Rcpp;

/**
 * Exponential density
 * 
 * @param x Value to evaluate.
 * @param lambda Rate paramter of the distribution.
 * 
 * @return Density value
 */
double dexp_(double x, double lambda);

/**
 * Rayleigh density
 * 
 * @param x Value to evaluate.
 * @param lambda shape paramter of the distribution.
 * 
 * @return Density value
 */
double drayleigh_(double x, double lambda);

/**
 * Log-normal density
 * 
 * @param x Value to evaluate.
 * @param mu mean
 * @param sigma variance
 * 
 * @return Density value
 */
double dlognormal_(double x, double mu, double sigma);

/**
 * Cumulative distribution function of the standard normal distribution
 * 
 * @param x Value to evaluate.
 * 
 * @return Probability of X > x.
 */
double normal_cdf(double x);
