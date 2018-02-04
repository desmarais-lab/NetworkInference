double dexp_(float x, float lambda);
double drayleigh_(float x, float lambda);
double normal_cdf(double x);
void update_children_(Rcpp::IntegerVector &children, 
                      Rcpp::IntegerVector &candidates);
std::string make_pair_id_(int &u, int &v);
int which_int_(int value, Rcpp::IntegerVector x);
double sum_vector(Rcpp::NumericVector x);
Rcpp::NumericVector copy_vector(Rcpp::NumericVector x);
void print_time_estimate(std::chrono::duration<double, std::milli> fp_ms,
                         bool auto_edges, int n_edges);