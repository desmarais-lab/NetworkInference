using namespace Rcpp;

double dexp_(float x, float lambda);
double drayleigh_(float x, float lambda);
double normal_cdf(double x);
void update_children_(IntegerVector &children, 
                      IntegerVector &candidates);
std::string make_pair_id_(int &u, int &v);
int which_int_(int value, IntegerVector x);
double sum_vector(NumericVector x);
NumericVector copy_vector(NumericVector x);
void print_time_estimate(std::chrono::duration<double, std::milli> fp_ms,
                         bool auto_edges, int n_edges);
double edge_weight_(double &event_time_i, double &event_time_j, double &lambda, 
                   double &beta, double &epsilon, bool tied, int &model);