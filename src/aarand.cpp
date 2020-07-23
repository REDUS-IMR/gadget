#include "aarand.h"
#include <cstdlib>
#include <Rcpp.h>

namespace myrand {
	void srand(unsigned int seed) {
		Rcpp::Environment base_env("package:base");
		Rcpp::Function set_seed_r = base_env["set.seed"];
		set_seed_r(seed);
	}

	int rand(void) {
		Rcpp::NumericVector x = Rcpp::runif(1, 0.0, RAND_MAX);
		return round(x[0]);
	}
}
