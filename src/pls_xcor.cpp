// pls_xcor.cpp
// Fast cross-correlation operations using RcppArmadillo
// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

//' Fast Cross-Correlation (Pearson)
//'
//' Computes Pearson correlation between design and data matrices.
//'
//' @param design Design matrix (n_obs x n_design)
//' @param datamat Data matrix (n_obs x n_features)
//' @return Cross-correlation matrix (n_design x n_features)
//'
//' @keywords internal
// [[Rcpp::export]]
arma::mat xcor_pearson_cpp(const arma::mat& design, const arma::mat& datamat) {
  int n = datamat.n_rows;
  int p = datamat.n_cols;
  int d = design.n_cols;

  // Z-score datamat (columnwise)
  arma::mat data_z = datamat;
  arma::rowvec data_mean = mean(datamat, 0);
  arma::rowvec data_sd = stddev(datamat, 0, 0);

  // Handle zero variance columns
  for (int j = 0; j < p; j++) {
    if (data_sd(j) < 1e-10) {
      data_z.col(j).zeros();
      data_sd(j) = 1.0;
    } else {
      data_z.col(j) = (datamat.col(j) - data_mean(j)) / data_sd(j);
    }
  }

  // Z-score design (columnwise)
  arma::mat design_z = design;
  arma::rowvec design_mean = mean(design, 0);
  arma::rowvec design_sd = stddev(design, 0, 0);

  for (int j = 0; j < d; j++) {
    if (design_sd(j) < 1e-10) {
      design_z.col(j).zeros();
      design_sd(j) = 1.0;
    } else {
      design_z.col(j) = (design.col(j) - design_mean(j)) / design_sd(j);
    }
  }

  // Cross-product normalized by (n-1)
  arma::mat result = design_z.t() * data_z;
  result /= (n - 1);

  return result;
}

//' Fast Cross-Covariance
//'
//' Computes covariance between design and data matrices.
//'
//' @param design Design matrix (n_obs x n_design)
//' @param datamat Data matrix (n_obs x n_features)
//' @return Cross-covariance matrix (n_design x n_features)
//'
//' @keywords internal
// [[Rcpp::export]]
arma::mat xcor_cov_cpp(const arma::mat& design, const arma::mat& datamat) {
  int n = datamat.n_rows;

  // Center only (no scaling)
  arma::mat data_c = datamat.each_row() - mean(datamat, 0);
  arma::mat design_c = design.each_row() - mean(design, 0);

  // Cross-product normalized by (n-1)
  arma::mat result = design_c.t() * data_c;
  result /= (n - 1);

  return result;
}

//' Fast Cross-Correlation (Cosine Angle)
//'
//' Computes cosine similarity between design and data matrices.
//'
//' @param design Design matrix (n_obs x n_design)
//' @param datamat Data matrix (n_obs x n_features)
//' @return Cross-correlation matrix (n_design x n_features)
//'
//' @keywords internal
// [[Rcpp::export]]
arma::mat xcor_cosine_cpp(const arma::mat& design, const arma::mat& datamat) {
  int p = datamat.n_cols;
  int d = design.n_cols;

  // Scale by std (no centering)
  arma::mat data_s = datamat;
  arma::rowvec data_sd = stddev(datamat, 0, 0);

  for (int j = 0; j < p; j++) {
    if (data_sd(j) < 1e-10) {
      data_s.col(j).zeros();
    } else {
      data_s.col(j) = datamat.col(j) / data_sd(j);
    }
  }

  arma::mat design_s = design;
  arma::rowvec design_sd = stddev(design, 0, 0);

  for (int j = 0; j < d; j++) {
    if (design_sd(j) < 1e-10) {
      design_s.col(j).zeros();
    } else {
      design_s.col(j) = design.col(j) / design_sd(j);
    }
  }

  return design_s.t() * data_s;
}

//' Fast Cross-Correlation (Dot Product)
//'
//' Computes dot product between design and data matrices.
//'
//' @param design Design matrix (n_obs x n_design)
//' @param datamat Data matrix (n_obs x n_features)
//' @return Cross-product matrix (n_design x n_features)
//'
//' @keywords internal
// [[Rcpp::export]]
arma::mat xcor_dot_cpp(const arma::mat& design, const arma::mat& datamat) {
  return design.t() * datamat;
}

//' Fast Cross-Correlation (All Modes)
//'
//' Computes cross-correlation using specified correlation mode.
//'
//' @param design Design matrix (n_obs x n_design)
//' @param datamat Data matrix (n_obs x n_features)
//' @param cormode Correlation mode: 0=Pearson, 2=Cov, 4=Cosine, 6=Dot
//' @return Cross-correlation matrix (n_design x n_features)
//'
//' @export
// [[Rcpp::export]]
arma::mat pls_xcor_cpp(const arma::mat& design, const arma::mat& datamat,
                        int cormode = 0) {
  switch(cormode) {
    case 0:
      return xcor_pearson_cpp(design, datamat);
    case 2:
      return xcor_cov_cpp(design, datamat);
    case 4:
      return xcor_cosine_cpp(design, datamat);
    case 6:
      return xcor_dot_cpp(design, datamat);
    default:
      Rcpp::stop("cormode must be 0, 2, 4, or 6");
  }
}
