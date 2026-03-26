// pls_procrustes.cpp
// Procrustes rotation for bootstrap alignment
// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

//' Procrustes Rotation
//'
//' Aligns matrix Y to matrix X using Procrustes rotation.
//' Finds orthogonal matrix Q that minimizes ||X - Y*Q||_F
//'
//' @param X Target matrix
//' @param Y Source matrix to rotate
//' @param scale Allow scaling (default false)
//'
//' @return Rotated Y matrix
//'
//' @export
// [[Rcpp::export]]
arma::mat procrustes_cpp(const arma::mat& X, const arma::mat& Y,
                          bool scale = false) {

  // SVD of Y'X
  arma::mat YtX = Y.t() * X;
  arma::mat U, V;
  arma::vec s;
  arma::svd(U, s, V, YtX);

  // Optimal rotation
  arma::mat Q = U * V.t();

  if (scale) {
    // Optimal scaling factor
    double c = arma::trace(Y.t() * X * Q) / arma::trace(Y.t() * Y);
    return c * Y * Q;
  } else {
    return Y * Q;
  }
}

//' Procrustes Rotation with Sign Flipping
//'
//' First tries simple sign flipping for each column,
//' then falls back to full Procrustes if needed.
//'
//' @param X Target matrix
//' @param Y Source matrix
//' @param max_iter Maximum iterations for optimization
//' @param tol Tolerance for convergence
//'
//' @return List with rotated Y and rotation matrix
//'
//' @export
// [[Rcpp::export]]
Rcpp::List procrustes_boot_cpp(const arma::mat& X, const arma::mat& Y,
                                int max_iter = 100, double tol = 1e-8) {

  int n_cols = X.n_cols;
  arma::mat Y_aligned = Y;

  // Try simple sign flipping first (common case)
  arma::vec signs(n_cols);
  for (int j = 0; j < n_cols; j++) {
    double dot_prod = arma::dot(X.col(j), Y.col(j));
    signs(j) = (dot_prod >= 0) ? 1.0 : -1.0;
    Y_aligned.col(j) *= signs(j);
  }

  // Compute fit
  double fit_sign = arma::norm(X - Y_aligned, "fro");

  // Try full Procrustes
  arma::mat Y_proc = procrustes_cpp(X, Y, false);
  double fit_proc = arma::norm(X - Y_proc, "fro");

  // Use whichever is better
  if (fit_proc < fit_sign) {
    Y_aligned = Y_proc;
  }

  return Rcpp::List::create(
    Rcpp::Named("Y_aligned") = Y_aligned,
    Rcpp::Named("fit") = std::min(fit_sign, fit_proc)
  );
}

//' Fast Column-wise Correlation
//'
//' Computes correlation between corresponding columns of two matrices.
//'
//' @param X First matrix
//' @param Y Second matrix
//'
//' @return Vector of correlations
//'
//' @keywords internal
// [[Rcpp::export]]
arma::vec colwise_cor_cpp(const arma::mat& X, const arma::mat& Y) {
  int n_cols = X.n_cols;
  arma::vec cors(n_cols);

  for (int j = 0; j < n_cols; j++) {
    arma::mat combined(X.n_rows, 2);
    combined.col(0) = X.col(j);
    combined.col(1) = Y.col(j);

    arma::mat cor_mat = arma::cor(combined);
    cors(j) = cor_mat(0, 1);
  }

  return cors;
}

//' Fast Task Mean Computation
//'
//' Computes task means from stacked data matrix.
//'
//' @param datamat Data matrix (n_subj*n_cond x n_features)
//' @param n_subj Number of subjects
//' @param n_cond Number of conditions
//'
//' @return Task mean matrix (n_cond x n_features)
//'
//' @export
// [[Rcpp::export]]
arma::mat task_mean_cpp(const arma::mat& datamat, int n_subj, int n_cond) {
  int n_features = datamat.n_cols;
  arma::mat task_mean(n_cond, n_features);

  for (int c = 0; c < n_cond; c++) {
    int start_row = c * n_subj;
    int end_row = (c + 1) * n_subj - 1;
    task_mean.row(c) = mean(datamat.rows(start_row, end_row), 0);
  }

  return task_mean;
}

//' Fast Row Normalization
//'
//' Normalizes each row to unit L2 norm.
//'
//' @param X Matrix to normalize
//'
//' @return Normalized matrix
//'
//' @export
// [[Rcpp::export]]
arma::mat normalize_rows_cpp(const arma::mat& X) {
  arma::mat result = X;
  arma::vec norms = arma::sqrt(arma::sum(X % X, 1));

  for (arma::uword i = 0; i < X.n_rows; i++) {
    if (norms(i) > 1e-10) {
      result.row(i) /= norms(i);
    }
  }

  return result;
}
