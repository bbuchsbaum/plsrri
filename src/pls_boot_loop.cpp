// pls_boot_loop.cpp
// Fast bootstrap testing loop using RcppArmadillo
// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

//' Sign Flip Alignment
//'
//' Aligns bootstrap u vectors to observed u by sign flipping.
//'
//' @param u_boot Bootstrap u matrix
//' @param u_obs Observed u matrix
//' @return Sign-aligned u_boot
//'
//' @keywords internal
// [[Rcpp::export]]
arma::mat align_signs_cpp(arma::mat u_boot, const arma::mat& u_obs) {
  int n_lv = u_boot.n_cols;

  for (int lv = 0; lv < n_lv; lv++) {
    double dot_prod = arma::dot(u_obs.col(lv), u_boot.col(lv));
    if (dot_prod < 0) {
      u_boot.col(lv) *= -1;
    }
  }

  return u_boot;
}

//' Fast Bootstrap Test for Task PLS
//'
//' Runs the inner loop of bootstrap testing for task PLS.
//' Accumulates bootstrap saliences for SE computation.
//'
//' @param stacked_datamat Stacked data matrix
//' @param bootsamp Bootstrap sample matrix (total_rows x num_boot)
//' @param observed_u Observed u (saliences)
//' @param num_groups Number of groups
//' @param num_subj_lst Subjects per group
//' @param num_cond Number of conditions
//' @param meancentering_type Mean-centering type
//'
//' @return List with u_sum and u_sq_sum
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List boot_test_task_cpp(const arma::mat& stacked_datamat,
                               const arma::imat& bootsamp,
                               const arma::mat& observed_u,
                               int num_groups,
                               const arma::ivec& num_subj_lst,
                               int num_cond,
                               int meancentering_type) {

  int num_boot = bootsamp.n_cols;
  int n_features = observed_u.n_rows;
  int n_lv = observed_u.n_cols;

  arma::mat u_sum = arma::zeros(n_features, n_lv);
  arma::mat u_sq_sum = arma::zeros(n_features, n_lv);

  for (int b = 0; b < num_boot; b++) {
    // Apply bootstrap reordering
    arma::uvec boot_idx = arma::conv_to<arma::uvec>::from(bootsamp.col(b) - 1);
    arma::mat datamat_boot = stacked_datamat.rows(boot_idx);

    // Compute task means for each group
    arma::mat datamatsvd_boot;

    int row_offset = 0;
    for (int g = 0; g < num_groups; g++) {
      int n_subj = num_subj_lst(g);

      // Extract group data
      arma::mat group_data = datamat_boot.rows(row_offset, row_offset + n_subj * num_cond - 1);

      // Compute task means
      arma::mat task_mean(num_cond, n_features);
      for (int c = 0; c < num_cond; c++) {
        task_mean.row(c) = mean(group_data.rows(c * n_subj, (c + 1) * n_subj - 1), 0);
      }

      // Mean-centering
      if (meancentering_type == 0) {
        arma::rowvec group_mean = mean(group_data, 0);
        task_mean.each_row() -= group_mean;
      }

      datamatsvd_boot = join_cols(datamatsvd_boot, task_mean);
      row_offset += n_subj * num_cond;
    }

    // SVD
    arma::mat u_boot, v_boot;
    arma::vec s_boot;

    if (datamatsvd_boot.n_rows <= datamatsvd_boot.n_cols) {
      arma::svd_econ(u_boot, s_boot, v_boot, datamatsvd_boot.t());
    } else {
      arma::svd_econ(v_boot, s_boot, u_boot, datamatsvd_boot);
    }

    // Ensure correct dimensions
    if (u_boot.n_cols > (arma::uword)n_lv) {
      u_boot = u_boot.cols(0, n_lv - 1);
    } else if (u_boot.n_cols < (arma::uword)n_lv) {
      u_boot = join_rows(u_boot, arma::zeros(n_features, n_lv - u_boot.n_cols));
    }

    // Sign alignment
    u_boot = align_signs_cpp(u_boot, observed_u);

    // Accumulate
    u_sum += u_boot;
    u_sq_sum += u_boot % u_boot;  // Element-wise square
  }

  return Rcpp::List::create(
    Rcpp::Named("u_sum") = u_sum,
    Rcpp::Named("u_sq_sum") = u_sq_sum
  );
}

//' Compute Bootstrap Ratio
//'
//' Computes bootstrap ratio (u / SE) from accumulated sums.
//'
//' @param u_sum Sum of bootstrap u
//' @param u_sq_sum Sum of squared bootstrap u
//' @param observed_u Observed u
//' @param num_boot Number of bootstrap samples
//'
//' @return Bootstrap ratio matrix
//'
//' @keywords internal
// [[Rcpp::export]]
arma::mat compute_bsr_cpp(const arma::mat& u_sum,
                           const arma::mat& u_sq_sum,
                           const arma::mat& observed_u,
                           int num_boot) {

  // Mean
  arma::mat u_mean = u_sum / num_boot;

  // Variance
  arma::mat u_var = (u_sq_sum / num_boot) - (u_mean % u_mean);
  u_var = arma::clamp(u_var, 0.0, arma::datum::inf);  // Ensure non-negative

  // SE
  arma::mat u_se = arma::sqrt(u_var);

  // BSR = observed / SE
  arma::mat bsr = observed_u / u_se;

  // Handle division by zero
  bsr.elem(arma::find_nonfinite(bsr)).zeros();

  return bsr;
}
