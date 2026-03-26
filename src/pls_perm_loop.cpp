// pls_perm_loop.cpp
// Fast permutation testing loop using RcppArmadillo
// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

//' Fast Permutation Test for Task PLS
//'
//' Runs the inner loop of permutation testing for task PLS.
//' Computes singular values for each permutation and compares to observed.
//'
//' @param stacked_datamat Stacked data matrix
//' @param permsamp Permutation sample matrix (total_rows x num_perm)
//' @param observed_s Observed singular values
//' @param num_groups Number of groups
//' @param num_subj_lst Subjects per group
//' @param num_cond Number of conditions
//' @param meancentering_type Mean-centering type
//'
//' @return Vector of counts (permuted s >= observed s)
//'
//' @keywords internal
// [[Rcpp::export]]
arma::ivec perm_test_task_cpp(const arma::mat& stacked_datamat,
                               const arma::imat& permsamp,
                               const arma::vec& observed_s,
                               int num_groups,
                               const arma::ivec& num_subj_lst,
                               int num_cond,
                               int meancentering_type) {

  int num_perm = permsamp.n_cols;
  int n_lv = observed_s.n_elem;
  int n_features = stacked_datamat.n_cols;

  arma::ivec sp = arma::zeros<arma::ivec>(n_lv);

  for (int p = 0; p < num_perm; p++) {
    // Apply permutation
    arma::uvec perm_idx = arma::conv_to<arma::uvec>::from(permsamp.col(p) - 1);  // 0-indexed
    arma::mat datamat_perm = stacked_datamat.rows(perm_idx);

    // Compute task means for each group
    arma::mat datamatsvd_perm;

    int row_offset = 0;
    for (int g = 0; g < num_groups; g++) {
      int n_subj = num_subj_lst(g);

      // Extract group data
      arma::mat group_data = datamat_perm.rows(row_offset, row_offset + n_subj * num_cond - 1);

      // Compute task means
      arma::mat task_mean(num_cond, n_features);
      for (int c = 0; c < num_cond; c++) {
        task_mean.row(c) = mean(group_data.rows(c * n_subj, (c + 1) * n_subj - 1), 0);
      }

      // Mean-centering type 0: subtract group mean
      if (meancentering_type == 0) {
        arma::rowvec group_mean = mean(group_data, 0);
        task_mean.each_row() -= group_mean;
      }

      datamatsvd_perm = join_cols(datamatsvd_perm, task_mean);
      row_offset += n_subj * num_cond;
    }

    // SVD - only need singular values
    arma::vec s_perm;
    if (datamatsvd_perm.n_rows <= datamatsvd_perm.n_cols) {
      s_perm = svd(datamatsvd_perm.t());
    } else {
      s_perm = svd(datamatsvd_perm);
    }

    // Count s_perm >= observed_s
    int n_compare = std::min((int)s_perm.n_elem, n_lv);
    for (int i = 0; i < n_compare; i++) {
      if (s_perm(i) >= observed_s(i)) {
        sp(i)++;
      }
    }
  }

  return sp;
}

//' Fast Permutation Test with Pre-computed Cross-block Matrices
//'
//' More efficient version that pre-computes task means and only shuffles.
//'
//' @param task_means_lst List of task mean matrices by group
//' @param permsamp Permutation sample matrix
//' @param observed_s Observed singular values
//'
//' @return Vector of counts
//'
//' @keywords internal
// [[Rcpp::export]]
arma::ivec perm_test_fast_cpp(const Rcpp::List& task_means_lst,
                               const arma::imat& permsamp,
                               const arma::vec& observed_s) {

  int num_perm = permsamp.n_cols;
  int n_lv = observed_s.n_elem;
  int num_groups = task_means_lst.size();

  arma::ivec sp = arma::zeros<arma::ivec>(n_lv);

  // Stack task means
  arma::mat all_task_means;
  std::vector<int> group_sizes;

  for (int g = 0; g < num_groups; g++) {
    arma::mat tm = Rcpp::as<arma::mat>(task_means_lst[g]);
    all_task_means = join_cols(all_task_means, tm);
    group_sizes.push_back(tm.n_rows);
  }

  for (int p = 0; p < num_perm; p++) {
    // Apply permutation to stacked task means
    arma::uvec perm_idx = arma::conv_to<arma::uvec>::from(permsamp.col(p) - 1);
    arma::mat datamatsvd_perm = all_task_means.rows(perm_idx);

    // SVD - only need singular values
    arma::vec s_perm;
    if (datamatsvd_perm.n_rows <= datamatsvd_perm.n_cols) {
      s_perm = svd(datamatsvd_perm.t());
    } else {
      s_perm = svd(datamatsvd_perm);
    }

    // Count
    int n_compare = std::min((int)s_perm.n_elem, n_lv);
    for (int i = 0; i < n_compare; i++) {
      if (s_perm(i) >= observed_s(i)) {
        sp(i)++;
      }
    }
  }

  return sp;
}
