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

static arma::mat normalize_cols_boot_cpp(const arma::mat& X) {
  arma::mat out = X;
  arma::rowvec norms = arma::sqrt(arma::sum(X % X, 0));
  for (arma::uword j = 0; j < X.n_cols; j++) {
    if (norms(j) > 0) {
      out.col(j) /= norms(j);
    }
  }
  return out;
}

static arma::mat task_means_boot_cpp(const arma::mat& datamat,
                                     int num_groups,
                                     const arma::ivec& num_subj_lst,
                                     int num_cond,
                                     bool center_within_group) {
  arma::mat out;
  int row_offset = 0;

  for (int g = 0; g < num_groups; g++) {
    int n_subj = num_subj_lst(g);
    arma::mat group_data = datamat.rows(row_offset, row_offset + n_subj * num_cond - 1);
    arma::mat task_mean(num_cond, datamat.n_cols);

    for (int c = 0; c < num_cond; c++) {
      int start_row = c * n_subj;
      int end_row = (c + 1) * n_subj - 1;
      task_mean.row(c) = arma::mean(group_data.rows(start_row, end_row), 0);
    }

    if (center_within_group) {
      arma::rowvec group_mean = arma::mean(group_data, 0);
      task_mean.each_row() -= group_mean;
    }

    out = arma::join_cols(out, task_mean);
    row_offset += n_subj * num_cond;
  }

  return out;
}

static arma::mat subject_centered_boot_cpp(const arma::mat& datamat,
                                           int num_groups,
                                           const arma::ivec& num_subj_lst,
                                           int num_cond) {
  arma::mat out(datamat.n_rows, datamat.n_cols, arma::fill::zeros);
  int row_offset = 0;

  for (int g = 0; g < num_groups; g++) {
    int n_subj = num_subj_lst(g);
    int n_rows = n_subj * num_cond;
    arma::mat group_data = datamat.rows(row_offset, row_offset + n_rows - 1);
    arma::rowvec group_mean = arma::mean(group_data, 0);
    out.rows(row_offset, row_offset + n_rows - 1) =
      group_data.each_row() - group_mean;
    row_offset += n_rows;
  }

  return out;
}

static arma::mat condition_means_boot_cpp(const arma::mat& datamat,
                                          int num_groups,
                                          const arma::ivec& num_subj_lst,
                                          int num_cond) {
  arma::mat out;
  int row_offset = 0;

  for (int g = 0; g < num_groups; g++) {
    int n_subj = num_subj_lst(g);
    for (int c = 0; c < num_cond; c++) {
      int start_row = row_offset + c * n_subj;
      int end_row = start_row + n_subj - 1;
      out = arma::join_cols(out, arma::mean(datamat.rows(start_row, end_row), 0));
    }
    row_offset += n_subj * num_cond;
  }

  return out;
}

static arma::mat boot_procrustes_rot_cpp(const arma::mat& origlv,
                                         const arma::mat& bootlv) {
  arma::mat temp = origlv.t() * bootlv;
  arma::mat U, V;
  arma::vec s;
  arma::svd(U, s, V, temp);
  return V * U.t();
}

//' Fast Reduced-Space Bootstrap Test for Task PLS
//'
//' Runs the reduced-space inner bootstrap loop for balanced task methods 1/2.
//' Returns accumulated saliences and per-bootstrap task score distributions.
//'
//' @param task_scores Reduced row-space scores (`U D`)
//' @param task_loadings Right singular vectors used to lift back to features
//' @param stacked_datamat Original stacked data matrix
//' @param bootsamp Bootstrap sample matrix (total_rows x num_boot)
//' @param observed_v Observed design-side saliences/contrasts
//' @param stacked_designdata Normalized design matrix for method 2, or 0-col matrix for method 1
//' @param num_groups Number of groups
//' @param num_subj_lst Subjects per group
//' @param num_cond Number of conditions
//' @param method Task method (1 or 2)
//'
//' @return List with `u_sum`, `u_sq`, and `usc_distrib_boot`
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List boot_test_task_reduced_cpp(const arma::mat& task_scores,
                                      const arma::mat& task_loadings,
                                      const arma::mat& stacked_datamat,
                                      const arma::imat& bootsamp,
                                      const arma::mat& observed_v,
                                      const arma::mat& stacked_designdata,
                                      int num_groups,
                                      const arma::ivec& num_subj_lst,
                                      int num_cond,
                                      int method) {
  int num_boot = bootsamp.n_cols;
  int n_features = task_loadings.n_rows;
  int n_lv = observed_v.n_cols;
  int n_usc_rows = num_groups * num_cond;

  arma::mat u_sum = arma::zeros(n_features, n_lv);
  arma::mat u_sq = arma::zeros(n_features, n_lv);
  arma::cube usc_distrib_boot(n_usc_rows, n_lv, num_boot, arma::fill::zeros);

  for (int b = 0; b < num_boot; b++) {
    arma::uvec boot_idx = arma::conv_to<arma::uvec>::from(bootsamp.col(b) - 1);
    arma::mat score_boot = task_scores.rows(boot_idx);

    arma::mat u_reduced_scaled;
    arma::mat u_boot_scaled;
    arma::mat tmp_orig_usc;

    if (method == 1) {
      arma::mat datamatsvd_boot =
        task_means_boot_cpp(score_boot, num_groups, num_subj_lst, num_cond, true);
      arma::mat smeanmat_boot =
        subject_centered_boot_cpp(score_boot, num_groups, num_subj_lst, num_cond);

      arma::mat pu, pv;
      arma::vec d;
      int n_keep = std::min(n_lv, (int)std::min(datamatsvd_boot.n_rows, datamatsvd_boot.n_cols));

      if (datamatsvd_boot.n_rows <= datamatsvd_boot.n_cols) {
        arma::svd_econ(pu, d, pv, datamatsvd_boot.t());
      } else {
        arma::svd_econ(pv, d, pu, datamatsvd_boot);
      }

      pu = pu.cols(0, n_keep - 1);
      pv = pv.cols(0, n_keep - 1);
      d = d.subvec(0, n_keep - 1);

      arma::mat rot = boot_procrustes_rot_cpp(observed_v.cols(0, n_keep - 1), pv);
      u_reduced_scaled = arma::zeros(task_scores.n_cols, n_lv);
      u_reduced_scaled.cols(0, n_keep - 1) =
        pu * arma::diagmat(d) * rot;

      double lead = arma::abs(d).max();
      double tol = 1e-12 * lead;
      for (int j = 0; j < n_keep; j++) {
        if (!std::isfinite(d(j)) || std::abs(d(j)) <= tol) {
          u_reduced_scaled.col(j).zeros();
        }
      }

      u_boot_scaled = task_loadings * u_reduced_scaled;
      arma::mat tmp_usc2 = smeanmat_boot * normalize_cols_boot_cpp(u_reduced_scaled);
      tmp_orig_usc = condition_means_boot_cpp(tmp_usc2, num_groups, num_subj_lst, num_cond);
    } else {
      arma::mat datamatsvd_boot =
        task_means_boot_cpp(score_boot, num_groups, num_subj_lst, num_cond, false);
      u_reduced_scaled = (stacked_designdata.t() * datamatsvd_boot).t();
      u_boot_scaled = task_loadings * u_reduced_scaled;
      arma::mat tmp_usc =
        stacked_datamat * normalize_cols_boot_cpp(u_boot_scaled);
      tmp_orig_usc = condition_means_boot_cpp(tmp_usc, num_groups, num_subj_lst, num_cond);
    }

    u_sum += u_boot_scaled;
    u_sq += u_boot_scaled % u_boot_scaled;
    usc_distrib_boot.slice(b) = tmp_orig_usc;
  }

  return Rcpp::List::create(
    Rcpp::Named("u_sum") = u_sum,
    Rcpp::Named("u_sq") = u_sq,
    Rcpp::Named("usc_distrib_boot") = usc_distrib_boot
  );
}
