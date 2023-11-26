#' Two tests in series
#'
#' Calculate sensitivity, specificity, and predictive value of two tests used in
#' series.
#'
#' @param se1 The sensitivity estimate for test 1.
#' @param sp1 The specificity estimate for test 1.
#' @param se2 The sensitivity estimate for test 2.
#' @param sp2 The specificity estimate for test 2.
#' @param prev The estimated prevalence of the condition of interest.
#'
#' @return A list containing the sensitivity estimate (\code{se_estimate}),
#'   specificity estimate (\code{sp_estimate}), positive predictive value
#'   estimate (\code{ppv_estimate}), and negative predictive value estimate
#'   (\code{npv_estimate}) of the two tests used together.
#' @export
#'
two_test_series <- function(se1, sp1, se2, sp2, prev) {
  ppa_estimate <- se1 * se2
  npa_estimate <- sp1 + ((1 - sp1) * sp2)
  ppv_estimate <- ppv(ppa_estimate, npa_estimate, prev)
  npv_estimate <- npv(ppa_estimate, npa_estimate, prev)
  list(
    se_estimate = ppa_estimate,
    sp_estimate = npa_estimate,
    ppv_estimate = ppv_estimate,
    npv_estimate = npv_estimate
  )
}

#' Risk analysis for two tests in series
#'
#' @param n The population size
#' @param prev The prevalence of the condition of interest
#' @param se1 The sensitivity estimate for test 1.
#' @param sp1 The specificity estimate for test 1.
#' @param se2 The sensitivity estimate for test 2.
#' @param sp2 The specificity estimate for test 2.
#'
#' @return A list of population and combined test statistics.
#' @export
#'
risk_analysis_series <- function(n, prev, se1, sp1, se2, sp2) {
  n_need_test <- function(se, prev) {
    round(1 / se * 1 / prev)
  }
  strategy <-
    two_test_series(se1, sp1, se2, sp2, prev)
  n_pos <- round(n * prev)
  n_neg <- n - n_pos
  tp1 <- round(se1 * n_pos)
  fn1 <- n_pos - tp1
  tn1 <- round(sp1 * n_neg)
  fp1 <- n_neg - tn1
  stopifnot(tp1 + fn1 + tn1 + fp1 == n)
  tp <- round(strategy$se_estimate * n_pos)
  fn <- n_pos - tp
  tn <- round(strategy$sp_estimate * n_neg)
  fp <- n_neg - tn
  stopifnot(tp + fn + tn + fp == n)
  percent_test2 <- se1 * prev + (1 - sp1) * (1 - prev)
  n_test2 <- tp1 + fp1

  list(
    n = n,
    prev = prev,
    n_pos = n_pos,
    n_neg = n_neg,
    se1 = se1,
    sp1 = sp1,
    ppv1 = ppv(se1, sp1, prev),
    npv1 = npv(se1, sp1, prev),
    n_need_test1 = n_need_test(se1, prev),
    se2 = se2,
    sp2 = sp2,
    ppv2 = ppv(se2, sp2, prev),
    npv2 = npv(se2, sp2, prev),
    n_need_test2 = n_need_test(se2, prev),
    se_combined = strategy$se_estimate,
    sp_combined = strategy$sp_estimate,
    ppv_combined = strategy$ppv_estimate,
    npv_combined = strategy$npv_estimate,
    n_need_test_combined = n_need_test(strategy$se_estimate, prev),
    tp1 = tp1,
    fn1 = fn1,
    tn1 = tn1,
    fp1 = fp1,
    tp = tp,
    fn = fn,
    tn = tn,
    fp = fp,
    percent_test2 = percent_test2,
    n_test2 = n_test2,
    test2_string = paste0(format_big(n_test2), " / ", format_big(n), " = ",
                          format_percent(percent_test2))
  )

}
