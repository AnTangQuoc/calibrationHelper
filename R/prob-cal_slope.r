#' Calibration slope
#'
#' @description
#' `cal_slope()` is a metric that computes the calibration slope.
#'
#' @details
#' The calibration slope evaluates the spread of the estimated risks and has a 
#' target value of 1. A slope < 1 suggests that estimated risks are too extreme, 
#' i.e., too high for patients who are at high risk and too low for patients 
#' who are at low risk. A slope > 1 suggests the opposite, i.e., that risk 
#' estimates are too moderate.
#'
#' @family class probability metrics
#' @templateVar fn cal_slope
#' @template return
#' @template event_first
#'
#' @references
#' Van Calster, B., McLernon, D.J., van Smeden, M. et al. Calibration: 
#' the Achilles heel of predictive analytics. 
#' BMC Med 17, 230 (2019). https://doi.org/10.1186/s12916-019-1466-7
#'
#' @author An Tang
#'
#' @export
cal_slope <- function(data, ...) {
  UseMethod("cal_slope")
}
cal_slope <- yardstick::new_prob_metric(
  cal_slope,
  direction <- "zero"
)

#' @export
#' @rdname cal_slope
cal_slope.data.frame <- function(data,
                                 truth,
                                 ...,
                                 estimator = NULL,
                                 na_rm = TRUE,
                                 case_weights = NULL,
                                 event_level = "first") {
  yardstick::prob_metric_summarizer(
    name = "cal_slope",
    fn = cal_slope_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    ...,
    estimator = estimator,
    na_rm = na_rm,
    case_weights = !!rlang::enquo(case_weights),
    event_level = event_level
  )
}

#' @rdname cal_slope
#' @export
cal_slope_vec <- function(truth,
                          estimate,
                          estimator = NULL,
                          na_rm = TRUE,
                          event_level = "first",
                          case_weights = NULL) {
  estimator <- yardstick::finalize_estimator(
    truth, estimator,
    metric_class = "cal_slope",
    case_weights
  )

  yardstick::check_prob_metric(truth, estimate, case_weights, estimator)

  if (na_rm) {
    result <- yardstick::yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
  } else if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  cal_slope_impl(
    truth = truth,
    estimate = estimate,
    event_level = event_level
  )
}

cal_slope_impl <- function(truth, estimate, estimator, event_level, case_weights) {
  cal_slope_binary(truth, estimate, event_level, case_weights)
}

cal_slope_binary <- function(truth, estimate, event_level, case_weights) {
  lvls <- levels(truth)

  event <- lvls[[1]]
  control <- lvls[[2]]

  if (compute_n_occurrences(truth, event) == 0L) {
    # Warn here and return `NA`.
    warn_cal_truth_no_event(event)
    return(NA_real_)
  }
  if (compute_n_occurrences(truth, control) == 0L) {
    # Warn here and return `NA`.
    warn_cal_truth_no_control(control)
    return(NA_real_)
  }

  truth <- truth |>
    as.numeric() - 1 |>
    as.logical()

  estimate <- 1 - estimate

  calperf <- CalibrationCurves::valProbggplot(estimate, truth)

  calperf$Calibration$Slope[[1]] - 1 # remember to re-add 1 in post-processing
}

finalize_estimator_internal.cal_slope <- function(metric_dispatcher, x, estimator, call) {
  yardstick::validate_estimator(estimator, estimator_override = "binary")
  if (!is.null(estimator)) {
    return(estimator)
  }

  lvls <- levels(x)
  if (length(lvls) > 2) {
    stop("A multiclass `truth` input was provided, but only `binary` is supported.")
  }
  "binary"
}

compute_n_occurrences <- function(x, what) {
  sum(x == what)
}

msg_cal_truth_no_control <- function(control) {
  paste0(
    "No control observations were detected in {.arg truth} ",
    "with control level '", control, "'."
  )
}
warn_cal_truth_no_control <- function(control) {
  cli::cli_warn(
    msg_cal_truth_no_control(control),
    class = "yardstick_warning_cal_truth_no_control"
  )
}
stop_cal_truth_no_control <- function(control) {
  cli::cli_abort(
    msg_cal_truth_no_control(control),
    class = "yardstick_error_cal_truth_no_control"
  )
}

msg_cal_truth_no_event <- function(event) {
  paste0(
    "No event observations were detected in {.arg truth} ",
    "with event level '", event, "'."
  )
}
warn_cal_truth_no_event <- function(event) {
  cli::cli_warn(
    msg_cal_truth_no_event(event),
    class = "yardstick_warning_cal_truth_no_event"
  )
}
stop_cal_truth_no_event <- function(event) {
  cli::cli_abort(
    msg_cal_truth_no_event(event),
    class = "yardstick_error_cal_truth_no_event"
  )
}