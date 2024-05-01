#' Calibration-in-the-large
#'
#' @description
#' `cal_intercept()` is a metric that computes the calibration-in-the-large.
#'
#' @details
#' Calibration-in-the-large compares the average predicted risk with the 
#' overall event rate. When the average predicted risk is higher than the 
#' overall event rate, the model overestimates risk in general. Conversely, 
#' underestimation occurs when the observed event rate is higher than the 
#' average predicted risk. The calibration intercept, which is an assessment 
#' of calibration-in-the-large, has a target value of 0; negative values 
#' suggest overestimation, whereas positive values suggest underestimation.
#'
#' @family class probability metrics
#' @templateVar fn cal_intercept
#' @template return
#' @template event_first
#'
#' @references
#' Van Calster, B., McLernon, D.J., van Smeden, M. et al. Calibration: 
#' the Achilles heel of predictive analytics. BMC Med 17, 230 (2019). 
#' https://doi.org/10.1186/s12916-019-1466-7
#'
#' @author An Tang
#'
#' @export
cal_intercept <- function(data, ...) {
  UseMethod("cal_intercept")
}
cal_intercept <- yardstick::new_prob_metric(
  cal_intercept,
  direction = "zero"
)

#' @export
#' @rdname cal_intercept
cal_intercept.data.frame <- function(data,
                                     truth,
                                     ...,
                                     estimator = NULL,
                                     na_rm = TRUE,
                                     case_weights = NULL,
                                     event_level = "first") {
  yardstick::prob_metric_summarizer(
    name = "cal_intercept",
    fn = cal_intercept_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    ...,
    estimator = estimator,
    na_rm = na_rm,
    case_weights = !!rlang::enquo(case_weights),
    event_level = event_level
  )
}

#' @rdname cal_intercept
#' @export
cal_intercept_vec <- function(truth,
                              estimate,
                              estimator = NULL,
                              na_rm = TRUE,
                              event_level = "first",
                              case_weights = NULL) {
  estimator <- yardstick::finalize_estimator(
    truth, estimator,
    metric_class = "cal_intercept",
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

  cal_intercept_impl(
    truth = truth,
    estimate = estimate,
    event_level = event_level
  )
}

cal_intercept_impl <- function(truth, estimate, estimator, event_level, case_weights) {
  cal_intercept_binary(truth, estimate, event_level, case_weights)
}

cal_intercept_binary <- function(truth, estimate, event_level, case_weights) {
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

  # Change factors 1 and 2 back to numerics 0 and 1 for calibration
  truth <- truth |>
    as.numeric() - 1 |>
    as.logical()

  # Don't understand why it has to be this
  estimate <- 1 - estimate

  calperf <- CalibrationCurves::valProbggplot(estimate, truth)

  calperf$Calibration$Intercept[[1]]
}

finalize_estimator_internal.cal_intercept <- function(metric_dispatcher, x, estimator, call) {
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