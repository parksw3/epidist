#' Observation process for primary and secondary events
#' @export
observe_process <- function(linelist) {
  clinelist <- linelist |>
    data.table::copy() |>
    data.table:::DT(, ptime_daily := floor(ptime)) |>
    data.table:::DT(, ptime_lwr := ptime_daily) |>
    data.table:::DT(, ptime_upr := ptime_daily + 1) |>
    # How the second event would be recorded in the data
    data.table:::DT(, stime_daily := floor(stime)) |>
    data.table:::DT(, stime_lwr := stime_daily) |>
    data.table:::DT(, stime_upr := stime_daily + 1) |>
    # How would we observe the delay distribution
    # previously: delay_daily=floor(delay)
    data.table:::DT(, delay_daily := stime_daily - ptime_daily) |>
    data.table:::DT(, delay_lwr := purrr::map_dbl(delay_daily, ~ max(0, . - 1))) |>
    data.table:::DT(, delay_upr := delay_daily + 1) |>
    # We assume observation time is the ceiling of the maximum delay
    data.table:::DT(, obs_at := stime |>
        max() |>
        ceiling()
    )
  return(clinelist)
}

#' Filter observations based on a observation time of secondary events
#' @export
filter_obs_by_obs_time <- function(linelist, obs_time) {
  truncated_linelist <- linelist |>
    data.table::copy() |>
    # Update observation time by when we are looking
    data.table:::DT(, obs_at := obs_time) |>
    data.table:::DT(, obs_time := obs_time - ptime) |>
    # Assuming truncation at the beginning of the censoring window
    data.table:::DT(,
      censored_obs_time := obs_at - ptime_lwr
    ) |>
    data.table:::DT(, censored := "interval") |>
    data.table:::DT(stime_upr <= obs_at)
  return(truncated_linelist)
}

#' Filter observations based on the observation time of primary events
#' @export
filter_obs_by_ptime <- function(linelist, obs_time,
                                obs_at = c("obs_secondary", "max_secondary")) {
  obs_at <- match.arg(obs_at)

  pfilt_t <- obs_time
  truncated_linelist <- linelist |>
    data.table::copy() |>
    DT(, censored := "interval") |>
    DT(ptime_upr <= pfilt_t)

  if (obs_at == "obs_secondary") {
    truncated_linelist <- truncated_linelist |>
      # Update observation time to be the same as the maximum secondary time
      DT(, obs_at := stime_upr)
  } else if (obs_at == "max_secondary") {
    truncated_linelist <- truncated_linelist |>
      DT(, obs_at := stime_upr |> max() |> ceiling())
  }

  # make observation time as specified
  truncated_linelist <- truncated_linelist |>
    DT(, obs_time := obs_at - ptime) |>
    # Assuming truncation at the beginning of the censoring window
    DT(, censored_obs_time := obs_at - ptime_lwr)

  # set observation time to artifial observation time
  if (obs_at == "obs_secondary") {
    truncated_linelist <- truncated_linelist |>
      DT(, obs_at := pfilt_t)
  }
  return(truncated_linelist)
}

#' Pad zero observations as unstable in a lognormal distribution
#' @export
pad_zero <- function(data, pad = 1e-3) {
  data <- data |>
    data.table::copy() |>
    # Need upper bound to be greater than lower bound
    data.table:::DT(censored_obs_time == 0, censored_obs_time := 2 * pad) |>
    data.table:::DT(delay_lwr == 0, delay_lwr := pad) |>
    data.table:::DT(delay_daily == 0, delay_daily := pad)
}

#' Drop zero observations as unstable in a lognormal distribution
#' @export
drop_zero <- function(data) {
  data <- data |>
    data.table::copy() |>
    data.table:::DT(delay_daily != 0)
}
