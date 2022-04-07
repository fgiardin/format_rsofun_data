# A set of cost functions to evaluate / calibrate
# the models with. You are free to create your own
# but this is an initial, most common, set.

#' Root mean squared error
#'
#' Root mean squared error (RMSE) cost function on
#' the kphio parameter.
#'
#' @param par parameters
#' @param obs observations
#' @param drivers driver data
#' @param inverse invert the function (1-value)
#'
#' @importFrom magrittr '%>%'
#'
#' @return the RMSE on the kpio parameter
#' @export

cost_rmse_kphio_s0 <- function(
    par,
    obs,
    drivers,
    inverse = FALSE
){

  # predefine variables for CRAN check compliance
  sitename <- data <- NULL

  ## execute model for this parameter set
  ## For calibrating quantum yield efficiency only
  params_modl <- list(
    kphio           = par[[1]],
    soilm_par_a     = 0.0,
    soilm_par_b     = 0.0,
    tau_acclim_tempstress = 10,
    par_shape_tempstress  = 0.0
  )

  # run the model
  df <- runread_pmodel_f(
    drivers,
    par = params_modl,
    makecheck = TRUE,
    parallel = FALSE
  )

  # cleanup
  df <- df %>%
    dplyr::select(sitename, data) %>%
    tidyr::unnest(data) %>%
    dplyr::rename(
      'gpp_mod' = 'gpp'
    )

  obs <- obs %>%
    dplyr::select(sitename, data) %>%
    tidyr::unnest(data)

  # left join with observations
  df <- dplyr::left_join(df, obs, by = c("sitename", "date"))

  # Calculate cost (RMSE)
  cost <- sqrt( mean( (df$gpp - df$gpp_mod )^2, na.rm = TRUE ) )

  if (inverse) cost <- 1.0 / cost

  return(cost)
}
