### Function to compute pointwise log-likelihood [will take a few seconds]
expand_log_lik <- function(model, likelihood, stan_data, outcome) {
  ### Extract Posterior Samples
  posterior <- extract(model)
  n_iter <- length(posterior$log_lik)
  if (n_iter == 0) {
    n_iter <- length(posterior$lp__)
  }

  log_lik <- matrix(NA, nrow = n_iter, ncol = stan_data$num_games)


  for (i in 1:n_iter) {
    ### Mean of normal distrbution according to model we are fitting
    if (likelihood == "split_bye") {
      mu <- posterior$theta[cbind(i, stan_data$home_team_code, stan_data$season)] -
        posterior$theta[cbind(i, stan_data$away_team_code, stan_data$season)] +
        (posterior$alpha_ha_trend[i] * stan_data$season +
          posterior$alpha_ha_intercept[i]) * stan_data$h_adv +
        posterior$alpha_bye[i, stan_data$era] * stan_data$bye +
        posterior$alpha_mnf[i] * stan_data$mnf +
        posterior$alpha_mini[i] * stan_data$mini
    } else if (likelihood == "no_split") {
      mu <- posterior$theta[cbind(i, stan_data$home_team_code, stan_data$season)] -
        posterior$theta[cbind(i, stan_data$away_team_code, stan_data$season)] +
        (posterior$alpha_ha_trend[i] * stan_data$season +
          posterior$alpha_ha_intercept[i]) * stan_data$h_adv +
        posterior$alpha_bye[i] * stan_data$bye +
        posterior$alpha_mnf[i] * stan_data$mnf +
        posterior$alpha_mini[i] * stan_data$mini
    }

    log_lik[i, ] <- dnorm(x = outcome, mean = mu, sd = posterior$sigma_m[i], log = T)
  }

  return(log_lik)
}

# Takes an NFL prior points favored by and converts it to a probability
points_to_prob <- function(points) {
  return(stats::pnorm(points, mean = 0, sd = 13.5))
}


# Displays number as rounded number to given number of digits
pretty_digits <- function(value, digits) {
  number_return <- format(round(value, digits = digits), nsmall = digits)
  return(number_return)
}
