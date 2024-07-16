data {
  int<lower=1> num_clubs;                                     // number of clubs
  int<lower=1> num_games;                                     // number of games

  int<lower=1> num_seasons;                                   // number of seasons
  int<lower=1> num_eras;                                      // number of seasons
  int<lower=1, upper=num_seasons> season[num_games];          // seasons
  int<lower=1, upper=num_eras> era[num_games];          // seasons

  int<lower=1,upper=num_clubs> home_team_code[num_games];     // home club for game g
  int<lower=1,upper=num_clubs> away_team_code[num_games];     // away club for game g

  real outcome[num_games];                                    // outcome for game g

  int<lower=0,upper=1> h_adv[num_games];                      // indicator if game is home or neutral site
  int<lower=-1,upper=1> bye[num_games];                       // signed indicator if there is a bye advantage
  int<lower=-1,upper=1> mnf[num_games];                       // signed indicator if there is a mnf advantage
  int<lower=-1,upper=1> mini[num_games];                       // signed indicator if there is a mini advantage
}
parameters {
  matrix[num_clubs, num_seasons] theta;                   // team strength
  real<lower=0> sigma_team_strength;                     // team strength sd
  real<lower=0> sigma_game;                     // game sd
  real<lower=0,upper=1> gamma;
  real alpha_ha_trend;                 // home advantage trend
  real alpha_ha_intercept;              // home advantage intercept
  vector[num_eras] alpha_bye;                            // bye advantage
  real alpha_mnf;                           // mnf advantage
  real alpha_mini;                           // mini advantage

}
model {
  vector[num_games] Ey;

  // priors (half normal on scale parameters)
  sigma_team_strength ~ normal(0, 5);
  sigma_game ~ normal(0, 5);
  alpha_ha_trend ~ normal(0, 5);
  alpha_ha_intercept ~ normal(0, 5);
  alpha_bye ~ normal(0, 5);
  alpha_mnf ~ normal(0, 5);
  alpha_mini ~ normal(0, 5);
  gamma ~ uniform(0,1);

  for (t in 1:num_clubs) {

    theta[t, 1] ~ normal(0, sigma_team_strength);

    for(s in 2:num_seasons){

      theta[t, s] ~ normal(gamma * theta[t, s - 1], sigma_team_strength);

    }

  }


  // likelihood
  for (g in 1:num_games) {
    Ey[g] = theta[home_team_code[g], season[g]] - theta[away_team_code[g], season[g]] + (alpha_ha_trend * season[g] + alpha_ha_intercept) * h_adv[g] + alpha_bye[era[g]] * bye[g] + alpha_mnf * mnf[g] + alpha_mini * mini[g];
  }

  outcome ~ normal(Ey, sigma_game);

}
generated quantities{
  vector[num_games] Ey;

  real log_lik = 0;

  for(g in 1:num_games) {
    Ey[g] = theta[home_team_code[g], season[g]] - theta[away_team_code[g], season[g]] + (alpha_ha_trend * season[g] + alpha_ha_intercept) * h_adv[g] + alpha_bye[era[g]] * bye[g] + alpha_mnf * mnf[g] + alpha_mini * mini[g];

    // Update total log likelihood
    log_lik += normal_lpdf(outcome[g] | Ey[g], sigma_game);
  }
}
