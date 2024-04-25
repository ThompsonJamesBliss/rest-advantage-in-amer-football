data {
  int<lower=1> num_clubs;                                     // number of clubs
  int<lower=1> num_games;                                     // number of games

  int<lower=1> num_seasons;                                   // number of seasons
  int<lower=1, upper=num_seasons> season[num_games];                     // seasons

  int<lower=1,upper=num_clubs> home_team_code[num_games];     // home club for game g
  int<lower=1,upper=num_clubs> away_team_code[num_games];     // away club for game g

  real outcome[num_games];                                    // home point differential for game g

  int<lower=0,upper=1> h_adv[num_games];                      // indicator if game is home or neutral site
  int<lower=-1,upper=1> bye_pre_cba[num_games];               // indicator if there is a bye advantage pre cba change
  int<lower=-1,upper=1> bye_post_cba[num_games];              // indicator if there is a bye advantage post cba change
}
parameters {
  vector[num_clubs] theta;                // team strength

  real<lower=0> sigma_t;                  // team strength sd

  real<lower=0> sigma_a;                  // home advantage sd

  real<lower=0> sigma_m;                  // outcome sd

  real<lower=0> sigma_beta_pre;           // bye pre cba sd

  real<lower=0> sigma_beta_post;          // bye post cba sd

  vector[num_seasons] alpha;              // home advantage

  real beta_pre;                           // bye pre cba

  real beta_post;                           // bye post cba
}
model {
  vector[num_games] mu;

  // priors (half normal on scale parameters)
  sigma_a ~  normal(0, 5);
  sigma_m ~  normal(0, 5);
  sigma_t ~ normal(0, 5);
  sigma_beta_pre ~  normal(0, 5);
  sigma_beta_post ~ normal(0, 5);

  theta ~ normal(0, sigma_t);
  alpha ~ normal(0, sigma_a);
  beta_pre ~ normal(0, sigma_beta_pre);
  beta_post ~ normal(0, sigma_beta_post);



  // likelihood
  for (g in 1:num_games) {
    mu[g] = theta[home_team_code[g]] - theta[away_team_code[g]] + alpha[season[g]] * h_adv[g] + beta_pre * bye_pre_cba[g] + beta_post * bye_post_cba[g];
  }

  outcome ~ normal(mu, sigma_m);

}
generated quantities{
  vector[num_games] mu;

  real log_lik = 0;

  for(g in 1:num_games) {
    mu[g] = theta[home_team_code[g]] - theta[away_team_code[g]] + alpha[season[g]] * h_adv[g] + beta_pre * bye_pre_cba[g] + beta_post * bye_post_cba[g];

    // Update total log likelihood
    log_lik += normal_lpdf(outcome[g] | mu[g], sigma_m);
  }
}
