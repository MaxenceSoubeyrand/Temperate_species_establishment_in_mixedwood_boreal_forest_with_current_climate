                                                                                    
data {
  int<lower=1> n_pls; // plot-years
  int<lower=1> n_tree; // individual trees
  int<lower=1> n_sp; // species
  int<lower=1> n_mes; // growth measures
  int<lower=1> n_ngb; // neighbors
  
  int<lower=1,upper=n_pls> id_pls[n_mes]; // ID of plot-year for each measurement
  int<lower=1,upper=n_tree> id_tree[n_mes]; // ID of tree for each measurement
  int<lower=1,upper=n_sp> id_sp[n_ngb]; // ID of species for each neighbor
  // First and last neighbor index for each measurement
  int<lower=1, upper=n_ngb> first_ngb[n_mes];
  int<lower=1, upper=n_ngb> last_ngb[n_mes];
  
  vector[n_mes] gr; // log growth rate
  vector[n_mes] ln_dbh; // standardized log DBH
  vector[n_mes] shade; // shading (0 to 1)
  
  vector<lower=0>[n_ngb] ngb_dbh; // neighbor DBH
  vector<lower=0>[n_ngb] ngb_dist; // neighbor distance
}

transformed data {
  // Log-transformed DBH and distance of neighbors
  vector[n_ngb] log_ngb_dbh = log(ngb_dbh);
  vector[n_ngb] log_ngb_dist = log(ngb_dist);
//  real log_q = log(100); // q parameter of NCI to rescale DBH
  real log_q = 0;
}

parameters {
  real interc;
  real b_dbh_lin;
  real<upper=0> b_dbh_quad;
  real<upper=0> b_shade;
  real<lower=0> sigma;

  // Random effects
  real<lower=0> sd_pls;
  real<lower=0> sd_tree;
  vector[n_pls] r_pls;
  vector[n_tree] r_tree;
    
  // NCI term parameters
  real<lower=0> alpha;
  real<lower=0> beta;
  vector<lower=0>[n_sp] nl_lambda; // negative log of lambda
}

model {
  vector[n_ngb] partial_nci;
  vector[n_mes] partial_mu;
  vector[n_mes] mu;
  
  // Prior distributions
  interc ~ normal(-1, 1);
  b_dbh_lin ~ normal(0, 1);
  b_dbh_quad ~ normal(0, 1);
  b_shade ~ normal(0, 1);
  sigma ~ normal(0, 0.1);
  
  // sd_: standard deviation of random effect, r_: normalized random effect
  sd_pls ~ normal(0, 0.1);
  sd_tree ~ normal(0, 0.1);
  r_pls ~ normal(0, 1);
  r_tree ~ normal(0, 1);
  
  alpha ~ exponential(1);
  beta ~ exponential(1);
  nl_lambda ~ exponential(1); // Equivalent to uniform distribution on (0,1) for lambda
  
  // Vector of NCI contribution for each neighbor
  // Equivalent to lambda * (DBH/q)^alpha / dist^beta but easier to calculate on log scale
  partial_nci = exp(-nl_lambda[id_sp] + alpha * (log_ngb_dbh - log_q) - beta * log_ngb_dist);
  // Expected growth before adding NCI
  partial_mu = interc + b_dbh_lin * ln_dbh + b_dbh_quad * square(ln_dbh) + b_shade * shade;
  // Add sum of NCI contributions to each measurement
  for (i in 1:n_mes) {
    mu[i] = exp(partial_mu[i] - sum(partial_nci[first_ngb[i]:last_ngb[i]])) + sd_pls * r_pls[id_pls[i]] + sd_tree * r_tree[id_tree[i]]; 
  }  
  
  gr ~ normal(mu, sigma);
}

generated quantities {
  vector[n_mes] log_lik;
  
  {
    vector[n_ngb] partial_nci;
    vector[n_mes] partial_mu;
    vector[n_mes] mu;
    
    partial_nci = exp(-nl_lambda[id_sp] + alpha * (log_ngb_dbh - log_q) - beta * log_ngb_dist);
    partial_mu = interc + b_dbh_lin * ln_dbh + b_dbh_quad * square(ln_dbh) + b_shade * shade;
    
    for (i in 1:n_mes) {
      mu[i] = exp(partial_mu[i] - sum(partial_nci[first_ngb[i]:last_ngb[i]])) + sd_pls * r_pls[id_pls[i]] + sd_tree * r_tree[id_tree[i]]; 
      log_lik[i] = normal_lpdf(gr[i] | mu[i], sigma);
    }
  }
  
}
