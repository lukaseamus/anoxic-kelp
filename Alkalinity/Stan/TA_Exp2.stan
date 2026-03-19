data{
  int n;
  vector[n] Day; // Main continuous predictor
  vector[n] TA; // Response
  array[n] int Treatment; // Main categorical predictor
  int n_Treatment;
  array[n] int Flask;
  int n_Flask;
}

parameters{
  // Global parameters
  real log_TA_base; // TA baseline
  real log_TA_prod_mu;
  real log_r_mu;
  real log_m_mu;
  real<lower=0> theta; // Likelihood scale
  
  real<lower=0> log_TA_prod_T_sigma;
  real<lower=0> log_r_T_sigma;
  real<lower=0> log_m_T_sigma;
  
  real<lower=0> log_TA_prod_F_sigma;
  real<lower=0> log_r_F_sigma;
  real<lower=0> log_m_F_sigma;
  
  // Treatment parameters
  vector[n_Treatment] log_TA_prod_T_z;
  vector[n_Treatment] log_r_T_z;
  vector[n_Treatment] log_m_T_z;
  
  // Flask parameters
  vector[n_Flask] log_TA_prod_F_z;
  vector[n_Flask] log_r_F_z;
  vector[n_Flask] log_m_F_z;
}

transformed parameters{
  // Convert z-scores
  vector[n_Treatment] log_TA_prod_T = log_TA_prod_T_z * log_TA_prod_T_sigma + log_TA_prod_mu;
  vector[n_Treatment] log_r_T = log_r_T_z * log_r_T_sigma + log_r_mu;
  vector[n_Treatment] log_m_T = log_m_T_z * log_m_T_sigma + log_m_mu;
  
  vector[n_Flask] log_TA_prod_F = log_TA_prod_F_z * log_TA_prod_F_sigma + 0;
  vector[n_Flask] log_r_F = log_r_F_z * log_r_F_sigma + 0;
  vector[n_Flask] log_m_F = log_m_F_z * log_m_F_sigma + 0;
}

model{
  // Global priors
  log_TA_base ~ normal( log(2300) , 0.5 ); // normal seawater TA
  log_TA_prod_mu ~ normal( log(5e4) , 0.5 );
  log_r_mu ~ normal( log(0.1) , 0.5 );
  log_m_mu ~ normal( log(50) , 0.5 );
  
  log_TA_prod_T_sigma ~ normal( 0 , 0.5 ) T[0,];
  log_r_T_sigma ~ normal( 0 , 0.5 ) T[0,];
  log_m_T_sigma ~ normal( 0 , 0.5 ) T[0,];
  
  log_TA_prod_F_sigma ~ normal( 0 , 0.1 ) T[0,];
  log_r_F_sigma ~ normal( 0 , 0.1 ) T[0,];
  log_m_F_sigma ~ normal( 0 , 0.1 ) T[0,];
  
  theta ~ exponential( 1 );
  
  // Treatment priors
  log_TA_prod_T_z ~ normal( 0 , 1 );
  log_r_T_z ~ normal( 0 , 1 );
  log_m_T_z ~ normal( 0 , 1 );
  
  // Flask priors
  log_TA_prod_F_z ~ normal( 0 , 1 );
  log_r_F_z ~ normal( 0 , 1 );
  log_m_F_z ~ normal( 0 , 1 );
  
  // Model
  /// Parameters
  real TA_base = exp( log_TA_base );
  vector[n] TA_prod = exp( log_TA_prod_T[Treatment] + log_TA_prod_F[Flask] );
  vector[n] r = exp( log_r_T[Treatment] + log_r_F[Flask] );
  vector[n] m = exp( log_m_T[Treatment] + log_m_F[Flask] );
  
  /// Function
  vector[n] mu = TA_prod .* inv_logit( r .* ( Day - m ) ) + TA_base;
  
  // Gamma likelihood
  TA ~ gamma( mu / theta , 1 / theta );
}