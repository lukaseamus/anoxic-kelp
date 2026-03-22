data{
  int n;
  vector[n] DIC;
  vector[n] TA;
  array[n] int Treatment;
  int n_Treatment;
  array[n] int Flask;
  int n_Flask;
}

parameters{
  real A0; // intercept, TA when DIC = 0
  real q_mu;
  real<lower=0> q_T_sigma;
  real<lower=0> q_F_sigma;
  real<lower=0> sigma; // Likelihood standard deviation
  
  vector[n_Treatment] q_T_z;
  vector[n_Flask] q_F_z;
}

transformed parameters{
  // Convert z-scores
  vector[n_Treatment] q_T = q_T_z * q_T_sigma + q_mu;
  vector[n_Flask] q_F = q_F_z * q_F_sigma + 0;
}

model{
  // Priors
  A0 ~ normal( 0 , 1e3 );
  q_mu ~ normal( 8 , 4 );
  q_T_sigma ~ normal( 0 , 0.5 ) T[0,];
  q_F_sigma ~ normal( 0 , 0.1 ) T[0,];
  sigma ~ exponential( 1 );
  
  q_T_z ~ normal( 0 , 1 );
  q_F_z ~ normal( 0 , 1 );

  // Model
  vector[n] q = q_T[Treatment] + q_F[Flask];
  vector[n] mu = A0 + q .* DIC;
  
  // Normal likelihood
  TA ~ normal( mu , sigma );
}