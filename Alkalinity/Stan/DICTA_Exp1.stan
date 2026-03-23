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
  real A0_mu; // intercept, TA when DIC = 0
  real q_mu; // slope, biogeochemical process quotient
  real<lower=0> A0_T_sigma;
  real<lower=0> A0_F_sigma;
  real<lower=0> q_T_sigma;
  real<lower=0> q_F_sigma;
  real<lower=0> sigma; // Likelihood standard deviation
  
  vector[n_Treatment] A0_T_z;
  vector[n_Flask] A0_F_z;
  vector[n_Treatment] q_T_z;
  vector[n_Flask] q_F_z;
}

transformed parameters{
  // Convert z-scores
  vector[n_Treatment] A0_T = A0_T_z * A0_T_sigma + A0_mu;
  vector[n_Flask] A0_F = A0_F_z * A0_F_sigma + 0;
  vector[n_Treatment] q_T = q_T_z * q_T_sigma + q_mu;
  vector[n_Flask] q_F = q_F_z * q_F_sigma + 0;
}

model{
  // Priors
  A0_mu ~ normal( 0 , 1e3 );
  q_mu ~ normal( 8 , 0.8 ); // iron reduction q = 8
  A0_T_sigma ~ normal( 0 , 1e3 ) T[0,];
  A0_F_sigma ~ normal( 0 , 100 ) T[0,];
  q_T_sigma ~ normal( 0 , 0.8 ) T[0,];
  q_F_sigma ~ normal( 0 , 0.4 ) T[0,];
  sigma ~ exponential( 1 );
  
  A0_T_z ~ normal( 0 , 1 );
  A0_F_z ~ normal( 0 , 1 );
  q_T_z ~ normal( 0 , 1 );
  q_F_z ~ normal( 0 , 1 );

  // Model
  vector[n] A0 = A0_T[Treatment] + A0_F[Flask];
  vector[n] q = q_T[Treatment] + q_F[Flask];
  vector[n] mu = A0 + q .* DIC;
  
  // Normal likelihood
  TA ~ normal( mu , sigma );
}