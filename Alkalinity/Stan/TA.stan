data{
  int n;
  vector[n] H_free;
  vector[n] H_added;
  array[n] int Sample;
  int n_Sample;
}

parameters{
  // Titration-specific parameters
  vector<lower=0>[n_Sample] TA;
  vector<lower=0>[n_Sample] beta;
  
  // Likelihood uncertainty
  real<lower=0> sigma;
}

model{
  // Titration-specific priors
  TA ~ gamma( square(2300) / square(500) , 2300 / square(500) ); 
  beta ~ gamma( square(1) / square(0.5) , 1 / square(0.5) );
  
  // Likelihood uncertainty prior
  sigma ~ exponential( 1 );

  // Model
  vector[n] mu = beta[Sample] .* ( H_added - TA[Sample] );

  // Likelihood
  H_free ~ normal( mu , sigma );
}