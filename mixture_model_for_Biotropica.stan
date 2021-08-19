
data {
 int<lower = 0> N0; // Number of waiting times without monkeys present.
 int<lower = 0> N1; // Number of waiting times with monkeys present.
 int<lower = 0> N_trees; // Number of unique trees
 int<lower = 1, upper = N_trees> tree_id0[N0]; // Tree IDs in the no monkey condition
 int<lower = 1, upper = N_trees> tree_id1[N1]; // Tree IDs in the monkey condition
 vector[N0] y0; // Vector of log-waiting times without monkeys present.
 vector[N1] y1; // Vector of log-waiting times with monkeys present.
 vector[N_trees] tree_rel_freq; // Vector of relative frequencies of tree IDs for marginalization. 
}

parameters {
  real mu; // Mean parameter for waiting times of meander movements. Shared across conditions.
  real<lower=mu> mu0; // Mean parameter for log-waiting times without monkeys present. 
  real<lower=mu> mu1; // Mean parameter for log-waiting times with monkeys present.
  real<lower=0> sigma; // SD of waiting times shared across conditions.
  real<lower=0, upper=1> p; // Mixing probability for the two modes shared across conditions.
  vector[N_trees] eta_tree; // Untransformed vector of random effects for trees
  real<lower=0> sigma_tree; // tree-to-tree standard deviation
}

transformed parameters {
  vector[N_trees] RE_tree; // Vector of random effects for trees
  vector[N0] yhat0; // Predicted values for no-monkey condition
  vector[N1] yhat1; // Predicted values for monkey condition
  RE_tree = sigma_tree * eta_tree; // Using the cholesky transform to produce the random effects
  for (n in 1:N0) // Loop over log-waiting times without monkeys, evaluate linear predictor.
    yhat0[n] = RE_tree[tree_id0[n]]; // Nested reference for the tree effect for observation n
  for (n in 1:N1) // Loop over log-waiting times with monkeys, evaluate linear predictor.
    yhat1[n] = RE_tree[tree_id1[n]]; // Nested reference for the tree effect for observation n
}

model {
 p ~ uniform(0, 1); // Uniform prior for mixing probability. 
 mu ~ normal(log(5), 1); // Informative Gaussian prior for mean of meander waiting times. 
 mu0 ~ normal(log(2160), 5); // Informative Gaussian prior for mean without monkeys present. 
 mu1 ~ normal(log(2160), 5); // Informative Gaussian prior for mean with monkeys present.
 eta_tree ~ normal(0, 1); // Gaussian prior for untransformed tree effects.
 sigma ~ cauchy(0, 1); // Half-Cauchy prior for SD of log waiting times. 
 sigma_tree ~ cauchy(0, 1); // Half-Cauchy prior for SD of tree random effects. 

 for (n in 1:N0) // Loop over log-waiting times without monkeys, evaluate log pdf. 
   target += log_mix(p,
                     normal_lpdf(y0[n] | mu, sigma),
                     normal_lpdf(y0[n] | mu0 + yhat0[n], sigma)); // Mean is the general mean for the no-monkey condition (mu0) plus the tree effect for the particular tree (yhat0[n]).
 for (n in 1:N1) // Loop over log-waiting times with monkeys, evaluate log pdf.
   target += log_mix(p,
                     normal_lpdf(y1[n] | mu, sigma),
                     normal_lpdf(y1[n] | mu1 + yhat1[n], sigma)); // Mean is the general mean for the monkey condition (mu1) plus the tree effect for the particular tree (yhat1[n]).
}

generated quantities{
real mean0_marginal;
real mean1_marginal;
real RE_tree_marginal=0; 

// Weighted average of tree random effects, for marginal means below. 
for(n in 1:N_trees) RE_tree_marginal = RE_tree_marginal + tree_rel_freq[n]*RE_tree[n]; 

mean0_marginal = mu0 + RE_tree_marginal; 
mean1_marginal = mu1 + RE_tree_marginal; 
} 
