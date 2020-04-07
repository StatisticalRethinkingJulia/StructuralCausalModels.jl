# Load Julia packages (libraries) needed.

using StatisticalRethinking

ProjDir = @__DIR__

m_5_1 = "
data {
 int < lower = 1 > N; // Sample size
 vector[N] D; // Outcome
 vector[N] A; // Predictor
}

parameters {
 real a; // Intercept
 real bA; // Slope (regression coefficients)
 real < lower = 0 > sigma;    // Error SD
}

model {
  vector[N] mu;               // mu is a vector
  a ~ normal(0, 0.2);         //Priors
  bA ~ normal(0, 0.5);
  sigma ~ exponential(1);
  mu = a + bA * A;
  D ~ normal(mu , sigma);     // Likelihood
}
";

# Define the SampleModel and set the output format to :mcmcchains.

m5_1s = SampleModel("m5.1", m_5_1);

# Input data for cmdstan

ad_data = Dict(
  "N" => size(df, 1),
  "D" => df[!, :D],
  "A" => df[!, :A]
);

# Sample using StanSample

rc = stan_sample(m5_1s, data=ad_data);

if success(rc)

  # Describe the draws

  dfa1 = read_samples(m5_1s; output_format=:dataframe)
  Particles(dfa1)

end
