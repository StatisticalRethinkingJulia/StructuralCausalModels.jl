# Load Julia packages (libraries) needed for clip

using StatisticalRethinking

ProjDir = @__DIR__

# Define the Stan language model

m_5_3 = "
data {
  int N;
  vector[N] D;
  vector[N] M;
  vector[N] A;
}
parameters {
  real a;
  real bA;
  real bM;
  real<lower=0> sigma;
}
model {
  vector[N] mu = a + bA * A + bM * M;
  a ~ normal( 0 , 0.2 );
  bA ~ normal( 0 , 0.5 );
  bM ~ normal( 0 , 0.5 );
  sigma ~ exponential( 1 );
  D ~ normal( mu , sigma );
}
";

# Define the SampleModel
m5_3s = SampleModel("m5.3", m_5_3);

# Input data

m5_3_data = Dict(
  "N" => size(df, 1), 
  "D" => df[:, :D],
  "M" => df[:, :M],
  "A" => df[:, :A] 
);

# Sample using cmdstan

rc = stan_sample(m5_3s, data=m5_3_data);

if success(rc)

  # Describe the draws

  dfa3 = read_samples(m5_3s; output_format=:dataframe)
  Particles(dfa3)

end
