# Load Julia packages (libraries) needed for clip

using StatisticalRethinking

ProjDir = @__DIR__

# Define the Stan language model

m_5_2 = "
data {
  int N;
  vector[N] D;
  vector[N] M;
}
parameters {
  real a;
  real bM;
  real<lower=0> sigma;
}
model {
  vector[N] mu = a + bM * M;
  a ~ normal( 0 , 0.2 );
  bM ~ normal( 0 , 0.5 );
  sigma ~ exponential( 1 );
  D ~ normal( mu , sigma );
}
";

# Define the SampleModel
m5_2s = SampleModel("m5.2", m_5_2);

# Input data

m5_2_data = Dict(
  "N" => size(df, 1), 
  "D" => df[:, :D],
  "M" => df[:, :M] 
);

# Sample using cmdstan

rc = stan_sample(m5_2s, data=m5_2_data);

if success(rc)

  # Describe the draws

  dfa2 = read_samples(m5_2s; output_format=:dataframe)
  Particles(dfa2)
  
end
