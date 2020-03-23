using StructuralCausalModels, StatisticalRethinking

ProjDir = @__DIR__

scm_01 = Dict(
  :Y => [:X1, :X2]
)

ex_1_3_10 = "
data {
  int N;
  real x1[N];
  real x2[N];
  real y[N];
}
parameters {
  real beta;
  real <lower=0.0> sigma;
}
model {
  vector[N] mu;
  real alpha;
  sigma ~ uniform( 0 , 5 );
  //alpha ~ normal(3.5, 1);
  beta ~ normal( 0, 2 );
  for (i in 1:N) {
    alpha = sum(x1) / N;
    mu[i] = alpha + beta * x2[i];
  }
  y ~ normal( mu, sigma );
}
";

# Define the Stanmodel.
ex_1_3_10s = SampleModel("ex_1_3_10", ex_1_3_10,
);

# Use 100 observations

N = 100
x1 = rand(Uniform(1, 6), N)
x2 = rand(Uniform(1, 6), N)
y = x1 + x2

# Input data for cmdstan

ex_1_3_10s_data = Dict("N" => N, "x1" => x1 , "x2" => x2, "y" => y);

# Sample using cmdstan
 
rc = stan_sample(ex_1_3_10s, data=ex_1_3_10s_data);

# Describe the draws

if success(rc)
  df = read_samples(ex_1_3_10s; output_format=:dataframe)
  p = Particles(df)
  display(p)
end
