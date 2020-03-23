using StructuralCausalModels, StatisticalRethinking, StatsPlots

ProjDir = @__DIR__

ex_1_3_10 = "
data {
  int N;
  real x1[N];
  real y[N];
}
parameters {
  real alpha;
  real beta;
  real <lower=0.0> sigma;
}
model {
  vector[N] mu;
  sigma ~ uniform( 0 , 2 );
  alpha ~ normal(0, 2);
  beta ~ normal( 0, 2 );
  for (i in 1:N) {
    mu[i] = alpha + beta * x1[i];
  }
  y ~ normal( mu, sigma );
}
";

# Define the Stanmodel.
tmpdir = joinpath(ProjDir, "tmp")
ex_1_3_10s = SampleModel("ex_1_3_10", ex_1_3_10, tmpdir=tmpdir);
