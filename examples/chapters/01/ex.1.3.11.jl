# Multiple regression (simulated).

using StructuralCausalModels, StatisticalRethinking, StatsPlots

ProjDir = @__DIR__

include("$ProjDir/m.1.3.11.jl")

# Use 1000 observations

N = 1000
df = DataFrame(
  :x1 => rand(Normal(2, 1), N),
  :x2 => rand(Normal(4, 1), N)
)
df[!, :x2] = df[:, :x2] + 0.25 .* df[:, :x1]
df[!, :y] = df[:, :x1] - df[:, :x2] + rand(Normal(0, 0.1), N)
scale!(df, [:x1, :x2, :y])

# Input data for cmdstan

ex_1_3_11s_data = Dict("N" => N, "x1" => df[:, :x1_s], "x2" => df[:, :x2],
  "y" => df[:, :y_s]);

# Sample using cmdstan
 
rc = stan_sample(ex_1_3_11s, data=ex_1_3_11s_data);

# Describe the draws

if success(rc)

  dfs = read_samples(ex_1_3_11s; output_format=:dataframe)
  p = Particles(dfs)
  display(p)

  # x1 and x2 are only partial independent:

  ucor = round(cor(df.x1, df.x2), digits=3)
  println("\nUnconditioned correlation: cor(df.x1, df.x2) = $(ucor)\n")

  # Conditiona on df.y

  dfc = df[df.y .> -3.0, [:x1, :x2, :y]]
  dfc = dfc[dfc.y .< -1.8, [:x1, :x2]]
  ccor = round(cor(dfc.x1, dfc.x2), digits=3)
  println("Conditioned correlation, e.g. -3.0 < y < -1.0: cor(dfc.x1, dfc.x2) = $(ccor)\n")
  dfc = df[df.y .> -2.6, [:x1, :x2, :y]]
  dfc = dfc[dfc.y .< -2.2, [:x1, :x2]]
  ccor = round(cor(dfc.x1, dfc.x2), digits=3)
  println("Conditioned correlation, e.g. -2.6 < y < -2.2: cor(dfc.x1, dfc.x2) = $(ccor)\n")

end
