# Assumes `ex.1.3.10.jl` has been executed.

using StructuralCausalModels, StatisticalRethinking, StatsPlots

ProjDir = @__DIR__

include("$ProjDir/m.1.3.10.jl")

# Use 10 observations

N = 10
df = DataFrame(
  :x1 => round.(rand(Uniform(1, 6), N), digits=0),
  :x2 => round.(rand(Uniform(1, 6), N), digits=0)
)
df[!, :y] = df[:, :x1] + df[:, :x2]
scale!(df, [:x1, :x2, :y])

# Input data for cmdstan

ex_1_3_10s_data = Dict("N" => N, "x2" => df[:, :x2_s], "y" => df[:, :y_s]);

# Sample using cmdstan
 
rc = stan_sample(ex_1_3_10s, data=ex_1_3_10s_data);

# Describe the draws

if success(rc)
  dfs = read_samples(ex_1_3_10s; output_format=:dataframe)
  p = Particles(dfs)
  display(p)

  xbar = mean(df[:, :x2])
  xstd = std(df[:, :x2])
  ybar = mean(df[:, :y])
  ystd = std(df[:, :y])

  xi = minimum(df[:, :x2_s]):0.01:maximum(df[:, :x2_s])
  yi = mean(dfs[:, :alpha]) .+ mean(dfs[:, :beta]) .* xi
  mu = link(dfs, [:alpha, :beta], xi)
  mu_r = [rescale(mu[i], ybar, ystd) for i in 1:length(xi)]
  mu_means_r = [mean(mu_r[i]) for i in 1:length(xi)]

  bnds_range = [[minimum(mu_r[i]), maximum(mu_r[i])] for i in 1:length(xi)]
  bnds_quantile = [quantile(mu_r[i], [0.055, 0.945]) for i in 1:length(xi)]
  bnds_hpd = [hpdi(mu_r[i], alpha=0.11) for i in 1:length(xi)];
  
  title = "y vs. x2 ( N=10 )" * "\nshowing sample and hpd range"
  p2 = plot(xlab="x2", ylab="y",
    title=title)

  x_r = rescale(xi, xbar, xstd)
  for i in 1:length(xi)
    plot!([x_r[i], x_r[i]], bnds_range[i],
      color=:lightgrey, leg=false)
  end

  for i in 1:length(xi)
    plot!([x_r[i], x_r[i]], bnds_hpd[i],
      color=:grey, leg=false)
  end

  plot!(x_r , mu_means_r, color=:black)
  scatter!(df[:, :x2], df[:, :y], leg=false, color=:darkblue)
  plot(p1, p2, layout=(2,1))
  
  savefig("$ProjDir/Fig-1-13-10a.png")


end
