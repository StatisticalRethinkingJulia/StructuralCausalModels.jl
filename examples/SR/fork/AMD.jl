using StatisticalRethinking, StructuralCausalModels
using StatsPlots

ProjDir = @__DIR__

println()
N = 100
df = DataFrame();
df[!, :A] = rand(Normal(), N)
df[!, :M] = [rand(Normal(-a), 1)[1] for a in df[:, :A]]
df[!, :D] = [rand(Normal(a), 1)[1] for a in df[:, :A]]

StatsPlots.cornerplot(Array(df), label=names(df))
savefig("$(ProjDir)/AMD_df_1.png")

println()

for i in 1:3
  include(scm_path("..", "examples", "SR", "fork", "AMD_m$i.jl"))
end

if success(rc)

  r1 = plotcoef([m5_1s, m5_2s, m5_3s], [:bA, :bM], "$(ProjDir)/AMD_fork_1.png",
    "Particles (Normal) estimates")
  display(r1)

end

d = OrderedDict(
  :M => [:A],
  :D => [:A]
);

dag = DAG("fork_correlated", d, df=df);
show(dag)

display(dag.s); println()

shipley_test_dag_3 = shipley_test(dag)
if !isnothing(shipley_test_dag_3)
  display(shipley_test_dag_3)
end
println()

f = [:M]; s = [:D]; sel = vcat(f, s)
c = [:A]

e = d_separation(dag, f, s)
println("d_separation($(dag.name), $f, $s) = $e")

e = d_separation(dag, f, s, c=c)
println("d_separation($(dag.name), $f, $s, c=$c) = $e")
println()
N = 100
df = DataFrame();
df[!, :A] = rand(Normal(), N)
df[!, :M] = [rand(Normal(-a), 1)[1] for a in df[:, :A]]
df[!, :D] = [rand(Normal(a), 1)[1] for a in df[:, :A]] -
  [rand(Normal(m), 1)[1] for m in df[:, :M]]


StatsPlots.cornerplot(Array(df), label=names(df))
savefig("$(ProjDir)/AMD_df_2.png")

println()

for i in 1:3
  include(scm_path("..", "examples", "SR", "fork", "AMD_m$i.jl"))
end

if success(rc)

  r1 = plotcoef([m5_1s, m5_2s, m5_3s], [:bA, :bM], "$(ProjDir)/AMD_fork_2.png",
    "Particles (Normal) estimates")
  display(r1)

end

d = OrderedDict(
  :M => [:A],
  :D => [:A]
);

dag = DAG("fork_correlated", d, df=df);
show(dag)

display(dag.s); println()

shipley_test_dag_3 = shipley_test(dag)
if !isnothing(shipley_test_dag_3)
  display(shipley_test_dag_3)
end
println()

f = :M; s = :D; sel = vcat(f, s)
c = :A

e = d_separation(dag, f, s)
println("d_separation($(dag.name), $f, $s) = $e")

e = d_separation(dag, f, s, c=c)
println("d_separation($(dag.name), $f, $s, c=$c) = $e")

as = adjustment_sets(dag, :A, :D)
as |> display

