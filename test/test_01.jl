using StructuralCausalModels, Test

ProjDir = @__DIR__
cd(ProjDir) #do

# Experiments with unicode symbols, replacements for e.g. _||_

N = 100
b_AU = 0.5
b_AC = 3
b_UX = -1
b_UB = 2
b_CB = -1.5
b_CY = 1
b_XY = 5

df = DataFrame(
  :a => rand(Normal(), N)
);
df[!, :u] = rand(Normal(1, 2), N) + b_AU * df[:, :a]
df[!, :c] = rand(Normal(0, 1), N) + b_AC * df[:, :a]
df[!, :b] = rand(Normal(1, 1), N) + b_UB * df[:, :u]
df[!, :x] = rand(Normal(-2, 1), N) + b_UX * df[:, :u]
df[!, :y] = rand(Normal(1, 2), N) + b_XY * df[:, :x] + b_CY * df[:, :c]

d = OrderedDict(
  :x => [:y],
  :a => [:c, :u],
  :c => [:y, :b],
  :u => [:x, :b]
);
u = [:u]

dag = DAG("sr6.4.2", d, df);
show(dag)

adjustmentsets = adjustment_sets(dag, :x, :y, u)
println("Adjustment sets:")
adjustmentsets |> display
