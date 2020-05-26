using StructuralCausalModels, Test

ProjDir = @__DIR__
cd(ProjDir) #do

# Experiments with unicode symbols, replacements for e.g. _||_

println("\u2561\u255E")
println("\u2550\u2550")
println("\u21d0 \u21d1 \u21d2 \u27f9   \u21e2")

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

fname = scm_path("..", "examples", "SR", "SR6.4.2", "sr6.4.2.dot")
!isdefined(Main, :Test) && Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

d = OrderedDict(
  :x => [:y],
  :a => [:c, :u],
  :c => [:y, :b],
  :u => [:x, :b]
);
u = [:u]

dag = DAG("sr6.4.2", d, df);
show(dag)

basisset = StructuralCausalModels.basis_set(dag)
println("Basis_set:")
display(basisset)
println()

allpaths  = all_paths(dag, :x, :y)
println("All paths between :x and :y:")
allpaths |> display
println()

backdoorpaths = backdoor_paths(dag, allpaths, :x)
println("All backdoors between :x and :y:")
backdoorpaths |> display
println()

openpaths = open_paths(dag, backdoorpaths)
println("All open (backdoor) paths between :x and :y:")
openpaths |> display
println()

println("Show path: $(allpaths[2])")
show_dag_path(dag, allpaths[2]) |> display
println()

adjustmentsets = adjustment_sets(dag, :x, :y, u)
println("Adjustment sets for: $(openpaths)")
adjustmentsets |> display
