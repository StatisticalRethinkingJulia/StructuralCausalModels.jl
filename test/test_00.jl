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

#fname = scm_path("..", "examples", "SR", "SR6.4.2", "sr6.4.2.dot")
#Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

u = [:u]

d = OrderedDict(
  :y => [:c, :x],
  :b => [:c, :u],
  :x => :u,
  :c => :a,
  :u => :a
  )

dag = DAG("sr6_4_2", d, df);
show(dag)

display(dag.a)

fn = "sr6_4_2.dot"
to_graphviz(dag, fn)
Sys.isapple() && run(`open -a GraphViz.app $(fn)`)

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

println("Show path: $(allpaths[1])")
show_dag_path(dag, allpaths[1]) |> display
println()

println("Show path: $(allpaths[2])")
show_dag_path(dag, allpaths[2]) |> display
println()

adjustmentsets = adjustment_sets(dag, :x, :y, u)
println("Adjustment sets for: $(openpaths)")
adjustmentsets |> display
