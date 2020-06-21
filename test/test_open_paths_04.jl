#using Revise
using StructuralCausalModels
using Combinatorics

ProjDir = @__DIR__
cd(ProjDir) #do

d_string = "dag {k -> i; k -> D; m -> l; l -> k; n -> m; o -> n; p -> o; " *
  "q -> p; r -> q; r -> D; b -> a; " *
  "c -> b; f -> c; g -> f; h -> g; i -> h; a -> E; i -> E; E -> D}"

dag = DAG("test_open_paths_04", d_string);
show(dag)

bs = basis_set(dag)
bs |> display

fname = joinpath(ProjDir, "test_open_paths_04.dot")
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

println()
adjs = adjustment_sets(dag, :E, :D)
adjs |> display