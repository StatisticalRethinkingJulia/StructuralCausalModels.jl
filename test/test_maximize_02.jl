using StructuralCausalModels, Test

ProjDir = @__DIR__

amat_data = reshape([
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
  0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
  0,0,0,0,1,0,1,0,1,1,0,0,0,0,0,0,
  1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
  0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0
], (16,16));


vars = [Symbol("n$i") for i in 1:size(amat_data, 1)]
a = NamedArray(Int.(amat_data), (vars, vars), ("Rows", "Cols"));

a |> display

dag = DAG("ag_example", a)

fn = joinpath(mktempdir(), "ag.dot")
to_graphviz(dag, fn)
#Sys.isapple() && run(`open -a GraphViz.app $(fn)`)

m = [:n3, :n5, :n6, :n15, :n16];
c = [:n4, :n7];

ag = ancestral_graph(dag; m=m, c=c)
indx = filter(x -> !isnothing(x), indexin(vars, names(ag, 1)))
ag = ag[indx, indx]

rg = ribbon_graph(dag; m=m, c=c)
indx = filter(x -> !isnothing(x), indexin(vars, names(rg, 1)))
rg = rg[indx, indx]

maximize(ag) |> display
maximize(rg) |> display
