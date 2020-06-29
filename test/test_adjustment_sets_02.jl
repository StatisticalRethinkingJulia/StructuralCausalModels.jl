using StructuralCausalModels, Test

d_str = "dag{k1 -> k2;k2 -> y;v -> x2;w -> k1;x1 -> v;x1 -> w;x2 -> y;x3 -> w;x3 -> y}"

dag = DAG("test_desc", d_str)

to_dagitty(dag) |> display

fname = joinpath(ProjDir, "test_descendents_03.dot")
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

display(dag)
display(dag.a)
println(dag.a[:x1, :w])
println(dag.e[:x1, :w])
println()

bs = basis_set(dag)
bs |> display

@show adjustment_sets(dag, :x2, :y)
