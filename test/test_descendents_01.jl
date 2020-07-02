using StructuralCausalModels

ProjDir = @__DIR__

d_str = "DAG(u ~ x + w, x ~ v + z, y ~ v, s1 ~ u, w ~ y, k ~ y, s2 ~ w + s3 + k, s3 ~ s1)"

dag = DAG("test_desc", d_str)

to_dagitty(dag) |> display

#=
fname = joinpath(ProjDir, "test_descendents_01.dot")
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)
=#

display(dag)
display(dag.a)
println(dag.a[:x, :u])
println(dag.e[:x, :u])
println()

bs = basis_set(dag)
display(bs)

as = adjustment_sets(dag, :u, :s2)
display(as)
