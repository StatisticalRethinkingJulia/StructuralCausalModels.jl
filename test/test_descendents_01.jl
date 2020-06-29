using StructuralCausalModels

d_str = "DAG(u ~ x + w, x ~ v + z, y ~ v, s1 ~ u, w ~ y, k ~ y, s2 ~ w + s3 + k, s3 ~ s1)"

dag = DAG("test_desc", d_str)

to_dagitty(dag) |> display

fname = joinpath(ProjDir, "test_descendents_01.dot")
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

display(dag)
display(dag.a)
println(dag.a[:x, :u])
println(dag.e[:x, :u])
println()

@show d_separation(dag, :x, :w)
@show d_separation(dag, :x, :w; cset=:v)
@show d_separation(dag, :x, :w; cset=:y)
@show d_separation(dag, :x, :v; cset=:s3)
println()

@show adjustment_sets(dag, :x, :w)
@show adjustment_sets(dag, :x, :s1)
@show adjustment_sets(dag, :x, :s2)
@show adjustment_sets(dag, :u, :k)
@show adjustment_sets(dag, :y, :s2)
