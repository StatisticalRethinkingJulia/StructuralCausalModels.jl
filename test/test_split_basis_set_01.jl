using StructuralCausalModels, Test

ProjDir = @__DIR__

d_str = "dag{k1 -> k2;k2 -> y;v -> x2;w -> k1;x1 -> v;x1 -> w;x2 -> y;x3 -> w;x3 -> y}"

dag = DAG("test_desc", d_str)

dagitty_str = to_dagitty(dag)

fname = joinpath(ProjDir, "test_descendents_03.dot")
to_graphviz(dag, fname)
#Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

bs = basis_set(dag)
println()

bs[6] |> display
@show d_separation(dag, :v, :x3; c=[:x1])
@show d_separation(dag, :v, :x3)
@show d_separation(dag, :x1, :x3; c=[:x1]) # <====
println()

bs[7] |> display
@show d_separation(dag, :v, :w; c=[:x1, :x3])
@show d_separation(dag, :v, :w; c=[:x1])
@show d_separation(dag, :v, :w; c=[:x3])
println()

bs[8] |> display
@show d_separation(dag, :v, :k1; c=[:x1, :w])
@show d_separation(dag, :v, :k1; c=[:x1])
@show d_separation(dag, :v, :k1; c=[:w])
println()

bs[9] |> display
@show d_separation(dag, :v, :y; c=[:x1, :x2, :x3, :k2])
@show d_separation(dag, :v, :y; c=[:k2, :x2, :x3])
@show d_separation(dag, :v, :y; c=[:k1, :x2, :x3])
@show d_separation(dag, :v, :y; c=[:w, :x2, :x3])
@show d_separation(dag, :v, :y; c=[:x1, :x2])
println()

bs[10] |> display
@show d_separation(dag, :x2, :x3; c=[:v])
@show d_separation(dag, :x2, :x3)
println()
