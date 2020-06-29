using StructuralCausalModels, Test

ProjDir = @__DIR__

d_str = "dag{k1 -> k2;k2 -> y;v -> x2;w -> k1;x1 -> v;x1 -> w;x2 -> y;x3 -> w;x3 -> y}"

dag = DAG("test_desc", d_str)

to_dagitty(dag) |> display

#=
fname = joinpath(ProjDir, "test_descendents_03.dot")
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)
=#

display(dag)

bs = basis_set(dag)
bs |> display

show(ConditionalIndependency(bs[1]))
@show d_separation(dag, :k1, :y, cset=[:w, :k2, :x2, :x3])
@show d_separation(dag, :k1, :y, cset=[:k2, :x1, :x3])
@show d_separation(dag, :k1, :y, cset=[:k2, :x2, :x3])
@show d_separation(dag, :k1, :y, cset=[:k2, :x2, :x3])
@show d_separation(dag, :k1, :y, cset=[:k2, :v, :x3])
@show d_separation(dag, :k1, :y, cset=[:k2, :x1, :x3])
@show d_separation(dag, :k1, :y, cset=[:k2, :x1])
@show d_separation(dag, :k1, :y, cset=[:k2, :x3])
@show d_separation(dag, :k1, :y, cset=[:k2, :w])
@show d_separation(dag, :k1, :y, cset=[:w])
@show d_separation(dag, :k1, :y, cset=[:k2])
println()

#=
@show d_separation(dag, :k1, :v, cset=[:x1])
@show d_separation(dag, :k1, :v, cset=[:w])
@show d_separation(dag, :k1, :x1, cset=[:w])
@show d_separation(dag, :k1, :x2, cset=[:v])
@show d_separation(dag, :k1, :x2, cset=[:x1])
@show d_separation(dag, :k1, :x2, cset=[:w])
@show d_separation(dag, :k1, :x3, cset=[:w])

bs[9] |> display
@show d_separation(dag, :x1, :x2; cset=[:v])
println()

bs[10] |> display
@show d_separation(dag, :x1, :x3)
println()

bs[11] |> display
@show d_separation(dag, :x1, :k1; cset=[:w])
println()

bs[12] |> display
@show d_separation(dag, :x1, :k2; cset=[:k2])
println()

bs[13] |> display
@show d_separation(dag, :x1, :y; cset=[:x2, :x3, :k2])
println()

@show d_separation(dag, :k2, :v, cset=[:x1])
@show d_separation(dag, :k2, :v, cset=[:w])
@show d_separation(dag, :k2, :v, cset=[:k1])
@show d_separation(dag, :k2, :w, cset=[:k1])
@show d_separation(dag, :k2, :x1, cset=[:w])
@show d_separation(dag, :k2, :x1, cset=[:k1])
@show d_separation(dag, :k2, :x2, cset=[:v])
@show d_separation(dag, :k2, :x2, cset=[:x1])
@show d_separation(dag, :k2, :x2, cset=[:w])
@show d_separation(dag, :k2, :x2, cset=[:k1])
@show d_separation(dag, :k2, :x3, cset=[:w])
@show d_separation(dag, :k2, :x3, cset=[:k1])
println()

@show d_separation(dag, :v, :w; cset=:x1)
@show d_separation(dag, :v, :x3)
@show d_separation(dag, :v, :y; cset=[:k2, :x2, :x3])
@show d_separation(dag, :v, :y; cset=[:k1, :x2, :x3])
@show d_separation(dag, :v, :y; cset=[:w, :x2, :x3])
@show d_separation(dag, :v, :y; cset=[:x2, :x1])
println()

@show d_separation(dag, :w, :x2; cset=[:v])
@show d_separation(dag, :w, :x2; cset=[:x1])
@show d_separation(dag, :w, :y; cset=[:k2, :x2, :x3])
@show d_separation(dag, :w, :y; cset=[:k2, :v, :x3])
@show d_separation(dag, :w, :y; cset=[:k1, :x2, :x3])
@show d_separation(dag, :w, :y; cset=[:k1, :v, :x3])
@show d_separation(dag, :w, :y; cset=[:k1, :x1, :x3])
@show d_separation(dag, :w, :y; cset=[:k2, :x2, :x3])
println()

@show d_separation(dag, :x1, :y; cset=[:k1, :v, :x3])
@show d_separation(dag, :x1, :y; cset=[:v, :w, :x3])
println()

@show d_separation(dag, :x2, :x3)
println()
=#
