using StructuralCausalModels
#using StatsPlots
#gr(size=(700,800))

ProjDir = @__DIR__
cd(ProjDir) #do

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

#StatsPlots.cornerplot(Array(df), label=names(df))
#savefig("$(ProjDir)/SR6.4.2.png")

d = OrderedDict(
  :u => :a,
  :c => :a,
  :b => [:u, :c],
  :y => :c,
  :x => :u
);
u = [:u]

dag = DAG("sr6_4_2", d, df=df);
show(dag)

fname = joinpath(mktempdir(), "sr6.4.2.dot")
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)
to_dagitty(dag) |> display
to_ggm(dag) |> display

display(dag.s); println()

bs = basis_set(dag)
display(bs); println()

t = shipley_test(dag)
display(t); println()

f = [:a]; s = [:b]; conditioning_set = [:u, :c]

e = d_separation(dag, f, s, c=conditioning_set)
println("d_separation($(dag.name), $f, $s, c=$conditioning_set) = $e")
println()

adjustmentsets = adjustment_sets(dag, :x, :y)
println("\nAdjustment sets:")
adjustmentsets |> display

#=
adjustmentsets = adjustment_sets(dag, :x, :y, u)
println("\nAdjustment sets:")
adjustmentsets |> display
=#

#end
