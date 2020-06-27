using StructuralCausalModels, Test

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

d = OrderedDict(
  :y => [:x],
  [:c, :u] => :a,
  [:y, :b] => :c,
  [:x, :b] => :u
);
u = [:u]

dag = DAG("sr6_4_2a", d; df=df);
show(dag)

fn = joinpath(mktempdir(), "sr6_4_2a.dot")
to_graphviz(dag, fn)
Sys.isapple() && run(`open -a GraphViz.app $(fn)`)

adjustmentsets = adjustment_sets(dag, :x, :y)
println("Adjustment sets:")
adjustmentsets |> display
