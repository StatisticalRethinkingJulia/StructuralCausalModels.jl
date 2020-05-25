using StructuralCausalModels, StatsPlots
gr(size=(700,800))

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

StatsPlots.cornerplot(Array(df), label=names(df))
savefig("$(ProjDir)/SR6.4.2.png")

#=
R_dag = "
    :x :y :a :c :b :u
:x   0  0  0  0  0  1
:y   1  0  0  1  0  0   
:a   0  0  0  0  0  0   
:c   0  0  1  0  0  0  
:b   0  0  0  1  0  1
:u   0  0  1  0  0  0   
";
=#

d = OrderedDict(
  :x => [:y],
  :a => [:c, :u],
  :c => [:y, :b],
  :u => [:x, :b]
);
u = [:u]

dag6_4_2 = DAG("sr6.4.2", d, df);
show(dag6_4_2)

fname = ProjDir * "/sr6.4.2.dot"
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

display(dag6_4_2.s); println()

# basis_set() not exported

bs = StructuralCausalModels.basis_set(dag6_4_2)
display(bs); println()

t = shipley_test(dag6_4_2)
display(t); println()

f = [:a]; s = [:b]; cond = [:u, :c]

e = d_separation(dag6_4_2, f, s, cond)
println("d_separation($(dag6_4_2.name), $f, $s, $cond) = $e")

adjustmentsets = adjustment_sets(dag6_4_2, :x, :y, u)
println("\nAdjustment sets:")
adjustmentsets |> display

#end
