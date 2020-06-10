using StructuralCausalModels, StatsPlots
gr(size=(700,600))

ProjDir = @__DIR__
cd(ProjDir) #do

df = CSV.read(scm_path("..", "data", "WaffleDivorce.csv"), delim=';');
df = DataFrame(
  :s => df[:, :South],
  :a => df[:, :MedianAgeMarriage],
  :m => df[:, :Marriage],
  :w => df[:, :WaffleHouses],
  :d => df[:, :Divorce]
)
first(df, 5) |> display

StatsPlots.cornerplot(Array(df), label=names(df))
savefig("$(ProjDir)/SR6.4.3.png")

d = OrderedDict(
  :w => :s,
  :d => [:a, :w, :m],
  :m => [:a, :s],
  :a => :s
);

dag = DAG("sr6_4_3", d, df);
show(dag)

fname = ProjDir * "/sr6.4.3.dot"
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

display(dag.s); println()

println("Basis set:")
bs = basis_set(dag)
display(bs); println()

t = shipley_test(dag)
display(t); println()

f = [:w]; s = [:d];
cond = [:m, :a]

e = d_separation(dag, f, s, cond)
println("d_separation($(dag.name), $f, $s, $cond) = $e\n")

adjustmentsets = adjustment_sets(dag, :w, :d)
println("Adjustment sets:")
adjustmentsets |> display

#end
