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

fname = ProjDir * "/sr6.4.3.dot"
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

d = OrderedDict(
  :s => [:a, :m, :w],
  :a => [:m, :d],
  :m => [:d],
  :w => [:d]
);

dag = DAG("sr6.4.3", d, df);
show(dag)

display(dag.s); println()

bs = StructuralCausalModels.basis_set(dag)
display(bs); println()

t = shipley_test(dag)
display(t); println()

f = [:w]; s = [:d]; sel = vcat(f, s)
cond = [:m, :a]

e = d_separation(dag, f, s, cond)
println("d_separation($(dag.name), $f, $s, $cond) = $e")

adjustmentsets = adjustment_sets(dag, :w, :d)
println("Adjustment sets for: $(openpaths)")
adjustmentsets |> display

#end
