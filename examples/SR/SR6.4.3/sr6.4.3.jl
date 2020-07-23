using StructuralCausalModels
#using StatsPlots
#gr(size=(700,600))

ProjDir = @__DIR__
cd(ProjDir) #do

df = DataFrame!(CSV.File(scm_path("..", "data", "WaffleDivorce.csv"), delim=';'));
df = DataFrame(
  :s => df[:, :South],
  :a => df[:, :MedianAgeMarriage],
  :m => df[:, :Marriage],
  :w => df[:, :WaffleHouses],
  :d => df[:, :Divorce]
)
first(df, 5) |> display

if isdefined(Main, :StatsPlots)
  StatsPlots.cornerplot(Array(df), label=names(df))
  savefig("$(ProjDir)/SR6.4.3.png")
end

d = OrderedDict(
  [:w, :m, :a] => :s,
  :d => [:a, :w, :m],
  :m => [:a]
);

dag = DAG("sr6_4_3", d, df=df);
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

f = :w; s = :d;

e = d_separation(dag, [f], [s], c=[:m, :a])
println("d_separation($(dag.name), $f, $s, c=[:m, :a]) = $e\n")

#ap = all_paths(dag, f, s)
ap = all_paths(dag, f, s)
ap |> display
println()

bp = backdoor_paths(dag, ap, f)
bp |> display
println()

adjustmentsets = adjustment_sets(dag, :w, :d)
println("Adjustment sets:")
adjustmentsets |> display

#end
