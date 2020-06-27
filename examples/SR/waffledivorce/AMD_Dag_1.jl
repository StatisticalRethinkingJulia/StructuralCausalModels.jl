# Load Julia packages (libraries) needed.

using StatisticalRethinking
using StructuralCausalModels

ProjDir = @__DIR__
cd(ProjDir) #do

# ### snippet 5.1

println()
df1 = CSV.read(rel_path("..", "data", "WaffleDivorce.csv"), delim=';');
first(df1, 5) |> display

df = DataFrame()
df[!, :A] = df1[:, :MedianAgeMarriage]
df[!, :M] = df1[:, :Marriage]
df[!, :D] = df1[:, :Divorce]

d = OrderedDict(
  :D => [:M, :A],
  :M => [:A]
);

dag = DAG("waffles", d, df=df);
show(dag)

fname = ProjDir * "/AMD_1.dot"
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

display(dag.s); println()

ord = StructuralCausalModels.topological_order(dag.a)
display(ord); println()
display(dag.vars[ord]); println()

shipley_test_dag_1 = shipley_test(dag)
if !isnothing(shipley_test_dag_1)
  display(shipley_test_dag_1)
end
println()

f = [:A]; s = [:D]; sel = vcat(f, s)
cset = [:M]

e = d_separation(dag, f, s)
println("d_separation($(dag.name), $f, $s) = $e")

e = d_separation(dag, f, s, cset=cset)
println("d_separation($(dag.name), $f, $s, cset=$cset) = $e\n")

bs = basis_set(dag)
display(bs)

adjustmentsets = adjustment_sets(dag, f[1], s[1])
println("Adjustment sets:")
adjustmentsets |> display

#end