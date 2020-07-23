# Load Julia packages (libraries) needed.

using StatisticalRethinking
using StructuralCausalModels

ProjDir = @__DIR__
cd(ProjDir) #do

# ### snippet 5.1

df1 = DataFrame!(CSV.File(rel_path("..", "data", "WaffleDivorce.csv"), delim=';'));

df = DataFrame()
df[!, :A] = df1[:, :MedianAgeMarriage]
df[!, :M] = df1[:, :Marriage]
df[!, :D] = df1[:, :Divorce]

d = OrderedDict(
  :M => [:A],
  :D => [:M]
);

dag = DAG("waffles", d, df=df);
show(dag)

fname = ProjDir * "/AMD_2.dot"
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

display(dag.s); println()

shipley_test_dag_2 = shipley_test(dag)
if !isnothing(shipley_test_dag_2)
  display(shipley_test_dag_2)
end
println()

f = [:A]; s = [:D]; sel = vcat(f, s)
c = [:M]

e = d_separation(dag, f, s)
println("d_separation($(dag.name), $f, $s) = $e")

e = d_separation(dag, f, s, c=c)
println("d_separation($(dag.name), $f, $s, c=$c) = $e\n")

bs = basis_set(dag)
display(bs)

adjustmentsets = adjustment_sets(dag, f[1], s[1])
println("Adjustment sets:")
adjustmentsets |> display

#end