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
  :M => [:A],
  :D => [:A]
);

dag = DAG("waffles", d, df);
show(dag)

fname = ProjDir * "/AMD_2.dot"
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

display(dag.s); println()

f = [:M]; s = [:D]; sel = vcat(f, s)
cond = [:A]

e = d_separation(dag, f, s)
println("d_separation($(dag.name), $f, $s) = $e")

e = d_separation(dag, f, s, cond)
println("d_separation($(dag.name), $f, $s, $cond) = $e")

#end