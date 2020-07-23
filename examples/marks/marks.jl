# This example came from the R package ggm.

using StructuralCausalModels

ProjDir = @__DIR__
cd(ProjDir)

df = DataFrame!(CSV.File(scm_path("..", "data", "marks.csv")));

#=
R_dag = "
           mechanics vectors algebra statistics analysis
mechanics          0       0       0          0        0
vectors            1       0       0          0        0
algebra            1       1       0          1        1
statistics         0       0       0          0        0
analysis           0       0       0          1        0
";
=#

d = from_ggm("DAG(
    mechanics ~ vectors+algebra, 
    vectors ~ algebra, 
    statistics ~ algebra+analysis, 
    analysis ~ algebra)"
)
display(d)

# DAG accepts either an OrderedDict, an adjacency_matrix or a ggm/dagitty string.
# Below d_string holds a ggm DAG definition.

d_string = "DAG(
    mechanics ~ vectors+algebra, 
    vectors ~ algebra, 
    statistics ~ algebra+analysis, 
    analysis ~ algebra)"

dag = DAG("marks", d_string, df=df);
show(dag)

fname = ProjDir * "/marks.dot"
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

display(dag.s); println()

bs = basis_set(dag)
display(bs); println()

t = shipley_test(dag)
display("shipley_test = $t"); println()

pt = pcor_test(dag, [:analysis, :statistics, :mechanics], 1, 88)
display("pcor_test = $pt"); println()

f = [:statistics]; s = [:mechanics]; sel = vcat(f, s)

e = d_separation(dag, f, s; c=:algebra)
println("d_separation($(dag.name), $f, $s; c=:algebra) = $e")

e = d_separation(dag, f, s)
println("d_separation($(dag.name), $f, $s) = $e")

print("d_separation($(dag.name), [:statistics], [:mechanics]; c=[:vectors]) = ")
println(d_separation(dag, [:statistics], [:mechanics]; c=[:vectors]))

print("d_separation($(dag.name), [:statistics], [:mechanics]; c=[:analysis, :vectors]) = ")
println(d_separation(dag, [:statistics], [:mechanics]; c=[:analysis, :vectors]))

print("d_separation($(dag.name), [:statistics, :analysis], [:mechanics]; c=[:algebra]) = ")
println(d_separation(dag, [:statistics, :analysis], [:mechanics]; c=[:algebra]))

print("d_separation($(dag.name), [:statistics], [:mechanics, :vectors]; c=[:algebra]) = ")
println(d_separation(dag, [:statistics], [:mechanics, :vectors]; c=[:algebra]))

print("d_separation($(dag.name), [:statistics], [:mechanics, :analysis]; c=[:algebra]) = ")
println(d_separation(dag, [:statistics], [:mechanics, :analysis]; c=[:algebra]))

print("d_separation($(dag.name), [:analysis], [:vectors]) = ")
println(d_separation(dag, [:analysis], [:vectors]))

print("d_separation($(dag.name), [:analysis], [:vectors]; c=[:algebra]) = ")
println(d_separation(dag, [:analysis], [:vectors]; c=[:algebra]))

print("d_separation($(dag.name), [:vectors], [:statistics]; c=[:algebra]) = ")
println(d_separation(dag, [:analysis], [:vectors]; c=[:algebra]))

print("d_separation($(dag.name), [:statistics], [:algebra]; c=[:analysis]) = ")
println(d_separation(dag, [:statistics], [:algebra]; c=[:analysis]))

print("d_separation($(dag.name), [:statistics, :analysis], [:mechanics, :vectors]) = ")
println(d_separation(dag, [:statistics, :analysis], [:mechanics, :vectors]))

print("d_separation($(dag.name), [:statistics, :analysis], [:mechanics, :vectors]; c=[:algebra]) = ")
println(d_separation(dag, [:statistics, :analysis], [:mechanics, :vectors]; c=[:algebra]))

adjustmentsets = adjustment_sets(dag, :statistics, :mechanics)
println("\nAdjustment sets:")
adjustmentsets |> display

adjustmentsets = adjustment_sets(dag, :mechanics, :statistics)
println("\nAdjustment sets:")
adjustmentsets |> display

#end
