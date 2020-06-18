# This example came from the R package ggm.

using StructuralCausalModels

ProjDir = @__DIR__
cd(ProjDir)

df = CSV.read(scm_path("..", "data", "marks.csv"));

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

dag = DAG("marks", d_string, df);
show(dag)

fname = ProjDir * "/marks.dot"
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

display(dag.s); println()

#=
ord = topological_order(dag)
display(ord); println()
display(dag.vars[ord]); println()
=#

bs = basis_set(dag)
display(bs); println()

t = shipley_test(dag)
display(t); println()

f = [:statistics]; s = [:mechanics]; sel = vcat(f, s)
cond = :algebra

e = d_separation(dag, f, s, cond)
println("d_separation($(dag.name), $f, $s, $cond) = $e")

e = d_separation(dag, f, s)
println("d_separation($(dag.name), $f, $s) = $e")

print("d_separation($(dag.name), [:statistics], [:mechanics], [:vectors]) = ")
println(d_separation(dag, [:statistics], [:mechanics], [:vectors]))

print("d_separation($(dag.name), [:statistics], [:mechanics], [:analysis, :vectors]) = ")
println(d_separation(dag, [:statistics], [:mechanics], [:analysis, :vectors]))

print("d_separation($(dag.name), [:statistics, :analysis], [:mechanics], [:algebra]) = ")
println(d_separation(dag, [:statistics, :analysis], [:mechanics], [:algebra]))

print("d_separation($(dag.name), [:statistics], [:mechanics, :vectors], [:algebra]) = ")
println(d_separation(dag, [:statistics], [:mechanics, :vectors], [:algebra]))

print("d_separation($(dag.name), [:statistics], [:mechanics, :analysis], [:algebra]) = ")
println(d_separation(dag, [:statistics], [:mechanics, :analysis], [:algebra]))

print("d_separation($(dag.name), [:analysis], [:vectors]) = ")
println(d_separation(dag, [:analysis], [:vectors]))

print("d_separation($(dag.name), [:analysis], [:vectors], [:algebra]) = ")
println(d_separation(dag, [:analysis], [:vectors], [:algebra]))

print("d_separation($(dag.name), [:vectors], [:statistics], [:algebra]) = ")
println(d_separation(dag, [:analysis], [:vectors], [:algebra]))

print("d_separation($(dag.name), [:statistics], [:algebra], [:analysis]) = ")
println(d_separation(dag, [:statistics], [:algebra], [:analysis]))

print("d_separation($(dag.name), [:statistics, :analysis], [:mechanics, :vectors]) = ")
println(d_separation(dag, [:statistics, :analysis], [:mechanics, :vectors]))

print("d_separation($(dag.name), [:statistics, :analysis], [:mechanics, :vectors], [:algebra]) = ")
println(d_separation(dag, [:statistics, :analysis], [:mechanics, :vectors], [:algebra]))

adjustmentsets = adjustment_sets(dag, :statistics, :mechanics)
println("\nAdjustment sets:")
adjustmentsets |> display

adjustmentsets = adjustment_sets(dag, :mechanics, :statistics)
println("\nAdjustment sets:")
adjustmentsets |> display

#end
