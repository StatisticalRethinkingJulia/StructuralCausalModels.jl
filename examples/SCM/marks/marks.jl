using StructuralCausalModels, RData

ProjDir = @__DIR__
cd(ProjDir) #do

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

d = OrderedDict(
  :mechanics => [:vectors, :algebra],
  :vectors => [:algebra],
  :analysis => [:algebra],
  :statistics => [:algebra, :analysis]
);

dag = DAG("marks", d, df);
show(dag)

fname = ProjDir * "/marks.dot"
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

display(dag.s); println()

#=
ord = topological_order(dag)
display(ord); println()
display(dag.vars[ord]); println()
=#

# basis_set() not exported

bs = StructuralCausalModels.basis_set(dag)
display(bs); println()

t = shipley_test(dag)
display(t); println()

f = [:statistics]; s = [:mechanics]; sel = vcat(f, s)
cond = [:algebra]

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

#end
