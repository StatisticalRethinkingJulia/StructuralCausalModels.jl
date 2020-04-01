using StructuralCausalModels, RData

ProjDir = @__DIR__
cd(ProjDir) #do

idf = CSV.read(scm_path("..", "data", "marks.csv"));

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
  :statistics => [:algebra, :analysis],
  :analysis => [:algebra],
);

dag = DAG(d, idf)
println()

fname = "algebra.dot"
run(`open -a GraphViz.app $(fname)`)

display(dag.s)
println()
ord = dag.order
display(ord)
println()
display(dag.vars[ord])
println()
bs = basis_set(dag)
display(bs)
println()
t = shipley_test(dag)
display(t)
println()

#end
