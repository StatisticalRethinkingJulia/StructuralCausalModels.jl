using StructuralCausalModels, RData

objs = load("/Users/rob/Projects/R/ggm/data/anger.rda");
df = objs["anger"]

cov_m = cov(Array(df))
display(cov_m)

isposdef(cov_m) |> display

R_dag = "
           mechanics vectors algebra statistics analysis
mechanics          0       0       0          0        0
vectors            1       0       0          0        0
algebra            1       1       0          1        1
statistics         0       0       0          0        0
analysis           0       0       0          1        0
";

d = OrderedDict(
  :mechanics => [:vectors, :algebra],
  :vectors => [:algebra],
  :statistics => [:algebra, :analysis],
  :analysis => [:algebra]
);

dag = DAG(d; df=df)
dag |> display
println()

ord = dag.order
display(ord)
println()
display(dag.cov_matrix)
println()
display(dag.vars[ord])
println()
bs = basis_set(dag)
display(bs)
println()
