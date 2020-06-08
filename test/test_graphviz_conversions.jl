

using StructuralCausalModels

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

d = from_ggm("DAG(
    mechanics ~ vectors+algebra, 
    vectors ~ algebra, 
    statistics ~ algebra+analysis, 
    analysis ~ algebra)"
);

dag = DAG("marks", d, df);
show(dag)

fn = "marks.dot"
to_graphviz(dag, fn)
Sys.isapple() && run(`open -a GraphViz.app $(fn)`)

#=
digraph marks {
  "algebra" [shape="box", fillcolor="gray85", style="filled"];
  "algebra" -> "analysis";
  "algebra" -> "mechanics";
  "algebra" -> "statistics";
  "algebra" -> "vectors";
  "analysis" -> "statistics";
  "vectors" -> "mechanics";
}
=#
