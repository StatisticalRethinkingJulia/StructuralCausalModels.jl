

using StructuralCausalModels, Test

ProjDir = @__DIR__
cd(ProjDir) #do

df = CSV.read(scm_path("..", "data", "marks.csv"), DataFrame);

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

dag = DAG("marks", d, df=df);

fn = joinpath(mktempdir(), "marks.dot")
to_graphviz(dag, fn)
#Sys.isapple() && run(`open -a GraphViz.app $(fn)`)

@testset "GraphViz conversion" begin

  @test read(fn, String) == "digraph marks {\n  algebra -> vectors;\n  algebra -> mechanics;\n  algebra -> analysis;\n  algebra -> statistics;\n  vectors -> mechanics;\n  analysis -> statistics;\n}\n"

end
