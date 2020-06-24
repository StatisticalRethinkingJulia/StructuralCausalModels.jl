using StructuralCausalModels, Test

ProjDir = @__DIR__

include(scm_path("..", "test", "test_methods", "test_ag.jl"))

amat_data = transpose(reshape([
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
  0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
  0,0,0,0,1,0,1,0,1,1,0,0,0,0,0,0,
  1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
  0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0
], (16,16)));


vars = [Symbol("n$i") for i in 1:size(amat_data, 1)]
a = NamedArray(Int.(amat_data), (vars, vars), ("Rows", "Cols"));

dag = DAG("ag_example", a)

fn = joinpath(mktempdir(), "ag.dot")
to_graphviz(dag, fn)
Sys.isapple() && run(`open -a GraphViz.app $(fn)`)

m = [:n3, :n5, :n6, :n15, :n16];
c = [:n4, :n7];

fr = test_ag(a; m=m, c=c)

fr1 = ancestral_graph(a; m=m, c=c)
@test all(fr .== fr1)

fr2 = test_ag(dag.a; m=m, c=c)
for i in names(fr, 1)
  for j in names(fr, 1)
    @test fr[i, j] == fr2[i, j]
  end
end

println()
display(fr)
println()
