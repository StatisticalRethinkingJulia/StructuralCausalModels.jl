using StructuralCausalModels, Test

ProjDir = @__DIR__
cd(ProjDir) #do

d_string = "dag {I -> Y; X -> I; X -> Y; Z -> I; Z -> X}"
dag = DAG("test_open_paths_03", d_string);

bs = basis_set(dag)


fname = joinpath(ProjDir, "test_open_paths_03.dot")
to_graphviz(dag, fname)
#Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

ap  = all_paths(dag, :X, :Y)
bp = backdoor_paths(dag, ap, :X)
adjs = adjustment_sets(dag, :X, :Y)

@testset "Open_path_03" begin

  @test bs[1][1] == :Z
  @test bs[1][2] == :Y
  @test bs[1][3:end] == [:X, :I]
  @test bp[1] == [:X, :Z, :I, :Y]
  @test adjs == [[:Z], [:I]]

end
