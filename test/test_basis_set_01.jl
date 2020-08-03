using StructuralCausalModels, Test

ProjDir = @__DIR__

d_str = "dag{k1 -> k2;k2 -> y;v -> x2;w -> k1;x1 -> v;x1 -> w;x2 -> y;x3 -> w;x3 -> y}"

dag = DAG("test_desc", d_str)

to_dagitty(dag)

#=
fname = joinpath(ProjDir, "test_descendents_03.dot")
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)
=#

bs = basis_set(dag)


@testset "basis_set_01" begin
  
  @test length(bs) == 48
  @test bs[18] == [:x2, :w, :v]
  @test bs[19] == [:x2, :w, :x1]
  @test bs[20] == [:x2, :w, :v, :x1]
  @test bs[44] == [:y, :k1, :w, :k2]
  @test bs[45] == [:y, :k1, :w, :x2, :k2]
  @test bs[46] == [:y, :k1, :w, :x3, :k2]
  @test bs[47] == [:y, :k1, :x2, :x3, :k2]
  @test bs[48] == [:y, :k1, :w, :x2, :x3, :k2]

end