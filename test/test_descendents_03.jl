using StructuralCausalModels, Test

ProjDir = @__DIR__

d_str = "dag{k1 -> k2;k2 -> y;v -> x2;w -> k1;x1 -> v;x1 -> w;x2 -> y;x3 -> w;x3 -> y}"

dag = DAG("test_desc", d_str)

to_dagitty(dag)

fname = joinpath(ProjDir, "test_descendents_03.dot")
to_graphviz(dag, fname)
#Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

bs = basis_set(dag)
as = adjustment_sets(dag, :x2, :y)

@testset "Test_descendents_03" begin

  @test length(bs) == 48
  @test bs[44] == [:y, :k1, :w, :k2]
  @test bs[45] == [:y, :k1, :w, :x2, :k2]
  @test bs[46] == [:y, :k1, :w, :x3, :k2]
  @test bs[47] == [:y, :k1, :x2, :x3, :k2]
  @test bs[48] == [:y, :k1, :w, :x2, :x3, :k2]

end

@testset "Adjustment_sets" begin

  @test length(as) == 5
  @test as[1] == [:v]
  @test as[2] == [:x1]
  @test as[3] == [:k1]
  @test as[4] == [:k2]
  @test as[5] == [:w, :x3]

end
