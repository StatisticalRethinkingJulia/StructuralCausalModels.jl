using StructuralCausalModels, Test

ProjDir = @__DIR__

d_str = "DAG(u ~ x + w, x ~ v + z, y ~ v, s1 ~ u, w ~ y, k ~ y, s2 ~ w + s3 + k, s3 ~ s1)"

dag = DAG("test_desc", d_str)

to_dagitty(dag)

#=
fname = joinpath(ProjDir, "test_descendents_01.dot")
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)
=#

bs = basis_set(dag)
as = adjustment_sets(dag, :u, :s2)

@testset "Test_descendents_01" begin

  @test dag.a[:x, :u] == 0
  @test dag.e[:x, :u] == 1
  @test length(bs) == 83
  @test bs[70] == [:y, :s1, :u]
  @test bs[71] == [:y, :s1, :v, :u]
  @test bs[81] == [:z, :k, :y]
  @test bs[82] == [:z, :s2, :k, :w, :s3]
  @test bs[83] == [:z, :s3, :s1]

end

@testset "Adjustment_sets" begin

  @test as[1] == [:x, :w]
  @test as[2] == [:v, :w]
  @test as[3] == [:y, :w]
  @test as[4] == [:w, :k]

end