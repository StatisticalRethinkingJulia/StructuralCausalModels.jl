using StructuralCausalModels
using Test

@testset "SR6.4.2" begin

  include("test_sr6_4_2.jl")
  @test allpaths[1] == [:x, :u, :b, :c, :y]
  @test length(allpaths) == 2
  @test backdoorpaths[1] == [:x, :u, :b, :c, :y]
  @test length(backdoorpaths) == 2
  @test openpaths[1] == [:x, :u, :a, :c, :y]
  @test length(openpaths) == 1
  @test show_dag_path(dag, openpaths[1]) == ":x ⇐ :u ⇐ :a ⇒ :c ⇒ :y"
  @test adjustmentsets == [[:a], [:c]]
  include("test_sr6_4_2a.jl")
  @test adjustmentsets == [[:a], [:c]]

end

@testset "SR6.4.3" begin

  include("test_sr6_4_3.jl")
  @test allpaths[1] == [:w, :s, :a, :d]
  @test length(allpaths) == 4
  @test backdoorpaths == Array{Symbol,1}[]
  @test length(backdoorpaths) == 0
  @test show_dag_path(dag, allpaths[1]) == ":w ⇒ :s ⇐ :a ⇐ :d"
  @test adjustmentsets == Array{Symbol,1}[]

end 

#include("test_sr6_4_3a.jl")
include("test_sr6_4_3b.jl")
include("test_ag.jl")
include("test_agb.jl")
include("test_dag.jl")
include("test_dagitty_conversion.jl")
include("test_ggm_conversion.jl")
include("test_graphviz_conversions.jl")
