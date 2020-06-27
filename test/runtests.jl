using StructuralCausalModels
using Test

@testset "DAGs" begin

  include("test_dag_formulations.jl")
  include("test_set_dag_df.jl")

end
  
@testset "DAG conversions" begin

  include("test_dagitty_conversion.jl")
  include("test_ggm_conversion.jl")
  include("test_graphviz_conversions.jl")

end
  
@testset "d_separation" begin
  
  include("test_d_separation.jl")
  @test to_ggm(dag) == "DAG(u ~ x + v, s1 ~ u, w ~ v + y, s2 ~ w)"
  @test d_separation(dag, :x, :v; debug=false) == true
  @test d_separation(dag, :x, [:v]; cset=:u, debug=false) == false
  @test d_separation(dag, [:x], :v; cset=:s1, debug=false) == false
  @test d_separation(dag, [:u], [:w]; debug=false) == false
  @test d_separation(dag, :u, [:w]; cset=:v, debug=false) == true
  @test d_separation(dag, [:x], :y; debug=false) == true
  @test d_separation(dag, :x, :y; cset=[:u, :w], debug=false) == false
  @test d_separation(dag, :x, :y; cset=[:s1, :s2], debug=false) == false
  @test d_separation(dag, :x, :y; cset=[:u, :v, :w], debug=false) == true
  @test d_separation(dag, :x, [:v]; cset=:u, debug=false) == false
  @test d_separation(dag, [:x], :v; cset=:s1, debug=false) == false

end

@testset "Methods" begin

  include("test_sr6_4_3b.jl")
  include("test_ag.jl")

end

@testset "Adjustment_sets" begin

  include("test_open_paths_01.jl")
  include("test_open_paths_02.jl")
  include("test_open_paths_03.jl")
  include("test_open_paths_04.jl")

end 

@testset "SR6.4.2" begin

  include("test_sr6_4_2.jl")

end

@testset "SR6.4.3" begin

  include("test_sr6_4_3.jl")
  
end 
