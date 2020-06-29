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

  include("test_descendents_02.jl")
  @test d_separation(dag, :k1, :v, cset=[:x1]) == true
  @test d_separation(dag, :k1, :v, cset=[:w]) == true
  @test d_separation(dag, :k1, :x1, cset=[:w]) == true
  @test d_separation(dag, :k1, :x2, cset=[:v]) == true
  @test d_separation(dag, :k1, :x2, cset=[:x1]) == true
  @test d_separation(dag, :k1, :x2, cset=[:w]) == true
  @test d_separation(dag, :k1, :x3, cset=[:w]) == true
  @test d_separation(dag, :k1, :y, cset=[:k2, :x2, :x3]) == true
  @test d_separation(dag, :k1, :y, cset=[:k2, :v, :x3]) == true
  @test d_separation(dag, :k1, :y, cset=[:k2, :x1, :x3]) == true
  @test d_separation(dag, :k1, :y, cset=[:k2, :w]) == true
  @test d_separation(dag, :k2, :v, cset=[:x1]) == true
  @test d_separation(dag, :k2, :v, cset=[:w]) == true
  @test d_separation(dag, :k2, :v, cset=[:k1]) == true
  @test d_separation(dag, :k2, :w, cset=[:k1]) == true
  @test d_separation(dag, :k2, :x1, cset=[:w]) == true
  @test d_separation(dag, :k2, :x1, cset=[:k1]) == true
  @test d_separation(dag, :k2, :x2, cset=[:v]) == true
  @test d_separation(dag, :k2, :x2, cset=[:x1]) == true
  @test d_separation(dag, :k2, :x2, cset=[:w]) == true
  @test d_separation(dag, :k2, :x2, cset=[:k1]) == true
  @test d_separation(dag, :k2, :x3, cset=[:w]) == true
  @test d_separation(dag, :k2, :x3, cset=[:k1]) == true
  @test d_separation(dag, :v, :w; cset=:x1) == true
  @test d_separation(dag, :v, :x3) == true
  @test d_separation(dag, :v, :y; cset=[:k2, :x2, :x3]) == true
  @test d_separation(dag, :v, :y; cset=[:k1, :x2, :x3]) == true
  @test d_separation(dag, :v, :y; cset=[:w, :x2, :x3]) == true
  @test d_separation(dag, :v, :y; cset=[:x2, :x1]) == true
  @test d_separation(dag, :w, :x2; cset=[:v]) == true
  @test d_separation(dag, :w, :x2; cset=[:x1]) == true
  @test d_separation(dag, :w, :y; cset=[:k2, :x2, :x3]) == true
  @test d_separation(dag, :w, :y; cset=[:k2, :v, :x3]) == true
  @test d_separation(dag, :w, :y; cset=[:k1, :x2, :x3]) == true
  @test d_separation(dag, :w, :y; cset=[:k1, :v, :x3]) == true
  @test d_separation(dag, :w, :y; cset=[:k1, :x1, :x3]) == true
  @test d_separation(dag, :w, :y; cset=[:k2, :x2, :x3]) == true
  @test d_separation(dag, :x1, :x2; cset=[:v]) == true
  @test d_separation(dag, :x1, :x3) == true
  @test d_separation(dag, :x1, :y; cset=[:k2, :x2, :x3]) == true
  @test d_separation(dag, :x1, :y; cset=[:k1, :x2, :x3]) == true
  @test d_separation(dag, :x1, :y; cset=[:w, :x2, :x3]) == true
  @test d_separation(dag, :x1, :y; cset=[:k2, :v, :x3]) == true
  @test d_separation(dag, :x1, :y; cset=[:k1, :v, :x3]) == true
  @test d_separation(dag, :x1, :y; cset=[:v, :w, :x3]) == true
  @test d_separation(dag, :x2, :x3) == true

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
