using StructuralCausalModels
using Test

@testset "DAGs" begin

  include("test_dag_formulations.jl")
  include("test_set_dag_df.jl")

end
  
@testset "DAG conversionss" begin

  include("test_dagitty_conversion.jl")
  @test d1 == OrderedDict(
    :S2 => :W,
    :W  => [:Y, :V],
    :S1 => :U,
    :U  => [:X, :V]
  )
  @test d2 == OrderedDict(
    :S2 => :W,
    :W  => [:Y, :V],
    :U  => [:X, :V],
    :S1 => :U
  )
  @test d3 == OrderedDict(
    :S2 => :W,
    :W  => [:V, :Y],
    :U  => [:V, :X],
    :S1 => :U
  )
  @test g1 == "dag { S2 <- W; W <- { Y V }; S1 <- U; U <- { X V } }"
  @test g2 == "dag { S2 <- W; W <- { Y V }; U <- { X V }; S1 <- U }"
  @test g3 == "dag { S2 <- W; W <- { V Y }; U <- { V X }; S1 <- U }"
  @test g4 == "dag { u <- v; w <- v; u <- x; s1 <- u; w <- y; s2 <- w }"
  @test dag.d == OrderedDict(
    :S2 => :W,
    :W  => [:Y, :V],
    :S1 => :U,
    :U  => [:X, :V]
  )

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
  println("\n")

end

@testset "Adjustment_sets" begin

  include("test_open_paths_01.jl")
  include("test_open_paths_02.jl")
  include("test_open_paths_03.jl")
  include("test_open_paths_04.jl")
  #=
  @test allpaths[1] == [:w, :s, :a, :d]
  @test length(allpaths) == 4
  @test backdoorpaths == Array{Symbol,1}[]
  @test length(backdoorpaths) == 0
  @test show_dag_path(dag, allpaths[1]) == ":w ⇒ :s ⇐ :a ⇐ :d"
  @test adjustmentsets == Array{Symbol,1}[]
  =#
  println("\n")

end 
@testset "SR6.4.2" begin

  include("test_sr6_4_2.jl")
  #=
  @test allpaths[1] == [:x, :u, :b, :c, :y]
  @test length(allpaths) == 2
  @test backdoorpaths[1] == [:x, :u, :b, :c, :y]
  @test length(backdoorpaths) == 2
  @test show_dag_path(dag, backdoorpaths[1]) == ":x ⇐ :u ⇐ :a ⇒ :c ⇒ :y"
  @test adjustmentsets == [[:a], [:c]]
  include("test_sr6_4_2a.jl")
  @test adjustmentsets == [[:a], [:c]]
  =#
  println("\n")

end

@testset "SR6.4.3" begin

  include("test_sr6_4_3.jl")
  #=
  @test allpaths[1] == [:w, :s, :a, :d]
  @test length(allpaths) == 4
  @test backdoorpaths == Array{Symbol,1}[]
  @test length(backdoorpaths) == 0
  @test show_dag_path(dag, allpaths[1]) == ":w ⇒ :s ⇐ :a ⇐ :d"
  @test adjustmentsets == Array{Symbol,1}[]
  =#
  println("\n")
  
end 

