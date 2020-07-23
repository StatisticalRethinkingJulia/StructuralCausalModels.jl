#using Debugger
using StructuralCausalModels, Test

ProjDir = @__DIR__
cd(ProjDir) #do

fname = scm_path("..", "examples", "Shipley", "fig2.6", "fig2.6.dot")
#Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

# Read `=>` as `~` in regression models, or `<-` in causal models.
d = OrderedDict(
  :u => [:x, :v],
  :s1 => [:u],
  :w => [:v, :y],
  :s2 => [:w]
);

dag = DAG("fig2.6", d);

@testset "d_separation" begin
  
  @test to_ggm(dag) == "DAG(u ~ x + v, s1 ~ u, w ~ v + y, s2 ~ w)"
  @test d_separation(dag, :x, :v; debug=false) == true
  @test d_separation(dag, :x, [:v]; c=:u, debug=false) == false
  @test d_separation(dag, [:x], :v; c=:s1, debug=false) == false
  @test d_separation(dag, [:u], [:w]; debug=false) == false
  @test d_separation(dag, :u, [:w]; c=:v, debug=false) == true
  @test d_separation(dag, [:x], :y; debug=false) == true
  @test d_separation(dag, :x, :y; c=[:u, :w], debug=false) == false
  @test d_separation(dag, :x, :y; c=[:s1, :s2], debug=false) == false
  @test d_separation(dag, :x, :y; c=[:u, :v, :w], debug=false) == true
  @test d_separation(dag, :x, [:v]; c=:u, debug=false) == false
  @test d_separation(dag, [:x], :v; c=:s1, debug=false) == false

  include("test_descendents_02.jl")
  @test d_separation(dag, :k1, :v, c=[:x1]) == true
  @test d_separation(dag, :k1, :v, c=[:w]) == true
  @test d_separation(dag, :k1, :x1, c=[:w]) == true
  @test d_separation(dag, :k1, :x2, c=[:v]) == true
  @test d_separation(dag, :k1, :x2, c=[:x1]) == true
  @test d_separation(dag, :k1, :x2, c=[:w]) == true
  @test d_separation(dag, :k1, :x3, c=[:w]) == true
  @test d_separation(dag, :k1, :y, c=[:k2, :x2, :x3]) == true
  @test d_separation(dag, :k1, :y, c=[:k2, :v, :x3]) == true
  @test d_separation(dag, :k1, :y, c=[:k2, :x1, :x3]) == true
  @test d_separation(dag, :k1, :y, c=[:k2, :w]) == true
  @test d_separation(dag, :k2, :v, c=[:x1]) == true
  @test d_separation(dag, :k2, :v, c=[:w]) == true
  @test d_separation(dag, :k2, :v, c=[:k1]) == true
  @test d_separation(dag, :k2, :w, c=[:k1]) == true
  @test d_separation(dag, :k2, :x1, c=[:w]) == true
  @test d_separation(dag, :k2, :x1, c=[:k1]) == true
  @test d_separation(dag, :k2, :x2, c=[:v]) == true
  @test d_separation(dag, :k2, :x2, c=[:x1]) == true
  @test d_separation(dag, :k2, :x2, c=[:w]) == true
  @test d_separation(dag, :k2, :x2, c=[:k1]) == true
  @test d_separation(dag, :k2, :x3, c=[:w]) == true
  @test d_separation(dag, :k2, :x3, c=[:k1]) == true
  @test d_separation(dag, :v, :w; c=:x1) == true
  @test d_separation(dag, :v, :x3) == true
  @test d_separation(dag, :v, :y; c=[:k2, :x2, :x3]) == true
  @test d_separation(dag, :v, :y; c=[:k1, :x2, :x3]) == true
  @test d_separation(dag, :v, :y; c=[:w, :x2, :x3]) == true
  @test d_separation(dag, :v, :y; c=[:x2, :x1]) == true
  @test d_separation(dag, :w, :x2; c=[:v]) == true
  @test d_separation(dag, :w, :x2; c=[:x1]) == true
  @test d_separation(dag, :w, :y; c=[:k2, :x2, :x3]) == true
  @test d_separation(dag, :w, :y; c=[:k2, :v, :x3]) == true
  @test d_separation(dag, :w, :y; c=[:k1, :x2, :x3]) == true
  @test d_separation(dag, :w, :y; c=[:k1, :v, :x3]) == true
  @test d_separation(dag, :w, :y; c=[:k1, :x1, :x3]) == true
  @test d_separation(dag, :w, :y; c=[:k2, :x2, :x3]) == true
  @test d_separation(dag, :x1, :x2; c=[:v]) == true
  @test d_separation(dag, :x1, :x3) == true
  @test d_separation(dag, :x1, :y; c=[:k2, :x2, :x3]) == true
  @test d_separation(dag, :x1, :y; c=[:k1, :x2, :x3]) == true
  @test d_separation(dag, :x1, :y; c=[:w, :x2, :x3]) == true
  @test d_separation(dag, :x1, :y; c=[:k2, :v, :x3]) == true
  @test d_separation(dag, :x1, :y; c=[:k1, :v, :x3]) == true
  @test d_separation(dag, :x1, :y; c=[:v, :w, :x3]) == true
  @test d_separation(dag, :x2, :x3) == true

end
