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

@testset "m_separation_02" begin
  
  @test to_ggm(dag) == "DAG(u ~ x + v, s1 ~ u, w ~ v + y, s2 ~ w)"
  @test m_separation(dag, :x, :v; debug=false) == true
  @test m_separation(dag, :x, [:v]; c=:u, debug=false) == false
  @test m_separation(dag, [:x], :v; c=:s1, debug=false) == false
  @test m_separation(dag, [:u], [:w]; debug=false) == false
  @test m_separation(dag, :u, [:w]; c=:v, debug=false) == true

  @test m_separation(dag, [:x], :y) == true
  @test m_separation(dag, :x, :y; c=[:u, :w], debug=false) == false
  @test m_separation(dag, :x, :y; c=[:s1, :s2], debug=false) == false
  @test m_separation(dag, :x, :y; c=[:u, :v, :w], debug=false) == true
  @test m_separation(dag, :x, [:v]; c=:u, debug=false) == false
  @test m_separation(dag, [:x], :v; c=:s1, debug=false) == false

end
