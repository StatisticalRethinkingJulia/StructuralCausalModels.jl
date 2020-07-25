using StructuralCausalModels, Test

ProjDir = @__DIR__
cd(ProjDir) #do

ProjDir = @__DIR__

d_str = "dag{k1 -> k2;k2 -> y;v -> x2;w -> k1;x1 -> v;x1 -> w;x2 -> y;x3 -> w;x3 -> y}"

dag = DAG("test_desc_02", d_str)

@testset "m_separation_03" begin
  
  @test m_separation(dag, :k1, :v, c=[:x1]) == true
  @test m_separation(dag, :k1, :v, c=[:w]) == true
  @test m_separation(dag, :k1, :x1, c=[:w]) == true
  @test m_separation(dag, :k1, :x2, c=[:v]) == true
  @test m_separation(dag, :k1, :x2, c=[:x1]) == true
  @test m_separation(dag, :k1, :x2, c=[:w]) == true
  @test m_separation(dag, :k1, :x3, c=[:w]) == true
  @test m_separation(dag, :k1, :y, c=[:k2, :x2, :x3]) == true
  @test m_separation(dag, :k1, :y, c=[:k2, :v, :x3]) == true
  @test m_separation(dag, :k1, :y, c=[:k2, :x1, :x3]) == true
  @test m_separation(dag, :k1, :y, c=[:k2, :w]) == true
  @test m_separation(dag, :k2, :v, c=[:x1]) == true
  @test m_separation(dag, :k2, :v, c=[:w]) == true
  @test m_separation(dag, :k2, :v, c=[:k1]) == true
  @test m_separation(dag, :k2, :w, c=[:k1]) == true
  @test m_separation(dag, :k2, :x1, c=[:w]) == true
  @test m_separation(dag, :k2, :x1, c=[:k1]) == true
  @test m_separation(dag, :k2, :x2, c=[:v]) == true
  @test m_separation(dag, :k2, :x2, c=[:x1]) == true
  @test m_separation(dag, :k2, :x2, c=[:w]) == true
  @test m_separation(dag, :k2, :x2, c=[:k1]) == true
  @test m_separation(dag, :k2, :x3, c=[:w]) == true
  @test m_separation(dag, :k2, :x3, c=[:k1]) == true
  @test m_separation(dag, :v, :w; c=:x1) == true
  @test m_separation(dag, :v, :x3) == true
  @test m_separation(dag, :v, :y; c=[:k2, :x2, :x3]) == true
  @test m_separation(dag, :v, :y; c=[:k1, :x2, :x3]) == true
  @test m_separation(dag, :v, :y; c=[:w, :x2, :x3]) == true
  @test m_separation(dag, :v, :y; c=[:x2, :x1]) == true
  @test m_separation(dag, :w, :x2; c=[:v]) == true
  @test m_separation(dag, :w, :x2; c=[:x1]) == true
  @test m_separation(dag, :w, :y; c=[:k2, :x2, :x3]) == true
  @test m_separation(dag, :w, :y; c=[:k2, :v, :x3]) == true
  @test m_separation(dag, :w, :y; c=[:k1, :x2, :x3]) == true
  @test m_separation(dag, :w, :y; c=[:k1, :v, :x3]) == true
  @test m_separation(dag, :w, :y; c=[:k1, :x1, :x3]) == true
  @test m_separation(dag, :w, :y; c=[:k2, :x2, :x3]) == true
  @test m_separation(dag, :x1, :x2; c=[:v]) == true
  @test m_separation(dag, :x1, :x3) == true
  @test m_separation(dag, :x1, :y; c=[:k2, :x2, :x3]) == true
  @test m_separation(dag, :x1, :y; c=[:k1, :x2, :x3]) == true
  @test m_separation(dag, :x1, :y; c=[:w, :x2, :x3]) == true
  @test m_separation(dag, :x1, :y; c=[:k2, :v, :x3]) == true
  @test m_separation(dag, :x1, :y; c=[:k1, :v, :x3]) == true
  @test m_separation(dag, :x1, :y; c=[:v, :w, :x3]) == true
  @test m_separation(dag, :x2, :x3) == true

end
