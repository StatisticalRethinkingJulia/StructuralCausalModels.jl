using StructuralCausalModels, Test

ProjDir = @__DIR__

a= [
  0    0    0    0;
  1    0    0    1;
  0    1    0    0;
  0    0    0    0
]

vars = [Symbol("n$i") for i in 1:size(a, 1)]
amat = NamedArray(Int.(a), (vars, vars), ("Rows", "Cols"));

dag = DAG("m_separation", amat)

m = maximize(ribbon_graph(amat, c=[:n1], m=[:n4]))

f = [:n1]
s = [:n4]
c = [:n3]

ms1 = m_separation(dag, f, s; debug=false)
ms2 = m_separation(dag, f, s; c=c)

@testset "m_separation" begin

  @test ms1 == true
  @test ms2 == false

end