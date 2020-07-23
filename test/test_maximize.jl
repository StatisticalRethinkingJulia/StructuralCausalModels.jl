using StructuralCausalModels, Test

ProjDir = @__DIR__

a= [
    0  100    0    0;
  100    0  100    1;
    1  100    0  100;
    0    0  100    0
]

vars = [Symbol("n$i") for i in 1:size(a, 1)]
amat = NamedArray(Int.(a), (vars, vars), ("Rows", "Cols"));

max_amat = maximize(amat)

max_a= [
    0  100    0  100;
  100    0  100    1;
    1  100    0  100;
  100    0  100    0
]

@testset "Maximize" begin

  @test all(max_amat .== max_a)

end