using StructuralCausalModels
using Test

@testset "StrucruralCausalModels.jl" begin

include("test_00.jl")
@test adjustmentsets == [[:a], [:c]]

end 
