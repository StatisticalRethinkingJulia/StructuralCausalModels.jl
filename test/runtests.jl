using StructuralCausalModels
using Test

include("test_00.jl")
@test adjustmentsets == [[:a], [:c]]

