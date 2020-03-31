module StructuralCausalModels

using Reexport

@reexport using DataStructures, LinearAlgebra, Statistics
@reexport using Distributions, NamedArrays, DataFrames, CSV

src_path = @__DIR__

using DocStringExtensions: SIGNATURES, FIELDS, TYPEDEF

const scm_src_path = @__DIR__

"""

# scm_path

Relative path using the StatisticalRethinking src/ directory. Copied from
[DynamicHMCExamples.jl](https://github.com/tpapp/DynamicHMCExamples.jl)

### Example to get access to the data subdirectory
```julia
scm_path("..", "data")
```
"""
scm_path(parts...) = normpath(joinpath(scm_src_path, parts...))

include("types/Dag.jl")
include("methods/dag_methods.jl")
include("methods/basis_set.jl")
include("methods/adjacency_matrix.jl")
include("methods/shipley_test.jl")
include("methods/d_separation.jl")

export
  scm_path

end # module
