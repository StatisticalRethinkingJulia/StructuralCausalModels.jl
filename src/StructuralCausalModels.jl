module StructuralCausalModels

using Reexport

@reexport using DataStructures, LinearAlgebra, Statistics
@reexport using Distributions, NamedArrays, DataFrames, CSV

src_path = @__DIR__

using DocStringExtensions: SIGNATURES, FIELDS, TYPEDEF

const scm_src_path = @__DIR__

"""

# scm_path

Relative path using the StatisticalRethinking src/ directory.

### Example to get access to the data subdirectory
```julia
scm_path("..", "data")
```
"""
scm_path(parts...) = normpath(joinpath(scm_src_path, parts...))

SymbolList = Union{Nothing, Symbol, Vector{Symbol}}
OrderedDictOrNothing = Union{OrderedDict, Nothing}
NamedArrayOrNothing = Union{NamedArray, Nothing}
DataFrameOrNothing = Union{DataFrame, Nothing}

include("types/DAG.jl")
include("types/Path.jl")
include("types/ConditionalIndependence.jl")
include("methods/dag_methods.jl")
include("methods/basis_set.jl")
include("methods/shipley_test.jl")
include("methods/d_separation.jl")
include("methods/pcor.jl")
include("methods/induced_covariance_graph.jl")
include("methods/all_paths.jl")
include("methods/open_paths.jl")
include("methods/backdoor_paths.jl")
include("methods/adjustment_sets.jl")
include("methods/ancestral_graph.jl")

include("utils/show_dag_path.jl")
include("utils/ggm_conversions.jl")
include("utils/dagitty_conversions.jl")
include("utils/graphviz_conversions.jl")

include("test_methods/test_ag.jl")

#include("methods/all_edges.jl")

export
  scm_path

end # module
