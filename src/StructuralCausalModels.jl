"""

# StructuralCausalModels

StructuralCausalModels.jl provides functionality to analyse directed acyclic
graph (DAG) based causal models as described in `StatisticalRethinking`, `Causal
Inference in Statistics` and `Cause and Correlation in Biology`.

My initial goal for this package is to have a way to apply SCM ideas to the
examples in the context of StatisticalRethinking, i.e. a working version of
`basis_set()`, `d_separation()`, `m_separation()` and `adjustment_sets()`.

SCM can be used as an alias to StructuralCausalModels.
"""
module StructuralCausalModels

using Reexport

@reexport using DataStructures, LinearAlgebra, Statistics
@reexport using Distributions, NamedArrays, DataFrames, CSV
@reexport using Combinatorics

src_path = @__DIR__

using DocStringExtensions: SIGNATURES, FIELDS, TYPEDEF

const scm_src_path = @__DIR__

"""

SCM

Alias for StructuralCausalModels
"""
const SCM = StructuralCausalModels

"""

# scm_path

Relative path using the StructuralCausalModels.jl src/ directory.

### Example to get access to the data subdirectory
```julia
scm_path("..", "data")
```

Part of the API, exported.
"""
scm_path(parts...) = normpath(joinpath(scm_src_path, parts...))

SymbolList = Union{Symbol, Vector{Symbol}}
SymbolListOrNothing = Union{SymbolList, Nothing}
OrderedDictOrNothing = Union{OrderedDict, Nothing}
NamedArrayOrNothing = Union{NamedArray, Nothing}
DataFrameOrNothing = Union{DataFrame, Nothing}
ModelDefinition = Union{OrderedDict, AbstractString, NamedArray}

include("types/DAG.jl")
include("types/Path.jl")
include("types/BasisSet.jl")
include("types/ConditionalIndependence.jl")

include("methods/dag_methods.jl")
include("methods/basis_set.jl")
include("methods/shipley_test.jl")
include("methods/d_separation.jl")
include("methods/pcor.jl")
include("methods/pcor_test.jl")
include("methods/induced_covariance_graph.jl")
include("methods/all_paths.jl")
include("methods/open_paths.jl")
include("methods/backdoor_paths.jl")
include("methods/adjustment_sets.jl")
include("methods/graph_utils.jl")
include("methods/ancestral_graph.jl")
include("methods/ribbon_graph.jl")
include("methods/maximize.jl")
include("methods/m_separation.jl")

include("utils/show_dag_path.jl")
include("utils/ggm_conversions.jl")
include("utils/dagitty_conversions.jl")
include("utils/graphviz_conversions.jl")

export
  SCM,
  scm_path

end # module
