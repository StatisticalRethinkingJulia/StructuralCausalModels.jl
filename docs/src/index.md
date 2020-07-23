```@meta
CurrentModule = StructuralCausalModels
```
## StructuralCausalModels
```@docs
StructuralCausalModels
```

## SCM
```@docs
SCM
```

## scm_path
```@docs
scm_path(parts...)
```

## DAG
```@docs
DAG(name::AbstractString, model::ModelDefinition; df::DataFrameOrNothing=nothing)
```

## d_separation
```@docs
d_separation(d::DAG, f::SymbolList, s::SymbolList; c::SymbolListOrNothing, debug=false)
```

## m_separation
```@docs
m_separation(d::DAG, f::SymbolList, s::SymbolList; c::SymbolListOrNothing, debug=false)
```

## shipley_test
```@docs
shipley_test(d::DAG)
```

## basis_set
```@docs
basis_set(dag::DAG)
```

## ancestral_graph
```@docs
ancestral_graph(d::DAG; m=Symbol[], c=Symbol[])
```

## ribbbon_graph
```@docs
ribbon_graph(d::DAG; m=Symbol[], c=Symbol[])
```

## adjustment_sets
```@docs
adjustment_sets(d::DAG, f::Symbol, l::Symbol; debug=false)
```

## paths
```@docs
all_paths(d::DAG, f::Symbol, l::Symbol)
backdoor_paths(d::DAG, paths::Vector{Vector{Symbol}}, f::Symbol)
```

## Support_functions
```@docs
adjacency_matrix(d::OrderedDict)
adjacency_matrix(e::NamedArray)
adjacency_matrix_to_dict(ea::NamedArray)
ancester_graph(e::NamedArray)
ancestral_graph(a::NamedArray{Int, 2}; m=Symbol[], c=Symbol[])
check_open(d::DAG, path::Vector{Symbol}, conditioning_set::SymbolList; debug=false)
DAG
dag_show(io::IO, d::DAG)
dag_vars(d::OrderedDict)
edge_matrix(d::OrderedDict)
edge_matrix(a::NamedArray, inv=false)
indicator_matrix(e::NamedArray)
induced_covariance_graph(d::DAG, sel::Vector{Symbol}, cond::SymbolList; debug=false)
node_edges(p::Path, s::Symbol, l::Symbol)
open_paths(d::DAG, paths::Vector{Vector{Symbol}}, cond::Vector{Symbol};debug=false)
pcor(d::DAG, u::SymbolList)
pcor_test(d::DAG, u::SymbolList, q, n)
ribbon_graph(a::NamedArray{Int, 2}; m=Symbol[], c=Symbol[])
sym_in_all_paths(paths, sym)
syms_in_paths(paths, f, l)
topological_order(a::NamedArray)
topological_sort(a::NamedArray)
topological_sort(dag::DAG)
topological_sort!(dag::DAG)
transitive_closure(a::NamedArray)
set_dag_df!(d::DAG, df::DataFrameOrNothing; force=false)
set_dag_cov_matrix!(d::DAG, cm::NamedArrayOrNothing; force=false)
undirected_matrix(d::DAG)
```