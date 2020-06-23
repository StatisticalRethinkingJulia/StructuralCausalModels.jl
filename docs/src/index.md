```@meta
CurrentModule = StructuralCausalModels
```

## scm_path
```@docs
scm_path(parts...)
```

## DAG
```@docs
DAG
DAG(name::AbstractString, d::OrderedDict, df::DataFrame)
```

## d_separation
```@docs
d_separation(d::DAG, first::SymbolList, second::SymbolList; cond::SymbolListOrNothing, debug=false)
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
DAG(name::AbstractString, d::OrderedDict)
DAG(name::AbstractString, str::AbstractString, df::DataFrame)
DAG(name::AbstractString, str::AbstractString)
DAG(name::AbstractString, a::NamedArray, df::DataFrame)
DAG(name::AbstractString, a::NamedArray)
dag_show(io::IO, d::DAG)
dag_vars(d::OrderedDict)
edge_matrix(d::OrderedDict)
edge_matrix(a::NamedArray, inv=false)
indicator_matrix(e::NamedArray)
induced_covariance_graph(d::DAG, sel::Vector{Symbol}, cond::SymbolList; debug=false)
node_edges(p::Path, s::Symbol, l::Symbol)
pcor(u::Vector{Symbol}, S::NamedArray)
topological_order(a::NamedArray)
topological_sort(a::NamedArray)
transitive_closure(a::NamedArray)
set_dag_df!(d::DAG, df::DataFrameOrNothing; force=false)
set_dag_cov_matrix!(d::DAG, cm::NamedArrayOrNothing; force=false)
undirected_matrix(d::DAG)
```