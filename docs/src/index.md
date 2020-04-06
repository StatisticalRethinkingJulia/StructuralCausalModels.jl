```@meta
CurrentModule = StructuralCausalModels
```

## scm_path (exported)
```@docs
scm_path(parts...)
```
## DAG struct (exported)
```@docs
DAG
```

## DAG constructor (exported)
DAG(name::AbstractString, d::OrderedDict{Symbol, Vector{Symbol}}, df::DataFrame)
```

## d_separation (exported)
```@docs
d_separation(d::DAG, first::Vector{Symbol}, second::Vector{Symbol}, cond::SymbolList=nothing)
```

## shipley_test (exported)
```@docs
shipley_test(d::DAG)
```

## basis_set (not exported)
```@docs
basis_set(dag::DAG)
```

## pcor (not exported)
```@docs
pcor(u::Vector{Symbol}, S::NamedArray)
```

# Internals
```@docs
dag_vars(d::OrderedDict{Symbol, Vector{Symbol}})
edge_matrix(d::OrderedDict{Symbol, Vector{Symbol}})
edge_matrix(a::NamedArray, inv=false)
adjacency_matrix(d::OrderedDict{Symbol, Vector{Symbol}})
adjacency_matrix(e::NamedArray)
topological_order(a::NamedArray)
topological_sort(a::NamedArray)
ancester_graph(e::NamedArray)
indicator_matrix(e::NamedArray)
transitive_closure(a::NamedArray)
induced_covariance_graph(d::DAG, sel::Vector{Symbol}, cond::SymbolList; debug=false)
```