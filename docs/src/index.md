# StructuralCausalModels

*Documentation goes here.*

```@meta
CurrentModule = StructuralCausalModels
```

## `scm_path`
```@docs
scm_path(parts...)
```

## `DAG`
```@docs
DAG
DAG(name::AbstractString, d::OrderedDict{Symbol, Vector{Symbol}}, df::DataFrame) 
```

## `d_separation`
```@docs
d_separation(d::DAG, first::Vector{Symbol}, second::Vector{Symbol}, cond::SymbolList=nothing) 
```

##`basis_set`
```@docs
basis_set(dag::DAG)
```

##`shipley_test`
```@docs
shipley_test(d::DAG)
```

##`pcor`
```@docs
pcor(u::Vector{Symbol}, S::NamedArray)
```
