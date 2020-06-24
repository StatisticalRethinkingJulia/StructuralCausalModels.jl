import Base.show

"""

# DAG

Directed acyclic graph struct

### Struct
```julia
DAG(
* `name::AbstractString`                    : Name for the DAG object
* `d::OrderedDictOrNothing`                 : DAG definition as an OrderedDict
* `a::NamedArrayOrNothing`                  : Adjacency matrix
* `e::NamedArrayOrNothing`                  : Edge matrix
* `s::NamedArrayOrNothing`                  : Covariance matrix
* `df::DataFrameOrNothing`                  : Variable observations
* `vars::Vector{Symbol}`                    : Names of variables in DAG
)
```

Part of API, exported.
"""
mutable struct DAG
  name::AbstractString
  d::OrderedDictOrNothing                    # Name of DAG
  a::NamedArrayOrNothing                     # Adjacency matrix
  e::NamedArrayOrNothing                     # Edge matrix
  s::NamedArrayOrNothing                     # Covariance matrix as NamedArray
  df::DataFrameOrNothing                     # DataFrame with variables
  vars::Vector{Symbol}                       # Names of variables in DAG
end

# Constructor

"""

# DAG

Directed acyclic graph constructor

$(SIGNATURES)

### Required arguments
```julia
* `name::AbstractString`               : Name for the DAG object
* `d::ModelDefinition`                 : DAG definition
```

where
```
ModelDefinition = Union{OrderedDict, AbstractString, NamedArray}
```

See the extended help for a usage example.

### Keyword arguments
```julia
* `df::DataFrame`                      : DataFrame with observations
```

### Returns
```julia
* `dag::DAG`                           : Boolean result of test
```

# Extended help

In the definition of the OrderedDict, read `=>` as `~` in regression models
or `<-` in causal models, e.g.:

```julia
d = OrderedDict(
  :u => [:x, :v],
  :s1 => [:u],
  :w => [:v, :y],
  :s2 => [:w]
);
dag = DAG("my_name", d)
```

Coming from R's dagitty:

amat <- dagitty("dag { {X V} -> U; S1 <- U; {Y V} -> W; S2 <- W}”)
```julia
dag = DAG("my_name", "dag { {X V} -> U; S1 <- U; {Y V} -> W; S2 <- W}”)
display(dag) # Show the DAG
```

Coming from R's ggm:

amat <- DAG(U~X+V, S1~U, W~V+Y, S2~W, order=FALSE)
```julia
dag = DAG("my_name", "DAG(U~X+V, S1~U, W~V+Y, S2~W”)
display(dag) # Show the DAG
```

### Acknowledgements

Original author:                       Giovanni M. Marchetti

Translated to Julia:                   Rob J Goedman

### License

The R package ggm is licensed under License: GPL-2.

The Julia translation is licenced under: MIT.

Part of API, exported.
"""
function DAG(name::AbstractString, model::ModelDefinition; df::DataFrameOrNothing=nothing)

  local d
  if typeof(model) <: OrderedDict
    d = model
  elseif typeof(model) <: AbstractString
    ds = strip(model)
    if ds[1:3] == "DAG"
      d = from_ggm(ds)
    elseif ds[1:3] == "dag"
      d = from_dagitty(ds)
    else
      @error "Unrecognized model string: $(ds))"
    end
  elseif typeof(model) <: NamedArray
    d = adjacency_matrix_to_dict(model)
  end    

  vars = dag_vars(d)
  a = adjacency_matrix(d)
  e = edge_matrix(d)

  if isnothing(df)
    s = nothing
  else
    # Compute covariance matrix and store as NamedArray
    @assert length(names(df)) == length(vars) "DataFrame has different number of columns"
    s = NamedArray(cov(Array(df)), (names(df), names(df)), ("Rows", "Cols"))
  end

  # Create object

  DAG(name, d, a, e, s, df, vars)

end

"""

# `set_dag_df!`

Set or update Dataframe associated to DAG

$(SIGNATURES)

### Required arguments
```julia
* `d::DAG`                                  : Previously defined DAG object 
* `df::DataFrameOrNothing`                  : DataFrame associated with DAG
)
```

### Optional arguments
```julia
* `force=false`                             : Force assignment of df 
)
```

The `force = true` option can be used if the DAG involves unobserved nodes.

Part of API, exported.
"""
function set_dag_df!(d::DAG, df::DataFrameOrNothing; force=false)
  # Compute covariance matrix and store as NamedArray

  if !(force || df == nothing)
    @assert length(names(df)) == length(d.vars) "DataFrame has different number of columns"
    @assert names(df, 1) !== d.vars "DataFrame names differ from DAG variable names"
  end

  d.df = df
  if df == nothing || nrow(df) == 0
    d.s = nothing
  else
    if nrow(df) > 1 && length(names(df)) > 0
      d.s = NamedArray(cov(Array(df)), (names(df), names(df)), ("Rows", "Cols"))
    else
      d.s = nothing
    end
  end

end  

"""

# `set_dag_cov_matrix!`

Set or update the covariance matrix associated to DAG

$(SIGNATURES)

### Required arguments
```julia
* `d::DAG`                                  : Previously defined DAG object 
* `cm::NamedArrayOrNothing`                 : Covariance matrix in NamedArray format
)
```

### Optional arguments
```julia
* `force=false`                             : Force assignment of df 
)
```

The `force = true` option can be used if the DAG involves unobserved nodes.

Part of API, exported.
"""
function set_dag_cov_matrix!(d::DAG, cm::NamedArrayOrNothing; force=false)
  # Compute covariance matrix and store as NamedArray

  if !(force || cm == nothing)
    @assert length(names(cm)) == length(d.vars) "Covariance matrix has different number of columns"
    @assert names(cm) !== d.vars "Covariance matrix names differ from DAG variable names"
  end

  if cm == nothing
    d.s = nothing
  else
    d.s = cm
  end

end  

"""

# `dag_show`

$(SIGNATURES)

Internal
"""
function dag_show(io::IO, d::DAG)
  println("\nDAG object:\n")
  println(io, "name = \"$(d.name)\"")
  println(io, "vars = $(d.vars)")
  println()
  !isnothing(d.d) && display(d.d)
  println()
end

show(io::IO, d::DAG) = dag_show(io, d)

export
  DAG,
  set_dag_df!,
  set_dag_cov_matrix!
  