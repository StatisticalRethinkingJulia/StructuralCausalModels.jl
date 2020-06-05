import Base.show

"""

# DAG

Directed acyclic graph struct

### Struct
```julia
DAG(
* `name::AbstractString`                    : Variables used to compute correlation
* `d::OrderedDictOrNothing`                 : DAG definition as a Dict
* `a::NamedArrayOrNothing`                  : Adjacency matrix
* `e::NamedArrayOrNothing`                  : Edge matric
* `s::NamedArrayOrNothing`                  : Covariance matrix
* `df::DataFrameOrNothing`                  : Variable observations
* `vars::Vector{Symbol}`                    : Names of variables
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
  vars::Vector{Symbol}                       # Names of variables
end

# Constructor

"""

# DAG

Directed acyclic graph constructor

$(SIGNATURES)

### Required arguments
```julia
* `name::AbstractString`                         : Variables used to compute correlation
* `d::OrderedDict{SymbolList, SymbolList}`       : DAG definition as a Dict
```

where
```julia
SymbolList = Union{Nothing, Symbol, Vector{Symbol}}
```

### Optional arguments
```julia
* `df::DataFrame`                                : DataFrame with observations
```

### Returns
```julia
* `res::DAG`                                     : Boolean result of test
```
# Extended help

### Example

### Define and create a DAG
```julia
using StructuralCausalModels, CSV

df = CSV.read(scm_path("..", "data", "marks.csv");

# Create a Dict describing the DAG

# Read `=>` as `~` in regression models or `<-` in causal models, e.g.
# fig2.6.dag <- dagitty("dag { {X V} -> U; S1 <- U; {Y V} -> W; S2 <- W}”)
# fig2.6.ggm <- DAG(U~X+V, S1~U, W~V+Y, S2~W, order=FALSE)
# d = OrderedDict(
#   :u => [:x, :v],
#   :s1 => [:u],
#   :w => [:v, :y],
#   :s2 => [:w]
# );
#
# The same DAG could also be specified as:
#
# d =OrderedDict{SymbolList, SymbolList}(
#   [:u, :w] => :v,
#   [:u] => :x,
#   :s1 => [:u],
#   :w => :y,
#   [:s2] => [:w]
# )

d = OrderedDict(
  :mechanics => [:vectors, :algebra],
  :vectors => [:algebra],
  :analysis => [:algebra],
  :statistics => [:algebra, :analysis]
);

dag = DAG("marks", d, df);
```
### Acknowledgements

Original author:                       Giovanni M. Marchetti

Translated to Julia:                   Rob J Goedman

### License

The R package ggm is licensed under License: GPL-2.

The Julia translation is licenced under: MIT.

Part of API, exported.
"""
function DAG(name::AbstractString, d::OrderedDict, df::DataFrame)

  vars = dag_vars(d)
  a = adjacency_matrix(d)
  e = edge_matrix(d)

  # Compute covariance matrix and store as NamedArray

  @assert length(names(df)) == length(vars) "DataFrame has different number of columns"
  s = NamedArray(cov(Array(df)), (names(df), names(df)), ("Rows", "Cols"))

  # Create object

  DAG(name, d, a, e, s, df, vars)

end

function DAG(name::AbstractString, d::OrderedDict{Symbol, Vector{Symbol}})

  vars = dag_vars(d)
  a = adjacency_matrix(d)
  e = edge_matrix(d)

  DAG(name, d, a, e, nothing, nothing, vars)
end


"""

# DAG

Directed acyclic graph constructor

$(SIGNATURES)

### Required arguments
```julia
DAG(
* `name::AbstractString`                    : Variables used to compute correlation
* `d::NamedArray`                           : Adjacency matrix of DAG
)
```

### Returns
```julia
* `res::DAG`                                : Boolean result of test
```
"""
function DAG(name::AbstractString, a::NamedArray; df::DataFrameOrNothing=nothing)

  vars = names(a, 1)
  e = StructuralCausalModels.edge_matrix(a)

  # Compute covariance matrix and store as NamedArray if df is present

  if !isnothing(df)
    @assert length(names(df)) == length(vars) "DataFrame has different number of columns"
    s = NamedArray(cov(Array(df)), (names(df), names(df)), ("Rows", "Cols"))
  end

  # Create object

  DAG(name, nothing, a, e, nothing, nothing, vars)

end

"""

# set_dag_df!

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

# set_dag_cov_matrix!

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
