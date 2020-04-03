import Base.show

```

# DAG

Directed acyclic graph struct

$(FIELDS)

Part of API, exported.
```
struct DAG
  name::AbstractString
  d::OrderedDict{Symbol, Vector{Symbol}}      # DAG
  a::NamedArray                               # Adjacency matrix
  e::NamedArray                               # Edge matrix
  s::NamedArray                               # Covariance matrix as NamedArray
  df::DataFrame                               # DataFrame with variables
  vars::Vector{Symbol}                        # Names of variables
end

# Constructor

```

# DAG

Directed acyclic graph constructor

$(SIGNATURES)

Part of API, exported.
```
function DAG(name::AbstractString, d::OrderedDict{Symbol, Vector{Symbol}}, df::DataFrame)

  vars = dag_vars(d)
  a = adjacency_matrix(d)
  e = edge_matrix(d)

  # Compute covariance matrix and store as NamedArray

  @assert length(names(df)) == length(vars) "DataFrame has different number of columns"
  s = NamedArray(cov(Array(df)), (names(df), names(df)), ("Rows", "Cols"))

  # Create object

  DAG(name, d, a, e, s, df, vars)

end

function dag_show(io::IO, d::DAG)
  println("\nDAG object:\n")
  println(io, "name = \"$(d.name)\"")
  println(io, "vars = $(d.vars)")
  println()
  display(d.d)
  println()
end

show(io::IO, d::DAG) = dag_show(io, d)

export
  DAG
