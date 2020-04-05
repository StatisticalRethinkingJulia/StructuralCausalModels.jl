import Base.show

"""

# DAG

Directed acyclic graph struct

### Struct
```julia
DAG(
* `name::AbstractString`                    : Variables used to compute correlation
* `d::OrderedDict{Symbol, Vector{Symbol}}`  : DAG definition aas a Dict
* `a::NamedArray`                           : Adjacency matrix
* `e::NamedArray`                           : Edge matric
* `s::NamedArray`                           : Covariance matrix
* `df::DataFrame`                           : Variable observations
* `vars::Vector{Symbol}`                    : Names of variables
)
```

Part of API, exported.
"""
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

"""

# DAG

Directed acyclic graph constructor

$(SIGNATURES)

### Method
```julia
DAG(
* `name::AbstractString`                    : Variables used to compute correlation
* `d::OrderedDict{Symbol, Vector{Symbol}}`  : DAG definition aas a Dict
* `df::DataFrame`                           : Variable observations
)
```

### Returns
```julia
* `res::DAG`                                : Boolean result of test
```
# Extended help

### Example

### Define and create a DAG
```julia
using StructuralCausalModels, CSV

df = CSV.read(scm_path("..", "data", "marks.csv");

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
