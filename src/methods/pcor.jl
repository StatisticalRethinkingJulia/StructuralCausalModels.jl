"""
    
# pcor

$(SIGNATURES)

Computes the partial correlation between two variables given a set of other
variables.

### Method
```julia
pcor(;
* `d::DAG`                             : DAG object
* `u::Vector{Symbol}`                  : Variables used to compute correlation
)
```
where:

  u[1], u[2]: Variables used to compute correlation between, remaining variables
  are the conditioning set


### Returns
```julia
* `res::Float64`                       : Correlation between u[1] and u[2]
```
# Extended help

### Example

### Correlation between vectors and algebra, conditioning on analysis and statistics
```julia
using StructuralCausalModels, CSV

df = DataFrame!(CSV.File(scm_path("..", "data", "marks.csv"));
S = cov(Array(df))

u = [2, 3, 4, 5]
pcor(u, S)
u = [:vectors, :algebra, :statistics, :analysis]
```
### Acknowledgements

Original author:                       Giovanni M. Marchetti

Translated to Julia:                   Rob J Goedman

### License

The R package ggm is licensed under License: GPL-2.

The Julia translation is licenced under: MIT.

Part of the api, not exported.
"""
function pcor(d::DAG, u::SymbolList)
  us = String.(u)
  k = inv(d.s[us, us])
  -k[1,2] / sqrt(k[1,1] * k[2,2])
end

export
  pcor