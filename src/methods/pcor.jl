"""
    
# pcor

$(SIGNATURES)

Computes the partial correlation between two variables given a set of other
variables.

### Method
```julia
pcor(;
* `u::Vector{Int}`                     : Variables used to compute correlation
* `S::Matrix`                          : Sample covariance matrix
)
```
where:

  u[1], u[2]: Variables used to compute correlation between, remaining indices
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

df = CSV.read(scm_path("..", "data", "marks.csv");
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

Exported as part of the api
"""
function pcor(u::Vector{Symbol}, S::NamedArray)
  k = inv(S[u, u])
  -k[1,2] / sqrt(k[1,1] * k[2,2])
end
