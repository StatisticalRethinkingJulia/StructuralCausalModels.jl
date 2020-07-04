"""
    
# pcor_test

$(SIGNATURES)

Computes the partial correlation between two variables given a set of other
variables.

### Method
```julia
pcor_test(;
* `u::Vector{Symbol}`                  : Variables used to compute correlation
* `q::Int`                             : Number of variables in conditioning set
* `n::Int`                             : Number of observations
* `S::Matrix`                          : Sample covariance matrix
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

### Acknowledgements

Original author:                       Giovanni M. Marchetti

Translated to Julia:                   Rob J Goedman

### License

The R package ggm is licensed under License: GPL-2.

The Julia translation is licenced under: MIT.

Part of the api, not exported.
"""
function pcor_test(d::DAG, u::SymbolList, q, n)

  r = pcor(d, u)
  df = n - 2 - q
  tval = r * sqrt(df) / sqrt(1 - r*r)
  pv = 2 * cdf(TDist(df), -abs(tval))
  (pv=tval, df=df, pvalue=pv)

end

export
  pcor_test