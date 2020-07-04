"""
    
# shipley_test

$(SIGNATURES)

Test of all independencies implied by a given DAG

Computes a simultaneous test of all independence relationships implied by a
given Gaussian model defined according to a directed acyclic graph, based on
the sample covariance matrix.

The test statistic is C = -2 sum(ln(p_j)) where p_j are the
p-values of tests of conditional independence in the basis set computed by
basiSet(A). The p-values are independent uniform variables on
(0,1) and the statistic has exactly a chi square distribution on
2k degrees of freedom where k is the number of elements of the
basis set.  Shipley (2002) calls this test Fisher's C test.

### Method
```julia
shipley_test(;
* `d::Dag`                             : Directed acyclic graph
)
```

### Returns
```julia
* `res::NamedTuple`                    : (ctest=..., dof=..., pval=...)
```
where:

  ctest: Test statistic C
  dof:   Degrees of freedom.
  pval:  The P-value of the test, assuming a two-sided alternative.

# Extended help

### Example

### Shipley_test for the mathematics marks data
```julia
using StructuralCausalModels, RData

objs = RData.load(scm_path("..", "data", "marks.rda");
marks_df = objs["marks"]

d = OrderedDict(
  :mechanics => [:vectors, :algebra],
  :vectors => [:algebra],
  :statistics => [:algebra, :analysis],
  :analysis => [:algebra]
);
dag = Dag(d; df=df)
shipley_test(dag)
```

### See also
```julia
?Dag
?basis_set
?pcor_test
```

### Acknowledgements

Original author:                       Giovanni M. Marchetti

Translated to Julia:                   Rob J Goedman

### References

Shipley, B. (2000). A new inferential test for path models based
on directed acyclic graphs. Structural Equation Modeling, 7(2),
206--218.

### Licence

The R package ggm is licensed under License: GPL-2.

The Julia translation is licenced under: MIT.

Part of the api, exported.
"""
function shipley_test(d::DAG)

  @assert isposdef(d.s) "Covariance matrix is not positive definite."
  n = nrow(d.df)

  # see pcor()
  function pval(r, q, n)
    df = n - 2 - q
    tval = r * sqrt(df) / sqrt(1 - r * r)
    2 * cdf(TDist(df), -abs(tval))
  end

  l = basis_set(d)
  k = length(l)
  if k == 0
    @info "Basis set empty, Shipley test not possible."
    return nothing
  end
  p = zeros(k)
  for i in 1:k
    u = l[i]
    println(u)
    r = pcor(d, u)
    q = length(u) - 2
    p[i] = pval(r, q, n)
  end
  ctest = -2 * sum(log.(p))
  df = 2 * k
  pv = 1 - cdf(Chisq(df), ctest)
  (ctest=ctest, df=df, pv=pv,)
end

export
  shipley_test
