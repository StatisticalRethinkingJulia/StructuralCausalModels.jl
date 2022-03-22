# StructuralCausalModels.jl

![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)<!--
![Lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)
![Lifecycle](https://img.shields.io/badge/lifecycle-stable-green.svg)
![Lifecycle](https://img.shields.io/badge/lifecycle-retired-orange.svg)
![Lifecycle](https://img.shields.io/badge/lifecycle-archived-red.svg)
![Lifecycle](https://img.shields.io/badge/lifecycle-dormant-blue.svg) -->
[![Build Status](https://travis-ci.com/StatisticalRethinkingJulia/StructuralCausalModels.jl.svg?branch=master)](https://travis-ci.com/StatisticalRethinkingJulia/StructuralCausalModels.jl)
[![codecov.io](http://codecov.io/github/StatisticalRethinkingJulia/StructuralCausalModels.jl/coverage.svg?branch=master)](http://codecov.io/github/StatisticalRethinkingJulia/StructuralCausalModels.jl?branch=master)
[![Documentation](https://img.shields.io/badge/docs-stable-blue.svg)](https://StatisticalRethinkingJulia.github.io/StructuralCausalModels.jl/stable)
[![Documentation](https://img.shields.io/badge/docs-master-blue.svg)](https://StatisticalRethinkingJulia.github.io/StructuralCausalModels.jl/dev)
[![ColPrac: Contributor's Guide on Collaborative Practices for Community Packages](https://img.shields.io/badge/ColPrac-Contributor's%20Guide-blueviolet)](https://github.com/SciML/ColPrac)

# Introduction

StructuralCausalModels.jl is part of the StatisticalRethinkingJulia eco system and contains functionality to analyse directed acyclic graph (DAG) based causal models as described in [StatisticalRethinking](https://xcelab.net/rm/statistical-rethinking/), [Causal Inference in Statistics](http://bcs.wiley.com/he-bcs/Books?action=index&bcsId=10288&itemId=1119186846) and [Cause and Correlation in Biology](https://www.cambridge.org/core/books/cause-and-correlation-in-biology/247799189B31939D24BC0F61FD59E9BB#).

My initial goal for this package is to have a way to apply SCM ideas to the examples in [StatisticalRethinking.jl](https://github.com/StatisticalRethinkingJulia), i.e. a working version of `basis_set()`, `d_separation()`, `m_separations()` and `adjustment_sets()`.

From the point of view of above functionality, I believe the package is close to R's `ggm` (including most of Sadeghi's additions). I'm hoping version 1.0.0 has a similar API but many more test cases, including more comparisons with R's `dagitty`.

The status of the package remains experimental and is, as is StatisticalRethinking.jl, primarily intended for learning statistical modeling approaches and pitfalls.

StructuralCausalModels.jl can be installed using
`] add StructuralCausalModels`.

# Versions

## 0.1.0

1. Initial commit to Julia's registry.

# Acknowledgements

Important links are:

1. [Dagitty](http://www.dagitty.net/)
2. [R dagitty package](https://cran.r-project.org/web/packages/dagitty/index.html)
3. [R ggm package](https://cran.r-project.org/web/packages/ggm/index.html)
4. Sadeghi, K. (2011). Stable classes of graphs containing directed acyclic
graphs, implementation as included in ggm.

The latter two have been used for the Julia implementations of most fuctions
in this package, e.g. `basis_set()`, `d_separation()`, `m_separation`,
`shipley_test()`, `pcor_test()` and `ancestral_graph`.


# References

1. [StatisticalRethinking](https://xcelab.net/rm/statistical-rethinking/)
2. [Causal Inference in Statistics - a primer](https://www.wiley.com/en-us/Causal+Inference+in+Statistics%3A+A+Primer-p-9781119186847)
3. [Cause and Correlation in Biology](https://www.cambridge.org/core/books/cause-and-correlation-in-biology/247799189B31939D24BC0F61FD59E9BB#)
4. Sadeghi, K. (2011). Stable classes of graphs containing directed acyclic
graphs.
5. Richardson, T.S. and Spirtes, P. (2002).  Ancestral graph Markov
models {Annals of Statistics}, 30(4), 962-1030.
6. [Separators and Adjustment Sets in Causal Graphs: Complete Criteria and an Algorithmic Framework](https://doi.org/10.1016/j.artint.2018.12.006)

