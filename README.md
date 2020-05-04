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

# Introduction

These are some early tests to see what it would take to create a Julia version to analyse directed acyclic graph (DAG) based Structural Causal Models (SCM) as described in [StatisticalRethinking](https://xcelab.net/rm/statistical-rethinking/) and [Causal Inference in Statistics](http://bcs.wiley.com/he-bcs/Books?action=index&bcsId=10288&itemId=1119186846).

My initial goal for this package is to have a way to apply SCM ideas to the examples in [StatisticalRethinking.jl](https://github.com/StatisticalRethinkingJulia), i.e. a working version of `d_separation()`, `adjustment_sets()` and `implied_conditional_independencies()`.

The package is not intended to compete with the references below and it is the intention to provide simple interoperability methods between Dagitty (and possible R's dagitty package), R's ggm package and this package.

StructuralCausalModels.jl is not (yet?) registered. You can install it as:
`] dev https://github.com/StatisticalRethinkingJulia/StructuralCausalModels.jl`

# Todo

A lot!

1. Provide methods to generate Dagitty, GraphViz and LightGraph plots from the DAG model.
2. Investigate other ways to represent a DAG (vs. the current Dict formulation).
3. Method `adjustment_sets(dag, paths)` - options for conditioning
4. Method `impliedConditionalIndependencies()`
5. Investigate Lightgraphs.jl to display the DAGs.
6. Documentation
7. Interoperability methods with Dagitty and ggm.
8. ...

More testing:

1. Method `paths=all_paths(dag, :f, :x)` - find all paths between nodes :f and :l.
2. Method `backdoor_paths(bd_paths)` - which are backdoor paths?
3. Method `backdoor_paths=select_backdoor_paths(paths)` - which paths are open?
4. Method `show_dag_path(dag, path)` - show path directions using arrows
6. ...

# Versions

## 0.1.0

1. Initial commit to Julia's registry.


# Acknowledgements

Important links are:

1. [Dagitty](http://www.dagitty.net/)
2. [R dagitty package](https://cran.r-project.org/web/packages/dagitty/index.html)

and particularly:

3. [R ggm package](https://cran.r-project.org/web/packages/ggm/index.html)

The latter has been used as the basis for the Julia implementations of `shipley_test()` and `d_separation()`.

# References

1. [StatisticalRethinking](https://xcelab.net/rm/statistical-rethinking/)
2. [Causal Inference in Statistics - a primer](https://www.wiley.com/en-us/Causal+Inference+in+Statistics%3A+A+Primer-p-9781119186847)
3. [Cause and Correlation in Biology](https://www.cambridge.org/core/books/cause-and-correlation-in-biology/247799189B31939D24BC0F61FD59E9BB#)
