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

These are some early tests to see what it would take to create a Julia version of the [SCM primer book](http://bcs.wiley.com/he-bcs/Books?action=index&bcsId=10288&itemId=1119186846).

My initial goal for this package is to have a minimal way to apply SCM ideas to the examples in [StatisticalRethinking.jl](https://github.com/StatisticalRethinkingJulia), i.e. a working version of `d_separation()`.

The package is not intended to compete with reference 1 below

# Todo

A lot!

1. Provide methods to generate Dagitty, GraphViz and LightGraph plots from the DAG model.
2. Investigate other ways to represent a DAG (vs. the current Dict formulation).
3. Method `paths=all_paths(dag)` - find all paths between 2 nodes.
4. Method `open_paths(bd_paths)` - which are open?
5. Method `bd_paths=select_backdoor_paths(open_paths)` - which open baths are backdoors?
6. Method `condition_variables_to_close(bd_paths)` - close open path
7. Implement `impliedConditionalIndependencies()`
8. Investigate Lightgraphs.jl to display the DAGs.
9. Documantation
10. ...

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

1. SR
2. SCM Primer
3. Shipley?
