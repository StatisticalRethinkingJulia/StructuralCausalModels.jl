# Introduction

These are some early tests to see what it would take to create a Julia version to analyse directed acyclic graph (DAG) based Structural Causal Models (SCM) as described in [StatisticalRethinking](https://xcelab.net/rm/statistical-rethinking/) and [Causal Inference in Statistics](http://bcs.wiley.com/he-bcs/Books?action=index&bcsId=10288&itemId=1119186846).

My initial goal for this package is to have a way to apply SCM ideas to the examples in [StatisticalRethinking.jl](https://github.com/StatisticalRethinkingJulia), i.e. a working version of `d_separation()`, `adjustment_sets()` and `implied_conditional_independencies()`.

StructuralCausalModels.jl is part of the [StatisticalRethinkingJulia](https://github.com/StatisticalRethinkingJulia) eco system.

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
6. Documentation, documatation, ...
7. Interoperability methods with Dagitty and ggm.
8. ...

More testing:

1. Method `all_paths(dag, :x, :y)` - find all paths between nodes :f and :l.
2. Method `backdoor_paths(dag, paths, :x)` - which are backdoor paths to :x?
3. Method `open_paths(dag, paths)` - which paths are open?
4. Method `show_dag_path(dag, path)` - show path directions using arrows
6. ...

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

The latter two have been used as the basis for the Julia implementations of e.g. `shipley_test()`, `d_separation()` and `ancestral_graph()`.

# References

1. [StatisticalRethinking](https://xcelab.net/rm/statistical-rethinking/)
2. [Causal Inference in Statistics - a primer](https://www.wiley.com/en-us/Causal+Inference+in+Statistics%3A+A+Primer-p-9781119186847)
3. [Cause and Correlation in Biology](https://www.cambridge.org/core/books/cause-and-correlation-in-biology/247799189B31939D24BC0F61FD59E9BB#)
4. Sadeghi, K. (2011). Stable classes of graphs containing directed acyclic
graphs.
5. Richardson, T.S. and Spirtes, P. (2002).  Ancestral graph Markov
models {Annals of Statistics}, 30(4), 962-1030.
6. [Separators and Adjustment Sets in Causal Graphs: Complete Criteria and an Algorithmic Framework](https://doi.org/10.1016/j.artint.2018.12.006)

