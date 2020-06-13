# Introduction

StructuralCausalModels.jl (SCM) is a Julia package to analyse directed acyclic graphs (DAGs) as described in [StatisticalRethinking](https://xcelab.net/rm/statistical-rethinking/), [Causal Inference in Statistics](http://bcs.wiley.com/he-bcs/Books?action=index&bcsId=10288&itemId=1119186846) and [Cause and Correlation in Biology](https://www.cambridge.org/core/books/cause-and-correlation-in-biology/247799189B31939D24BC0F61FD59E9BB#).

My initial goal for this package is to have a way to apply SCM ideas to the examples in [StatisticalRethinking.jl](https://github.com/StatisticalRethinkingJulia), i.e. a working version of `d_separation()`, `adjustment_sets()` and `implied_conditional_independencies()`(still being developed).

All three above references are great introductions to the use of causal models to help in understanding confounding in statistical models. 

[StructuralCausalModels.jl](https://github.com/StatisticalRethinkingJulia/StructuralCausalModels.jl) is part of the [StatisticalRethinkingJulia](https://github.com/StatisticalRethinkingJulia) eco system. The package, once registered, can be installed using `] add StructuralCausalModels`.

Please report issues or file pull requests on Github if you find a problem.
