"""

# m_separation

$(SIGNATURES)

Computes the m_separation between 2 sets of nodes conditioned on a third set.

### Required arguments
```julia
m_separation(
* `d::DAG`                             : DAG
* `f::SymbolList`                      : First vertex or set
*  s::SymbolList`                      : Second vertex or set
)
```

### Keyword arguments
```julia
* `c::SymbolListOrNothing=nothing`     : Conditioning set
* `debug=false`                        : Trace execution
```

### Returns
```julia
* `res::Bool`                          : Boolean result of test
```

# Extended help

### Example

### m_separation between mechanics and statistics, conditioning on algebra
```julia
using StructuralCausalModels, CSV

df = DataFrame!(CSV.File(scm_path("..", "data", "marks.csv"));

d = OrderedDict(
  :mechanics => [:vectors, :algebra],
  :vectors => [:algebra],
  :analysis => [:algebra],
  :statistics => [:algebra, :analysis]
);

dag = DAG("marks", d, df);
m_separation(marks, [:statistics], [:mechanics]; c=[:algebra]))
```
### Acknowledgements

Original author:                       Giovanni M. Marchetti

Translated to Julia:                   Rob J Goedman

### License

The R package ggm is licensed under License: GPL-2.

The Julia translation is licenced under: MIT.

Part of the API, exported.
"""
function m_separation(d::DAG, f::SymbolList, s::SymbolList;
  c::SymbolListOrNothing=nothing, debug=false)

  if typeof(f) == Symbol
    f = [f]
  end
  if typeof(s) == Symbol
    s = [s]
  end
  if typeof(c) == Symbol
    c = [c]
  end

  if isnothing(c)
    m = setdiff(d.vars, vcat(f, s))
    debug && println("m = $m")
    ar = maximize(ribbon_graph(d, m=m))
    debug && println("ar = $ar")
  else
    m = setdiff(d.vars, vcat(f, s, c))
    debug && println("m = $m, c = $c")
    ar = maximize(ribbon_graph(d, m=m, c=c))
    debug && println("ar = $ar")
  end

  all(Array(ar[f, s]) + Array(ar[s, f]) .== 0)
end

export
  m_separation