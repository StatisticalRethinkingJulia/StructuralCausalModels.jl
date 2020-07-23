"""
    
# ancestral_graph

$(SIGNATURES)

Ancestral graphs after marginalization and conditioning.

### Required arguments
```julia
* `amat::NamedArray{Int, 2}`           : Adjacency matrix of a DAG
```

### Optional arguments
```julia
* `m::Vector{Symbol}`                  : Nodes in DAG that are marginalized
* `c::Vector{Symbol})`                 : Nodes in DAG there are conditioned on
```

### Returns
```julia
* `ag::NamedArray`                     : Ancestral graph remaining
```

# Extended help

### Example

### Adjacency matrix used for testing in ggm

```julia
amat_data = transpose(reshape([
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
  0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
  0,0,0,0,1,0,1,0,1,1,0,0,0,0,0,0,
  1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
  0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0
], (16,16)));

vars = [Symbol("n\\\$i") for i in 1:size(amat_data, 1)]
amat = NamedArray(Int.(amat_data), (vars, vars), ("Rows", "Cols"));
m = [:n3, :n5, :n6, :n15, :n16];
c = [:n4, :n7];

ag = ancestral_graph(amat; m = m, c = c)
```

### Acknowledgements

Original author:                       Kayvan Sadeghi

Translated to Julia:                   Rob J Goedman

### References

Sadeghi, K. (2011). Stable classes of graphs containing directed acyclic
graphs.

Richardson, T.S. and Spirtes, P. (2002).  Ancestral graph Markov
models {Annals of Statistics}, 30(4), 962-1030.

### Licence

The R package ggm is licensed under License: GPL-2.

The Julia translation is licenced under: MIT.

Part of the api, exported.
"""
function ancestral_graph(amat::NamedArray{Int, 2}; m=Symbol[], c=Symbol[])
  
  a = amat'
  vars = names(a, 1)
  s = update_s(a, c)

  ar = copy(a)
  at = 2a

  while true

    at = copy(ar)
    ar = update_21(ar, m)
    ar .+= update_22(ar, s)
    ar .+= update_23(ar, m)
    ar .+= update_24(ar, m)
    ar .+= update_25(ar, m)
    ar .+= update_26(ar, m)
    ar .+= update_27(ar, s)
    ar .+= update_28(ar, s)
    ar .+= update_29(ar, m)
    ar .+= update_30(ar, m)

    ar == at && break

  end

  ar = update_a(ar)
  ar = update_b(ar, s)
  an = update_an(ar)

  while true
    at = copy(ar)

    a27n = update_27n(ar, an)
    an .+= a27n
    ar .+= a27n
    a22n = update_22n(ar, an)
    an .+= a22n
    ar .+= a22n

    at == ar && break
  end

  #ar_old = copy(ar)
  ar = update_i(ar)
  select = setdiff(vars, vcat(c, m))
  fr = ar[select, select]

end

"""
    
# ancestral_graph

$(SIGNATURES)

Ancestral graphs after marginalization and conditioning.

### Required arguments
```julia
* `d::DAG`                             : DAG onject
```

### Optional arguments
```julia
* `m::Vector{Symbol}`                  : Nodes in DAG that are marginalized
* `c::Vector{Symbol})`                 : Nodes in DAG there are conditioned on
```


### Returns
```julia
* `ag::NamedArray`                     : Ancestral graph remaining
```
"""
function ancestral_graph(d::DAG; m=Symbol[], c=Symbol[])
  ancestral_graph(d.a; m=m, c=c)
end

export
  ancestral_graph