"""

# `syms_in_paths`

Collect vertices in all paths.

$(SIGNATURES)

Part of the API, Exported
"""
function syms_in_paths(paths, f, l)
  thepaths = deepcopy(paths)
  syms = Symbol[]
  for p in thepaths
    setdiff!(p, [f, l])
    append!(syms, p)
    unique!(syms)
  end
  syms
end

"""

# `syms_in_all_paths`

Check if a vertex is part of all paths.

$(SIGNATURES)

Part of the API, Exported
"""
function sym_in_all_paths(paths, sym)
  all([sym in p for p in paths])
end


"""

# `adjustment_sets`

$(SIGNATURES)

Computes the covariance adjustment vertex set. 

### Required arguments
```julia
* `dag::DAG`                           : DAG
* `f::Symbol`                          : Start variable
* `l::Symbol`                          : End variable
```

### Optional arguments
```julia
* `debug::Bool`                        : Show debug trace
```

### Returns
```julia
* `adjustmentsets=Vector{Symbol}[]`    : Array of adjustment sets
```

# Extended help

### Acknowledgements

Original author                        : Rob J Goedman

### Licence

Licenced under: MIT.

Part of the api, exported.
"""
function adjustment_sets(dag::DAG, f::Symbol, l::Symbol; debug=false)

  ap  = all_paths(dag, f, l)
  paths = backdoor_paths(dag, ap, f)
  debug && display(paths)
  lsyms = syms_in_paths(paths, f, l)
  adjustmentsets = Vector{Symbol}[]
  for s in lsyms
    debug && println("checking $s in $lsyms")
    if sym_in_all_paths(paths, s)
      opensets = length(open_paths(dag, paths, [s]))
      if  opensets == 0
        debug && println("$s closes all paths.")
        append!(adjustmentsets, [[s]])
      end
    elseif length(open_paths(dag, paths, [s])) == 0
        debug && println("$s not in all paths but no open paths.")
        append!(adjustmentsets, [[s]])
    end
  end
  for s in adjustmentsets
    setdiff!(lsyms, s)
  end
  debug && lsyms |> display
  len = 2
  local csyms
  while length(lsyms) >= len
    csyms = collect(combinations(lsyms, len))
    debug && csyms |> display
    for s in csyms
      if length(open_paths(dag, paths, s)) == 0
        debug && println("$s closes all paths")
        append!(adjustmentsets, [s])
        setdiff!(lsyms, s)
      end
    end
    len += 1
  end
  debug && lsyms |> display

  adjustmentsets
end

export
  adjustment_sets,
  syms_in_paths,
  sym_in_all_paths