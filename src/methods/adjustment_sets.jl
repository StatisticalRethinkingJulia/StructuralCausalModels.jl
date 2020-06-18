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

Compute the covariance adjustment vertex set.

$(SIGNATURES)

Part of the API, Exported
"""
function adjustment_sets(d::DAG, f::Symbol, l::Symbol, debug=false)

  ap  = all_paths(d, f, l)
  paths = backdoor_paths(d, ap, f)
  lsyms = syms_in_paths(paths, f, l)
  adjustmentsets = Vector{Symbol}[]
  for s in lsyms
    debug && println("checking $s in $lsyms")
    if sym_in_all_paths(paths, s)
      opensets = length(open_paths(d, paths, [s]))
      if  opensets == 0
        debug && println("$s closes all paths")
        append!(adjustmentsets, [[s]])
      else
        debug && println("Symbol $s has $(opensets) paths open")
      end
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
      if length(open_paths(d, paths, s)) == 0
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
  syms_in_all_paths