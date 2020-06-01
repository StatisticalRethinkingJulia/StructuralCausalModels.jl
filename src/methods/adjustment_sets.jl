function forward_path(d::DAG, path)
  res = true
  for (i, s) in enumerate(path[1:end-1])
    res = res && d.a[path[i+1], path[i]] == 1
  end
  res
end

function blocking_sets(asets::Array{Array{Symbol,1},1})
  result_sets = Vector{Symbol}[]
  #println(asets)
  syms = union(asets...)
  reduced_syms = deepcopy(syms)
  for sym in syms
    local res = true
    for i in 1:length(asets)
      if sym in asets[i] 
        res = true
      else
        res = false
      end
      #println("sym = $sym, res = $res")
      length(asets) == 1 && break
      !res && break
    end
    if res
      push!(result_sets, [sym])
      setdiff!(reduced_syms, [sym])
    end
  end
  #println("reduced_syms = $reduced_syms")
  length(reduced_syms) > 0 && push!(result_sets, reduced_syms)
  result_sets
end

function adjustment_sets(d::DAG, f::Symbol, l::Symbol, u::Vector{Symbol})
  asets = Vector{Symbol}[]
  allpaths  = all_paths(d, f, l)
  backdoorpaths = backdoor_paths(d, allpaths, f)
  for path in open_paths(d, backdoorpaths)
    aset = Symbol[]
    indx = findall(x -> x in u, path)[end]
    for (ind, sym) in enumerate(path[indx+1:end])
     forward_path(d, path[indx+ind:end]) && push!(aset, sym)
    end
    setdiff!(aset, [path[end]])
    push!(asets, aset)
  end
  if length(asets) > 0
    return blocking_sets(asets)
  else
    return asets
  end
end

function adjustment_sets(d::DAG, f::Symbol, l::Symbol)
  asets = Vector{Symbol}[]
  allpaths  = all_paths(d, f, l)
  backdoorpaths = backdoor_paths(d, allpaths, f)
  for path in open_paths(d, backdoorpaths)
    aset = Symbol[]
    for (ind, sym) in enumerate(path[2:end])
     forward_path(d, path[ind+1:end]) && push!(aset, sym)
    end
    setdiff!(aset, [path[end]])
    push!(asets, aset)
  end
  if length(asets) > 0
    return blocking_sets(asets)
  else
    return asets
  end
end

export
  adjustment_sets