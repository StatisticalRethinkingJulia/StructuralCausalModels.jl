function forward_path(d::DAG, path)
  res = true
  for (i, s) in enumerate(path[1:end-1])
    res = res && d.a[path[i+1], path[i]] == 1
  end
  res
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
  asets
end

export
  adjustment_sets