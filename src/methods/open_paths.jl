"""

# `check_open`

$(SIGNATURES)

Internal.
"""
function check_open(d::DAG, path::Vector{Symbol}, conditioning_set::SymbolList; debug=false)
  res = :open
  path_closed = false
  if typeof(conditioning_set) == Symbol
    conditioning_set = [conditioning_set]
  end
  r = range(2, stop=length(path)-1)
  for i in r

    # If this vertex a collider, i.e. path[i-1] -> path[i] <- path[i+1], and 
    # 1. not in the conditioning_set or
    # 2. has does not have a causal descendent that is in the conditioning set
    # then the path is blocked

    vertex_type = :unknown

    if d.a[path[i], path[i-1]] == 1 && d.a[path[i], path[i+1]] == 1
      vertex_type = :collider
      if length(conditioning_set) > 0
        path_closed = path[i] in conditioning_set ? false : true
      else
        path_closed = true
      end
    elseif d.a[path[i-1], path[i]] == 1 && d.a[path[i+1], path[i]] == 1
      vertex_type = :fork
      if length(conditioning_set) > 0
        path_closed = path[i] in conditioning_set ? true : false
      else
        path_closed = false
      end
    elseif d.a[path[i-1], path[i]] == 1 && d.a[path[i], path[i+1]] == 1 ||
        d.a[path[i], path[i-1]] == 1 && d.a[path[i+1], path[i]] == 1
      vertex_type = :chain
      if length(conditioning_set) > 0
        path_closed = path[i] in conditioning_set ? true : false
      else
        path_closed = false
      end
    else
      @info "Vertex_type of $(path[i]) = $(vertex_type)\n"*
        ""
    end
    debug && println("Vertex_type of $(path[i]) in $path = $(vertex_type)\n")

    # If path_closed == true, check that no causal descendent 
    # of path[i] is in conditioning set. If that is the case,
    # flip the state. Max_length of decendent path? What if forks
    # in the path?

    res = path_closed ? :closed : :open
    res == :closed && break
  end
  res
end

"""

# `open_paths`

$(SIGNATURES)

Internal.
"""
function open_paths(d::DAG, paths::Vector{Vector{Symbol}}, cond::Vector{Symbol};debug=false)
  open = fill(:closed, length(paths))
  for (ind, path) in enumerate(paths)
    if length(path) == 2
      if d.a[path[2], path[1]] == 1 
        open[ind] = :open
      end
    else
      open[ind] = check_open(d, path, cond)
    end
  end
  ind = findall(x -> x == :open, open)
  paths[ind]
end

export
  open_paths,
  check_open