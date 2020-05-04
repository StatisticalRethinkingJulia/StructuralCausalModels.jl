function check_open(d::DAG, path::Vector{Symbol})
  res = :open
  r = range(2, stop=length(path)-1)
  for i in r
    cond = d.a[path[i], path[i-1]] == 1 && d.a[path[i], path[i+1]] == 1
    res = cond ? :closed : :open
    res == :closed && break
  end
  res
end

function open_paths(d::DAG, paths::Vector{Vector{Symbol}})
  open = fill(:closed, length(paths))
  for (ind, path) in enumerate(paths)
    if length(path) == 2
      if d.a[path[2], path[1]] == 1 
        open[ind] = :open
      end
    else
      open[ind] = check_open(d, path)
    end
  end
  ind = findall(x -> x == :open, open)
  paths[ind]
end

export
  open_paths