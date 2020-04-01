function dag_vars(d::OrderedDict{Symbol, Vector{Symbol}})
  vars = []
  for var in keys(d)
    append!(vars, [var])
    for rhsvar in d[var]
      if !(rhsvar in vars)
        append!(vars, [rhsvar])
        
      end
    end
  end
  unique(vars)
end

function edge_matrix(d::OrderedDict{Symbol, Vector{Symbol}})
  vars = dag_vars(d)
  l = length(vars)
  a = zeros(l, l)
  for (ind, var) in enumerate(vars)
    if !(var in keys(d))
      a[ind, :] = zeros(Int, l)
    else
      v = zeros(Int, l)
      for (i, vr) in enumerate(vars)
        if vr in d[var]
          v[i] = 1
        end
      end
      a[ind, :] = v
    end
  end
  na = NamedArray(Int.(a))
  setnames!(na, String.(vars), 1)
  setnames!(na, String.(vars), 2)
  na
end

function adjacency_matrix(d::OrderedDict{Symbol, Vector{Symbol}})
  transpose(edge_matrix(d))
end

function top_order(a::NamedArray)
  #@assert is_dag(a)
  n = size(a, 1)
  nod = 1:n
  indeg = zeros(Int, n)
  if sum(tril(a)) == sum(tril(ones(Int, n, n)))
    return nod
  end
  zero_indeg = Int[]
  for i in nod
    indeg[i] = sum(a[:, i])
    if indeg[i] == 0
      append!(zero_indeg, [i])
    end
  end
  s = 1
  ord = zeros(Int, n)
  while length(zero_indeg) > 0
    v = pop!(zero_indeg)
    ord[s] = v
    s += 1
    cs = filter(i -> a[v, i] !== 0, nod)
    length(cs) == 0 && continue
    for j in 1:length(cs)
      k = cs[j]
      indeg[k] = indeg[k] - 1
      indeg[k] == 0 && push!(zero_indeg, k)
    end
  end
  ord
end

function top_sort(a::NamedArray)
  ord = top_order(a)
  a[ord, ord]
end

export
  dag_vars,
  adjacency_matrix,
  edge_matrix,
  top_sort,
  top_order