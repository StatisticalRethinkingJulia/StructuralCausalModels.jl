"""

# dag_vars

$(SIGNATURES)

Internal
"""
function dag_vars(d::OrderedDict)
  vars = Symbol[]
  for var in keys(d)
    if isnothing(var)
      @warn "LHS can't be an empty set: $var."
    elseif typeof(var) == Symbol
      append!(vars, [var])
      handle_rhs!(vars, d[var])
    elseif typeof(var) == Vector{Symbol}
      append!(vars, var)
      handle_rhs!(vars, d[var])
    end
  end
  unique(vars)
end

function handle_rhs!(vars::Vector{Symbol}, rhs::SymbolList)
  if isnothing(var)
    append!(vars, [])
  elseif typeof(rhs) == Symbol
    append!(vars, [rhs])
  elseif typeof(rhs) == Vector{Symbol}
    append!(vars, rhs)
  end
end

"""

# edge_matrix

$(SIGNATURES)

Internal
"""
function edge_matrix(d::OrderedDict)
  vars = dag_vars(d)
  l = length(vars)
  a = zeros(Int, l, l)

  for key in keys(d)
    if typeof(key) == Symbol
      for (ind, var) in enumerate(vars)
        # Is this Symbol the LHS
        if var == key
          # What's in the RHS
          for (i, vr) in enumerate(vars)
            if typeof(d[key]) == Symbol
              a[i, ind] += vr == d[key] ? 1 : 0
            elseif typeof(d[key]) == Vector{Symbol}
              if vr in d[key]
                a[i, ind] += 1
              end
            end
          end
        end
      end
    elseif typeof(key) == Vector{Symbol}
      for (ind, var) in enumerate(vars)
        # Are these Symbols (key could hold a single Symbol) the LHS
        if var in key
          for (i, vr) in enumerate(vars)
            if typeof(d[key]) == Symbol
              a[i, ind] += vr == d[key] ? 1 : 0
            elseif typeof(d[key]) == Vector{Symbol}
              if vr in d[key]
                a[i, ind] += 1
              end
            end
          end
        end
      end
    end
  end
  NamedArray(Int.(a), (vars, vars), ("Rows", "Cols"))
end

"""

# edge_matrix

$(SIGNATURES)

Internal
"""
function edge_matrix(a::NamedArray, inv=false)
  a = sign.(a)
  if inv
    ord = topological_order(a)
    ord = reverse(ord)
    a = a[ord, ord]
  end
  transpose(a) + I(size(a, 1))
end

"""

# adjacency_matrix

$(SIGNATURES)

Internal
"""
function adjacency_matrix(d::OrderedDict)
  transpose(edge_matrix(d))
end

"""

# adjacency_matrix

$(SIGNATURES)

Internal
"""
function adjacency_matrix(e::NamedArray)
  a = transpose(e)
  a - I(size(a, 1))
end

"""

# undirected_matrix

$(SIGNATURES)

Internal
"""
function undirected_matrix(d::DAG)
  u = Array(d.a) + Array(d.e)
  vars = names(d.a)[1]
  NamedArray(Int.(u), (vars, vars), ("Rows", "Cols"))
end


"""

# topological_order

$(SIGNATURES)

Internal
"""
function topological_order(a::NamedArray)
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
  reverse(ord)
end

"""

# topological_sort

$(SIGNATURES)

Internal
"""
function topological_sort(a::NamedArray)
  ord = topological_order(a)
  a[ord, ord]
end

export
  dag_vars,
  adjacency_matric,
  edge_matrix,
  topological_sort,
  topological_order