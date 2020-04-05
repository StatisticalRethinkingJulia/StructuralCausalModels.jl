"""

# ancestor_graph

$(SIGNATURES)

Internal
"""
function ancester_graph(e::NamedArray)
  if sum(size(e)) == 0
    return(e)
  end
  indicator_matrix(Int.(inv(2 * I(size(e, 1)) - e)))
end

"""

# indicator_matrix

$(SIGNATURES)

Internal
"""
function indicator_matrix(e::NamedArray)
  for i in 1:size(e, 1)
    for j in 1:size(e, 2)
      e[i, j] = e[i, j] == 0 ? 0 : 1
    end
  end
  e
end

"""

# transitive_closure

$(SIGNATURES)

Internal
"""
function transitive_closure(a::NamedArray)
  size(a, 1) == 1 && return(a)
  r = copy(a)
  while true
    b = sign.( r * r )
    all( b == r ) && break
    r = b
  end
  r
end

"""

# induced_covariance_graph

$(SIGNATURES)

Internal
"""
function induced_covariance_graph(d::DAG, sel::Vector{Symbol}, cond::SymbolList; debug=false)

  @assert all([c in d.vars for c in sel]) "Selection nodes are not among vertices."

  if length(cond) > 0
    @assert all([c in d.vars for c in cond]) "Conditioning nodes are not among vertices."
    @assert !all([c in sel for c in cond]) "Conditioning nodes in selected nodes."
  end
  
  l = setdiff(d.vars, union(sel, cond))
  debug && println(l)
  l = union(l, sel)
  debug && println(l)
  r = union(sel, cond)
  debug && println(r)
   
  e = edge_matrix(d.a)                  # From adjacency matrix to edge matrix
  debug && println(e)
  al = ancester_graph(e[l, l])
  debug && println(al)
  if length(cond) > 0
    trl = indicator_matrix( e[cond, l] * al)
  else
    trl = al - al
  end
  debug && println(trl)
  dlr = indicator_matrix(I(length(l)) + transpose(trl) * trl)
  debug && println(dlr)
  cl = transitive_closure(dlr)
  debug && println(cl)
  out = indicator_matrix(( al * cl * transpose(al)))
  debug && println(out)
  out = out[sel, sel]
  debug && println(out)
  debug && println(adjacency_matrix(out))
  adjacency_matrix(out)

end
