function ancester_graph(e::NamedArray)
  if sum(size(e)) == 0
    return(e)
  end
  indicator_matrix(Int.(inv(2 * I(size(e, 1)) - e)))
end

function indicator_matrix(e::NamedArray)
  for i in 1:size(e, 1)
    for j in 1:size(e, 2)
      e[i, j] = e[i, j] == 0 ? 0 : 1
    end
  end
  e
end

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

function induced_covariance_graph(d::DAG, sel::Vector{Symbol}, cond::SymbolList)

  @assert all([c in d.vars for c in sel]) "Selection nodes are not among vertices."
  @assert all([c in d.vars for c in cond]) "Conditioning nodes are not among vertices."
  @assert !all([c in sel for c in cond]) "Conditioning nodes in selected nodes."

  l = setdiff(d.vars, union(sel, cond))
  l = union(l, sel)
  r = union(sel, cond)

  e = edge_matrix(d.a)                  # From adjacency matrix to edge matrix
  al = ancester_graph(e[l, l])
  trl = indicator_matrix( e[cond, l] * al)
  dlr = indicator_matrix(I(length(l)) + transpose(trl) * trl)
  cl = transitive_closure(dlr)
  out = indicator_matrix(( al * cl * transpose(al)))
  out = out[sel, sel]
  adjacency_matrix(out)

end
