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

function induced_covariance_graph(d::DAG, sel::Vector{Symbol}, cond::SymbolList;
  debug=false)

  @assert all([c in d.vars for c in sel]) "Selection nodes are not among vertices."

  if length(cond) > 0
    @assert all([c in d.vars for c in cond]) "Conditioning nodes are not among vertices."
    @assert !all([c in sel for c in cond]) "Conditioning nodes in selected nodes."
  end
  
  l = setdiff(d.vars, union(sel, cond))
  println(l)
  l = union(l, sel)
  println(l)
  r = union(sel, cond)
  println(r)
   
  e = edge_matrix(d.a)                  # From adjacency matrix to edge matrix
  println(e)
  al = ancester_graph(e[l, l])
  println(al)
  if length(cond) > 0
    trl = indicator_matrix( e[cond, l] * al)
  else
    trl = al - al
  end
  println(trl)
  dlr = indicator_matrix(I(length(l)) + transpose(trl) * trl)
  println(dlr)
  cl = transitive_closure(dlr)
  println(cl)
  out = indicator_matrix(( al * cl * transpose(al)))
  println(out)
  out = out[sel, sel]
  println(out)
  println(adjacency_matrix(out))
  adjacency_matrix(out)

end
