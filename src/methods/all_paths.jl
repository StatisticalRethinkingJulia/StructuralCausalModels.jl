"""

# `node_edges`

$(SIGNATURES)

Internal.
"""
function node_edges(p::Path, s::Symbol, l::Symbol)
  edges = Vector{Symbol}[]
  for sym in names(p.ug)[1]
    if p.ug[s, sym] == 1 && !(sym == l)
      append!(edges, [[s, sym]])
    end
  end
  edges
end

"""

# `all_edges`

$(SIGNATURES)

Part of the API, exported.
"""
function all_paths(d::DAG, f::Symbol, l::Symbol)
  paths = Vector{Symbol}[]
  p = Path(d, f, l)
  stack = Path[]
  push!(stack, p)
  while !isempty(stack)
    p = pop!(stack)
    if length(p.visited) == 1 && p.visited == p.next
      p.next = node_edges(p, p.next[1][2], p.next[1][1])
    end
    for edge in p.next
      setdiff!(p.next, [edge])
      tmp = node_edges(p, edge[2], edge[1])
      while !isempty(tmp)
        pnew = deepcopy(p)
        push!(pnew.visited, edge)       # Edge to visited
        push!(pnew.path, edge[2])       # Neighbor to path
        next_edge = pop!(tmp)
        setdiff!(pnew.next, [next_edge])
        if !(next_edge in pnew.visited)
          if next_edge[end] in pnew.path
            #println("Drop path, back to initial start.")
          elseif next_edge[end] == l
            push!(pnew.visited, [next_edge[2], l])
            push!(pnew.path, l)
            push!(paths, pnew.path)
          else
            pnew.next = [next_edge]
            push!(stack, pnew)
          end
        end
      end
    end
  end
  paths
end

export
  all_paths