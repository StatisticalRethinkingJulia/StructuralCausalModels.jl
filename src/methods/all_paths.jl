"""

# `node_edges`

$(SIGNATURES)

Internal.
"""
function node_edges(p::Path, s::Symbol, l::Symbol; debug=false)
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
function all_paths(d::DAG, f::Symbol, l::Symbol; debug=false)
  paths = Vector{Symbol}[]
  p = Path(d, f, l)
  debug && println(p)
  stack = Path[]
  push!(stack, p)
  debug && println("Starting stack = $(stack)\n")
  while !isempty(stack)
    debug && println("stack = $(stack)\n")
    p = pop!(stack)
    debug && println("stack = $(stack)\n")
    if length(p.visited) == 1 && p.visited == p.next
      p.next = node_edges(p, p.next[1][2], p.next[1][1]; debug=debug)
      debug && println(length(p.next))
      if length(p.next) > 1
        # Add a new Path to the stack
        for next in p.next[2:end]
          debug && println(next)
          np = deepcopy(p)
          np.next = p.next
          push!(stack, np)
          debug && println(stack)
        end
      end
    end
    debug && println("p.next = $(p.next)\n")
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