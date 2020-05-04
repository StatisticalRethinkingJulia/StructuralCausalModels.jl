function all_paths(d::DAG, f::Symbol, l::Symbol)
  paths = Vector{Symbol}[]
  stack = [Path(StructuralCausalModels.undirected_matrix(d), f, l)];
  while length(stack) > 0
    p = pop!(stack);
    p = all_paths(p);
    symbol_list = copy(p.symbol_stack)
    for sym in symbol_list
      #println(sym)
      if sym == l || false
        path = deepcopy(p.path)
        push!(path, sym)
        setdiff!(p.symbol_stack, [sym])
        if !(path in paths)
          push!(paths, path)
          #println("Added $(p.path) to paths as $path")
        end
      elseif size(p.ug, 1) > 1
        newp = deepcopy(p)
        newp.f = sym
        newp.symbol_stack = Symbol[]
        push!(stack, newp)
      end
    end
  end
  paths
end

function all_paths(p::Path)
  p.f in p.visited && return p
  vars = names(p.ug)[1]
  e = Symbol.(vars[findall(e -> e == 1, p.ug[p.f, :])])
  is = setdiff(names(p.ug)[1], [p.f])
  p.ug = p.ug[is, is]
  setdiff!(e, [p.f])
  push!(p.symbol_stack, e...)
  union!(p.visited, [p.f])
  setdiff!(p.symbol_stack, p.visited)
  size(p.ug, 1) > 1 && push!(p.path, p.f)
  p
end

export
  all_paths