function to_graphviz(d::DAG, file::AbstractString)
  isfile(file) && rm(file)
  io = open(file, "w")
  gs = topological_sort(d.a)
  write(io, "digraph $(d.name) {\n")
  vars = names(gs, 1)
  for var in vars
    for (ind, entry) in enumerate(gs[:, var])
      if entry == 1
        write(io, "  $(var) -> $(vars[ind]);\n")
      end
    end
  end
  write(io, "}\n")
  close(io)
end

export
  to_graphviz