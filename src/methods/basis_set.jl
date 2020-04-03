function basis_set(dag::DAG)
  as = topological_sort(dag.a)
  nod = dag.vars[topological_order(dag.a)]
  dv = 1:length(nod)

  # Correct if adj matixi is upper triangular
  @assert istriu(as) "Sorted adj matrix not upper triangular."

  ind = Vector{Vector{Symbol}}()
  for r in dv
    for s in r:length(nod)
      ((as[r, s] !== 0) || (s == r)) && continue
      ed = nod[[r, s]]
      pa_r = filter(i -> as[i, r] == 1, dv)
      pa_s = filter(i -> as[i, s] == 1, dv)
      dsep = union(nod[pa_r], nod[pa_s])
      dsep = setdiff(dsep, ed)
      append!(ed, dsep)
      append!(ind, [ed])
    end
  end
  ind
end
