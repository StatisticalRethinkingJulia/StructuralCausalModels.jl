
"""

# d_separation

$(SIGNATURES)

Part of the exported API
"""
function d_separation(d::DAG, first::Vector{Symbol}, second::Vector{Symbol},
  cond::SymbolList=nothing)

  e = induced_covariance_graph(d, vcat(first, second), cond)
  sum(e[vcat(first, second), vcat(first, second)]) == 0

end

export
  d_separation
