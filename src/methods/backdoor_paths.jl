"""

# `backdoor_paths`

$(SIGNATURES)

Internal.
"""
function backdoor_paths(d::DAG, paths::Vector{Vector{Symbol}}, f::Symbol)
  backdoors = Vector{Symbol}[]
  for path in paths
    if d.a[f, path[2]] == 1
      append!(backdoors, [path])
    end
  end
  backdoors
end

export
  backdoor_paths