import Base: show

mutable struct Path
  ug::NamedArray
  f::Symbol
  l::Symbol
  path::Vector{Symbol}
  visited::Vector{Vector{Symbol}}
  next::Vector{Vector{Symbol}}
end

Path(d::DAG, f, l) = Path(
  undirected_matrix(d),                # Create undirected graph from DAG
  f,                                   # Path continuation symbol
  l,                                   # Path completed symbol
  [f],                                 # Symbols on path
  [[l, f]],                            # Edges already walked
  [[l, f]]                             # Edge to do next
)

function path_show(io::IO, p::Path)
  println(io, "(f = $(p.f), l = $(p.l))")
  println(io, "path = $(p.path)")
  println(io, "visited = $(p.visited)")
  println(io, "next = $(p.next)")
end

show(io::IO, p::Path) = path_show(io, p)

export
  Path
