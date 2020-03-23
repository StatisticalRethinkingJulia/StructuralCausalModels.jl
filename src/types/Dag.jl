struct Dag
  d::OrderedDict{Symbol, Vector{Symbol}}
  a::Matrix{Int}
  e::Matrix{Int}
  cov_matrix::Matrix{Float64}
  data::DataFrame
  vars::Vector{Symbol}
  order::Vector{Int}
end

# Constructor

function Dag(d::OrderedDict{Symbol, Vector{Symbol}};
  df=DataFrame())
  vars = dag_vars(d)
  a = adjacency_matrix(d)
  e = edge_matrix(d)
  if nrow(df) == 0
    cov_matrix = zeros(Float64, length(vars), length(vars))
  else
    cov_matrix = cov(Array(df))
  end
  order = top_order(a)
  Dag(d, a, e, cov_matrix, df, vars, order)
end

export
  Dag