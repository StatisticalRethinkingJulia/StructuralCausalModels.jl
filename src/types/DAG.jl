struct DAG
  d::OrderedDict{Symbol, Vector{Symbol}}      # DAG
  a::NamedArray                               # Adjacency matrix
  e::NamedArray                               # Edge matrix
  s::Matrix{Float64}                          # Covariance matrix
  df::DataFrame                               # DataFrame with variables
  vars::Vector{Symbol}                        # Names of variables
  order::Vector{Int}                          # Topological order
  nmap::Dict{Symbol, Int}                     # Mapping name to index for adj matrix
end

# Constructor

function DAG(d::OrderedDict{Symbol, Vector{Symbol}}, idf::DataFrame)

  vars = dag_vars(d)
  a = adjacency_matrix(d)
  e = edge_matrix(d)

  # Compute covariance matrix if nrow(df) > 0

  if nrow(idf) > 0
    @assert length(names(idf)) >= length(vars) "DataFrame has different number of columns"

    if names(idf) !== vars

      @info "Column sequence in data does not match vars in Dag"
      @info "DataFrame names: $(names(idf))"
      @info "Vars: $(vars)"
      @info "DataFrame columns re-ordered"

      df = DataFrame()
      for name in vars
        df[!, name] = idf[:, name]
      end

    else

      df = idf

    end

    s = cov(Array(df))
  end

  # Topological ordering

  order = top_order(a)

  # Create Dict[:name_symbol] => index in adj & covariance matrices

  nmap = Dict{Symbol, Int}()
  for (ind, sym) in enumerate(vars)
    nmap[sym] = ind
  end

  # Create object

  DAG(d, a, e, s, df, vars, order, nmap)

end

export
  DAG
