
"""
    ModelFrame(formula, data; model=StatisticalModel, contrasts=Dict())

Wrapper that encapsulates a `FormulaTerm`, schema, data table, and model type.

This wrapper encapsulates all the information that's required to transform data
of the same structure as the wrapped data frame into a model matrix (the
`FormulaTerm`), as well as the information about how that formula term was
instantiated (the schema and model type)

Creating a model frame involves first extracting the [`schema`](@ref) for the
data (using any contrasts provided as hints), and then applying that schema with
[`apply_schema`](@ref) to the formula in the context of the provided model type.

# Constructors

```julia
ModelFrame(f::FormulaTerm, data; model::Type{M} = StatisticalModel, contrasts::Dict = Dict())
```

# Fields

* `f::FormulaTerm`: Formula whose left hand side is the *response* and right hand
  side are the *predictors*.
* `schema::Any`: The schema that was applied to generate `f`.
* `data::D`: The data table being modeled.  The only restriction is that `data` 
  is a table (`Tables.istable(data) == true`)
* `model::Type{M}`: The type of the model that will be fit from this model frame.

# Examples

```julia
julia> df = (x = 1:4, y = 5:8)
julia> mf = ModelFrame(@formula(y ~ 1 + x), df)
```
"""
function d_separation()

end

export
  d_separations