using Documenter, StructuralCausalModels

makedocs(
    modules = [StructuralCausalModels],
    format = Documenter.HTML(; prettyurls = get(ENV, "CI", nothing) == "true"),
    authors = "Rob J Goedman",
    sitename = "StructuralCausalModels.jl",
    pages = Any["index.md"]
    # strict = true,
    # clean = true,
    # checkdocs = :exports,
)

deploydocs(
    repo = "github.com/StatisticalRethinkingJulia/StructuralCausalModels.jl.git",
    push_preview = true
)
