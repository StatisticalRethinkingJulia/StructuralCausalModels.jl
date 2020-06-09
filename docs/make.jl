using Documenter, StructuralCausalModels

DOC_ROOT = scm_path("..", "docs")
DocDir =  joinpath(DOC_ROOT, "src")

page_list = Array{Pair{String, Any}, 1}();
append!(page_list, [Pair("Introduction", "introduction.md")]);
append!(page_list, [Pair("Walkthrough", "walkthrough.md")]);
append!(page_list, [Pair("Versions", "versions.md")]);
append!(page_list, [Pair("Acknowledgements", "acknowledgements.md")]);
append!(page_list, [Pair("References", "references.md")]);
append!(page_list, [Pair("StructuralCausalModels API", "index.md")]);

makedocs(
    modules = [StructuralCausalModels],
    format = Documenter.HTML(; prettyurls = get(ENV, "CI", nothing) == "true"),
    authors = "Rob J Goedman",
    sitename = "StructuralCausalModels.jl",
    pages = page_list
    # strict = true,
    # clean = true,
    # checkdocs = :exports,
)

deploydocs(
    repo = "github.com/StatisticalRethinkingJulia/StructuralCausalModels.jl.git",
    push_preview = true
)
