using Documenter, StructuralCausalModels

DOC_ROOT = scm_path("..", "docs")
DocDir =  joinpath(DOC_ROOT, "src")

page_list = Array{Pair{String, Any}, 1}();
append!(page_list, [Pair("Introduction", "introduction.md")]);
append!(page_list, [Pair("Walkthrough", "scm.md")]);
append!(page_list, [Pair("DAGs", "dag.md")]);
append!(page_list, [Pair("Basis set", "basis_set.md")]);
append!(page_list, [Pair("Paths", "paths.md")]);
append!(page_list, [Pair("Adjustment_sets", "adj_sets.md")]);
append!(page_list, [Pair("D separation", "d_sep.md")]);
append!(page_list, [Pair("Shipley test", "shipley_test.md")]);
append!(page_list, [Pair("Partial correlation", "pcor.md")]);
append!(page_list, [Pair("Ancestral graph", "ag.md")]);
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
