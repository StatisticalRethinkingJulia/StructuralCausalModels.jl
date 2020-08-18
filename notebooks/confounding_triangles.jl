### A Pluto.jl notebook ###
# v0.11.4

using Markdown
using InteractiveUtils

# ╔═╡ cce045ac-d373-11ea-29af-5be52ed80e36
md"## Confounding triangles notebook"

# ╔═╡ 30cfc76c-d276-11ea-1e1a-6567e0173d69
using DrWatson

# ╔═╡ cfa1909e-d278-11ea-215d-4f34edbb8294
using StructuralCausalModels

# ╔═╡ dde39c20-d272-11ea-00b1-71e8b3c09e79
cd(joinpath(homedir(), ".julia/dev/StructuralCausalModels"))

# ╔═╡ 3ed7655e-d276-11ea-185b-29e9192ba4f0
@quickactivate "StructuralCausalModels"

# ╔═╡ 742ca656-d276-11ea-04ab-f36281c92836
d_string = "dag {A -> {E Z}; B -> {D Z}; Z -> {D E}; E -> D}";

# ╔═╡ 739fdd34-d276-11ea-0632-13aeca3d3aa5
dag = DAG("conf_triangles", d_string);

# ╔═╡ 98ce6cf6-d276-11ea-15d5-856fba9a3c50
dag.a

# ╔═╡ 182e0140-d374-11ea-254b-8f4a175f4f06
dag.d

# ╔═╡ eb94dbbc-d278-11ea-2f78-9bd8a49d14d9
fname = projectdir() * "/conf_triangles.dot";

# ╔═╡ eafca4d2-d278-11ea-08ce-11367ae96f69
to_graphviz(dag, fname);

# ╔═╡ 59229f0e-d277-11ea-0bc8-cb68e38ae3b3
Sys.isapple() && run(`open -a GraphViz.app $(fname)`);

# ╔═╡ 1223d8ee-d279-11ea-2ba4-7dbae68007ff
to_ggm(dag)

# ╔═╡ 11b28448-d279-11ea-1d52-2544424ea533
bs = basis_set(dag)

# ╔═╡ 8fd4685c-d374-11ea-3b9c-670bb9da8a7c
bs.bs

# ╔═╡ 2b60ff06-d2cd-11ea-0bf6-050ad1720b31
f = :A

# ╔═╡ 4dc673b4-d2cd-11ea-1ca4-219bfa2bf57d
d = :B

# ╔═╡ 582b9c98-d279-11ea-2daf-93e7a13c9966
d_separation(dag, f, :B)

# ╔═╡ 57534fc8-d279-11ea-0b9b-a99db30bc7b4
d_separation(dag, d, :E)

# ╔═╡ 5712eff0-d279-11ea-3978-2106eb0feb76
d_separation(dag, d, :E; c=[:A, :Z])

# ╔═╡ 49e58f04-d8c8-11ea-19e0-cb45efe39493
m_separation(dag, d, :E; c=[:A, :Z])

# ╔═╡ Cell order:
# ╠═cce045ac-d373-11ea-29af-5be52ed80e36
# ╠═dde39c20-d272-11ea-00b1-71e8b3c09e79
# ╠═30cfc76c-d276-11ea-1e1a-6567e0173d69
# ╠═3ed7655e-d276-11ea-185b-29e9192ba4f0
# ╠═742ca656-d276-11ea-04ab-f36281c92836
# ╠═cfa1909e-d278-11ea-215d-4f34edbb8294
# ╠═739fdd34-d276-11ea-0632-13aeca3d3aa5
# ╠═98ce6cf6-d276-11ea-15d5-856fba9a3c50
# ╠═182e0140-d374-11ea-254b-8f4a175f4f06
# ╠═eb94dbbc-d278-11ea-2f78-9bd8a49d14d9
# ╠═eafca4d2-d278-11ea-08ce-11367ae96f69
# ╠═59229f0e-d277-11ea-0bc8-cb68e38ae3b3
# ╠═1223d8ee-d279-11ea-2ba4-7dbae68007ff
# ╠═11b28448-d279-11ea-1d52-2544424ea533
# ╠═8fd4685c-d374-11ea-3b9c-670bb9da8a7c
# ╠═2b60ff06-d2cd-11ea-0bf6-050ad1720b31
# ╠═4dc673b4-d2cd-11ea-1ca4-219bfa2bf57d
# ╠═582b9c98-d279-11ea-2daf-93e7a13c9966
# ╠═57534fc8-d279-11ea-0b9b-a99db30bc7b4
# ╠═5712eff0-d279-11ea-3978-2106eb0feb76
# ╠═49e58f04-d8c8-11ea-19e0-cb45efe39493
