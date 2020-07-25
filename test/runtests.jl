using StructuralCausalModels
using Test

include("test_dag_formulations.jl")
include("test_set_dag_df.jl")

include("test_dagitty_conversion.jl")
include("test_ggm_conversion.jl")

include("test_graphviz_conversions.jl")

include("test_basis_set_01.jl")

include("test_d_separation.jl")

include("test_descendents_01.jl")
include("test_descendents_02.jl")
include("test_descendents_03.jl")

include("test_open_paths_01.jl")
include("test_open_paths_02.jl")
include("test_open_paths_03.jl")
include("test_open_paths_04.jl")

include("test_ancestral_graph.jl")
include("test_ribbon_graph.jl")
include("test_maximize.jl")
#include("test_maximize_02.jl")
include("test_m_separation.jl")
include("test_m_separation_02.jl")
include("test_m_separation_03.jl")

include("test_sr6_4_2.jl")

include("test_sr6_4_3.jl")
  
include("test_sr6_4_3b.jl")

