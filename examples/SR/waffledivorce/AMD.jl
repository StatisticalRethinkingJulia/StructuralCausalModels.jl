# Load Julia packages (libraries) needed for clip

using StatisticalRethinking
using StatsPlots

ProjDir = @__DIR__

for i in 1:3
  include(scm_path("..", "examples", "SR", "waffledivorce", "AMD_m$i.jl"))
end

if success(rc)

  r1 = plotcoef([m5_1s, m5_2s, m5_3s], [:bA, :bM], "$(ProjDir)/AMD.png",
    "Particles (Normal) estimates")
  display(r1)

end