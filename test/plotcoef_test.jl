import StatisticalRethinking: plotcoef

ProjDir = @__DIR__

models=[m5_1s, m5_2s, m5_3s];
pars=[:bA, :bM];
fig="$(ProjDir)/AMD_fork_1.png"
title="Particles (Normal) estimates"

function plotcoef(models::Vector{SampleModel}, pars::Vector{Symbol}, fig::AbstractString, 
  title="", func=nothing)

  mnames = [models[i].name for i in 1:length(models)]
  levels = length(models) * (length(pars) + 1)
  colors = [:blue, :red, :green, :darkred, :black]

  s = Vector{NamedTuple}(undef, length(models))
  for (mindx, mdl) in enumerate(models)
    if isnothing(func)
      s[mindx] = read_samples(mdl; output_format=:particles)
    else
      dfs = read_samples(mdl; output_format=:dataframe)      
      s[mindx] = func(dfs)
    end
  end

  xmin = 0; xmax = 0.0
  for i in 1:length(s)
    for par in pars
      syms = Symbol.(keys(s[i]))
      if Symbol(par) in syms
        mp = mean(s[i][Symbol(par)])
        sp = std(s[i][Symbol(par)])
        xmin = min(xmin, mp - sp)
        xmax = max(xmax, mp + sp)
      end
    end
  end

  ylabs = String[]
  for j in 1:length(models)
    for i in 1:length(pars)
      l = length(String(pars[i]))
      str = repeat(" ", 9-l) * String(pars[i])
      append!(ylabs, [str])
    end
    l = length(models[j].name)
    str = models[j].name * repeat(" ", 9-l)
    append!(ylabs, [str])
  end

  ys = [string(ylabs[i]) for i = 1:length(ylabs)]
  plot(xlims=(xmin, xmax), leg=false, framestyle=:grid)
  title!(title)
  yran = range(1, stop=length(ylabs), length=length(ys))
  yticks!(yran, ys)

  line = 0
  for (mindx, model) in enumerate(models)
    line += 1
    hline!([line] .+ length(pars), color=:darkgrey, line=(2, :dash))
    for (pindx, par) in enumerate(pars)
      line += 1
      syms = Symbol.(keys(s[mindx]))
      if Symbol(par) in syms
        ypos = (line - 1)
        mp = mean(s[mindx][Symbol(par)])
        sp = std(s[mindx][Symbol(par)])
        plot!([mp-sp, mp+sp], [ypos, ypos], leg=false, color=colors[pindx])
        scatter!([mp], [ypos], color=colors[pindx])
      end
    end
  end
  savefig(fig)
  s
end

plotcoef(models, pars, fig, title)
