import RDatasets
iris = RDatasets.dataset("datasets", "iris")
using StatsPlots, Interact
using Blink
w = Window()
body!(w, dataviewer(iris))