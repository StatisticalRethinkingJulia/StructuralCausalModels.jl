using ComponentArrays
using OrdinaryDiffEq
using LinearAlgebra
using Unitful

function newton(du, u, p, t)
    mu = 398600.4418u"km^3/s^2"
    r = norm(u.r)
    du.r = u.v
    du.v = -mu .* u.r / r^3
end

r0 = [1131.340, -2282.343, 6672.423]u"km"
v0 = [-5.64305, 4.30333, 2.42879]u"km/s"
Δt = 86400.0*365u"s"
rv0 = ComponentArray(r=r0, v=v0)

prob = ODEProblem(newton, rv0, (0.0u"s", Δt))
@time sol = solve(prob, Vern8(), dt=100u"s", adaptive=false)
#sol |> display
println()

@time sol2 = solve(prob, Tsit5(), dt=100u"s",adaptive=false)

sol2.u[1].r |> display
sol2.u[1].v |> display
