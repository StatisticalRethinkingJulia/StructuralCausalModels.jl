#=
`likGau` = function(K, S, n, k){
# deviance of the Gaussian model.
SK = S %*% K
tr = function(A) sum(diag(A))
(tr(SK) - log(det(SK)) - k) * n
}
=#


