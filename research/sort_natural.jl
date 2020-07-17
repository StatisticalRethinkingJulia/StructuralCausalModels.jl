function natural(x, y)
    k(x) = [occursin(r"\d+", s) ? parse(Int, s) : s 
            for s in split(replace(x, r"\d+" => s->" $s "))]
    A = k(x); B= k(y)    
    for (a, b) in zip(A, B)
        if !isequal(a, b)
            return typeof(a) <: typeof(b) ? isless(a, b) :
                   isa(a,Int) ? true : false
        end
    end
    return length(A) < length(B)
end

sort(["a1", "a2", "a10"]) |> display

sort(["a1", "a2", "a10"], lt=natural) |> display
