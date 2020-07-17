# Is a macro useful for the helper functions in AG(), MAG(), etc?

module C  
  macro makefunc(ex)
    return esc(quote
      wat() = $ex
    end)
  end
end

using .C

C.@makefunc(345)

wat() |> display
