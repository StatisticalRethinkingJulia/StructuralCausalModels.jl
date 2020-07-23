function maximize(a::NamedArray)

  amat = copy(a)
  #local sr
  #local one, two, onearrow, onearc, twoarrow, twoarc
  na = size(amat, 1)
  at = findall(amat .+ transpose(amat) .+ I(na) .== 0)
  if length(at) > 0
    for i in 1:length(at)
      s = [at[i][1], at[i][2]]
      st = Int[]
      while true
        st = s
        for j in s
          for k in 1:na
            if mod10(amat[k, j]) == 1
              s = vcat(k, filter(x -> !(k in s), s))
            end
          end
        end
        s == st && break
      end
      one = at[i][1]
      two = at[i][2]
      onearrow = Int[]
      onearc = Int[]
      twoarrow = Int[]
      twoarc = Int[]
      sr = s
      #println("sr = $sr")
      #sr = filter(x -> !(at[i][1] in sr), sr)
      #sr = filter(x -> !(at[i][2] in sr), sr)
      #println("sr = $sr")
      if length(sr) > 0
        for j in sr
          if mod10(amat[one, j]) == 1
            onearrow = vcat(onearrow, j)
          end
          if amat[one, j] > 99
            onearc = vcat(onearc, j)
          end
        end
        for j in sr
          if mod10(amat[two, j]) == 1
            for k in onearc
              if j == k || amat[j, k] > 99
                amat[at[i][2], at[i][1]] = 1
              end
              twoarrow = vcat(twoarrow, j)
            end
          end
          if amat[two, j] > 99
            for k in onearrow
              if j == k || amat[j, k] > 99
                amat[at[i][1], at[i][2]] = 1
              end
            end
            for k in onearc
              if j == k || amat[j, k] > 99
                amat[at[i][1], at[i][2]] = 100
                amat[at[i][2], at[i][1]] = 100
              end
            end
            twoarc = vcat(twoarc, j)
          end
        end
      end
      if length(vcat(onearc, onearrow, twoarc, twoarrow)) > 0
  
        for j in vcat(onearc,onearrow,twoarc,twoarrow)
          sr = filter(x -> !(j in sr), sr)
        end
        onearcn = Int[]
        twoarcn = Int[]
        onearrown = Int[]
        twoarrown = Int[]
        while true
          for l in onearc
            for j in sr
               if amat[l, j] > 99
                onearcn = vcat(onearcn, j)
              end
            end
          end
          for l in onearrow
            for j in sr
              if amat[l, j] > 99
                onearrown = vcat(onearrown, j)
              end
            end
          end
          for l in twoarc
            for j in sr
              if amat[l, j] > 99
                for k in onearrow
                  if j==k || amat[j, k] > 99
                    amat[at[i][1], at[i][2]] = 1
                  end
                end
                for k in onearc
                  if j==k || amat[j, k] > 99
                    amat[at[i][1], at[i][2]] = 100
                    amat[at[i][2], at[i][1]] = 100
                  end
                end
                twoarcn = vcat(twoarcn, j)
              end                  
            end
          end
          for l in twoarrow
            for j in sr
              if amat[l,j] > 99
                for k in onearc
                  if j==k || amat[j, k] > 99
                    amat[at[i][1], at[i][2]] = 100
                    amat[at[i][2], at[i][1]] = 100
                  end
                end
                twoarrown = vcat(twoarrown, j)
              end
            end
          end

          length(vcat(onearcn,onearrown,twoarcn,twoarrown)) == 0 && break
          
          for j in vcat(onearcn,onearrown,twoarcn,twoarrown)
            sr = filter(x -> !(j in sr), sr)
          end

          onearc = onearcn
          twoarc = twoarcn
          onearrow = onearrown
          twoarrow = twoarrown
          onearcn = Int[]
          twoarcn = Int[]
          onearrown = Int[]
          twoarrown = Int[]
        end
      end
    end
  end

  amat

end

export
  maximize