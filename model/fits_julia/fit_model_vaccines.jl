function fit_model_vaccines(dy, y, p, t)

    #=
    Parameter specification
    =#
    k  = 3                      #Número de categorías en el modelo
    ν  = repeat([0],k)
    ω  = repeat([0],k)
    q  = repeat([0],k)
    β  = p[(0*k + 1):(1*k)]  #Tasa de contagio como función de t
    λ  = p[(1*k + 1):(2*k)]  #Tasa de hospitalizacion
    θ1 = p[(2*k + 1):(3*k)]  #Tasa de recuperación para I
    m1 = p[(3*k + 1):(4*k)]  #Tasa de mortalidad para I
    m2 = p[(4*k + 1):(5*k)]  #Tasa de mortalidad para H
    θ2 = p[(5*k + 1):(6*k)]  #Tasa de recuperación para H
    γ  = p[(6*k + 1):(7*k)] #Tasa de reconversión de anticuerpos
    κ  = p[(7*k + 1):(8*k)]
    d  = p[8*k + 1]
    t0 = p[8*k + 2]
    #=
    Variables included in model
    =#
    S  = y[1:k]
    I  = y[(k + 1):(2*k)]
    H  = y[(2*k + 1):(3*k)]
    R  = y[(3*k + 1):(4*k)]
    M  = y[(4*k + 1):(5*k)]
    V  = y[(5*k + 1):(6*k)]


    #=
    ODE model
    =#
    Isum = sum(I) 
    for cat = 1:k
        dy[cat]       = q[cat]*ω[cat]*V[cat] + γ[cat]*R[cat] -(β[cat]*(1 + κ[cat]*cos( 2*pi*(t + t0) / d))*Isum + ν[cat])*S[cat]
        dy[k + cat]   = β[cat]*(1 + κ[cat]*cos( 2*pi*(t + t0) / d))*Isum*S[cat] - (λ[cat] + m1[cat] + θ1[cat])*I[cat]
        dy[2*k + cat] = λ[cat]*I[cat] - (m1[cat] + θ2[cat])*H[cat]
        dy[3*k + cat] = θ1[cat]*I[cat] + θ2[cat]*H[cat] - γ[cat]*R[cat]
        dy[4*k + cat] = m1[cat]*I[cat] + m2[cat]*H[cat]
        dy[5*k + cat] = ν[cat]*S[cat] - q[cat]*ω[cat]*V[cat]
    end

end
