function model_vaccines(dy, y, p, t)

    #=
    Parameter specification
    =#
    k  = convert(Int64, p[1])                    #Número de categorías en el modelo
    if (t > 300)
        ν  = p[2:(k + 1)]            #Tasa de vacunación
        ω  = p[(k + 2):(2*k + 1)]    #Tasa en la que vacunados se desvacunan
        q  = p[(2*k + 2):(3*k + 1)]  #(1 - Eficacia de la vacuna)
    else
        ν  = repeat([0], cat_num)            #Tasa de vacunación
        ω  = repeat([0], cat_num)
        q  = repeat([0], cat_num)
    end
    β  = p[(3*k + 2):(4*k + 1)]  #Tasa de contagio como función de t
    λ  = p[(4*k + 2):(5*k + 1)]  #Tasa de hospitalizacion
    θ1 = p[(5*k + 2):(6*k + 1)]  #Tasa de recuperación para I
    m1 = p[(6*k + 2):(7*k + 1)]  #Tasa de mortalidad para I
    m2 = p[(7*k + 2):(8*k + 1)]  #Tasa de mortalidad para H
    θ2 = p[(8*k + 2):(9*k + 1)]  #Tasa de recuperación para H
    γ  = p[(9*k + 2):(10*k + 1)] #Tasa de reconversión de anticuerpos
    κ  = p[(10*k + 2):(11*k + 1)]
    d  = p[11*k + 2]
    t0 = p[11*k + 3]
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
    Isum = sum(I) + sum(V)
    for cat = 1:k
        dy[cat]       = q[cat]*ω[cat]*V[cat] + γ[cat]*R[cat] -(β[cat]*(1 + κ[cat]*cos( 2*pi*(t + t0) / d))*Isum + ν[cat])*S[cat]
        dy[k + cat]   = β[cat]*(1 + κ[cat]*cos( 2*pi*(t + t0) / d))*Isum*S[cat] - (λ[cat] + m1[cat] + θ1[cat])*I[cat]
        dy[2*k + cat] = λ[cat]*I[cat] - (m1[cat] + θ2[cat])*H[cat]
        dy[3*k + cat] = θ1[cat]*I[cat] + θ2[cat]*H[cat] - γ[cat]*R[cat]
        dy[4*k + cat] = m1[cat]*I[cat] + m2[cat]*H[cat]
        dy[5*k + cat] = ν[cat]*S[cat] - q[cat]*ω[cat]*V[cat]
    end

end
