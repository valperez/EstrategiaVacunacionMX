cd("/Users/rod/Dropbox/EstatregiaVacunacionMX/")
include("model_one_vaccine_one_strain_one_state_by_age.jl")
include("fit_model_vaccines.jl")
@info "Included model code"
#Pkg.add(Pkg.PackageSpec(;name=“LearnBase”, version=“0.3”)
using RData, DifferentialEquations, Distributions, DiffEqBayes,
        NamedArrays, DataFrames, CSV, Plots, StatsPlots,
        DifferentialEquations.EnsembleAnalysis,
        FileIO, JLD2, Dates, Optim, DiffEqParamEstim,
        BlackBoxOptim, DataFramesMeta

#Lectura de la base de datos
base_datos = "data/processed/Processed_temp.rds"
datos_estado        = load(base_datos, convert=true)


#Categorías de edad
cat_num = convert(Int64, (ncol(datos_estado) - 1)/4)

#Fecha inicial
dia_min = minimum(datos_estado[!,:Dia])
dia_max = maximum(datos_estado[!,:Dia]) - 20
datos_estado = @where(datos_estado, :Dia .< dia_max)

#Estados iniciales
datos_init = @where(datos_estado, :Dia .== dia_min)
initial_state = [datos_init[!,:S_40], datos_init[!,:S_40_60], datos_init[!,:S_60_],
		 datos_init[!,:I_40], datos_init[!,:I_40_60], datos_init[!,:I_60_],
		 repeat([0], cat_num),
		 datos_init[!,:H_40], datos_init[!,:H_40_60], datos_init[!,:H_60_],
		 datos_init[!,:M_40], datos_init[!,:M_40_60], datos_init[!,:M_60_],
		 repeat([0],cat_num)]
initial_state = vcat(initial_state...)

#Tiempo a fitear
tspan = (convert(Float64, dia_min), convert(Float64, dia_max))

parameter_values = vcat([repeat([1/90], cat_num), repeat([1/50], cat_num),
        repeat([1/300], cat_num), repeat([1/200], cat_num), repeat([1/40], cat_num),
        repeat([1/30], cat_num), repeat([0.1], cat_num),
	repeat([1], cat_num), 180, 120]...)

prob   = ODEProblem(fit_model_vaccines, initial_state, tspan, parameter_values)
sol    = solve(prob, ode_algorithm)
@df datos_estado StatsPlots.scatter(:Dia, :I_40)
@df datos_estado StatsPlots.scatter!(:Dia, :I_40_60)
@df datos_estado StatsPlots.scatter!(:Dia, :I_60_)
Plots.plot!(sol, vars = [(0,cat_num + 1), (0,cat_num + 2), (0,cat_num + 3)])
#Plots.plot(sol, vars = (0,cat_num))


lsim   = convert(Int64, maximum(tspan))
t      = collect(range(0, stop=lsim, length=lsim + 1))

dats = convert(Array, datos_estado[:,[:I_40, :I_40_60, :I_60_, :H_40, :H_40_60, :H_60_, :M_40, :M_40_60, :M_60_]])
#scatter(dats)

ode_algorithm = BS5()
fitted_cats   = vcat([collect((cat_num + 1):(2*cat_num)),
                        collect((2*cat_num + 1):(3*cat_num)), collect((4*cat_num + 1):(5*cat_num))]...)
obj           = build_loss_objective(prob, ode_algorithm, L2Loss(t, dats'), save_idxs = fitted_cats)
#=
search_obj    = repeat([(0.0,1.0)], length(parameter_values) - 2)
search_obj    = vcat(search_obj, [(0.0, 365.0), (0.0, 365.0)]...)
prob_bb  = bboptimize(obj,
		SearchRange = search_obj,
                NumDimensions = length(parameter_values),
                Method = :adaptive_de_rand_1_bin_radiuslimited,
                MaxSteps = 100,
                TraceMode = :verbose)
parameter_nm = best_candidate(prob_bb)
eval_nm      = best_fitness(prob_bb)
=#

min_vals = fill(0.0, convert(Int64, length(parameter_values)))
max_vals = fill(1.0, convert(Int64, length(parameter_values)))
max_vals[length(max_vals) - 1] = 365.0
max_vals[length(max_vals)] = 365.0
optim_space   = Optim.optimize(obj, min_vals, max_vals, parameter_values,
				Fminbox(LBFGS()),
				Optim.Options(show_trace = true, iterations = 100, outer_iterations = 100))
parameter_nm  = Optim.minimizer(optim_space)
eval_nm       = Optim.minimum(optim_space)


prob_bfgs   = ODEProblem(fit_model_vaccines, initial_state, tspan, parameter_nm)
sol_bfgs    = solve(prob_bfgs, ode_algorithm)
@df datos_estado StatsPlots.scatter(:Dia, :I_40)
@df datos_estado StatsPlots.scatter!(:Dia, :I_40_60)
@df datos_estado StatsPlots.scatter!(:Dia, :I_60_)
Plots.plot!(sol_bfgs, vars = [(0,cat_num + 1), (0,cat_num + 2), (0,cat_num + 3)])

#Bayesian fitting to create CI
priors = [truncated(Normal(parameter_nm[1],0.001),max(parameter_nm[1] -  1.e-3,0.0), min(parameter_nm[1] +  1.e-3,1.0))]
for i = 2:(length(parameter_nm) - 2)
	priors = vcat(priors, truncated(Normal(parameter_nm[i],0.001),max(parameter_nm[i] -  1.e-3,0.0), min(parameter_nm[i] +  1.e-3,1.0)))
end
priors = vcat(priors, truncated(Normal(parameter_nm[length(parameter_nm) - 1],0.001),max(parameter_nm[length(parameter_nm) - 1] -  20.0,0.0), min(parameter_nm[length(parameter_nm) - 1] +  20.0,365.0)))
priors = vcat(priors, truncated(Normal(parameter_nm[length(parameter_nm)],0.001),max(parameter_nm[length(parameter_nm)] - 20.0,0.0), min(parameter_nm[length(parameter_nm)] + 20.0,365.0)))

#Bayesian inference
prob   = ODEProblem(fit_model_vaccines, initial_state, tspan, parameter_nm)
bayesian_result = turing_inference(prob, ode_algorithm, t, dats', priors,
                       num_samples = 100, epsilon = 0.1,
		       save_idxs = fitted_cats,
                       progress = true)

muestra = DataFrame(sample(bayesian_result, 100))
dessum  = Mamba.describe(bayesian_result)

params = vcat([cat_num, [0,0,0], [0, 0, 0], [0, 0, 0], vcat(muestra[1, 1:ncol(muestra)-1]...)]...)
prob_sim = ODEProblem(model_vaccines, initial_state, tspan, params)
function prob_func(prob_sim,i,repeat)
	params = vcat([cat_num, [0,0,0], [0, 0, 0], [0, 0, 0], vcat(muestra[i, 1:ncol(muestra)-1]...)]...)
	remake(prob_sim, p = params)
end


@info "Simulating posterior"
tspan = (0.0, 600.0)
sol  = solve(EnsembleProblem(prob_sim, prob_func = prob_func), ode_algorithm, EnsembleSerial(), trajectories = 100)

#summ = EnsembleSummary(sol)
summ = EnsembleSummary(sol,quantiles=[0.005,0.995])

@info "Plotting"
Plots.plot(summ,  idxs = (4*cat_num + 1,), title = "Defunciones sin vacuna", label = "M_40(t)", xlabel = "t")
Plots.plot!(summ, idxs = (4*cat_num + 2,), label = "M_40_60(t)", xlabel = "t")
Plots.plot!(summ, idxs = (4*cat_num + 3,), label = "M_60_(t)", xlabel = "t")
@df datos_estado StatsPlots.scatter!(:Dia, :M_40)
@df datos_estado StatsPlots.scatter!(:Dia, :M_40_60)
@df datos_estado StatsPlots.scatter!(:Dia, :M_60_)

tspan = (0.0, 600.0)
params = vcat([cat_num, [0.001,0.001,0.001], [1/40, 1/40, 1/40], [0.05, 0.05, 0.05], vcat(muestra[1, 1:ncol(muestra)-1]...)]...)
prob_sim = ODEProblem(model_vaccines, initial_state, tspan, params)
function prob_func(prob_sim,i,repeat)
	params = vcat([cat_num, [0.0,0.0,2], [1/40, 1/40, 1/40], [0.05, 0.05, 0.05], vcat(muestra[i, 1:ncol(muestra)-1]...)]...)
	remake(prob_sim, p = params)
end


@info "Simulating posterior"

sol2  = solve(EnsembleProblem(prob_sim, prob_func = prob_func), ode_algorithm, EnsembleSerial(), trajectories = 100)

#summ = EnsembleSummary(sol)
summ2 = EnsembleSummary(sol2,quantiles=[0.005,0.995])

plot(summ, idxs = (4*cat_num + 1, 4*cat_num + 2, 4*cat_num + 3,), title = "Defunciones con/sin vacuna velocidad 2", label = ["M_40(t)" "M_40_60(t)" "M_60(t)"], xlabel = "t")
plot!(summ2, idxs = (4*cat_num + 1, 4*cat_num + 2, 4*cat_num + 3), label = ["M_40(t)" "M_40_60(t)" "M_60(t)"])
savefig("Vacunadov1_todos.pdf")
