//
// Modelo para predecir el efecto de las vacunas en México basados en 
// otros países
// Fecha : 11 de Febrero 2020
//
//
// Autores : 
// Rodrigo Zepeda 	rzepeda17@gmail.com
// Valeria Pérez valeria.perez.mat@gmail.com



data {
  int<lower=1> nedades;                         //Cantidad de grupos de edad incluidos en el modelo
  int<lower=1> ndias;                            //Cantidad de días modelados
  int<lower=0> dias_predict;                     //Cantidad de días a predecir (futuro)
  int<lower=0> m;                                //Autoregresive order m
  int<lower=1> npaises;                         //Numero de paises que estamos considerando
  int<lower=1> nfilas;
  real<lower=0> sigma_mu_hiper;                     //Sigma del hiperparámetro de la media
  real<lower=0> mu_mu_hiper;                        //Media del hiperparámetro de la media
  real<lower=0> sigma_sigma_hiper;
  //real<lower=0> sigma_sigma_time_hiper;
  int<lower=0> P_edades[nedades*npaises, ndias]; //Proporción de hospitalizados para estado i día j
  vector[nedades*npaises] P_poblacion;           // es un vector que tiene la poblacion de cada pais dividida por grupo de edad
  //real<lower=0> P_poblacion[nedades*npaises];
  //Vamos a meter los datos de Israel
  real<lower=0> vacunados[nedades*npaises, ndias] ;  // vector de cuantos vacunados acumulados tiene cada pais 
  //int<lower=0> P_pais_2[1, ndias];           // vector de hosp o muertos de Israel (solo tiene un renglon porque no tenemos edades)
  //int<lower=1> pob_total_isr;
}

parameters {
  //Efectos autoregresivos
  real mu;
  real<lower=0> sigma;
  
  //Para la autoregresión con parámetros lambda
  real lambda[m];

  vector[nedades*npaises] alpha; //Agregamos un random effect
  //real gamma; Yo digo que gamma es vector
  vector[nedades*npaises] gamma;
  
  
  
  //Efectos del grupo de edad
  real mu_edad;    
  real mu_2;  
  real<lower=0> sigma_edad;
  
  //Seasonal effect for both 
  real mu_time;
  real<lower=0> sigma_time;
  
  vector[nedades*npaises] beta_yearly_cosine;
  vector[nedades*npaises] beta_yearly_sine;
}


model {
  
  //Parámetros
  vector[nedades*npaises] ln_p_edad;
  
  //Hiperparámetros
  mu           ~ normal(mu_mu_hiper, sigma_mu_hiper);
  sigma        ~ cauchy(0, sigma_sigma_hiper);
  mu_2         ~ normal(mu_mu_hiper, sigma_mu_hiper);
  
  mu_time      ~ normal(mu_mu_hiper, sigma_mu_hiper);
  sigma_time   ~ cauchy(0, sigma_sigma_hiper);
  
  mu_edad      ~ normal(mu_mu_hiper, sigma_mu_hiper);
  sigma_edad   ~ cauchy(0, sigma_sigma_hiper);
  
  //Creamos los parámetros
  lambda               ~ normal(mu_edad, sigma_edad);
  beta_yearly_cosine   ~ normal(mu_time, sigma_time);
  beta_yearly_sine     ~ normal(mu_time, sigma_time);
  gamma                ~ normal(mu_2, sigma_time);
  alpha                ~ normal(mu, sigma);

  
  //Loopeamos
  for (t in (m + 1):ndias){
    ln_p_edad = alpha + cos(2*pi()*(t + 50)/180) * beta_yearly_cosine + 
                         sin(2*pi()*(t + 50)/180) * beta_yearly_sine +
                         gamma .* to_vector(vacunados[1:(nedades*npaises), t]);
    for (k in 1:m){
      ln_p_edad = ln_p_edad + lambda[k]*to_vector(P_edades[1:(nedades*npaises),t-k]) ./ P_poblacion; //FIXME
    }
    P_edades[1:(nedades*npaises),t] ~ poisson_log(ln_p_edad);  
    }
}


//Adapted from https://jwalton.info/Stan-posterior-predictives/
generated quantities {
  
  // Generate posterior predictives
  int EdadPred[(nedades*npaises), dias_predict + ndias];
  vector[nedades*npaises] ln_p_edad;
  int EdadPred2[(nedades*npaises), dias_predict + ndias];
  vector[nedades*npaises] ln_p_edad2;       //CHECKME No estoy segura de las dimensiones
  
  
  // First m points are to start model
  EdadPred[1:(nedades*npaises), 1:m] = P_edades[1:(nedades*npaises), 1:m];
  EdadPred2[1:(nedades*npaises), 1:m] = P_edades[1:(nedades*npaises), 1:m];

  // Posterior dist for observed
  for (t in (m + 1):ndias){
    ln_p_edad = alpha + cos(2*pi()*(t + 50)/180) * beta_yearly_cosine +
                         sin(2*pi()*(t + 50)/180) * beta_yearly_sine;
    for (k in 1:m){
      ln_p_edad = ln_p_edad + lambda[k]* to_vector(P_edades[1:(nedades*npaises),t-k]) ./ P_poblacion; 
    }
    for (j in 1:(nedades*npaises)){
      if (ln_p_edad[j] > 20.79){
        EdadPred[j, t] = EdadPred[j, t-1];  
      } else {
        EdadPred[j, t] = poisson_log_rng(ln_p_edad[j]);
      }
    }
  }
  
  
  
  // Posterior dist for unobserved but still using some observed
  for (t in (ndias + 1):(ndias + m)){
    ln_p_edad = alpha + cos(2*pi()*(t + 50)/180) * beta_yearly_cosine + 
                         sin(2*pi()*(t + 50)/180) * beta_yearly_sine;
    for (k in 1:m){
      if (t - k <= ndias){
        ln_p_edad = ln_p_edad + lambda[k]*to_vector(P_edades[1:(nedades*npaises),t-k]) ./ P_poblacion; 
      } else {
        ln_p_edad = ln_p_edad + lambda[k]*to_vector(EdadPred[1:(nedades*npaises),t-k]) ./ P_poblacion; 
      }
    }
    
    for (j in 1:(nedades*npaises)){
      if (ln_p_edad[j] > 20.79){
        EdadPred[j, t] = EdadPred[j, t - 1];  
      } else {
        EdadPred[j, t] = poisson_log_rng(ln_p_edad[j]);
      }
    }
  }
  
  
  
  // Posterior dist for unobserved 
  for (t in (ndias + m + 1):(ndias +  dias_predict)){
    ln_p_edad = alpha + cos(2*pi()*(t + 50)/180) * beta_yearly_cosine + 
          sin(2*pi()*(t + 50)/180) * beta_yearly_sine;
    for (k in 1:m){
      ln_p_edad = ln_p_edad + lambda[k]* to_vector(EdadPred[1:(nedades*npaises),t-k]) ./ P_poblacion;
    }
    for (j in 1:(nedades*npaises)){
      if (ln_p_edad[j] > 20.79){
        EdadPred[j, t] = EdadPred[j, t-1];  
      } else {
        EdadPred[j, t] = poisson_log_rng(ln_p_edad[j]);
      }
    }
  }
  
  
  
    // Posterior dist for observed pais 2
  for (t in (m + 1):ndias){
    ln_p_edad2 = alpha +  
                  cos(2*pi()*(t + 50)/180) * beta_yearly_cosine + 
                  sin(2*pi()*(t + 50)/180) * beta_yearly_sine + 
                  gamma .*to_vector(vacunados[1:(nedades*npaises), t]);
    for (k in 1:m){
      ln_p_edad2 = ln_p_edad2 + lambda[k]* to_vector(P_edades[1:(nedades*npaises),t-k]) ./ P_poblacion; //FIXME
    }
    for (j in 1:(nedades*npaises)){
      if (ln_p_edad2[j] > 20.79){
        EdadPred2[j, t] = EdadPred2[j, t-1];  
      } else {
        EdadPred2[j, t] = poisson_log_rng(ln_p_edad2[j]);
      }
    }
  }
  
  // Posterior dist for unobserved but still using some observed pais 2
  for (t in (ndias + 1):(ndias + m)){ 
    ln_p_edad2 = alpha +  
                  cos(2*pi()*(t + 50)/180) * beta_yearly_cosine + 
                  sin(2*pi()*(t + 50)/180) * beta_yearly_sine + 
                         gamma .*to_vector(vacunados[1:(nedades*npaises), t]);
    for (k in 1:m){
      if (t - k <= ndias){
        ln_p_edad2 = ln_p_edad2 + lambda[k]*to_vector(P_edades[1:(nedades*npaises),t-k]) ./ P_poblacion; 
      } else { 
        ln_p_edad2 = ln_p_edad2 + lambda[k]*to_vector(EdadPred[1:(nedades*npaises),t-k]) ./ P_poblacion; 
      }
    }
    
    for (j in 1:(nedades*npaises)){
      if (ln_p_edad2[j] > 20.79){
        EdadPred2[j, t] = EdadPred2[j, t - 1];  
      } else {
        EdadPred2[j, t] = poisson_log_rng(ln_p_edad2[j]);
      }
    } 
  }
  

  
  // Posterior dist for unobserved pais 2
  for (t in (ndias + m + 1):(ndias +  dias_predict)){
    ln_p_edad2 = alpha +  
                  cos(2*pi()*(t + 50)/180) * beta_yearly_cosine + 
                  sin(2*pi()*(t + 50)/180) * beta_yearly_sine + 
                  gamma .*to_vector(vacunados[1:(nedades*npaises), t]);
    for (k in 1:m){
      ln_p_edad2 = ln_p_edad2 + lambda[k]* to_vector(EdadPred[1:(nedades*npaises),t-k]) ./ P_poblacion;
    } 
    for (j in 1:(nedades*npaises)){
      if (ln_p_edad2[j] > 20.79){
        EdadPred2[j, t] = EdadPred2[j, t-1];  
      } else {
        EdadPred2[j, t] = poisson_log_rng(ln_p_edad2[j]);
      }
    } 
    
  } 
}

