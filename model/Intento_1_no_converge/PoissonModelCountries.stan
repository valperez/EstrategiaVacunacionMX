//
// Modelo para predecir el efecto de las vacunas en México basados en 
// otros países
// Fecha : 11 de Febrero 2020
//
//
// Autor : 
// Rodrigo Zepeda 	rzepeda17@gmail.com
//Valeria Pérez valeria.perez.mat@gmail.com



data {
  int<lower=1> nedades;                         //Cantidad de grupos de edad incluidos en el modelo
  int<lower=1> ndias;                            //Cantidad de días modelados
  int<lower=0> dias_predict;                     //Cantidad de días a predecir (futuro)
  int<lower=0> m;                                //Autoregresive order m
  int<lower=1> npaises;                         //Numero de paises que estamos considerando
  real<lower=0> sigma_mu_hiper;                     //Sigma del hiperparámetro de la media
  real<lower=0> mu_mu_hiper;                        //Media del hiperparámetro de la media
  real<lower=0> sigma_sigma_hiper;
  real<lower=0> sigma_edad_hiper;
  real<lower=0> mu_mu_time_hiper;
  //real<lower=0> sigma_sigma_time_hiper;
  int<lower=0> P_edades[nedades*npaises, ndias]; //Proporción de hospitalizados para estado i día j
}

parameters {
  //Efectos autoregresivos
  real mu;
  real<lower=0> sigma;
  
  //Para la autoregresión con parámetros lambda
  real lambda[m];

  vector[nedades] alpha; //Agregamos un random effect
  
  //Efectos del grupo de edad
  real mu_edad[nedades];    
  real<lower=0> sigma_edad[nedades];
  
  //Seasonal effect for both 
  /*real mu_time[nedades];
  real<lower=0> sigma_time[nedades];*/
  
  /*vector[nedades] beta_yearly_cosine;
  vector[nedades] beta_yearly_sine;
  vector[nedades] beta_weekly_sine;
  vector[nedades] beta_weekly_cosine;
  vector[nedades] beta_monthly_sine;
  vector[nedades] beta_monthly_cosine;*/
}


model {
  
  //Parámetros
  vector[nedades*npaises] ln_p_edad;
  real alpha_model[nedades];
  real beta_model[nedades];
  
  //Hiperparámetros
  mu           ~ normal(mu_mu_hiper, sigma_mu_hiper);
  sigma        ~ normal(0, 1);
  
  /*mu_time      ~ normal(mu_mu_time_hiper, sigma_sigma_time_hiper);
  sigma_time   ~ cauchy(0, sigma_sigma_time_hiper);*/
  
  mu_edad      ~ normal(mu_mu_time_hiper, sigma_mu_hiper);
  sigma_edad   ~ normal(mu_mu_time_hiper, sigma_mu_hiper);
  
  //Creamos los parámetros
  lambda               ~ normal(mu, sigma);
  /*beta_yearly_cosine   ~ normal(mu_time, sigma_time);
  beta_yearly_sine     ~ normal(mu_time, sigma_time);
  beta_weekly_cosine   ~ normal(mu_time, sigma_time);
  beta_weekly_sine     ~ normal(mu_time, sigma_time);
  beta_monthly_sine    ~ normal(mu_time, sigma_time);
  beta_monthly_cosine  ~ normal(mu_time, sigma_time);*/
  alpha                ~ normal(mu_edad, sigma_edad);

  
  //Loopeamos
  for (t in (m + 1):ndias){
    ln_p_edad = alpha; /*+ cos(2*pi()*t/365) * beta_yearly_cosine + 
                        sin(2*pi()*t/365) * beta_yearly_sine + 
                        cos(2*pi()*t/30)  * beta_monthly_cosine + 
                        sin(2*pi()*t/30)  * beta_monthly_sine +
                        cos(2*pi()*t/7) * beta_weekly_cosine + 
                        sin(2*pi()*t/7) * beta_weekly_sine;*/
    for (k in 1:m){
      ln_p_edad[1:(nedades*npaises)] = ln_p_edad[1:(nedades*npaises)] + lambda[k]*to_vector(P_edades[1:(nedades*npaises),t-k]); //FIXME
    }
    P_edades[1:(nedades*npaises),t] ~ poisson_log(ln_p_edad);  
    }
}


//Adapted from https://jwalton.info/Stan-posterior-predictives/
generated quantities {
  // Generate posterior predictives
  int EdadPred[(nedades*npaises), dias_predict + ndias];
  vector[nedades*npaises] ln_p_edad;
  
  
  // First m points are to start model
  EdadPred[1:(nedades*npaises), 1:m] = P_edades[1:(nedades*npaises), 1:m];

  // Posterior dist for observed
  for (t in (m + 1):ndias){
    ln_p_edad     = alpha; /*+ cos(2*pi()*t/365) * beta_yearly_cosine + 
                    sin(2*pi()*t/365) * beta_yearly_sine + 
                    cos(2*pi()*t/30)  * beta_monthly_cosine + 
                    sin(2*pi()*t/30)  * beta_monthly_sine +
                    cos(2*pi()*t/7) * beta_weekly_cosine + 
                    sin(2*pi()*t/7) * beta_weekly_sine;*/
    for (k in 1:m){
      ln_p_edad[1:(nedades*npaises)] = ln_p_edad[1:(nedades*npaises)] + 
        lambda[k]*to_vector(P_edades[1:(nedades*npaises),t-k]);
    }
    EdadPred[1:(nedades*npaises), t] =  poisson_log_rng(ln_p_edad);
  }
  
  /*
  // Posterior dist for unobserved but still using some observed
  for (t in (ndias + 1):(ndias + m)){
    logit_p_estado = alpha + cos(2*pi()*t/365) * beta_yearly_cosine + 
                    sin(2*pi()*t/365) * beta_yearly_sine + 
                    cos(2*pi()*t/30)  * beta_monthly_cosine + 
                    sin(2*pi()*t/30)  * beta_monthly_sine +
                    cos(2*pi()*t/7) * beta_weekly_cosine + 
                    sin(2*pi()*t/7) * beta_weekly_sine;
    for (k in 1:m){
      if (t - k <= ndias){
        logit_p_estado[1:nedades] = logit_p_estado[1:nedades] + lambda[k]*PHosp[1:nedades,t-k];
      } else {
        logit_p_estado[1:nedades] = logit_p_estado[1:nedades] + lambda[k]*HospPred[1:nedades,t-k];
      }
    }
    //FIXME
    HospPred[1:nedades, t] = to_vector(poissn_rng(inv_logit(logit_p_estado)));
  }
  
  // Posterior dist for unobserved 
  for (t in (ndias + m + 1):(ndias +  dias_predict)){
    logit_p_estado = alpha + cos(2*pi()*t/365) * beta_yearly_cosine + 
                    sin(2*pi()*t/365) * beta_yearly_sine + 
                    cos(2*pi()*t/30)  * beta_monthly_cosine + 
                    sin(2*pi()*t/30)  * beta_monthly_sine +
                    cos(2*pi()*t/7) * beta_weekly_cosine + 
                    sin(2*pi()*t/7) * beta_weekly_sine;
    for (k in 1:m){
      logit_p_estado[1:nedades] = logit_p_estado[1:nedades] + lambda[k]*HospPred[1:nedades,t-k];
    }
    HospPred[1:nedades, t] = to_vector( beta_proportion_rng(inv_logit(logit_p_estado), phi) );
  }*/
}

