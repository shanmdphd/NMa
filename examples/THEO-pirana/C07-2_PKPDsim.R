## Libraries
library(PKPDsim)
library(ggplot2)


## Parameters
theta <- c(0.04, 0.462, 1.6, 0.677, 0.01)
pars <- list( 
  TVCL = theta[1],
  TVV  = theta[2],
  TVKA = theta[3],
  CL   = theta[1],
  V    = theta[2],
  KA   = theta[3],
  S2   = theta[2],
  K20  = CL/V
)
omega <- c(6.34E-02,
         3.07E-02, 1.51E-02,
         -1.52E-02, -1.60E-02, 4.26E-01)

## Regimen
reg <- new_regimen(amt = 100, n = 5, interval = 12, type='infusion', t_inf = 2)

## ODE system (!!! For infusions, add '+rate' to the dAdt[] definition for the infusion compartment!!!)
mod <- new_ode_model(code = ' 
  K20 = CL/V;
  dAdt[1] =  -KA*A[1] ;
  dAdt[2] =  -K20*A[2]  +KA*A[1];
', declare_variables=c('K20'))

## Simulate
dat <- sim_ode(ode = mod,
               parameters = pars,
               regimen = reg,
               omega = omega,
               n = 50,
               only_obs = TRUE)

## Plot
ggplot (data = dat, aes(x=t, y=y, group=id)) + geom_line()
