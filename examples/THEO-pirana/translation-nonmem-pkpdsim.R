## Libraries
library(PKPDsim)
library(tidyverse)

## Parameters

theta <- c(0.04, 0.462, 1.6, 0.677, 0.01)

pars <- tibble( 
  CL   = theta[1],
  V    = theta[2],
  KA   = theta[3]
)
omega <- c(6.34E-02,
           3.07E-02, 1.51E-02,
           -1.52E-02, -1.60E-02, 4.26E-01)

## Regimen
reg <- PKPDsim::new_regimen(amt = 320, n = 5, interval = 24)

## ODE system (!!! For infusions, add '+rate' to the dAdt[] definition for the infusion compartment!!!)
# mod <- PKPDsim::new_ode_model(code = ' 
#   K20 = CL/V;
#   dAdt[1] =  -KA*A[1] ;
#   dAdt[2] =  -K20*A[2]  +KA*A[1];
# ', declare_variables=c('K20'))


mod <- PKPDsim::new_ode_model('pk_1cmt_oral')

## Simulate
dat <- PKPDsim::sim(ode = mod,
                    par = pars,
                    regimen = reg,
                    omega = omega,
                    n = 10,
                    only_obs = TRUE)
head(dat)

Theoph %>% head
Theoph %>% 
  ggplot(aes(x = Time, y = conc, color = Wt)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Subject) +
  theme_light()

sim_ode_shiny(dat)

## Plot
ggplot (data = dat, aes(x=t, y=y, group=id)) + geom_line()

devtools::install_github('InsightRX/PKPDsimShiny')