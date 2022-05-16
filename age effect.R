#calculate age effect
age_effect <- function(age){
  age_eff <- 2.355 - 0.937*age/10 + 0.152*age^2/100
  return(age_eff)
}

seq <- seq(15,95, by=0.5)
min(age_effect(seq))
min(age_effect(31))

curve(age_effect(x), from = 15, to=95, xlab = "age", ylab="age effect")

#minimum at age 31



#calculate age effect
age_effect <- function(age){
  age_eff <- 1.097 - 0.392*age/10 + 0.031*age^2/100
  return(age_eff)
}
curve(age_effect(x), from = 15, to=95, xlab = "age", ylab="age effect")

age_effect <- function(age){
  age_eff <- 0.475 - 0.241*age/10 + 0.033*age^2/100
  return(age_eff)
}
curve(age_effect(x), from = 15, to=95, xlab = "age", ylab="age effect")


