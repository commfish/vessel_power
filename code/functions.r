# function ----

fpc_r <- function(data){
  cpue1 = data$res
  cpue2 = data$sol
  alpha = 0.05
  z = abs(qnorm(alpha / 2))
  
  if(length(cpue1)!=length(cpue2)) {
    stop("Lengths of cpue1 and cpue2 do not match.")
  }
  
  data.frame(cpue1=cpue1, cpue2=cpue2) %>% 
    filter_all(any_vars(. != 0)) -> df
  
  npairs = nrow(df)

  # ratio means
  
  df %>% 
    mutate(y = cpue1 + 1,
           x = cpue2 + 1,
           n = n(),
           m1 = mean(x),
           m2 = mean(y),
           FPCr = m2 / m1,
           var = (1 / m1^2) * ((sum((y - (FPCr * x))^2) / (npairs - 1)) / npairs),
           se = sqrt(var),
           cv = sqrt(var / (n-1)) / FPCr,
           ll = FPCr  - z * se,
           ul = FPCr +  z * se) %>% 
    summarise_all(mean) %>% 
    dplyr::select(resolution=x, solstice=y, n, FPCr, se, ll, ul) -> out
  out

}

f_rep <- function(x){
  replicate(10, x, simplify = FALSE)
}

f_smpl <- function(x){
  sample_n(x, size = nrow(x), replace = TRUE)
}

f_clean <- function(x){
  x %>% 
    transmute(res = log(res + 1),
           sol = log(sol + 1),
           haul = factor(1:n())) %>% 
    gather(vessel, value, -haul) %>% 
    mutate(vessel = factor(vessel)) 
}

f_model <- function(x){
  out = summary(glm(value ~ vessel + haul, data = x))$coef[1:2,1:2]
  fpc = exp(2 * out[2,1] * (1 + 0.5 * out[2,2]^2))
  fpc
}


