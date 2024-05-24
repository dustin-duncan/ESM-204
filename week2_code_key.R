#our demand was y = 10 - 2q
# our supply was y = q + 4
# our answers for the last time were p*=6 q*=2

# erase
rm(list = ls(all = TRUE)) 

# packages
library(pracma) # fzero


dd <- function(q) {
  y = 10-2*q
  return(y)
}
ss <- function(q) {
  y = q+4
  return(y)
}
f_diff <- function(x) {
  z = ss(x)-dd(x)
  return(z)
}


# method 1
q_star1 <- uniroot(f = f_diff, interval = c(-100, 100))$root

# method 2
q_star2 <- fzero(f_diff, c(-100, 100))$x

# how we get the price?
p_star <- ss(q_star2)

# use integral function to find producer surplus
ps <- 2*6-integral(ss, xmin=0, xmax=2)

# use integral function to find consumer surplus
cs <- integral(dd, xmin=0, xmax=2)-2*6
