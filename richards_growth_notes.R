# notes on Richards/Schnute growth equation in SS3

library(r4ss)

model <- SS_output("simple_small_richards")

A1 <- subset(model$Growth_Parameters, Sex==1, A1, drop=TRUE)
A2 <- subset(model$Growth_Parameters, Sex==1, A2, drop=TRUE)
L1 <- subset(model$Growth_Parameters, Sex==1, L_a_A1, drop=TRUE)
L2 <- subset(model$Growth_Parameters, Sex==1, L_a_A2, drop=TRUE)
k <- subset(model$Growth_Parameters, Sex==1, K, drop=TRUE)
b <- model$parameters["Richards_Fem_GP_1", "Value"]
Linf <- subset(model$Growth_Parameters, Sex==1, Linf, drop=TRUE)

t <- 0:20

growth_Arni <- function(t, L1, L2, k, b, A1, A2)
{
  (L1^b + (L2^b-L1^b) * ((1-exp(-k*(t-A1))) / (1-exp(-k*(A2-A1)))))^(1/b)
}
plot(Len_Beg~Age_Beg, model$endgrowth, subset=Sex==1, ylim=c(0,80))
lines(t, growth_Arni(t, L1, L2, k, b, A1, A2))

growth_code <- function(t, L1, L2, k, b, A1, A2)
{
  LminR <- L1^b
  LmaxR <- L2^b
  LinfR <- LminR + (LmaxR - LminR) / (1 - exp(-k*(A2-A1)))
  temp <- LinfR + (LminR - LinfR) * exp(-k*(t-A1))
  temp^(1/b)
}
lines(t, growth_code(t, L1, L2, k, b, A1, A2), lty=3, lwd=3, col=2)

# table to compare different sources
comparison <- data.frame(
  age = t,
  output = subset(model$endgrowth, Sex==1, Len_Beg, drop=TRUE),
  code = growth_code(t, L1, L2, k, b, A1, A2),
  arni = growth_Arni(t, L1, L2, k, b, A1, A2)
)
comparison
# age  output     code     arni
#   0 22.7690 22.76900 22.76900
#   1 39.3016 39.30161 39.30161
#   2 46.8948 46.89477 46.89477
#   3 51.9159 51.91588 51.91588
#   4 55.5968 55.59675 55.59675
#   5 58.4357 58.43566 58.43566
#   6 60.6924 60.69238 60.69238
#   7 62.5224 62.52241 62.52241
#   8 64.0274 64.02738 64.02738
#   9 65.2779 65.27789 65.27789
#  10 66.3251 66.32514 66.32514
#  11 67.2075 67.20754 67.20754
#  12 67.9547 67.95467 67.95467
#  13 68.5897 68.58973 68.58973
#  14 69.1313 69.13126 69.13126
#  15 69.5942 69.59425 69.59425
#  16 69.9909 69.99095 69.99095
#  17 70.3315 70.33147 70.33147
#  18 70.6242 70.62422 70.62422
#  19 70.8762 70.87622 70.87622
#  20 71.3850 71.09338 71.09338
