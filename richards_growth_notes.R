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

# variable names present in the SS3 source code
AFIX_delta <- A2 - A1

LminR <- L1^b
LmaxR <- L2^b
# Linf calculated at https://github.com/nmfs-ost/ss3-source-code/blob/3f26fa7999b6398698e094b012cc5f3f4a1e1850/SS_biofxn.tpl#L353
LinfR <- LminR + (LmaxR - LminR) / (1 - exp(-k * (A2 - A1)))

# Length at age calculated in https://github.com/nmfs-ost/ss3-source-code/blob/3f26fa7999b6398698e094b012cc5f3f4a1e1850/SS_biofxn.tpl#L473-L474
growth_code <- function(t) {
  temp <- LinfR + (LminR - LinfR) * exp(-k * (t - A1))
  temp^(1 / b)
}
lines(0:20, growth_code(0:20), col = 2)

# table to compare different sources
comparison <- data.frame(
  age = 0:5,
  output = model$endgrowth |> dplyr::filter(Sex == 1 & Age_Beg %in% 0:5) |> dplyr::pull(Len_Beg),
  code = growth_code(0:5),
  equation = growth_Arni(0:5),
  equation_scaled = growth_Arni(0:5) * Linf / growth_Arni(1e4)
)
#   age  output     code  equation equation_scaled
# 1   0 22.7690 22.76900  11804.07         2.28927
# 2   1 39.3016 39.30161 196332.66        38.07656
# 3   2 46.8948 46.89477 239042.70        46.35970
# 4   3 51.9159 51.91588 266192.42        51.62509
# 5   4 55.5968 55.59675 285774.07        55.42273
# 6   5 58.4357 58.43566 300742.82        58.32576
