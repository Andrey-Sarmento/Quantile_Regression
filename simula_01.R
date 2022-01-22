# Simulando dados de regressao quantilica

# ------------------------------------------------------------------------------
library(quantreg)
library(ggplot2)

# Determinando os qauntis e fixando os parametros do modelo

tau = c(0.25, 0.50, 0.75)
beta0 = c(0, 4, 8)
beta1 = c(-1, 1, 4)

# Simulando os dados

n = 1000
x = rnorm(n, 20, 4)
u = rnorm(n, 0, 1)
percentil = pnorm(u)

b0 = c()
b1 = c()

for (i in 1:n) {
  for (t in 1:length(tau)) {
    if (percentil[i] >= tau[t] - 0.05 & percentil[i] < tau[t] + 0.05) {
      b0[i] = beta0[t]
      b1[i] = beta1[t]
      break
    }
    else {
      if (t == length(tau)) {
        b0[i] = NA
        b1[i] = NA
      }
    }
  }
}

length(b0) ; length(b1) ; length(x) ; length(u)

y = b0 + b1 * x + u
df = data.frame(x, u, percentil, b0, b1, y)

dados = na.omit(df) ; dim(dados)

gg1 = ggplot(dados, aes(y = y, x = x)) +
  geom_point(shape = 1, size = 2, alpha = 0.6) +
  theme_classic(base_size = 15)
gg1

mod1 = rq(y ~ x, tau = tau, data = dados)

gg1 + geom_quantile(quantiles = tau, size = 0.8)
