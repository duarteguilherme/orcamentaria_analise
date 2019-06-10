library(foreign)
library(dplyr)
library(tidyr)
library(rstan)
library(readstata13)

df <- read.dta13('data/2010Final.dta', generate.factors = "T")

df <- df %>%
  select(universidade_publica_gratuita = taxpref12,
         ensino_medio_publico = taxpref13, 
         ensino_basico_publico = taxpref14, 
         sus = taxpref15, 
         aposentadoria_idoso_pobre_nao_contribuinte = taxpref16,
         aposentadoria_servidor_publico = taxpref17)


# 1 means too little
# 2 means about right
# 3 means too much


# Wrangling data

df <- df %>%
  mutate(i = row_number())

df <- df %>%
  gather(policy_area, y, universidade_publica_gratuita:aposentadoria_servidor_publico)

df <- df %>%
  mutate(j = as.integer(as.factor(policy_area)))

df <- df %>%
  filter(!is.na(y))

df <- df %>%
  mutate(y = ifelse(y == "Aumentar os impostos e gastar mais", 3,
                    ifelse(y == "Gastar o mesmo que gasta atualmente",2,
                           ifelse(y == "Diminuir os impostos e gastar menos", 1,
                                  ifelse(y == "Diminuir os impostos e n<e3>o oferecer",0,
                                         NA)))))
df <- df %>%
  mutate(y = as.integer(y) + 1)

stan_data <- list(
  n_y = nrow(df),
  n_i = max(df$i),
  n_j = max(df$j),
  y = df$y,
  respondent = df$i,
  j = df$j
)



stan_model <- '
data {
  int<lower=0> n_y;
  int<lower=0> n_i;
  int<lower=0> n_j;
  int<lower=0> y[n_y];
  int<lower=0> respondent[n_y];
  int<lower=0> j[n_y];
}
parameters {
  real beta[n_j];
  real x[n_i];
  ordered[3] k[n_j];
  real y_star[n_y];
}
model {
  beta ~ normal(0, 100);
  for (a in 1:3) {
     k[a] ~ normal(0, 100);
  }
  x ~ normal(0,1);

  for ( a in 1:n_y) {
      y[a] ~ ordered_logistic(x[respondent[a]] * beta[j[a]], k[j[a]]);
  }
}
'

modelo <- rstan::stan(model_code = stan_model, data = stan_data, chains = 1)

saveRDS(object = modelo , file = "data/modelo.rds")

modelo <- readRDS('data/modelo.rds')


betas <- rstan::extract(modelo, "beta")
betas_df <- tibble(
  mean = apply(betas$beta, 2, mean),
  p95 =  apply(betas$beta, 2, function(x) quantile(x, probs = 0.95)),
  p05 = apply(betas$beta, 2, function(x) quantile(x, probs = 0.05))
)
betas_df <- betas_df %>%
  mutate(j = row_number()) %>%
  inner_join(df %>% distinct(j, policy_area)) 

betas_df <- betas_df %>%
  mutate(area_name = factor(policy_area) %>%
           forcats::fct_reorder(mean))

library(ggplot2)
ggplot(betas_df, 
       aes(x = area_name, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymax = p95, ymin = p05), width = 0) +
  coord_flip()





df2 <- read.dta13('data/2010Final.dta', generate.factors = "T")


df2 <- df2 %>%
  select(universidade_publica_gratuita = taxpref12,
         ensino_medio_publico = taxpref13, 
         ensino_basico_publico = taxpref14, 
         sus = taxpref15, 
         aposentadoria_idoso_pobre_nao_contribuinte = taxpref16,
         aposentadoria_servidor_publico = taxpref17,
         ideology, Gen, polint
  )


# 1 means too little
# 2 means about right
# 3 means too much


# Wrangling data

df2 <- df2 %>%
  mutate(i = row_number())

df2 <- df2 %>%
  gather(policy_area, y, nateduc:natarms)

df2 <- df2 %>%
  mutate(j = as.integer(as.factor(policy_area)))

df2 <- df2 %>%
  filter(!is.na(y))


df <- df %>%
  gather(policy_area, y, universidade_publica_gratuita:aposentadoria_servidor_publico)

df <- df %>%
  mutate(j = as.integer(as.factor(policy_area)))

df <- df %>%
  filter(!is.na(y))

df <- df %>%
  mutate(y = ifelse(y == "Aumentar os impostos e gastar mais", 3,
                    ifelse(y == "Gastar o mesmo que gasta atualmente",2,
                           ifelse(y == "Diminuir os impostos e gastar menos", 1,
                                  ifelse(y == "Diminuir os impostos e n<e3>o oferecer",0,
                                         NA)))))
df <- df %>%
  mutate(y = as.integer(y) + 1)



x <- rstan::extract(modelo, "x")
x_df <- tibble(
  mean = apply(x$x, 2, mean),
  p95 =  apply(x$x, 2, function(x) quantile(x, probs = 0.95)),
  p05 = apply(x$x, 2, function(x) quantile(x, probs = 0.05))
)
x_df <- x_df %>%
  mutate(i = row_number()) %>%
  inner_join(df %>% distinct(i)) 

x_df <- x_df %>%
  inner_join(df2)



x_df <- x_df %>%
  mutate(ideology = as.character(ideology)) %>%
  mutate(ideology = ifelse(ideology == "Esquerda", "1", 
                           ifelse(ideology == "Direita", "10", ideology))) %>%
  filter(ideology != "NS") %>%
  filter(ideology != "NR") %>%
  mutate(ideology = as.integer(ideology))
  
x_df <- x_df %>%
  mutate(ideology2 = factor(ideology)) %>%
  mutate(ideology2 = forcats::fct_reorder(ideology2, ideology))


ggplot(x_df %>% 
         filter(!is.na(Gen)), aes(x = mean)) +
  geom_density(aes(color = Gen), alpha = 0.2) +
  scale_color_manual(values = c("blue","red")) +
  theme_bw()



ggplot(x_df %>% 
         filter(!is.na(polint)), aes(x = mean)) +
  geom_density(aes(color = polint), alpha = 0.2) +
  scale_color_manual(values = c("blue","red", "gray", "purple")) +
  theme_bw()

ggplot(x_df, aes(x = ideology2, y = mean)) +
  geom_boxplot() +
  theme_bw()

ggplot(x_df %>%
         mutate(mean = mean * (-1)) %>%
         mutate(visao_politica = forcats::fct_reorder(factor(visao_politica), polviews)
                  ), aes(y = mean, x = visao_politica) ) +
  geom_boxplot() +
  coord_flip()


x1 <- rnorm(1000)
x2 <- x1 + rnorm(1000, 0, 00000.1)

cov(x1, x2)

sd(x1)^2 + sd(x2)^2 - 2*cov(x1,x2)



x <- rnorm(1000,2 )
z <- rnorm(1000,1)
#z <- x + 2 + rnorm(100, 0, 0.000000001)
quantile(x, probs = c(0.05, 0.5, 0.95))
quantile(z, probs = c(0.05, 0.5, 0.95))

mean(x - z) - 1.96*sd(x-z)/sqrt(100)
t.test(x,z)
