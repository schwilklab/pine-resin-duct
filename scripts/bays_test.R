b.count.mod1 <- readRDS(file="../results/b_count_mod1.rds")

library(brms)
library(shinystan)
library(shiny)

launch_shiny(b.count.mod1)  # requires installing shinystan

# for standardized coefficient effects try:

marginal_effects(b.count.mod1)

summary(b.count.mod1)





ggplot(mdata, aes(age, bai)) +
  geom_point() +
  geom_smooth()+
  facet_wrap(~ spcode)

ggplot(mdata, aes(age, bai)) +
  geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~ spcode)

ggplot(filter(mdata, spcode=="PIPO"), aes(age, bai)) +
  geom_point() +
  geom_smooth(method = "loess")+
  facet_wrap(~ tag)
