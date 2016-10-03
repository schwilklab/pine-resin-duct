cmn.rw.mod.full <- mixed(log.rw_s ~ subsections*((age_s * PMDI_3yrlag_s) + BAF_s + elev_s) + mtn +
                           (age_s + PMDI_3yrlag_s|tag) + (1 | calendar.year), 
                         data=mdata, REML=FALSE)

summary(cmn.rw.mod.full)
anova(cmn.rw.mod.full)



cmn.rw.mod.simple <- mixed(log.rw_s ~ subsections*(age_s * PMDI_3yrlag_s) +
                             (age_s + PMDI_3yrlag_s | tag) + (1 | calendar.year), 
                           data=mdata, REML=FALSE)

cmn.rw.mod.rsi4 <- lmer(log.rw_s ~ subsections*(age_s * PMDI_3yrlag_s) +
                        (age_s+PMDI_3yrlag_s|tag) + (1 | calendar.year),
                        data=mdata, REML=FALSE)

cmn.rw.mod.rsi5 <- lmer(ring.width_s ~ subsections*(age_s * PMDI_3yrlag_s) +
                          (age_s+PMDI_3yrlag_s|tag) + (1 | calendar.year),
                        data=mdata, REML=FALSE)

fdfads<- lmer(ring.width_s ~ subsections + subsections:age_s + subsections:age_s:PMDI_3yrlag_s+
  (age_s+PMDI_3yrlag_s|tag) + (1 | calendar.year),
data=mdata, REML=FALSE)

anova(cmn.rw.mod.rsi4, cmn.rw.mod.rsi5)

plot(fitted(cmn.rw.mod.rsi4), residuals(cmn.rw.mod.rsi4),
     col="red"
     abline(h=0, lty=2)
     lines(smooth.spline(fitted(cmn.rw.mod.rsi4), residuals(cmn.rw.mod.rsi4)))

plot(fitted(cmn.rw.mod.rsi5), residuals(cmn.rw.mod.rsi5),
     abline(h=0, lty=2)
     lines(smooth.spline(fitted(cmn.rw.mod.rsi5), residuals(cmn.rw.mod.rsi5)))


)

mdata_pca<- mdata %>% select(age_s, ring.width_s, elev_s, BAF_s, radiation_s, msd_s, slope_s, 
                             PMDI_3yrlag_s, z_ridge_s, z_valley_s, duct.density.log_s, ldist_ridge_s,
                             ldist_ridge2_s, ldist_valley_s, ldist_valley2_s, zdist_ridge_s, zdist_valley_s)
mdata_subsections<- mdata %>% select(subsections)

mdata.pca <- prcomp(mdata_pca,
                 center = TRUE,
                 scale. = TRUE) 

summary(mdata.pca)

library(devtools)
install_github("ggbiplot", "vqv")
install_github("vgv/ggbiplot")


ggplot(unique.trees, aes(ldist_valley2, ring.width.mean)) +
  geom_point() +
  facet_grid(. ~ subsections)+
  geom_smooth(method= lm)


library(ggbiplot)
g <- ggbiplot(mdata.pca, obs.scale = 1, var.scale = 1, 
              groups = mdata_subsections, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

require(caret)
trans = preProcess(mdata_pca, 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC = predict(trans, mdata_pca)


summary(cmn.rw.mod.simple)
anova(cmn.rw.mod.simple)

# Run post-hoc tests on some significant relationships

lsmeans(cmn.rw.mod.simple, subsections |age_s)
lsmeans(cmn.rw.mod.simple, pairwise~ subsections)

sample()

capture.output(summary(cmn.rwden.mod.simple),file="density_mod.doc")
capture.output(anova(cmn.rwden.mod.simple),file="density_mod_anova.doc")

capture.output(summary(cmn.rw.mod.simple),file="ringwidth_mod.doc")
capture.output(anova(cmn.rw.mod.simple),file="ringwidth_mod_anova.doc")
