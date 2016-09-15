# lmer_mods_graphs.r

# This R script contains graphs representing some of the significant
# relationships expressed by models created in lmer_mods.R.

source("read_rings.R")

# Create dataframe with information unique to each tree
unique.trees<- mdata %>% group_by(tag) %>% top_n(1, age)

## Model 1 ##

# BAI by species 
ggplot(mdata, aes(spcode, bai.mean)) +
  geom_violin() 

# BAI by age 
ggplot(mdata, aes(age_s, bai_s, group=tag)) +
  geom_point() +
  geom_smooth(method="lm")

# BAI by age seperated by individual species
ggplot(mdata, aes(age_s, bai_s, group=tag)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm")

# BAI by competition (BAF)seperated by individual species
ggplot(unique.trees, aes(BAF, bai.mean)) +
  geom_point() +
  facet_grid(. ~ spcode)+
  geom_smooth(method= lm)

# BAI by drought seperated by individual species
ggplot(mdata, aes(PMDI_3yrlag_s, bai_s, group=tag)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm")


## Model 2 ##

# BAI by subsections 
ggplot(mdata, aes(subsections, bai.mean)) +
  geom_violin() 

# BAI by age 
## This would be the same as the previous model

# BAI by age seperated by subsections
ggplot(mdata, aes(age_s, bai_s, group=tag)) +
  geom_point() +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm")

# BAI by competition (BAF)seperated by subsections
ggplot(unique.trees, aes(BAF, bai.mean)) +
  geom_point() +
  facet_grid(. ~ subsections)+
  geom_smooth(method= lm)


# BAI by drought seperated by subsections
ggplot(mdata, aes(PMDI_3yrlag_s, bai_s, group=tag)) +
  geom_point() +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm")


## Model 5 ##

# Ring width by species 
ggplot(mdata, aes(spcode, ring.width.mean)) +
  geom_violin() 

# Ring width by elevation
ggplot(unique.trees, aes(elev, ring.width.mean)) +
       geom_point()+
       geom_smooth(method="lm")

# Ring width by age seperated by individual species
ggplot(mdata, aes(age, ring.width, group=tag)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm")

# Ring width by elevation seperated by individual species
ggplot(unique.trees, aes(elev, ring.width.mean)) +
  geom_point() +
  facet_grid(. ~ spcode)+
  geom_smooth(method= lm)


# Ring width by drought seperated by individual species
ggplot(mdata, aes(PMDI_3yrlag_s, ring.width, group=tag)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm")

ggplot(mutate(mdata, fage=cut(age, c(0,10,20,30,40, 50, 60, 70, 80))),
       aes(PMDI_3yrlag, ring.width, color=fage)) +
  geom_point(alpha=0.2) +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm", se=FALSE)

## Model 6 ##

# Ring width by subsections 
ggplot(mdata, aes(subsections, ring.width.mean)) +
  geom_violin() 

# Ring width by drought

ggplot(mutate(mdata, fage=cut(age, c(0,10,20,30,40, 50, 60, 70, 80))),
       aes(PMDI_3yrlag, ring.width, color=fage)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", se=FALSE)


# Ring width by age seperated by subsections
ggplot(mdata, aes(age, ring.width, group=tag)) +
  geom_point() +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm")


# Ring width by drought seperated by subsections
ggplot(mdata, aes(PMDI_3yrlag_s, ring.width, group=tag)) +
  geom_point() +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm")

ggplot(mutate(mdata, fage=cut(age, c(0,10,20,30,40, 50, 60, 70, 80))),
       aes(PMDI_3yrlag, ring.width, color=fage)) +
  geom_point(alpha=0.2) +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm", se=FALSE)


## Model 3 ##

# duct density by species
ggplot(mdata, aes(spcode, duct.density.log)) +
  geom_boxplot() 

ggplot(unique.trees, aes(spcode, duct.den.log.mean)) +
  geom_boxplot()

# duct density by age

ggplot(mdata, aes(age, duct.density.log, group=tag)) +
  geom_point() +
  geom_smooth(method="lm")

# duct density by BAI logged values and individual species

ggplot(mdata, aes(bai.log, duct.density.log, group=tag)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm")


## Model 4 ##

# duct density by subsections
ggplot(mdata, aes(subsections, duct.density.log)) +
  geom_boxplot() 

ggplot(unique.trees, aes(subsections, duct.den.log.mean)) +
  geom_boxplot()

# duct density by age

ggplot(mdata, aes(age, duct.density.log, group=tag)) +
  geom_point() +
  geom_smooth(method="lm")

# duct density by mountain range

ggplot(mdata, aes(mtn, duct.density.log)) +
  geom_boxplot()

ggplot(unique.trees, aes(mtn, duct.den.log.mean)) +
  geom_boxplot()

### Note: This is signficant in the model, but I don't see any relationship
# at all.  Weird.

# duct density by age with subsections

ggplot(mdata, aes(age, duct.density.log, group=tag)) +
  geom_point() +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm")

# duct density by elevation with subsections

ggplot(mdata, aes(age, duct.density.log, group=tag)) +
  geom_point() +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm")

# duct density by logged bai values by subsection

ggplot(unique.trees, aes(elev, duct.den.log.mean)) +
  geom_point() +
  facet_grid(. ~ subsections)+
  geom_smooth(method= lm)

# duct density by BAI logged values by subsections

ggplot(mdata, aes(bai.log, duct.density.log, group=tag)) +
  geom_point() +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm")

ggplot(unique.trees, aes(bai.log, duct.den.log.mean)) +
  geom_point() +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm")

## Model 7 ##

# resin duct density by species
ggplot(mdata, aes(spcode, duct.density.log)) +
  geom_boxplot() 

ggplot(unique.trees, aes(spcode, duct.den.log.mean)) +
  geom_boxplot()


# resin duct density by age

ggplot(mdata, aes(age, duct.density.log, group=tag)) +
  geom_point() +
  geom_smooth(method="lm")

# resin duct density by age and individual species

ggplot(mdata, aes(age, duct.density.log, group=tag)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm")

# resin duct density and ring width by species

ggplot(mdata, aes(ring.width, duct.density.log, group=tag)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm")

ggplot(unique.trees, aes(ring.width.mean, duct.den.log.mean)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm")

## Model 8 ##

# resin duct density by species
ggplot(mdata, aes(spcode, duct.density.log)) +
  geom_boxplot() 

ggplot(unique.trees, aes(spcode, duct.den.log.mean)) +
  geom_boxplot()


# resin duct density by age

ggplot(mdata, aes(age, duct.density.log, group=tag)) +
  geom_point() +
  geom_smooth(method="lm")

# resin duct density by age and subsections

ggplot(mdata, aes(age, duct.density.log, group=tag)) +
  geom_point() +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm")


# resin duct density and elevation by subsections

ggplot(unique.trees, aes(elev, duct.den.log.mean)) +
  geom_point() +
  facet_grid(. ~ subsections)+
  geom_smooth(method= lm)

# resin duct density and ring width by subsections

ggplot(mdata, aes(ring.width, duct.density.log, group=tag)) +
  geom_point() +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm")

ggplot(unique.trees, aes(ring.width.mean, duct.den.log.mean)) +
  geom_point() +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm")

ggplot(mutate(mdata, fage=cut(age, c(0,10,20,30,40, 50, 60, 70, 80))),
       aes(ring.width, duct.density.log, color=fage)) +
  geom_point(alpha=0.2) +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm", se=FALSE)

### Interesting, the first 10 years show a negative relationship with ring
# ring width and duct density, but that changes after that especially with strobus.
