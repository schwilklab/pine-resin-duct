# lmer_mods_graphs.r

# This R script contains graphs representing some of the significant
# relationships expressed by models created in lmer_mods.R.

source("read_rings.R")

# Create dataframe with information unique to each tree
unique.trees<- mdata %>% group_by(tag) %>% top_n(1, age)


## Ring width model ##

# Ring width by subsections 
ggplot(mdata, aes(subsections, ring.width.mean)) +
  geom_violin() 

ggplot(mdata, aes(subsections, ring.width)) +
  geom_boxplot()

ggplot(unique.trees, aes(subsections, ring.width.mean))+
  geom_violin()

# Ring width by drought

ggplot(mutate(mdata, fage=cut(age, c(0,10,20,30,40, 50, 60, 70, 80))),
       aes(PMDI_3yrlag, ring.width, color=fage)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", se=FALSE)

# ring width increases at all points in time of the tree's life as 
# drought conditions decrease (PMDI increases).

# Ring width by age seperated by subsections
ggplot(mdata, aes(age, ring.width, group=tag)) +
  geom_point() +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm")

ggplot(mdata, aes(age, ring.width)) +
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





## Resin duct density model ##

# resin duct density by subsection
ggplot(mdata, aes(subsections, duct.density.log)) +
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
