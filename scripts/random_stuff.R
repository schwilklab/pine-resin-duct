resin.duct.count.lm <- lm(resin.duct.count~ spcode +
                            spcode:calendar.year + calendar.year, data=dm.coordinates)
summary(resin.duct.count.lm)
anova(resin.duct.count.lm)

dm.coordinates<- ring_data %>% filter(mtn=="DM" & core.taken=="Y" & pith=="Y") %>%
  dplyr::select(tag, spcode, resin.duct.count, calendar.year) 
