## Themes for presentations and publications.

## provides pubtheme and pubtheme.nogridlines. col2 and col1 are sizes in cm
## for standard journal 2 and 1 column width figures.

library(ggplot2)
library(scales)

library(extrafont)
# font_import(pattern="Arial")
loadfonts()

## # for nicer prebuilt color themes
## #devtools::install_github('cttobin/ggthemr')
## library(ggthemr)


schwilkcolors <- c("#D68D18", "#836B43", "#A0AE6A", "#437683", "#18B0D6")


## The ggplot theme for all figures.
bestfit <- geom_smooth(method="lm",se = F, color = "black", size=1.5)
textsize <- 18
smsize <- textsize-2
pt2mm <- 0.35146
smsize.mm <- smsize*pt2mm
fontfamily = "Arial"
col2 <- 17.5 # cm
col1 <- 8.0 # cm

pubtheme   <-  theme_grey() +
             theme(axis.title.y = element_text(family=fontfamily,
                   size = textsize, angle = 90, vjust=0.3),
               axis.title.x = element_text(family=fontfamily, size = textsize, vjust=-0.3),
               axis.ticks = element_line(colour = "black"),
               panel.background = element_rect(size = 1.6, fill = NA),
               panel.border = element_rect(size = 1.6, fill=NA),
               axis.text.x  = element_text(family=fontfamily, size=smsize, color="black"),
               axis.text.y  = element_text(family=fontfamily, size=smsize, color = "black"),
               strip.text.x = element_text(family=fontfamily, size = textsize), #, face="italic"),
               strip.text.y = element_text(family=fontfamily, size = textsize), #, face="italic"),
           #   strip.background = element_blank(),
               legend.title = element_text(family=fontfamily, size=textsize),
               legend.text = element_text(family=fontfamily, size=smsize, face="italic"),
               legend.key = element_rect(fill=NA),
               panel.grid.major = element_line(colour = "grey90", size = 0.2),
               panel.grid.minor = element_line(colour = "grey95", size =0.5),
           #    panel.grid.minor = element_blank(),
           #    panel.grid.major = element_blank(),
                strip.background = element_rect(fill = "grey80", colour = "grey50")
                )

pubtheme.nogridlines <- pubtheme +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          strip.background = element_blank())


# presentation theme.
# Meant for beamer output with a graph width of "10cm".

prestxsz <- 10
pressmsz <- 8

prestheme   <- theme(axis.title.y = element_text(family=fontfamily,
               size = prestxsz, angle = 90, vjust=0.3),
               axis.title.x = element_text(family=fontfamily, size = prestxsz, vjust=-0.3),
               axis.ticks = element_line(colour = "black"),
               panel.background = element_rect(size = 1.6, fill = NA),
               panel.border = element_rect(size = 1.6, fill=NA),
               axis.text.x  = element_text(family=fontfamily, size=pressmsz, color="black"),
               axis.text.y  = element_text(family=fontfamily, size=pressmsz, color = "black"),
               strip.text.x = element_text(family=fontfamily, size = prestxsz),#, face="italic"),
               strip.text.y = element_text(family=fontfamily, size = prestxsz),#, face="italic"),
           #   strip.background = element_blank(),
               legend.title = element_text(family=fontfamily, size=pressmsz),
               legend.text = element_text(family=fontfamily, size=pressmsz-1),#, face="italic"),
               legend.key = element_rect(fill=NA),
               legend.key.height = unit(0.5, "lines"),
               legend.background = element_rect(fill="transparent"),
              # legend.spacing.y = unit(0.5, 'lines'),
               panel.grid.major = element_line(colour = "grey90", size = 0.2),
               panel.grid.minor = element_line(colour = "grey95", size =0.5),
                strip.background = element_rect(fill = "grey80", colour = "grey50")
                )

prestheme.nogridlines <- prestheme +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          strip.background = element_blank())

