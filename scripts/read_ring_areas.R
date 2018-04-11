# Read and clean all data ring area files

library(dplyr)
library(stringr)

DUCT_AREA_DIR <- "../data/duct_areas"


read.area.file <- function(mtn, tag, ring, file) {
  fn <- file.path(DUCT_AREA_DIR, mtn, tag, file)
  r <- read.csv(fn, stringsAsFactors=FALSE)
  r <-  tryCatch(
        {
            data.frame(mtn=mtn, tag=tag, ring=ring, area = r$Area)
        },
        error=function(cond) {
            message(paste("Problem reading file: ",fn ))
            message("Here's the original error message:")
            message(cond)
            # Choose a return value in case of error
            data.frame(mtn=mtn, tag=tag, ring=ring, area = NA)
        },
        warning=function(cond) {
            message(paste("Problem reading file: ",fn ))
            message("Here's the original warning message:")
            message(cond)
        }
        )
       

  r <- filter(r, !is.na(area))
  return(r)
}


# function below expects a character vector in which first element is mtn range
# and second is tag id
read.one.tag <- function(mtn, tag){
  d <- file.path(DUCT_AREA_DIR, mtn, tag)
  afiles <- list.files(path=d, pattern="*.csv",full.names=FALSE)
  res <- data.frame(mtn=mtn, tag=tag,
                    ring = as.numeric(gsub("Ring_(.+)\\.csv", "\\1", afiles)),
                    file = afiles,
                    stringsAsFactors=FALSE)
  res <- res %>% rowwise %>% do(read.area.file(.$mtn, .$tag, .$ring, .$file))
  return(res)
}

read.all.areas <- function(d){
  sfiles <- list.dirs(path=d, full.names=FALSE, recursive=TRUE)
  sfiles <- sfiles[grepl(".+/.+", sfiles)] # only actual tag folders
  sfiles <- str_split_fixed(sfiles, "/", 2)
  sfiles = data.frame(mtn=sfiles[,1], tag = sfiles[,2])
  res <- sfiles %>% rowwise() %>% do(read.one.tag(.$mtn, .$tag))
  return(res)                            
  # apply(sfiles, 1, read.one.tag)
  #read.one.tag(sfiles[1,])
}


ring_areas <-  ungroup(read.all.areas(DUCT_AREA_DIR))
ring_areas <- left_join(ring_areas, select(trees, tag, tree.age), by = "tag") %>%
  mutate(ring.age = as.integer(1 + tree.age - ring)) # %>% select(-ring)

ring_areas_sum <- ring_areas %>% group_by(tag, ring.age, ring) %>%
  summarize(total.area = sum(area), area_duct_count = length(area))
