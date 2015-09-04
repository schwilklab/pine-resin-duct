## species_count.R

## replication counts and data integrity checks


# read tree data
trees <- read.csv("../data/masters_trees.csv")

trees$spcode
# Code for sample count of cores taken per species in each mountain range
# Guadalupe Mountains
sum(trees$spcode=="PIPO" & trees$mtn=="GM" & trees$condition=="alive")
sum(trees$spcode=="PIED" & trees$mtn=="GM" & trees$condition=="alive")
sum(trees$spcode=="PIST3" & trees$mtn=="GM" & trees$condition=="alive")
# Davis Mountains
sum(trees$spcode=="PIPO" & trees$mtn=="DM" & trees$condition=="alive" &
      trees$core.taken=="Y")
sum(trees$spcode=="PICE" & trees$mtn=="DM" & trees$condition=="alive" &
      trees$core.taken=="Y")
sum(trees$spcode=="PIST3"& trees$mtn=="DM" & trees$condition=="alive" &
      trees$core.taken=="Y")
# Chisos Mountains
sum(trees$spcode=="PIAR" & trees$mtn=="CM" & trees$condition=="alive")
sum(trees$spcode=="PICE" & trees$mtn=="CM" & trees$condition=="alive")

# Code for number of needles samples per each species in each mountain range
# Guadalupe Mountains
sum(trees$spcode=="PICE"& trees$mtn=="GM" & trees$needles.collected=="Y")
sum(trees$spcode=="PIPO"& trees$mtn=="GM" & trees$needles.collected=="Y")
sum(trees$spcode=="PIST3"& trees$mtn=="GM" & trees$needles.collected=="Y")
# Davis Mountains
sum(trees$spcode=="PIST3"& trees$mtn=="DM" & trees$needles.collected=="Y")
sum(trees$spcode=="PIPO"& trees$mtn=="DM" & trees$needles.collected=="Y")
sum(trees$spcode=="PICE"& trees$mtn=="DM" & trees$needles.collected=="Y")
# Chisos Mountains
sum(trees$spcode=="PIPO"& trees$mtn=="CM" & trees$needles.collected=="Y")
sum(trees$spcode=="PIAR"& trees$mtn=="CM" & trees$needles.collected=="Y")
