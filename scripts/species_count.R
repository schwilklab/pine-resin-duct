species.count <-read.csv("C://Users/Erik/Documents/GitHub/bark-beetle/data/masters_trees.csv")
species.count$spcode
# Code for sample count of cores taken per species in each mountain range
# Guadalupe Mountains
sum(species.count$spcode=="PIPO" & species.count$mtn=="GM" & species.count$condition=="alive")
sum(species.count$spcode=="PIED" & species.count$mtn=="GM" & species.count$condition=="alive")
sum(species.count$spcode=="PIST3" & species.count$mtn=="GM" & species.count$condition=="alive")
# Davis Mountains
sum(species.count$spcode=="PIPO" & species.count$mtn=="DM" & species.count$condition=="alive" &
      species.count$core.taken=="Y")
sum(species.count$spcode=="PICE" & species.count$mtn=="DM" & species.count$condition=="alive" &
      species.count$core.taken=="Y")
sum(species.count$spcode=="PIST3"& species.count$mtn=="DM" & species.count$condition=="alive" &
      species.count$core.taken=="Y")
# Chisos Mountains
sum(species.count$spcode=="PIAR" & species.count$mtn=="CM" & species.count$condition=="alive")
sum(species.count$spcode=="PICE" & species.count$mtn=="CM" & species.count$condition=="alive")

# Code for number of needles samples per each species in each mountain range
# Guadalupe Mountains
sum(species.count$spcode=="PICE"& species.count$mtn=="GM" & species.count$needles.collected=="Y")
sum(species.count$spcode=="PIPO"& species.count$mtn=="GM" & species.count$needles.collected=="Y")
sum(species.count$spcode=="PIST3"& species.count$mtn=="GM" & species.count$needles.collected=="Y")
# Davis Mountains
sum(species.count$spcode=="PIST3"& species.count$mtn=="DM" & species.count$needles.collected=="Y")
sum(species.count$spcode=="PIPO"& species.count$mtn=="DM" & species.count$needles.collected=="Y")
sum(species.count$spcode=="PICE"& species.count$mtn=="DM" & species.count$needles.collected=="Y")
# Chisos Mountains
sum(species.count$spcode=="PIPO"& species.count$mtn=="CM" & species.count$needles.collected=="Y")
sum(species.count$spcode=="PIAR"& species.count$mtn=="CM" & species.count$needles.collected=="Y")
