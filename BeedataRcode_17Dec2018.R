setwd("C:/Users/nmcintyr/Desktop/R scripts and stuff/Bill_Johnson_bees")
 
install.packages("vegan")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")

library(vegan)
library(tidyr)
library(dplyr)
library(ggplot2)

bdata <- read.csv("Beedata.csv")

# Create a data frame using only columns 9,18, 19, 20, 26, 31, 32
# Because bdata has a large number of columns, only some columns were
# selected for analysis: Sex, Locality, Habitat, Site, Scientific name,
# Family, and Genus.
beedat <- bdata [c(9,18:20,26,31,32)]
# Visualize the data
beedat

# Function using dplyr
# Groups the counts by Locality Habitat, Site, and Scientific name.
# Then counts the total of each to create a column of each count.
# Then converts from long to wide format. 
# Creates a column for each Scientific name and populates
# the total counted of each species.
counts<- beedat %>%
  group_by(Locality, Habitat,Site, scientificName) %>%
  summarise(total = n()) %>%
  spread(scientificName, total)
# Allows a visualization to ensure proper data manipulation
counts
# Converts counts from a tibble to a data frame
counts1 <- as.data.frame(counts)
# Delete Row 1 (NA row created in dplyr)
counts1 <- counts1 [-c(1),]
# Delete Column 4 (spurious column created in dplyr)
counts1 <- counts1 [-c(4)]
# Convert columns 4:185 from integer to numeric format;
# Package vegan requires numeric form for data calculations
counts1[4:185] <- lapply(counts1[4:185], as.numeric)
# Convert all NA to 0 counts
# NA were populated during the dplyr manipulation
counts1[is.na(counts1)] <- 0

# Function using dplyr to remove columns containing Wasp or Bee
counts2 <- counts1 %>%
  dplyr::select(-c(Wasp, Bee))
counts2

# A function to create a tibble of just Buffalo Lake counts
buff <- counts2 %>%
  filter(Locality == "Buffalo Lake NWR") # BLNWR
buff
# Create a dataframe named buffcounts
# buffcounts contains only values of species, e.g. columns 4:183
buffcounts <- buff [c(4:183)]
# create a dataframe of the names 
buffnames <- buff [c(1:3)]
# create a new dataframe to bind later for the sums of columns
BuffaloLake <- data.frame(Locality = "Buffalo Lake NWR", Habitat = "None", Site = "None")
# Use Colsums to create a dataframe, buffsums of the column sums for each species within BLNWR
buffsums <- colSums(buffcounts)
# tbuffsums is a transposed version of buffsums, long to wide format
tbuffsums <- t(buffsums)
# convert tbuffsums to a dataframe
totalbuff <- as.data.frame(tbuffsums)
# rowbind the species counts to the total column counts
BuffaloLakePops <- rbind(buffcounts, totalbuff)
# rowbind the names with the new data fram for the total counts
BuffaloLakeNames <- rbind(buffnames,BuffaloLake)
# column bind the populations to the names to create a data frame 
BuffaloLakeDF <- cbind(BuffaloLakeNames, BuffaloLakePops)
# BuffaloLakeDF contains only BLNWR counts and total sums

# The following creates the same dataframe as above but for Muleshoe NWR
mule <- counts2 %>%
  filter(Locality == "Muleshoe NWR")
mule
mulecounts <- mule [c(4:183)]
mulenames <- mule [c(1:3)]
MuleshoeNWR <- data.frame(Locality = "Muleshoe NWR", Habitat = "None", Site = "None")
mulesums <- colSums(mulecounts)
tmulesums <- t(mulesums)
totalmule <- as.data.frame(tmulesums)
Muleshoepops <- rbind(mulecounts, totalmule)
MuleShoeNames <- rbind(mulenames,MuleshoeNWR)
MuleShoeDf <- cbind(MuleShoeNames, Muleshoepops)

# Bind Muleshoe and Buffalo Lake to create a dataframe with all counts for all locations
TotalPops <- rbind(MuleShoeDf, BuffaloLakeDF)
# Creates a new column called CombinedName
# This is a combination of Locality and Habitat for NMDS analysis below
TotalPops <- unite(TotalPops, CombinedName, Locality, Habitat, sep= "_", remove = FALSE)

# The following code is for NMDS analysis.
# These plots are used to help categorize and give a visual
# representation of the similarity between locations and habitats 
# for the bee counts within the data set;
# the package vegan is required to conduct such analyses
# and ggplot is used to visualize the data. 
# First delete rows 7 and 14 because they are the total counts for each location
# create a dataframe named nmds for the next steps
nmds <- TotalPops [-c(7,14),]
nmds
# create a new dataframe and remove the first four columns which contain the names
y <- nmds[-c(1:4)]
y
# create a dataframe using the names, cols 1-4
nmdsnames <- nmds[c(1:4)]
nmdsnames

# Run NMDS on the dataframe (y) of the species counts
vare.mds1 <- metaMDS(y)
vare.mds1
# Note the stress value = 0.09060592, indicating a very good model fit.
# plot the previous NMDS
# type = "p" indicates that points will be used instead of names 
plot(vare.mds1, type= "p")

# Using the scores function from vegan to extract the site scores and convert to a dataframe
data.scores <- as.data.frame(scores(vare.mds1))
# create a column of site names, from the rownames of data.scores
data.scores$site <- rownames(data.scores)  
data.scores$grp <- nmdsnames  #  add the grp variable created by the dataframe process earlier
scores <- head(data.scores, n = 12)
# Using the scores function from vegan to extract the species scores and convert to a dataframe
species.scores <- as.data.frame(scores(vare.mds1, "species"))  
# create a column of species, from the rownames of species.scores
species.scores$species <- rownames(species.scores)  
head(species.scores) 
names(data.scores)
data.scores
# To create polygons in an NMDS plot using convex hulls
# hull values for grp A (grassland sites at Muleshoe)
grp.a <- (data.scores[data.scores$grp$Combined == "Muleshoe NWR_Grassland", ]
          [chull(data.scores[data.scores$grp$Combined ==
                    "Muleshoe NWR_Grassland", c("NMDS1", "NMDS2")]), ] ) 
# hull values for grp B (prairie dog colony sites at Muleshoe)
grp.b <- (data.scores[data.scores$grp$Combined == "Muleshoe NWR_Prairie Dog Colony", ]
          [chull(data.scores[data.scores$grp$Combined ==
                     "Muleshoe NWR_Prairie Dog Colony", c("NMDS1", "NMDS2")]), ])
# hull values for grp C (grassland sites at Buffalo Lake)
grp.c <- (data.scores[data.scores$grp$Combined == "Buffalo Lake NWR_Grassland", ]
          [chull(data.scores[data.scores$grp$Combined ==
                     "Buffalo Lake NWR_Grassland", c("NMDS1", "NMDS2")]), ])  
# hull values for grp D (prairie dog colony sites at Buffalo Lake)
grp.d <- (data.scores[data.scores$grp$Combined == "Buffalo Lake NWR_Prairie Dog Colony", ]
          [chull(data.scores[data.scores$grp$Combined == 
                     "Buffalo Lake NWR_Prairie Dog Colony", c("NMDS1", "NMDS2")]), ])

# Combine all hull values from above
hull.data <- rbind(grp.a, grp.b, grp.c, grp.d) 
# Review hull.data scores for accuracy and all data present
hull.data
# Review all species scores from above for accuracy
species.scores

# Create an NMDS graph (Fig. 5) using the hull and data scores
# Hull scores remove points if necessary to complete a polygon 
ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2, group=grp$CombinedName),alpha=0.30) + # add the convex hulls
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2, shape= grp$Locality, color=grp$Habitat))+ # add the point markers
# Create a Label named Habitat where the Habitats are designated by either a light or dark gray
  scale_colour_manual(name = "Habitat", values=c("gray25","gray65")) + 
# Create a label Locality where each location is denoted by eithera circle (16) or a triangle (17)
  scale_shape_manual(name= "Locality", values=c (16,17))+
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(), # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(),   # Remove background for solid white background
        panel.grid.major = element_blank(),   # remove major-grid labels
        panel.grid.minor = element_blank(),   # remove minor-grid labels
        plot.background = element_blank(),    # remove plot background
        legend.position = "none")     

# Breaking up the data frames accordingly
TotalPops
# Delete the names of each, and rows 7 and 14
Populations<- TotalPops[-c(1:4)] [-c(7,14),]
Populations

# Create new rows from existing data frame for the column sums of each factor, ie Muleshoe, Muleshoe Grassland...

mulesum<- colSums(Populations[c(1:6),])
mulegrsum<- colSums(Populations[c(1:3),])
mulepdsum<- colSums(Populations[c(4:6),])
buffsum<- colSums(Populations[c(7:12),])
buffgrsum<- colSums(Populations[c(7:9),])
buffpdsum <- colSums(Populations[c(10:12),])

# Function producing the Simpson Index, Shannon Index, Species richness (S), and Pielou Evenness (J)
indices<- function(x) {
  H <- diversity(x)
  simp <- diversity(x, "simpson")
  shan <- diversity(x, "shannon")
  S <- specnumber(x) 
  J <- H/log(S)
  print(list("Diversity" = H, "Shannon Index"= summary(shan), 
             "Simpson Index" = summary(simp), "Species Number" = S, "Pielou" = J))
  
}
# Values for Table 2
indices(mulesum)
indices(mulegrsum)
indices(mulepdsum)
indices(buffsum)
indices(buffgrsum)
indices(buffpdsum)

# Using the exact method to produce a species accumulation curve (Fig. 4)
sp1 <- specaccum(Populations, "exact")
plot(sp1, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightgray")