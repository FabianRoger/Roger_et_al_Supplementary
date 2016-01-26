###############
# Script to make a bar chart of the Lietrature overview summerized in the google spreadsheets

# source data are from here:
# https://docs.google.com/spreadsheets/d/18ssd0m6cfpi70IRSRYkUvHUoKJbpylEZgpRaS825dbw/edit#gid=0

# based on the categorization here:
# https://docs.google.com/spreadsheets/d/1fR_uzB27T-mWSeMaeCHYJ9kUve5AJ8lMZRqbpTX-vOg/edit#gid=0
##############

require(ggplot2)
require(dplyr)
require(tidyr)

#############

# import Data
LitDat <- read.table("LiteratureDocs/Literature_overview.txt", sep = "\t", header = T)

# code missing values  as 0
LitDat[ is.na(LitDat)] <- 0

# reshape data
LitDatR <- gather(LitDat, resp_type, resp_count, -total, -EF) 

# sort variable
LitDatR$resp_type <- factor(LitDatR$resp_type, levels = c("none", "pos", "neg", "amb"))

# sort factor level after total
LitDatR <- LitDatR[ with( LitDatR, order( total, EF, resp_type)), ]
LitDatR$EF <- factor(LitDatR$EF, levels = unique(LitDatR$EF), labels = c("enhancing \n plant productivity",
                                                                         "invasion resistance",
                                                                         "extracellular enzyme \n multifunctionality",
                                                                         "temporal stability",
                                                                         "nitrogen cycling",
                                                                         "resilience",
                                                                         "resistance",
                                                                         "bacterial activity",
                                                                         "degradation \n of carbon substrates",
                                                                         "abundance / biomass "))
                                                                

# add position variabel for labels
group_midpoints = function(g) {
  cumsums = c(0, cumsum(g$resp_count))
  diffs = diff(cumsums)
  pos = head(cumsums, -1) + (0.5 * diffs)
  return(data.frame(EF=g$EF, resp_type = g$resp_type, resp_count= g$resp_count, pos=pos ))
}

LitDatR <- LitDatR %>% 
  group_by(EF) %>% 
  do(group_midpoints(.))

LitDatR[LitDatR == 0] <- NA

Lit_graph <- ggplot(LitDatR[LitDatR$EF != "total", ], aes( x = EF, y = resp_count, fill = resp_type, order = resp_type))+
  geom_bar(stat = "identity", position = "stack", width = 0.5)+
  geom_text( aes(label = resp_count, y = pos), size = 4)+
  coord_flip()+
  labs( y = "number of relationships in literature", x = "",
        title = "bacterial biodiveristy - ecosystem functioning experiments \n using dillution to extinction",
        fill = NULL)+ 
  scale_fill_manual(values = c( "orange", "darkgreen", "red", "grey"),
                    labels = c("no\nrelationship", "positive\nrelationship", "negative\nrealtionship", "ambiguous\ndata"))+
  theme_bw( base_size = 10)+
  theme(legend.position = "bottom")

ggsave("figures/Figure_4.pdf", Lit_graph, width = 8, height = 6)

