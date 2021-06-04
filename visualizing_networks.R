library(tidyverse)
library(corrplot)
library(RColorBrewer)

setwd('/Users/benjaminsilver/Documents/DSA/AssemblyNetwork')

df = read_csv('cosponsors.csv')

# similarity matrix, remove Heastie for now bc causing issues
df_leg <- df %>% 
  select(-c(BillNumber,Sponsor,Heastie))

# correlation matrix, possibly skewed because many bills have very few cosponsors
# one way to deal with this is exclude bills with no cosponsors
df_leg_cor <- cor(df_leg)

# this gets me number of shared co-sponsorships, but is skewed because 
# some people co-sponsor way more stuff than others
df_leg_tp <- as.data.frame(crossprod(as.matrix(df_leg)))

# what I need to do is normalize each column as a percent
func <- function(x) ((x/max(x))*100)
df_leg_tp <- df_leg_tp %>% 
  mutate_all(func) %>% 
  as.matrix()

# how to style this for best viewing?
pdf("network_matrix.pdf", width = 8, height = 6)
corrplot(df_leg_tp,
         method = "color",
         is.corr = F,
#         type = "lower",
         tl.col = "black",
         order = "hclust",
         addrect = 15,
         tl.cex = .2)
dev.off()
