library(tidyverse)
#Read in a table.
read.table(file="Saanich.metadata.txt")
read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t", na.strings="NAN")
metadata = read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t")
#Read in a table.
read.table(file="Saanich.OTU.txt")
metadata %>% 
  select(O2_uM)
metadata %>% 
  select(matches("O2|oxygen"))
metadata %>% 
  filter(O2_uM == 0)
metadata %>% 
  filter(O2_uM == 0) %>% 
  select(Depth_m)
metadata %>% 
  mutate(N2O_uM = N2O_nM/1000)
metadata %>% 
  transmute(N2O_uM = N2O_nM/1000)
metadata %>% 
  mutate(N2O_uM = N2O_nM/1000) %>% 
  ggplot() + geom_point(aes(x=Depth_m, y=N2O_uM))
