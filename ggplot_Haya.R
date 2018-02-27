library(tidyverse)
source("https://bioconductor.org/biocLite.R")
biocLite("phyloseq")
library(phyloseq)
load("phyloseq_object.RData")
source("https://bioconductor.org/biocLite.R")
biocLite("phyloseq")
library(phyloseq)
load("phyloseq_object.RData")
read.table(file="phyloseq_object.RData")
load("phyloseq_object.RData")
ggplot(metadata, aes(x=O2_uM, y=Depth_m))
ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point()
library(tidyverse)
source("https://bioconductor.org/biocLite.R")
biocLite("phyloseq")
[a]
a
library(phyloseq)
load("phyloseq_object.RData")
ggplot(metadata, aes(x=O2_uM, y=Depth_m))
ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point()
ggplot(metadata, aes(x=O2_uM, y=Depth_m, color="blue")) +
  geom_point()
ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point(color="blue")
ggplot(metadata, aes(x=O2_uM, y=Depth_m, size=OxygenSBE_V)) +
  geom_point()
ggplot(metadata, aes(x=PO4_uM, y=Depth_m)) +
  geom_point(shape=24) +
  geom_point(color="purple")
ggplot(metadata, aes(x=Temperature_C, y=Depth_m)) +
  geom_point()
metadata %>% 
  mutate(Temperature_F = Temperature_C*1.8+32) %>% 
  ggplot() + geom_point(aes(x=Temperature_F, y=Depth_m))
plot_bar(physeq, fill="Phylum")
physeq_percent = transform_sample_counts(physeq, function(x) 100 * x/sum(x))
plot_bar(physeq_percent, fill="Phylum")
plot_bar(physeq_percent, fill="Phylum") + 
  geom_bar(aes(fill=Phylum), stat="identity")
plot_bar(physeq_percent, fill="Genus") + 
  geom_bar(aes(fill=Genus), stat="identity")
plot_bar(physeq_percent, fill="Phylum") +
  geom_bar(aes(fill=Phylum), stat="identity") +
  facet_wrap(~Phylum)
plot_bar(physeq_percent, fill="Phylum") +
  geom_bar(aes(fill=Phylum), stat="identity") +
  facet_wrap(~Phylum, scales="free_y") +
  theme(legend.position="none")
gather=gather(metadata, nutrients, uM, NH4_uM, NO2_uM, NO3_uM, O2_uM, PO4_uM, siO2_uM)
library(tidyverse)
gather=gather(metadata, nutrients, uM, NH4_uM, NO2_uM, NO3_uM, O2_uM, PO4_uM, SiO2_uM)
ggplot(metadata, fill="uM") +
  geom_point(aes(fill=uM), stat="identity") +
  facet_wrap(~uM)
