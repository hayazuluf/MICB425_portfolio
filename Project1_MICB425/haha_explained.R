# Libraries
library(tidyverse)
library(phyloseq)
library(kableExtra)
library(vegan)

# Loading in Data
load("mothur_phyloseq.RData")
load("qiime2_phyloseq.RData")

#plot the phylum level communities by depth 
plot_bar(mothur, fill="Phylum")
plot_bar(qiime2, fill="Phylum")

#plot the genus level communities by depth 
plot_bar(mothur, fill="Genus")
plot_bar(qiime2, fill="Genus")

#too many genus, cannot see plot
#want to see percentage of each OTU in the total community
mothur_percent = transform_sample_counts(mothur, function(x) 100 * x/sum(x))
qiime2_percent = transform_sample_counts(qiime2, function(x) 100 * x/sum(x))

#plot the phyla, see 0-100% for all samples
plot_bar(mothur_percent, fill="Phylum")
plot_bar(qiime2_percent, fill="Phylum")

#choosing a taxa
  #Now lets find the top 10 abundant genus in the phylum proteobacteria by plotting a heat map
    #subset data is to select a smaller portion from a larger data structure. In this case we are selecting the phylum proteobacteria
   #prune taxa takes the sum of all taxas under proteobactera and only keeps the top 10 most abundant
    #plot_heatmap function displays abundancec in a graphical manner

mothur %>% 
  subset_taxa(Phylum=="Proteobacteria") %>% 
  prune_taxa(names(sort(taxa_sums(.),TRUE)[1:10]), .) %>% 
  plot_heatmap(taxa.label="Family")

qiime2 %>% 
  subset_taxa(Phylum=="D_1__Proteobacteria") %>% 
  prune_taxa(names(sort(taxa_sums(.),TRUE)[1:10]), .) %>% 
  plot_heatmap(taxa.label="Family")

#Now lets find the abundance of different OTUs of sulfurimonas

qiime2 %>% 
  subset_taxa(Genus=="D_5__Sulfurimonas") %>% 
  prune_samples(sample_sums(.)>=1, .) %>% 
  plot_heatmap(taxa.label="Genus")

mothur %>% 
  subset_taxa(Genus=="Sulfurimonas") %>% 
  prune_samples(sample_sums(.)>=1, .) %>% 
  plot_heatmap(taxa.label="Genus")

#finding the alpha diversity from the overall community
  # Calculates Depth vs. Chao1, Shannon, and InvSimpson
  #"samples" aka Depth is now a column in the object 

mothur_alpha = 
  mothur %>% 
  prune_taxa(taxa_sums(.) > 0, .) %>% 
  estimate_richness(., measures = c("Chao1", "Shannon", "InvSimpson")) %>% 
  rownames_to_column(., var = "Sample") 

qiime2_alpha = 
  qiime2 %>% 
  prune_taxa(taxa_sums(.) > 0, .) %>% 
  estimate_richness(., measures = c("Chao1", "Shannon", "InvSimpson")) %>% 
  rownames_to_column(., var = "Sample") 

# Results 1: making the Alpha Diversity Plot
  #The "plot_richness" function will calculate alpha diversity and plot it. We want to generate two plots, Chao1 and InvSimpson. 
  #Let's set X= Depth and the function will put alpha diversity in y as default
  #geom_point is a function that adds data as points to the graph. Here we are telling it to select O2_um values from the sam_data section in the mothur phyloseq
  #scale_colour_distiller differentiate the value of the dot by colour

mothur %>% 
  prune_taxa(taxa_sums(.) > 0, .) %>% 
  plot_richness(x = "Depth_m", measures=c("Chao1", "InvSimpson")) +
  geom_point(aes(colour = select(data.frame(mothur@sam_data), 
                                 starts_with("O2_uM"))), na.rm = TRUE) +
  scale_colour_distiller(palette = "RdYlGn") + # replace if using alt colours
  labs(x = "Depth (m)", y = "Alpha Diversity Indicator Value", 
       colour = expression(paste("[O"[2], "] (", mu, "M)")))

qiime2 %>% 
  prune_taxa(taxa_sums(.) > 0, .) %>% 
  plot_richness(x = "Depth_m", measures=c("Chao1", "InvSimpson")) +
  geom_point(aes(colour = select(data.frame(qiime2@sam_data), 
                                 starts_with("O2_uM"))), na.rm = TRUE) +
  scale_colour_distiller(palette = "RdYlGn") + # replace if using alt colours
  labs(x = "Depth (m)", y = "Alpha Diversity Indicator Value", 
       colour = expression(paste("[O"[2], "] (", mu, "M)")))

#Results 1: Taxa presence and abundance
  #selecting OTUs belonging to the sulfurimonas genus from the OTU_table in phyloseq and making it into an object 

mothur_otus = 
  mothur %>% 
  subset_taxa(Genus=="Sulfurimonas") %>% 
  .@otu_table %>% 
  data.frame() 

qiime2_asvs = 
  qiime2 %>% 
  subset_taxa(Genus=="D_5__Sulfurimonas") %>% 
  .@otu_table %>% 
  data.frame() %>%
  t() %>% 
  data.frame()

  #abundance: calculate the sum of all OTUs at each depth

mothur_totals =
  mothur %>% 
  .@otu_table %>% 
  data.frame() %>% 
  rowSums() %>% 
  data.frame() %>% 
  rename(., TotAbundance = ".") %>% 
  rownames_to_column(., "Sample") 

qiime2_totals =
  qiime2 %>% 
  .@otu_table %>% 
  data.frame() %>% 
  t() %>% 
  data.frame() %>% 
  rowSums() %>% 
  data.frame() %>% 
  rename(., TotAbundance = ".") %>% 
  rownames_to_column(., "Sample") 
