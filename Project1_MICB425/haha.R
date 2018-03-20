# Libraries
library(tidyverse)
library(phyloseq)
library(kableExtra)

# Loading in Data
load("data/mothur_phyloseq.RData")
load("data/qiime2_phyloseq.RData")

# Things
mothur_percent = 	mothur %>% transform_sample_counts(function(x) 100 * x/sum(x)) 

mothur_percent %>% 
	plot_bar(fill="Phylum") + 
	geom_bar(aes(fill=Phylum), stat="identity") +
	labs(x = "Sample depth", y = "Relative abundance (%)", title = "Phyla from 10 to 200 m in Saanich Inlet")

sample_data(mothur) %>% 
	select(matches("O2_uM|depth"),-matches("NO2|SiO2")) %>%
	gather(key = "Nutrient", value = "Concentration", -Depth_m) %>%
	ggplot(., aes(x = Depth_m, y = Concentration)) +
	geom_point() +
	geom_line() +
	facet_wrap( ~ Nutrient, scales = "free") +
	theme(legend.position = "none") +
	labs(x = "Depth (m)", y = expression(paste("Concentration (", mu, "M)")))

qiime2_percent = 	qiime2 %>% transform_sample_counts(function(x) 100 * x/sum(x)) 

qiime2_percent %>% 
	plot_bar(fill="Phylum") + 
	geom_bar(aes(fill=Phylum), stat="identity") +
	labs(x = "Sample depth", y = "Relative abundance (%)", title = "Phyla from 10 to 200 m in Saanich Inlet")

mothur %>% 
	subset_taxa(Phylum=="Proteobacteria") %>% 
	prune_taxa(names(sort(taxa_sums(.),TRUE)[1:10]), .) %>% 
	plot_heatmap(taxa.label="Family")

qiime2 %>% 
	subset_taxa(Phylum=="D_1__Proteobacteria") %>% 
	prune_taxa(names(sort(taxa_sums(.),TRUE)[1:10]), .) %>% 
	plot_heatmap(taxa.label="Family")
