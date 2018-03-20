# Libraries
library(tidyverse)
library(phyloseq)
library(kableExtra)
library(vegan)

mothur_sam = 
  mothur@sam_data %>% 
  rownames_to_column("Sample") %>% 
  data.frame() %>%
  na.omit()

  
# Loading in Data
load("data/mothur_phyloseq.RData")
load("data/qiime2_phyloseq.RData")

# Things
mothur_percent = 	mothur %>% transform_sample_counts(function(x) 100 * x/sum(x)) 

mothur_percent %>% 
	plot_bar(fill="Phylum") + 
	geom_bar(aes(fill=Phylum), stat="identity") +
	labs(x = "Sample depth", y = "Relative abundance (%)", 
			 title = "Phyla from 10 to 200 m in Saanich Inlet")

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
	labs(x = "Sample depth", y = "Relative abundance (%)", 
			 title = "Phyla from 10 to 200 m in Saanich Inlet")

mothur %>% 
	plot_heatmap(max.label = 30, sample.order = mothur_sam[,1]) +
	theme(axis.text.y = element_blank(), 
				axis.ticks.y = element_blank())

qiime2 %>% 
	plot_heatmap(max.label = 30, sample.order = mothur_sam[,1]) +
	theme(axis.text.y = element_blank(), 
				axis.ticks.y = element_blank())

#mothur_fr =
#	mothur %>% 
#	prune_taxa(as.vector(!grepl("uncultured|unclassified|Candidatus",
#															tax_table(.)[,"Genus"])), .) %>% 
#	filter_taxa(., function(x) sum (x >= 1) >= 3, TRUE)

#qiime2_fr = 
#	qiime2 %>% 
#	prune_taxa(as.vector(substr(tax_table(.)[,"Genus"], 0, 8)!="D_5__unc"), .) %>% 
#	prune_taxa(as.vector(tax_table(.)[,"Family"]!="D_4__Unknown Family"), .) %>% 
#	prune_taxa(as.vector(tax_table(.)[,"Genus"]!="D_5__"), .) %>% 
#	filter_taxa(., function(x) sum (x >= 1) >= 3, TRUE)

qiime2 %>% 
	subset_taxa(Genus=="D_5__Sulfurimonas") %>% 
	prune_samples(sample_sums(.)>=1, .) %>% 
	plot_heatmap(taxa.label="Genus")

mothur %>% 
	subset_taxa(Genus=="Sulfurimonas") %>% 
	prune_samples(sample_sums(.)>=1, .) %>% 
	plot_heatmap(taxa.label="Genus")

# !!!
# Must rarify data before estimating and plotting richness 
# Unless this is automatically done by phyloseq (unlikely)

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

# Alternate colour scheme that isn't R/G color-blind ambiguous
#	scale_colour_continuous(low="yellow", high="blue") +

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

mothur_taxon =
	mothur_otus %>% 
	rowSums(.) %>% 
	data.frame() %>% 
	rename(., TaxAbundance = ".") %>% 
	rownames_to_column(., var = "Sample") %>% 
	merge(., mothur_sam, "Sample") %>% 
	merge(., mothur_totals, "Sample") 

qiime2_taxon =
	qiime2_asvs %>% 
	rowSums(.) %>% 
	data.frame() %>% 
	rename(., TaxAbundance = ".") %>% 
	rownames_to_column(., "Sample") %>% 
	merge(., mothur_sam, "Sample") %>% 
	merge(., qiime2_totals, "Sample") 

  

# kruskal wallis, non-parametric anova
# null hypothesis is that slope is equal to zero
# alternate hypothesis is that slope 
  
  
#linear model  
#summary(lm(formula = Shannon ~ Sample, data = mothur_alpha))

summary(lm(formula = TaxAbundance ~ Depth_m, data = mothur_taxon))
summary(lm(TaxAbundance ~ Depth_m, data = qiime2_taxon))
summary(lm(TaxAbundance ~ TotAbundance, data = mothur_taxon))
summary(lm(TaxAbundance ~ TotAbundance, data = qiime2_taxon))
summary(lm(TaxAbundance ~ O2_uM, data = mothur_taxon))
summary(lm(TaxAbundance ~ O2_uM, data = qiime2_taxon))

#graph



# !!!
# kruskal wallis for each individual OTU/ASV
# incomplete, do not run

#function(dataset, sam_name = "sam"){
#	assign(paste(sam_name,"kruskal",sep="_"), 
#				 data.frame(x = row.names(dataset), y = rep(1)), 
#				 envir = .GlobalEnv)
	
#	for(i in length(dataset)) {
#		kruskal.test(TaxAbundance ~ O2_uM, data = dataset)$p.value
#	}
	
	
#}

# !!!
# try linear model of abundance vs depth/o2 

