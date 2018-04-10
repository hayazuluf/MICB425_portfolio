---
title: "MICB 425 portfolio- ggplot"
author: "Haya Abuzuluf 32601130"
date: "version `r format(Sys.time(), '%B %d, %Y')`"
output:
  html_document:
    toc: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#load library

```{r}
library(tidyverse)
source("https://bioconductor.org/biocLite.R")
biocLite("phyloseq")
library(phyloseq)
```

#read table
```{r}
load("phyloseq_object.RData")
```
#graph metadata, plot depth by oxygen
```{r}
ggplot(metadata, aes(x=O2_uM, y=Depth_m))
```
#use point plot
```{r}
ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point()
```
#change color of dots to blue 
```{r}
ggplot(metadata, aes(x=O2_uM, y=Depth_m, color="blue")) +
  geom_point()
```
#change color within geom function
```{r}
ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point(color="blue")
```
#change chape and size
```{r}
ggplot(metadata, aes(x=O2_uM, y=Depth_m, size=OxygenSBE_V)) +
  geom_point()
```
#Exercise 1: Depth against PO4 concentration
```{r}
ggplot(metadata, aes(x=PO4_uM, y=Depth_m)) +
  geom_point(shape=17, color="purple")
```
#Exercise 2: using dplyr plot temperature in Fahrenheit against depth
###plot in Celcius
```{r}
ggplot(metadata, aes(x=Temperature_C, y=Depth_m)) +
  geom_point() +
  geom_point(color="purple")
```
###use dplyr function mutate to covert to Fahrenheit
```{r}
metadata %>% 
  mutate(Temperature_F = Temperature_C*1.8+32) %>% 
  ggplot() + geom_point(aes(x=Temperature_F, y=Depth_m),color="purple")
```
#ggplot with phyloseq
```{r}
physeq
```
#plot phylum level communities by depth by filling the fill
```{r}
plot_bar(physeq, fill="Phylum")
```
#calculate percentage of total of each sample for each OTU
```{r}
physeq_percent = transform_sample_counts(physeq, function(x) 100 * x/sum(x))
```
#plot the phyla percentage
```{r}
plot_bar(physeq_percent, fill="Phylum")
```
#specify identity to remove black lines
```{r}
plot_bar(physeq_percent, fill="Phylum") + 
  geom_bar(aes(fill=Phylum), stat="identity")
```
#Exercise 3: bar plot
```{r}
plot_bar(physeq_percent, fill="Genus") + 
  geom_bar(aes(fill=Genus), stat="identity") +
  labs(x = "Sample Depth", y = "Percent relative abundance",
       title ="Genera from 10 to 200 m in Saanich Inlet")
```
#Faceting: separates data into multiple panels to see patterns more clearly
```{r}
plot_bar(physeq_percent, fill="Phylum") +
  geom_bar(aes(fill=Phylum), stat="identity") +
  facet_wrap(~Phylum)
```
#remove legend because each facet has a label
```{r}
plot_bar(physeq_percent, fill="Phylum") +
  geom_bar(aes(fill=Phylum), stat="identity") +
  facet_wrap(~Phylum, scales="free_y") +
  theme(legend.position="none")
```

#Exercise 4: nutrient concentrations in uM for O2, PO4, SiO2, NO3 and NO2 by depth
```{r}
gather(metadata, nutrients, uM, NH4_uM, NO3_uM, O2_uM, PO4_uM, SiO2_uM, NO2_uM) %>%
  ggplot(., aes(x=Depth_m, y=uM, color="blue")) +
    facet_wrap(~nutrients, scales="free_y") +
    geom_point() + geom_line() +
    theme(legend.position="none")
```
