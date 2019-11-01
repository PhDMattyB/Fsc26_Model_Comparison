##############################
## Comparing model likelihoods
##
## Matt Brachmann (PhDMattyB)
##
## 2019-10-01
##
##############################

setwd('~/PhD/SNP Demographic modelling/Fsc26/Model_lhoods')

library(tidyverse)
library(patchwork)
library(janitor)
library(devtools)
library(skimr)
library(rsed)
library(data.table)

theme_set(theme_bw())

## Read in the data sets we have created. 
GIM = read_tsv("GIM_colname.LHOODS") %>% 
  dplyr::rename(lhoods = 1, 
                OG_model = 2)
GIM_change = read_tsv('GIM_change.lhoods') 

## This section allows you to create the data frames 
## that we will be using to graph

## Make a label for each model that we are comparing 
# label = rep('GIM_Change', length(GIM_change$lhoods)) %>% 
#   as_tibble()

## Combine the columns from the original data frame
## with the label we made for the model
# GIM_change = bind_cols(GIM_change, label) %>% 
#   dplyr::rename(OG_model = value) 

## Write the new data frame to a file and re-read in the data above
# write_tsv(GIM_change, 'GIM_change.lhoods')

## Combine the data from all of the models that we have run
models = bind_rows(GIM,
                   GIM_change)

## Create a population label as we will be dealing with models 
## from multiple different populations
popn_label = rep('Galtabol', 
                 length(models$lhoods)) %>% 
  as_tibble()

## Make the data set we will be using and order the data properly
models = bind_cols(models, 
                   popn_label) %>% 
  rename(Population = value) %>% 
  dplyr::select(Population, 
         OG_model, 
         lhoods)
## Create a new column with the full model name
models = mutate(.data = models, 
                      Model = as.factor(case_when(
                        OG_model == 'GSC' ~ 'Secondary Contact',
                        OG_model == 'IM' ~ 'Isolation by migration',
                        OG_model == "GIM_Change" ~ 'Change in migration')))

# Likelihood visualization ------------------------------------------------

## Colour palettes for the graph
mod_pal = c("#258CE6", "#FF1C08", '#FF6114')
mod_pal = c('#258CE6', '#FF1C08')

## order the models so the graph makes sense
## all of the IM models first and then the secondary contact models
Galtabol_models = Galtabol_models %>%
  mutate(Model = fct_relevel(Model,
                             'Isolation by migration', 
                             'Change in migration'))

## Actual graph using the data and palettes that we have created
ggplot(data = Galtabol_models,
       aes(x = Model,
           y = lhoods))+
  geom_boxplot(aes(fill = Model), 
               col = 'Black')+
  scale_fill_manual(values = mod_pal)+
  labs(y = 'Maximum likelihood')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14), 
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.9, 
                                   hjust = 0.9),
        axis.title.x = element_blank())

## Save graph to a file 
ggsave('Galtabol_Fsc26_ModelCompare_twomods.tiff', 
       plot = last_plot(), 
       width = 20, 
       height = 20, 
       units = 'cm')

## Read in the data that has the AIC values for each model 
## so that we can compare them
aic = read_tsv("Model_AIC_values.txt") %>% 
  dplyr::select(-DeltaL) %>% 
  rename(DeltaL = 3)
  
