# Step 1: Read in data#########################################
getwd()
setwd("..")
getwd()
setwd("data")
getwd()
library(tidyverse)
sample_data<-read_csv("sample_data.csv")
# sample_data<-read_csv("data/sample_data.csv")

# Step 2: Summarize Dependent Variables ########################################
summarise(sample_data, average_cells = mean(cells_per_ml))
str(sample_data)

# Introduce pipe functions
sample_data %>% summarize(average_cells = mean(cells_per_ml))
  #ctrl+shift+M, this does not add an object column, do not write over no undo function

# Filtering rows with dplyr
sample_data %>% 
  filter(env_group == "Deep") %>% 
  summarise(average_cells = mean(cells_per_ml))
  # == "is equal to"; != "not equal"; %in% 
#sample_data %>% 
  #filter(env_group %in% c("Deep", "Shallow_May") %>% 
  #summarise(average_cells = mean(cells_per_ml))

# calculate average chlorophyll entire dataset and avg chloro in shallow_sept
sample_data %>% 
  summarise(average_chlorophyll = mean(chlorophyll))

sample_data %>% 
  filter(env_group == "Shallow_September") %>% 
  summarise(average_chlorophyll = mean(chlorophyll))

sample_data %>% 
  filter(str_detect(env_group, "September")) %>% 
  summarise(average_chlorophyll = mean(chlorophyll))

# group_by dplyr function
 sample_data %>% 
   group_by(env_group) %>% 
   summarise(avg_cells = mean(cells_per_ml),
             min_cells = min(cells_per_ml))
 
# calculate avg temp per env group
 sample_data %>% 
   group_by(env_group) %>% 
   summarise(avg_temp = mean(temperature))

# add new column with mutate dplyr function
 # TN:TP ratio
 
sample_data %>% 
  mutate(tn_tp_ratio = total_nitrogen/total_phosphorus) %>% 
  view()

sample_data %>% 
  mutate(temp_hot = temperature > 8) %>% 
  group_by(env_group, temp_hot) %>% 
  summarise(avg_temp = mean(temperature),
            avg_cells = mean(cells_per_ml))

# select columns with select function with dplyr 
 sample_data %>% 
  select(sample_id, depth)
 
 sample_data %>% 
   select(-env_group)
 
 sample_data %>% 
   select(sample_id : temperature) #all columns from sampleid to temp, range of values
 
 sample_data %>% 
   select(starts_with("total")) %>% 
   #select(ends_with())

# df with sample_id, env group, depth, temp, cells per ml
sample_data %>% 
  select(sample_id:temperature)
 
sample_data %>% 
   select(sample_id, env_group,depth, cells_per_ml, temperature)

sample_data %>% 
  select(1:5)

sample_data %>% 
  select(-total_nitrogen, -total_phosphorus, -diss_org_carbon, -chlorophyll)
 
sample_data %>% 
  select(-(total_nitrogen:chlorophyll))
 
 
# Step 3: Clean-up messy dataframes ############################################
getwd()

taxon_clean<-read_csv("taxon_abundance.csv", skip = 2) %>% #skips first two rows df
  select(-...10) %>% #gets rid of empty column on end
  rename(sequencer = ...9) %>% #renames to more logical column header name
  select(-Lot_Number, -sequencer) #removes unneeded columns
View(taxon)

taxon_long<-taxon_clean %>% 
  pivot_longer(cols = Proteobacteria:Cyanobacteria, 
               names_to = "Phylum", 
               values_to = "Abundance")
taxon_long %>% 
  group_by(Phylum) %>% 
  summarise(avg_abund = mean(Abundance))
# need long data for summary stats as well as for ggplot

# Step 4: Visualize in ggplot ##################################################
taxon_long %>% 
  ggplot()+
  aes(x = sample_id, y = Abundance, fill = Phylum)+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90))

# Making long data wide
taxon_long %>% 
  pivot_wider(names_from = "Phylum",
              values_from = "Abundance")

# Joining dataframes with Join family of functions, matching key values
head(sample_data)
head(taxon_clean)

# Inner join
inner_join<-inner_join(sample_data, taxon_clean, by = "sample_id") #joining by key value:sample_id
# lost a lot of rows, what happened? run an anti-join
anti_join(sample_data, taxon_clean, by = "sample_id")

sample_data$sample_id
taxon_clean$sample_id
# ids do not match

taxon_clean_goodSep<-taxon_clean %>% 
  mutate(sample_id = str_replace(sample_id, pattern = "Sep", replacement = "September"))

sample_and_taxon<-inner_join(sample_data, taxon_clean_goodSep, by = "sample_id")
getwd()
write_csv(sample_and_taxon, file = "sample_and_taxon.csv")

# Make a plot with joined data, what env variables predict microbial abundance
# where does chloroflexi like to live?
# exploratory data analysis
library(ggpubr)
sample_and_taxon %>% 
  ggplot()+
  aes(x = depth, 
      y = Chloroflexi)+
  geom_point()+
  labs(x = "Depth (m)",
       y = "Chloroflexi Relative Abundance (ug/L)",
       title = "Where does Chloropexi like to live?")+
  geom_smooth(method = "lm") +
  #stat_regline_equation() +
  stat_cor() + #pearson correlation coefficient
  annotate(geom = "text",
           x = 25, y = 0.3,
           label = "This is a text label")
# what is avg abundance and std dev chloroflexi in our three env groups

sample_and_taxon %>% 
  group_by(env_group) %>% 
  summarise(avg_abundance = mean(Chloroflexi),
            std_dev = sd(Chloroflexi, na.rm = TRUE)) 
  
 



