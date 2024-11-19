# ShellfishRcode
New repository of Shellfish data
###################################################################################################
#R packages used
library(readxl)
library(ggplot2)
library(dplyr)



#####################################################################################################
#Load data

shellfish_data1 <- read_excel("C:/Users/Kari Iamba/Desktop/My Research/Sinafa data/Finallized new data/Shellfish_final_edit.xlsx",
                                    sheet="Sheet1")


#Grouping data for Sites and species
shellfish_group <- shellfish_data1 %>%
  group_by(Sites) %>%
  count_()

shellfish_group


#Bar plot for number of samples per sampling site
ggplot(shellfish_group, aes(x= Sites, y= n)) +
  geom_col(aes(y = n,  stat = "summary"),color="black") +
  labs (x="Sites") + labs (y="Number of samples") +
  geom_text(aes(label = n), vjust = 2, colour = "white") +
  ggtitle("")+
  theme_classic(base_size = 11) + 
  theme(plot.title=element_text(hjust=0.5,face = "bold")) +
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1, face = "plain"),
        axis.text.y = element_text(size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"))


######################################################################################################################################
#Data of percentages (proportion)

#Grouping data for Sites and species prop
group_prop1 <- shellfish_data1 %>%
  group_by(Sites, species) %>%
  count_()

shellfish_group_prop

#mutating data
group_prop2 <- group_prop1 %>%
  group_by(Sites) %>%
  mutate(prop. = n/sum(n) * 100) 

group_prop2


#Bar plot for percentage of total abundance of species per sampling site
ggplot(group_prop2, aes(Sites, prop., fill= species,
                            label=paste0(round(prop.,2),"%"))) +
  geom_col( stat="summary",fun.y="mean", position = position_fill(),
            color="black") +
  geom_text(position = position_fill(vjust = 0.5), size=3.5) +
  scale_y_continuous(labels=scales::percent_format()) +
  ggtitle("")+
  labs (x="Sites") + labs (y="Proportion (%)") +
  theme_classic(base_size = 11) + 
  theme(plot.title=element_text(hjust=0.5,face = "bold")) +
  theme(axis.title =element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1, face = "plain"),
        axis.text.y = element_text(size = 12, angle = 0, hjust = 0.2, vjust = 0.2, face = "plain")) 

