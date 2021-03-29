# setRepositories()
# check packages installed installed.packages()[1:8,]
# get library path .libPaths()
# append new libarry to the existing one  .libPaths( c( .libPaths(), "/sc/arion/projects/rg_HPIMS/user/jiahao01/anaconda3/envs/r_env/lib/R/library") )
# Sys.getenv()[ grep("LIB|PATH", names(Sys.getenv())) ]
# R_LIBS_USER = '/sc/arion/projects/rg_HPIMS/user/jiahao01/anaconda3/envs/r_env/lib/R/library'
# install.packages('tidyverse')
# install.packages('data.table')
# install.packages('dplyr')
#install.packages('gplots')
# install.packages('corrplot')

library(tidyverse)
library(data.table)
library(dplyr)
library(gplots)
library(graphics)
library(chisq.posthoc.test)

setwd()
getwd()

dat <- fread('disease_cluster_table_w_etn.csv', stringsAsFactors = F)



expected_counts <- c(Cluster_1 = 205, 
                     Cluster_2 = 358,
                     Cluster_3 = 1690, 
                     Cluster_4 = 881,
                     Cluster_5 = 1097,
                     CLuster_6 = 1566)

total <- sum(expected_counts)


code_list <- unique(dat$phecode)

getwd()


sink("diagnosis_chisq_test.txt")


cat('phecode','n' ,"pct" ,
    'Cluster_1', 'Cluster_2', 'Cluster_3', 'Cluster_4', 'Cluster_5', 'CLuster_6',
    'Cluster_1_pct', 'Cluster_2_pct', 'Cluster_3_pct', 'Cluster_4_pct', 'Cluster_5_pct', 'Cluster_6_pct',
    "N_Non_Zero_Clusters", 'p','disease_list' ,'\n', sep='\t')




for (i in code_list) {
  sub <- dat %>% filter(phecode == i)
  n <- nrow(sub)
  tmp <- as.data.frame(table(sub$labels))
  observed_counts <- c(Cluster_1 = if (length(tmp$Freq[tmp$Var1 == 0])>0) {tmp$Freq[tmp$Var1 == 0]} else {0}, 
                       Cluster_2 = if (length(tmp$Freq[tmp$Var1 == 1])>0) {tmp$Freq[tmp$Var1 == 1]} else {0},
                       Cluster_3 = if (length(tmp$Freq[tmp$Var1 == 2])>0) {tmp$Freq[tmp$Var1 == 2]} else {0}, 
                       Cluster_4 = if (length(tmp$Freq[tmp$Var1 == 3])>0) {tmp$Freq[tmp$Var1 == 3]} else {0},
                       Cluster_5 = if (length(tmp$Freq[tmp$Var1 == 4])>0) {tmp$Freq[tmp$Var1 == 4]} else {0},
                       Cluster_6 = if (length(tmp$Freq[tmp$Var1 == 5])>0) {tmp$Freq[tmp$Var1 == 5]} else {0}
)
  N_Non_Zero_Clusters <- which(observed_counts != 0) %>% length()
  
  
  if (N_Non_Zero_Clusters>1) {
    erg <- chisq.test(x = observed_counts, p = expected_counts, rescale.p = TRUE)
    p <- erg$p.value
  } else {
    p <- NA
  }
  cat(phecode=i, n=n, pct= scales::percent(n/total), observed_counts, scales::percent(observed_counts/expected_counts), N_Non_Zero_Clusters, p, paste(sub$dx_name %>% unique(), collapse = ';'), '\n', sep='\t')
  rm(tmp, erg, p, sub, n)
}
sink()



# import ethincy and gender table
eth <- read.csv('gender_race_table.csv', header=T, stringsAsFactors=F)
eth
colnames(eth)
str(eth)
# filtering
rownames(eth) <- eth$X
filtered_eth <- eth[6:11,2:7]
filtered_eth


# convert data to table
dt <- as.table(as.matrix(filtered_eth))
# ballon plot
png('ballonplot.png')
balloonplot(t(dt), main ="ethnic groups", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
dev.off()

# mosaic plot
png('mosaicplot.png')
mosaicplot(dt, shade = TRUE, las=2,
           main = "ethnic groups")
dev.off()

# compute chi square for ethnic group
chisq <- chisq.test(filtered_eth)

chisq

# check observed counts
chisq$observed

# check expected counts
round(chisq$expected, 2)

# check residuals 
round(chisq$residuals,3)
# visualise
library(corrplot)
png('residualplot.png')
corrplot(chisq$residuals, is.cor = FALSE)
dev.off()

# Contibution in percentage (%)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
# visualize
png('residual_in_pct.png')
corrplot(contrib, is.cor = FALSE)
dev.off()

chisq.posthoc.test(filtered_eth, method = "bonferroni")


# import medication
med <- fread('medication_cluster_table_w_etn.csv')

med$meds_upper = toupper(med$event)

med_tbl = table(med$event, med$labels)

colnames(med_tbl) <- c('cluster_1', 'cluster_2', 'cluster_3', 'cluster_4', 'cluster_5', 'cluster_6')

med_list <- unique(med$meds_upper)



sink("meds_test.txt")

cat('medications','n' ,"pct" ,
    'Cluster_1', 'Cluster_2', 'Cluster_3', 'Cluster_4', 'Cluster_5', 'Cluster_6',
    'Cluster_1_pct', 'Cluster_2_pct', 'Cluster_3_pct', 'Cluster_4_pct', 'Cluster_5_pct', 'Cluster_6_pct',
    "N_Non_Zero_Clusters", 'p','description' ,'\n', sep='\t')


for (i in med_list) {
  sub <- med %>% filter(meds_upper == i)
  n <- nrow(sub)
  tmp <- as.data.frame(table(sub$labels))
  observed_counts <- c(Cluster_1 = if (length(tmp$Freq[tmp$Var1 == 0])>0) {tmp$Freq[tmp$Var1 == 0]} else {0}, 
                       Cluster_2 = if (length(tmp$Freq[tmp$Var1 == 1])>0) {tmp$Freq[tmp$Var1 == 1]} else {0},
                       Cluster_3 = if (length(tmp$Freq[tmp$Var1 == 2])>0) {tmp$Freq[tmp$Var1 == 2]} else {0}, 
                       Cluster_4 = if (length(tmp$Freq[tmp$Var1 == 3])>0) {tmp$Freq[tmp$Var1 == 3]} else {0},
                       Cluster_5 = if (length(tmp$Freq[tmp$Var1 == 4])>0) {tmp$Freq[tmp$Var1 == 4]} else {0},
                       Cluster_6 = if (length(tmp$Freq[tmp$Var1 == 5])>0) {tmp$Freq[tmp$Var1 == 5]} else {0})
  N_Non_Zero_Clusters <- which(observed_counts != 0) %>% length()
  
  
  if (N_Non_Zero_Clusters>1) {
    erg <- chisq.test(x = observed_counts, p = expected_counts, rescale.p = TRUE)
    p <- erg$p.value
  } else {
    p <- NA
  }
  cat(event=i, n=n, pct= scales::percent(n/total), observed_counts, scales::percent(observed_counts/expected_counts), N_Non_Zero_Clusters, p, paste(sub$description %>% unique(), collapse = ';'), '\n', sep='\t')
  rm(tmp, erg, p, sub, n)
}
sink()



#procedures
proc <- fread('procedure_table_w_etn.csv')
head(proc)

procedure_list <- unique(proc$proc_code)
length(code_list)

procedure_tab <- as.data.frame(table(proc$proc_code))



sink("procedure_test.txt")

cat('proc_code','n' ,"pct" ,
    'Cluster_1', 'Cluster_2', 'Cluster_3', 'Cluster_4', 'Cluster_5', 'CLuster_6',
    'Cluster_1_pct', 'Cluster_2_pct', 'Cluster_3_pct', 'Cluster_4_pct', 'Cluster_5_pct', 'Cluster_6_pct',
    "N_Non_Zero_Clusters", 'p','description' ,'\n', sep='\t')



for (i in procedure_list) {
  sub <- proc %>% filter(proc_code == i)
  n <- nrow(sub)
  tmp <- as.data.frame(table(sub$labels))
  observed_counts <- c(Cluster_1 = if (length(tmp$Freq[tmp$Var1 == 0])>0) {tmp$Freq[tmp$Var1 == 0]} else {0}, 
                       Cluster_2 = if (length(tmp$Freq[tmp$Var1 == 1])>0) {tmp$Freq[tmp$Var1 == 1]} else {0},
                       Cluster_3 = if (length(tmp$Freq[tmp$Var1 == 2])>0) {tmp$Freq[tmp$Var1 == 2]} else {0}, 
                       Cluster_4 = if (length(tmp$Freq[tmp$Var1 == 3])>0) {tmp$Freq[tmp$Var1 == 3]} else {0},
                       Cluster_5 = if (length(tmp$Freq[tmp$Var1 == 4])>0) {tmp$Freq[tmp$Var1 == 4]} else {0},
                       Cluster_6 = if (length(tmp$Freq[tmp$Var1 == 5])>0) {tmp$Freq[tmp$Var1 == 5]} else {0}
)
  N_Non_Zero_Clusters <- which(observed_counts != 0) %>% length()
  
  
  if (N_Non_Zero_Clusters>1) {
    erg <- chisq.test(x = observed_counts, p = expected_counts, rescale.p = TRUE)
    p <- erg$p.value
  } else {
    p <- NA
  }
  cat(phecode=i, n=n, pct= scales::percent(n/total), observed_counts, scales::percent(observed_counts/expected_counts), N_Non_Zero_Clusters, p, paste(sub$proc_description %>% unique(), collapse = ';'), '\n', sep='\t')
  rm(tmp, erg, p, sub, n)
}
sink()