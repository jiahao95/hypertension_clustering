library(data.table)
library(chisq.posthoc.test)
library(dplyr)
library(tidyverse)
library(writexl)

setwd('')
getwd()

disease_p <- fread('diagnosis_test.txt')


disease_p <- disease_p[order(disease_p$p),]
disease_p$p


disease_tbl <- disease_p[ , c(1,4,5,6,7,8,9)]


disease_tbl$phecode <- as.character(disease_tbl$phecode)
rownames(disease_tbl) <- disease_tbl$phecode
disease_tbl$phecode <- NULL

disease_table <-setDT(disease_tbl, keep.rownames=T)



res <- chisq.posthoc.test(disease_table, method='bonferroni', round = 100)

new_df <- merge(res, disease_p, by.x = "Dimension", by.y = "phecode")

head(new_df, 2)

residual_summary <- new_df[new_df$Value == 'Residuals',]


p_summary <- new_df[new_df$Value == 'p values', ]

merged_df <- merge(p_summary, residual_summary, by='Dimension')




sub_df <- subset(merged_df, select = c("Dimension",
                            "Cluster_1.x.x", "Cluster_2.x.x", "Cluster_3.x.x", "Cluster_4.x.x", "Cluster_5.x.x", "CLuster_6.x.x",
                            "Cluster_1.y.x", "Cluster_2.y.x", "Cluster_3.y.x", "Cluster_4.y.x", "Cluster_5.y.x", "CLuster_6.y.x",
                            "Cluster_1_pct.x", "Cluster_2_pct.x", "Cluster_3_pct.x", "Cluster_4_pct.x", "Cluster_5_pct.x", "Cluster_6_pct.x",
                            "Cluster_1.x.y", "Cluster_2.x.y", "Cluster_3.x.y", "Cluster_4.x.y", "Cluster_5.x.y", "CLuster_6.x.y", "p.y", "disease_list.y"))


colnames(sub_df) <- c('Phecode', 
                      'Cluster_1_adj_pvalue', 'Cluster_2_adj_pvalue', 'Cluster_3_adj_pvalue', 'Cluster_4_adj_pvalue', 'Cluster_5_adj_pvalue', 'Cluster_6_adj_pvalue',
                      'Cluster_1_count', 'Cluster_2_count', 'Cluster_3_count', 'Cluster_4_count', 'Cluster_5_count', 'Cluster_6_count',
                      'Cluster_1_count_%', 'CLuster_2_count_%', 'Cluster_3_count_%','Cluster_4_count_%', 'Cluster_5_count_%','Cluster_6_count_%',
                      'Cluster_1_residual', 'Cluster_2_residual', 'Cluster_3_residual', 'Cluster_4_residual', 'Cluster_5_residual', 'Cluster_6_residual',
                      'chisq pvalue', 'disease_name')


head(sub_df,2)

write_xlsx(sub_df, 'summary_stats_table_disease.xlsx')



medications <- fread('meds_test.txt')


medications <- medications[order(p),]
medications$p



medications_tbl <- medications[,c(1,4,5,6,7,8,9)]

rownames(medications_tbl) <- medications_tbl$medications
medications_tbl$medications <- NULL


medication_tbl <-setDT(medications_tbl, keep.rownames = TRUE)

res_med <- chisq.posthoc.test(medication_tbl, method='bonferroni', round = 500)

head(res_med,2)

new_df <- merge(res_med, medications, by.x = "Dimension", by.y = "medications")


med_p <- new_df[new_df$Value == 'p values',]
med_residual <- new_df[new_df$Value == 'Residuals',]

med_merged <- merge(med_p, med_residual, by='Dimension')
head(med_merged,2)
colnames(med_merged)

med_sub <- subset(med_merged, select=c('Dimension','Cluster_1.x.x','Cluster_2.x.x', 'Cluster_3.x.x', 'Cluster_4.x.x', 'Cluster_5.x.x', 'Cluster_6.x.x',
                  'n.x',
                  'Cluster_1.y.x', 'Cluster_2.y.x', 'Cluster_3.y.x', 'Cluster_4.y.x', 'Cluster_5.y.x', 'Cluster_6.y.x',
                  'Cluster_1_pct.x', 'Cluster_2_pct.x', 'Cluster_3_pct.x', 'Cluster_4_pct.x', 'Cluster_5_pct.x', 'Cluster_6_pct.x',
                  'Cluster_1.x.y','Cluster_2.x.y','Cluster_3.x.y','Cluster_4.x.y', 'Cluster_5.x.y','Cluster_6.x.y',
                  'p.x', 'description.y'))

colnames(med_sub) <- c('Medication', 
                      'Cluster_1_adj_pvalue', 'Cluster_2_adj_pvalue', 'Cluster_3_adj_pvalue', 'Cluster_4_adj_pvalue', 'Cluster_5_adj_pvalue', 'Cluster_6_adj_pvalue',
                      'n_total','Cluster_1_count', 'Cluster_2_count', 'Cluster_3_count', 'Cluster_4_count', 'Cluster_5_count', 'Cluster_6_count',
                      'Cluster_1_count_%', 'Cluster_2_count_%', 'Cluster_3_count_%','Cluster_4_count_%', 'Cluster_5_count_%','Cluster_6_count_%',
                      'Cluster_1_residual', 'Cluster_2_residual', 'Cluster_3_residual', 'Cluster_4_residual', 'Cluster_5_residual', 'Cluster_6_residual',
                      'chisq pvalue', 'Full_description')


head(med_sub,2)
write_xlsx(med_sub, 'summary_stats_table_medication.xlsx')





# import procedures
procedure_p <- fread('procedure_test.txt')
str(procedure_p)
length(unique(procedure_p$proc_code))
head(procedure)


procedure <- procedure_p[order(p), ]
procedure$p





procedure_tbl <- procedure[1:453,c(1,4,5,6,7,8,9)]
head(procedure_tbl)

rownames(procedure_tbl) <- procedure_tbl$proc_code
procedure_tbl$proc_code <- NULL


procedure_tbl <-setDT(procedure_tbl, keep.rownames = TRUE)

res_proc <- chisq.posthoc.test(procedure_tbl, method='bonferroni')

head(res_proc)

new_df <- merge(res_proc, procedure, by.x = "Dimension", by.y = "proc_code")


proc_p <- new_df[new_df$Value == 'p values',]
proc_residual <- new_df[new_df$Value == 'Residuals',]

proc_merged <- merge(proc_p, proc_residual, by='Dimension')

str(proc_merged)

proc_sub <- subset(proc_merged, select=c('Dimension','Cluster_1.x.x','Cluster_2.x.x', 'Cluster_3.x.x', 'Cluster_4.x.x', 'Cluster_5.x.x', 'CLuster_6.x.x', 
                  'n.x',
                  'Cluster_1.y.x', 'Cluster_2.y.x', 'Cluster_3.y.x', 'Cluster_4.y.x', 'Cluster_5.y.x', 'CLuster_6.y.x',
                  'Cluster_1_pct.x', 'Cluster_2_pct.x', 'Cluster_3_pct.x', 'Cluster_4_pct.x', 'Cluster_5_pct.x', 'Cluster_6_pct.x',
                  'Cluster_1.x.y','Cluster_2.x.y','Cluster_3.x.y','Cluster_4.x.y','Cluster_5.x.y','CLuster_6.x.y', 
                  'p.x', 'description.y'))

colnames(proc_sub) <- c('Procedure', 
                      'Cluster_1_adj_pvalue', 'Cluster_2_adj_pvalue', 'Cluster_3_adj_pvalue', 'Cluster_4_adj_pvalue', 'Cluster_5_adj_pvalue', 'Cluster_6_adj_pvalue',
                      'n_total','Cluster_1_count', 'Cluster_2_count', 'Cluster_3_count', 'Cluster_4_count',  'Cluster_5_count', 'Cluster_6_count', 
                      'Cluster_1_count_%', 'Cluster_2_count_%', 'Cluster_3_count_%','Cluster_4_count_%', 'Cluster_5_count_%','Cluster_6_count_%', 
                      'Cluster_1_residual', 'Cluster_2_residual', 'Cluster_3_residual', 'Cluster_4_residual', 'Cluster_5_residual', 'Cluster_6_residual', 
                      'chisq pvalue', 'Full_description')


library(writexl)

write_xlsx(proc_sub, 'summary_stats_table_procedure_integrative.xlsx')

