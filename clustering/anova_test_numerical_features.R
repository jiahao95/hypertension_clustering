library(data.table)
library(zoo)
library(dplyr)
library(ggpubr)
library(multcomp)
library(broom)
library(tidyverse)
library(car)
library(ggplot2)
library(openxlsx)


setwd('')
getwd()



# BP
prs_sbp <- fread('SBP_w_label.csv')
prs_dbp <- fread('DBP_w_label.csv')
prs_pp <- fread('PP_w_label.csv')

head(prs_sbp)

prs_sbp_summary <- group_by(prs_sbp, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE1_AVG, na.rm = TRUE),
    sd = sd(SCORE1_AVG, na.rm = TRUE),
    max = max(SCORE1_AVG, na.rm = TRUE),
    min = min(SCORE1_AVG, na.rm = TRUE),
    median = median(SCORE1_AVG, na.rm = TRUE)
  )


prs_dbp_summary <- group_by(prs_dbp, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE1_AVG, na.rm = TRUE),
    sd = sd(SCORE1_AVG, na.rm = TRUE),
    max = max(SCORE1_AVG, na.rm = TRUE),
    min = min(SCORE1_AVG, na.rm = TRUE),
    median = median(SCORE1_AVG, na.rm = TRUE)
  )

prs_pp_summary <- group_by(prs_pp, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE1_AVG, na.rm = TRUE),
    sd = sd(SCORE1_AVG, na.rm = TRUE),
    max = max(SCORE1_AVG, na.rm = TRUE),
    min = min(SCORE1_AVG, na.rm = TRUE),
    median = median(SCORE1_AVG, na.rm = TRUE)
  )

group_by(prs_pp, self_reported_race) %>%
  summarise(
    count = n(),
    mean = mean(SCORE1_AVG, na.rm = TRUE),
    sd = sd(SCORE1_AVG, na.rm = TRUE),
    max = max(SCORE1_AVG, na.rm = TRUE),
    min = min(SCORE1_AVG, na.rm = TRUE),
    median = median(SCORE1_AVG, na.rm = TRUE)
  )


write.csv(prs_sbp_summary, 'prs_sbp_summary.csv')
write.csv(prs_dbp_summary, 'prs_dbp_summary.csv')
write.csv(prs_pp_summary, 'prs_pp_summary.csv')

prs_sbp_afr <- prs_sbp[prs_sbp$self_reported_race == 'African American',]
prs_sbp_asian <- prs_sbp[prs_sbp$self_reported_race == 'Asian',]
prs_sbp_eu <- prs_sbp[prs_sbp$self_reported_race == 'European American',]
prs_sbp_his <- prs_sbp[prs_sbp$self_reported_race == 'Hispanic',]
prs_sbp_nat <- prs_sbp[prs_sbp$self_reported_race == 'Native American',]
prs_sbp_other <- prs_sbp[prs_sbp$self_reported_race == 'Other']

prs_dbp_afr <- prs_dbp[prs_dbp$self_reported_race == 'African American',]
prs_dbp_asian <- prs_dbp[prs_dbp$self_reported_race == 'Asian',]
prs_dbp_eu <- prs_dbp[prs_dbp$self_reported_race == 'European American',]
prs_dbp_his <- prs_dbp[prs_dbp$self_reported_race == 'Hispanic',]
prs_dbp_nat <- prs_dbp[prs_dbp$self_reported_race == 'Native American',]
prs_dbp_other <- prs_dbp[prs_dbp$self_reported_race == 'Other']

prs_pp_afr <- prs_pp[prs_pp$self_reported_race == 'African American',]
prs_pp_asian <- prs_pp[prs_pp$self_reported_race == 'Asian',]
prs_pp_eu <- prs_pp[prs_pp$self_reported_race == 'European American',]
prs_pp_his <- prs_pp[prs_pp$self_reported_race == 'Hispanic',]
prs_pp_nat <- prs_pp[prs_pp$self_reported_race == 'Native American',]
prs_pp_other <- prs_pp[prs_pp$self_reported_race == 'Other']


temp_1 <- tidy(aov(SCORE1_AVG ~ labels, data = prs_sbp)) 
res_1 <- temp_1 %>% filter(term=='labels')%>%mutate(trait='SBP PRS', ancestry='ALL')

aov_sbp_african <- tidy(aov(SCORE1_AVG ~ labels, data = prs_sbp_afr ))
aov_sbp_asian <- tidy(aov(SCORE1_AVG ~ labels, data = prs_sbp_asian ))
aov_sbp_european <- tidy(aov(SCORE1_AVG ~ labels, data = prs_sbp_eu ))
aov_sbp_native <- tidy(aov(SCORE1_AVG ~ labels, data = prs_sbp_nat )) 
aov_sbp_hispanic <- tidy(aov(SCORE1_AVG ~ labels, data = prs_sbp_his )) 
aov_sbp_other <- tidy(aov(SCORE1_AVG ~ labels, data = prs_sbp_other )) 

for(temp.dat in c('aov_sbp_african', 'aov_sbp_asian', 'aov_sbp_european', 'aov_sbp_native', 'aov_sbp_hispanic', 'aov_sbp_other')){
  temp <- get(temp.dat)
  temp <- temp %>% 
  filter(term=='labels')%>%
  mutate(trait=paste0(toupper(unlist(strsplit(temp.dat,"_"))[2]), " PRS"), 
  ancestry = toupper(unlist(strsplit(temp.dat,"_"))[3]))
  res_1 <- bind_rows(res_1, temp)
}

res_1
write.csv(res_1, 'aov_sbp_eth.csv')

# POSTHOC
post_sbp_african <- tidy(TukeyHSD(aov(SCORE1_AVG ~ labels, data = prs_sbp_afr )))
post_sbp_asian <- tidy(TukeyHSD(aov(SCORE1_AVG ~ labels, data = prs_sbp_asian )))
post_sbp_european <- tidy(TukeyHSD(aov(SCORE1_AVG ~ labels, data = prs_sbp_eu )))
post_sbp_native <- tidy(TukeyHSD(aov(SCORE1_AVG ~ labels, data = prs_sbp_nat ))) 
post_sbp_hispanic <- tidy(TukeyHSD(aov(SCORE1_AVG ~ labels, data = prs_sbp_his ))) 
post_sbp_other <- tidy(TukeyHSD(aov(SCORE1_AVG ~ labels, data = prs_sbp_other ))) # 2-1, 3-2, 5-2

post_sbp_african <- post_sbp_african %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_sbp_african',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_sbp_african',"_"))[3]))

post_sbp_asian <- post_sbp_asian %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_sbp_asian',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_sbp_asian',"_"))[3]))

post_sbp_european <- post_sbp_european %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_sbp_european',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_sbp_european',"_"))[3]))

post_sbp_other <- post_sbp_other %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_sbp_other',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_sbp_other',"_"))[3]))

post_sbp_hispanic <- post_sbp_hispanic %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_sbp_hispanic',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_sbp_hispanic',"_"))[3]))

post_sbp_native <- post_sbp_native %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_sbp_native',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_sbp_native',"_"))[3]))

post_sbp <- bind_rows(post_sbp_african, post_sbp_asian, post_sbp_european,
                              post_sbp_hispanic, post_sbp_native, post_sbp_other)

write.csv(post_sbp, 'post_sbp.csv')

jpeg('sbp_box_str.jpg')
ggplot(prs_sbp, aes(x=self_reported_race, y=SCORE1_AVG)) + 
    geom_boxplot(aes(fill=labels)) + 
    xlab('') + ylab('Polygenic risk score') + ggtitle('') +
    guides(fill=guide_legend(title="clusters")) +theme_bw()
dev.off()


temp_2 <- tidy(aov(SCORE1_AVG ~ labels, data = prs_dbp)) 
res_2 <- temp_1 %>% filter(term=='labels')%>%mutate(trait='DBP PRS', ancestry='ALL')

aov_dbp_african <- tidy(aov(SCORE1_AVG ~ labels, data = prs_dbp_afr ))
aov_dbp_asian <- tidy(aov(SCORE1_AVG ~ labels, data = prs_dbp_asian ))
aov_dbp_european <- tidy(aov(SCORE1_AVG ~ labels, data = prs_dbp_eu ))
aov_dbp_native <- tidy(aov(SCORE1_AVG ~ labels, data = prs_dbp_nat )) 
aov_dbp_hispanic <- tidy(aov(SCORE1_AVG ~ labels, data = prs_dbp_his )) #
aov_dbp_other <- tidy(aov(SCORE1_AVG ~ labels, data = prs_dbp_other )) #

for(temp.dat in c('aov_dbp_african', 'aov_dbp_asian', 'aov_dbp_european', 'aov_dbp_native', 'aov_dbp_hispanic', 'aov_dbp_other')){
  temp <- get(temp.dat)
  temp <- temp %>% 
  filter(term=='labels')%>%
  mutate(trait=paste0(toupper(unlist(strsplit(temp.dat,"_"))[2]), " PRS"), 
  ancestry = toupper(unlist(strsplit(temp.dat,"_"))[3]))
  res_2 <- bind_rows(res_2, temp)
}

res_2
write.csv(res_2, 'aov_dbp_eth.csv')


post_dbp_african <- tidy(TukeyHSD(aov(SCORE1_AVG ~ labels, data = prs_dbp_afr )))
post_dbp_asian <- tidy(TukeyHSD(aov(SCORE1_AVG ~ labels, data = prs_dbp_asian )))
post_dbp_european <- tidy(TukeyHSD(aov(SCORE1_AVG ~ labels, data = prs_dbp_eu )))
post_dbp_native <- tidy(TukeyHSD(aov(SCORE1_AVG ~ labels, data = prs_dbp_nat ))) 
post_dbp_hispanic <- tidy(TukeyHSD(aov(SCORE1_AVG ~ labels, data = prs_dbp_his ))) 
post_dbp_other <- tidy(TukeyHSD(aov(SCORE1_AVG ~ labels, data = prs_dbp_other ))) # 2-1, 3-2, 5-2

post_dbp_african <- post_dbp_african %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_dbp_african',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_dbp_african',"_"))[3]))

post_dbp_asian <- post_dbp_asian %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_dbp_asian',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_dbp_asian',"_"))[3]))

post_dbp_european <- post_dbp_european %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_dbp_european',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_dbp_european',"_"))[3]))

post_dbp_other <- post_dbp_other %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_dbp_other',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_dbp_other',"_"))[3]))

post_dbp_hispanic <- post_dbp_hispanic %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_dbp_hispanic',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_dbp_hispanic',"_"))[3]))

post_dbp_native <- post_dbp_native %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_dbp_native',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_dbp_native',"_"))[3]))

post_dbp <- bind_rows(post_dbp_african, post_dbp_asian, post_dbp_european,
                              post_dbp_hispanic, post_dbp_native, post_dbp_other)

write.csv(post_dbp, 'post_dbp.csv')

jpeg('dbp_box_str.jpg')
ggplot(prs_dbp, aes(x=self_reported_race, y=SCORE1_AVG)) + 
    geom_boxplot(aes(fill=labels)) + 
    xlab('') + ylab('Polygenic risk score') + ggtitle('') +
    guides(fill=guide_legend(title="clusters")) +theme_bw()
dev.off()

jpeg('dbp_box.jpg')
ggplot(prs_dbp, aes(x=labels, y=SCORE1_AVG)) + 
    geom_boxplot(aes(fill=labels)) + 
    xlab('') + ylab('Polygenic risk score') + ggtitle('') +
    guides(fill=guide_legend(title="clusters")) +theme_bw()
dev.off()

temp_3 <- tidy(aov(SCORE1_AVG ~ labels, data = prs_pp)) 
res_3 <- temp_1 %>% filter(term=='labels')%>%mutate(trait='PP PRS', ancestry='ALL')

aov_pp_african <- tidy(aov(SCORE1_AVG ~ labels, data = prs_pp_afr ))
aov_pp_asian <- tidy(aov(SCORE1_AVG ~ labels, data = prs_pp_asian ))
aov_pp_european <- tidy(aov(SCORE1_AVG ~ labels, data = prs_pp_eu ))
aov_pp_native <- tidy(aov(SCORE1_AVG ~ labels, data = prs_pp_nat )) 
aov_pp_hispanic <- tidy(aov(SCORE1_AVG ~ labels, data = prs_pp_his )) #
aov_pp_other <- tidy(aov(SCORE1_AVG ~ labels, data = prs_pp_other )) #

for(temp.dat in c('aov_pp_african', 'aov_pp_asian', 'aov_pp_european', 'aov_pp_native', 'aov_pp_hispanic', 'aov_pp_other')){
  temp <- get(temp.dat)
  temp <- temp %>% 
  filter(term=='labels')%>%
  mutate(trait=paste0(toupper(unlist(strsplit(temp.dat,"_"))[2]), " PRS"), 
  ancestry = toupper(unlist(strsplit(temp.dat,"_"))[3]))
  res_3 <- bind_rows(res_3, temp)
}

res_3
write.csv(res_3, 'aov_pp_eth.csv')

jpeg('pp_box_str.jpg')
ggplot(prs_pp, aes(x=self_reported_race, y=SCORE1_AVG)) + 
    geom_boxplot(aes(fill=labels)) + 
    xlab('') + ylab('Polygenic risk score') + ggtitle('') +
    guides(fill=guide_legend(title="clusters")) +theme_bw()
dev.off()

jpeg('pp_box.jpg')
ggplot(prs_sbp, aes(x=labels, y=SCORE1_AVG)) + 
    geom_boxplot(aes(fill=labels)) + 
    xlab('') + ylab('Polygenic risk score') + ggtitle('') +
    guides(fill=guide_legend(title="clusters")) +theme_bw()
dev.off()


post_pp_african <- tidy(TukeyHSD(aov(SCORE1_AVG ~ labels, data = prs_pp_afr )))
post_pp_asian <- tidy(TukeyHSD(aov(SCORE1_AVG ~ labels, data = prs_pp_asian )))
post_pp_european <- tidy(TukeyHSD(aov(SCORE1_AVG ~ labels, data = prs_pp_eu )))
post_pp_native <- tidy(TukeyHSD(aov(SCORE1_AVG ~ labels, data = prs_pp_nat ))) 
post_pp_hispanic <- tidy(TukeyHSD(aov(SCORE1_AVG ~ labels, data = prs_pp_his ))) 
post_pp_other <- tidy(TukeyHSD(aov(SCORE1_AVG ~ labels, data = prs_pp_other ))) # 2-1, 3-2, 5-2

post_pp_african <- post_pp_african %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_pp_african',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_pp_african',"_"))[3]))

post_pp_asian <- post_pp_asian %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_pp_asian',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_pp_asian',"_"))[3]))

post_pp_european <- post_pp_european %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_pp_european',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_pp_european',"_"))[3]))

post_pp_other <- post_pp_other %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_pp_other',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_pp_other',"_"))[3]))

post_pp_hispanic <- post_pp_hispanic %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_pp_hispanic',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_pp_hispanic',"_"))[3]))

post_pp_native <- post_pp_native %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_pp_native',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_pp_native',"_"))[3]))

post_pp <- bind_rows(post_pp_african, post_pp_asian, post_pp_european,
                              post_pp_hispanic, post_pp_native, post_pp_other)

write.csv(post_pp, 'post_pp.csv')




# alzheimer
alzheimer <- fread('alzheimer.csv')
head(alzheimer)

# anova test per cluster
aov_alzheimer_all <- tidy(aov(SCORE ~ labels, data = alzheimer))

# data distribution
jpeg('alz_hist.jpg')
hist(alzheimer$SCORE, xlab = "Alzheimer's disease PRS", main = "Histogram of Alzheimer's disease PRS distribution")
dev.off()
group_by(alzheimer, self_reported_race) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  )

# per cluster
alzheimer_1 <- alzheimer[alzheimer$labels == 'cluster_1',]
alzheimer_2 <- alzheimer[alzheimer$labels == 'cluster_2',]
alzheimer_3 <- alzheimer[alzheimer$labels == 'cluster_3',]
alzheimer_4 <- alzheimer[alzheimer$labels == 'cluster_4',]
alzheimer_5 <- alzheimer[alzheimer$labels == 'cluster_5',]

# per ethnicity
alzheimer_afr <- alzheimer[alzheimer$self_reported_race == 'African American',]
alzheimer_asian <- alzheimer[alzheimer$self_reported_race == 'Asian',]
alzheimer_eu <- alzheimer[alzheimer$self_reported_race == 'European American',]
alzheimer_his <- alzheimer[alzheimer$self_reported_race == 'Hispanic',]
alzheimer_nat <- alzheimer[alzheimer$self_reported_race == 'Native American',]
alzheimer_other <- alzheimer[alzheimer$self_reported_race == 'Other']


aov_alzheimer_all <- tidy(aov(SCORE ~ labels, data = alzheimer))
res_alz <- aov_alzheimer_all %>% filter(term=='labels')%>%mutate(trait="Alzheimer's disease", ancestry='ALL')

aov_alzheimer_african <- tidy(aov(SCORE ~ labels, data = alzheimer_afr ))
aov_alzheimer_asian <- tidy(aov(SCORE ~ labels, data = alzheimer_asian ))
aov_alzheimer_european <- tidy(aov(SCORE ~ labels, data = alzheimer_eu ))
aov_alzheimer_native <- tidy(aov(SCORE ~ labels, data = alzheimer_nat )) 
aov_alzheimer_hispanic <- tidy(aov(SCORE ~ labels, data = alzheimer_his )) #
aov_alzheimer_other <- tidy(aov(SCORE ~ labels, data = alzheimer_other )) 

for(temp.dat in c('aov_alzheimer_african', 'aov_alzheimer_asian', 'aov_alzheimer_european', 'aov_alzheimer_native', 'aov_alzheimer_hispanic', 'aov_alzheimer_other')){
  temp <- get(temp.dat)
  temp <- temp %>% 
  filter(term=='labels')%>%
  mutate(trait=paste0(toupper(unlist(strsplit(temp.dat,"_"))[2]), " PRS"), 
  ancestry = toupper(unlist(strsplit(temp.dat,"_"))[3]))
  res_alz <- bind_rows(res_alz, temp)
}

res_alz
write.csv(res_alz, 'alzheimer_eth_int.csv')

# boxplot per ethnicity and cluster
jpeg('SBP_box_afr.jpg')
ggplot(alzheimer_afr, aes(x=labels, y=SCORE)) + 
    geom_boxplot(aes(fill=labels)) + 
    xlab('') + ylab('Polygenic risk score') + ggtitle('PRS African American') +
    guides(fill=guide_legend(title="clusters"))
dev.off()

jpeg('SBP_box_asian.jpg')
ggplot(alzheimer_asian, aes(x=labels, y=SCORE)) + 
    geom_boxplot(aes(fill=labels)) + 
    xlab('') + ylab('Polygenic risk score') + ggtitle('PRS Asian American') +
    guides(fill=guide_legend(title="clusters"))
dev.off()

jpeg('SBP_box_eu.jpg')
ggplot(alzheimer_eu, aes(x=labels, y=SCORE)) + 
    geom_boxplot(aes(fill=labels)) + 
    xlab('') + ylab('Polygenic risk score') + ggtitle('PRS European American') +
    guides(fill=guide_legend(title="clusters"))
dev.off()

jpeg('SBP_box_his.jpg')
ggplot(alzheimer_his, aes(x=labels, y=SCORE)) + 
    geom_boxplot(aes(fill=labels)) + 
    xlab('') + ylab('Polygenic risk score') + ggtitle('PRS Hispanic American') +
    guides(fill=guide_legend(title="clusters"))
dev.off()

jpeg('SBP_box_native.jpg')
ggplot(alzheimer_nat, aes(x=labels, y=SCORE)) + 
    geom_boxplot(aes(fill=labels)) + 
    xlab('') + ylab('Polygenic risk score') + ggtitle('PRS Native American') +
    guides(fill=guide_legend(title="clusters"))
dev.off()

jpeg('SBP_box_other.jpg')
ggplot(alzheimer_other, aes(x=labels, y=SCORE)) + 
    geom_boxplot(aes(fill=labels)) + 
    xlab('') + ylab('Polygenic risk score') + ggtitle('PRS Other') +
    guides(fill=guide_legend(title="clusters"))
dev.off()

# summary table
group_by(alzheimer_afr, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  )


group_by(alzheimer_nat, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  )

group_by(alzheimer_his, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  )

group_by(alzheimer_other, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  )

group_by(alzheimer_asian, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  )

group_by(alzheimer_eu, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  )

# summary table per cluster
group_by(alzheimer, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  )

# summary table per ethnicity
group_by(alzheimer, self_reported_race) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  )
tidy(aov(SCORE ~ self_reported_race, data = alzheimer))

tidy(TukeyHSD(aov(SCORE ~ self_reported_race, data = alzheimer)))

jpeg('az_race_box.jpg')
ggboxplot(alzheimer, x = "self_reported_race", y = "SCORE", 
          color = "self_reported_race", palette = c('green', 'blue', 'gray', 'brown','red','violet'),
          order = c('European American', 'Asian', 'African American', 'Other', 'Hispanic', 'Native American'),
          ylab = "Alzheimer's disease PRS", xlab='Ethinic group') #add = 'jitter', shape = 'labels')
dev.off()




# asthma
asthma <- fread('asthma.csv')
head(asthma)

group_by(asthma, self_reported_race) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  )



jpeg('asthma_hist.jpg')
hist(asthma$SCORE, xlab = "Asthma PRS", main = "Histogram of asthma PRS distribution")
dev.off()



# anova test per cluster
aov_asthma_all <- tidy(aov(SCORE ~ labels, data = asthma))
res_asthma <- aov_asthma_all %>% filter(term=='labels')%>%mutate(trait="asthma", ancestry='ALL')

tidy(TukeyHSD(aov(SCORE ~ labels, data = asthma))) 

# summary table per ethnicity
group_by(asthma, self_reported_race) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  )

# summary table per cluster
group_by(asthma, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  )


tidy(TukeyHSD(aov(SCORE ~ self_reported_race, data = asthma)))

jpeg('asthma_race_box.jpg')
ggboxplot(asthma, x = "self_reported_race", y = "SCORE", 
          color = "self_reported_race", palette = c('green', 'blue', 'gray', 'brown','red','violet'),
          order = c('European American', 'Asian', 'African American', 'Other', 'Hispanic', 'Native American'),
          ylab = "Asthma PRS", xlab='Ethinic group') #add = 'jitter', shape = 'labels')
dev.off()

# per ethnicity
asthma_afr <- asthma[asthma$self_reported_race == 'African American',]
asthma_asian <- asthma[asthma$self_reported_race == 'Asian',]
asthma_eu <- asthma[asthma$self_reported_race == 'European American',]
asthma_his <- asthma[asthma$self_reported_race == 'Hispanic',]
asthma_nat <- asthma[asthma$self_reported_race == 'Native American',]
asthma_other <- asthma[asthma$self_reported_race == 'Other']

# AOV 
aov_asthma_african <- tidy(aov(SCORE ~ labels, data = asthma_afr ))
aov_asthma_asian <- tidy(aov(SCORE ~ labels, data = asthma_asian ))
aov_asthma_european <- tidy(aov(SCORE ~ labels, data = asthma_eu ))
aov_asthma_native <- tidy(aov(SCORE ~ labels, data = asthma_nat )) 
aov_asthma_hispanic <- tidy(aov(SCORE ~ labels, data = asthma_his )) #
aov_asthma_other <- tidy(aov(SCORE ~ labels, data = asthma_other )) 

for(temp.dat in c('aov_asthma_african', 'aov_asthma_asian', 'aov_asthma_european', 'aov_asthma_native', 'aov_asthma_hispanic', 'aov_asthma_other')){
  temp <- get(temp.dat)
  temp <- temp %>% 
  filter(term=='labels')%>%
  mutate(trait=paste0(toupper(unlist(strsplit(temp.dat,"_"))[2]), " PRS"), 
  ancestry = toupper(unlist(strsplit(temp.dat,"_"))[3]))
  res_asthma <- bind_rows(res_asthma, temp)
}

res_asthma
write.csv(res_asthma, 'asthma_eth_prs.csv')

tidy(TukeyHSD(aov(SCORE ~ labels, data = asthma_afr )))
tidy(TukeyHSD(aov(SCORE ~ labels, data = asthma_asian )))
tidy(TukeyHSD(aov(SCORE ~ labels, data = asthma_eu )))
tidy(TukeyHSD(aov(SCORE ~ labels, data = asthma_nat ))) 
tidy(TukeyHSD(aov(SCORE ~ labels, data = asthma_his ))) 
tidy(TukeyHSD(aov(SCORE ~ labels, data = asthma_other ))) 


# POSTHOC
post_asthma_african <- tidy(TukeyHSD(aov(SCORE ~ labels, data = asthma_afr )))
post_asthma_asian <- tidy(TukeyHSD(aov(SCORE ~ labels, data = asthma_asian )))
post_asthma_european <- tidy(TukeyHSD(aov(SCORE ~ labels, data = asthma_eu )))
post_asthma_native <- tidy(TukeyHSD(aov(SCORE ~ labels, data = asthma_nat ))) 
post_asthma_hispanic <- tidy(TukeyHSD(aov(SCORE ~ labels, data = asthma_his ))) 
post_asthma_other <- tidy(TukeyHSD(aov(SCORE ~ labels, data = asthma_other ))) # 2-1, 3-2, 5-2

post_asthma_african <- post_asthma_african %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_asthma_african',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_asthma_african',"_"))[3]))

post_asthma_asian <- post_asthma_asian %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_asthma_asian',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_asthma_asian',"_"))[3]))

post_asthma_european <- post_asthma_european %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_asthma_european',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_asthma_european',"_"))[3]))

post_asthma_other <- post_asthma_other %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_asthma_other',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_asthma_other',"_"))[3]))

post_asthma_hispanic <- post_asthma_hispanic %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_asthma_hispanic',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_asthma_hispanic',"_"))[3]))

post_asthma_native <- post_asthma_native %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_asthma_native',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_asthma_native',"_"))[3]))

post_asthma <- bind_rows(post_asthma_african, post_asthma_asian, post_asthma_european,
                              post_asthma_hispanic, post_asthma_native, post_asthma_other)

write.csv(post_asthma, 'post_asthma_int.csv')




# bmi
bmi <- fread('bmi.csv')
head(bmi)

jpeg('bmi_hist.jpg')
hist(bmi$SCORE, xlab = "BMI PRS", main = "Histogram of BMI PRS distribution")
dev.off()

jpeg('bmi_race_box.jpg')
ggboxplot(bmi, x = "self_reported_race", y = "SCORE", 
          color = "self_reported_race", palette = c('green', 'blue', 'gray', 'brown','red','violet'),
          order = c('European American', 'Asian', 'African American', 'Other', 'Hispanic', 'Native American'),
          ylab = "BMI PRS", xlab='Ethinic group')
dev.off()
# anova test per cluster
aov_bmi_all <- tidy(aov(SCORE ~ labels, data = bmi))

tidy(TukeyHSD(aov(SCORE ~ labels, data = bmi))) 

# summary table per cluster
bmi_sum <- as.data.frame(group_by(bmi, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  ))

write.csv(bmi_sum, 'bmi_sum.csv')

# per ethnicity
bmi_afr <- bmi[bmi$self_reported_race == 'African American',]
bmi_asian <- bmi[bmi$self_reported_race == 'Asian',]
bmi_eu <- bmi[bmi$self_reported_race == 'European American',]
bmi_his <- bmi[bmi$self_reported_race == 'Hispanic',]
bmi_nat <- bmi[bmi$self_reported_race == 'Native American',]
bmi_other <- bmi[bmi$self_reported_race == 'Other']


# summary table per cluster and ethnicity
group_by(bmi_afr, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  )


group_by(bmi_asian, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  )

group_by(bmi_eu, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  )

# AOV 
aov_bmi_afr <- tidy(aov(SCORE ~ labels, data = bmi_afr ))
aov_bmi_asian <- tidy(aov(SCORE ~ labels, data = bmi_asian ))
aov_bmi_eu <- tidy(aov(SCORE ~ labels, data = bmi_eu ))
aov_bmi_nat <- tidy(aov(SCORE ~ labels, data = bmi_nat )) #
aov_bmi_his <- tidy(aov(SCORE ~ labels, data = bmi_his )) 
aov_bmi_other <- tidy(aov(SCORE ~ labels, data = bmi_other )) 



aov_bmi_all <- tidy(aov(SCORE ~ labels, data = bmi))
res_bmi <- aov_bmi_all %>% filter(term=='labels')%>%mutate(trait="BMI PRS", ancestry='ALL')

aov_bmi_african <- tidy(aov(SCORE ~ labels, data = bmi_afr ))
aov_bmi_asian <- tidy(aov(SCORE ~ labels, data = bmi_asian ))
aov_bmi_european <- tidy(aov(SCORE ~ labels, data = bmi_eu ))
aov_bmi_native <- tidy(aov(SCORE ~ labels, data = bmi_nat )) #
aov_bmi_hispanic <- tidy(aov(SCORE ~ labels, data = bmi_his )) 
aov_bmi_other <- tidy(aov(SCORE ~ labels, data = bmi_other )) 

for(temp.dat in c('aov_bmi_african', 'aov_bmi_asian', 'aov_bmi_european', 'aov_bmi_native', 'aov_bmi_hispanic', 'aov_bmi_other')){
  temp <- get(temp.dat)
  temp <- temp %>% 
  filter(term=='labels')%>%
  mutate(trait=paste0(toupper(unlist(strsplit(temp.dat,"_"))[2]), " PRS"), 
  ancestry = toupper(unlist(strsplit(temp.dat,"_"))[3]))
  res_bmi <- bind_rows(res_bmi, temp)
}

res_bmi
write.csv(res_bmi, 'bmi_eth.csv')

# cad
cad <- fread('cad.csv')
head(cad)

jpeg('cad_hist.jpg')
hist(cad$SCORE, xlab = "CAD PRS", main = "Histogram of CAD PRS distribution")
dev.off()

jpeg('cad_box.jpg')
ggboxplot(cad, x = "self_reported_race", y = "SCORE", 
          color = "self_reported_race", palette = c('green', 'blue', 'gray', 'brown','red','violet'),
          order = c('European American', 'Asian', 'African American', 'Other', 'Hispanic', 'Native American'),
          ylab = "CAD PRS", xlab='Ethinic group')
dev.off()

# anova test per cluster
aov_cad_all <- tidy(aov(SCORE ~ labels, data = cad))

tidy(TukeyHSD(aov(SCORE ~ labels, data = cad)))

cad_summary <- as.data.frame(group_by(cad, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  )) # highest in 1, lowest in 2

write.csv(cad_summary, 'cad_stats_ehr.csv')


# per ethnicity
cad_afr <- cad[cad$self_reported_race == 'African American',]
cad_asian <- cad[cad$self_reported_race == 'Asian',]
cad_eu <- cad[cad$self_reported_race == 'European American',]
cad_his <- cad[cad$self_reported_race == 'Hispanic',]
cad_nat <- cad[cad$self_reported_race == 'Native American',]
cad_other <- cad[cad$self_reported_race == 'Other']

# AOV 
aov_cad_african <- tidy(aov(SCORE ~ labels, data = cad_afr ))
aov_cad_asian <- tidy(aov(SCORE ~ labels, data = cad_asian ))
aov_cad_european <- tidy(aov(SCORE ~ labels, data = cad_eu )) 
aov_cad_native <- tidy(aov(SCORE ~ labels, data = cad_nat )) 
aov_cad_hispanic <- tidy(aov(SCORE ~ labels, data = cad_his )) 
aov_cad_other <- tidy(aov(SCORE ~ labels, data = cad_other )) 


res_cad <- aov_cad_all %>% filter(term=='labels')%>%mutate(trait='CAD PRS', ancestry='ALL')


for(temp.dat in c('aov_cad_african', 'aov_cad_asian', 'aov_cad_european', 'aov_cad_native', 'aov_cad_hispanic', 'aov_cad_other')){
  temp <- get(temp.dat)
  temp <- temp %>% 
  filter(term=='labels')%>%
  mutate(trait=paste0(toupper(unlist(strsplit(temp.dat,"_"))[2]), " PRS"), 
  ancestry = toupper(unlist(strsplit(temp.dat,"_"))[3]))
  res_cad <- bind_rows(res_cad, temp)
}

res_cad
write.csv(res_cad, 'cad_eth.csv')

# POSTHOC
post_cad_afr <- tidy(TukeyHSD(aov(SCORE ~ labels, data = cad_afr )))
post_cad_asian <- tidy(TukeyHSD(aov(SCORE ~ labels, data = cad_asian )))
post_cad_eu <- tidy(TukeyHSD(aov(SCORE ~ labels, data = cad_eu ))) # 2-1, 5-1
post_cad_nat <- tidy(TukeyHSD(aov(SCORE ~ labels, data = cad_nat ))) 
post_cad_his <- tidy(TukeyHSD(aov(SCORE ~ labels, data = cad_his ))) 
post_cad_other <- tidy(TukeyHSD(aov(SCORE ~ labels, data = cad_other ))) 




# cholesterol
cholesterol <- fread('cholesterol.csv')
head(cholesterol)

jpeg('cho_hist.jpg')
hist(cholesterol$SCORE, xlab = "Cholesterol PRS", main = "Histogram of cholesterol PRS distribution")
dev.off()

jpeg('chol_box.jpg')
ggboxplot(cholesterol, x = "self_reported_race", y = "SCORE", 
          color = "self_reported_race", palette = c('green', 'blue', 'gray', 'brown','red','violet'),
          order = c('European American', 'Asian', 'African American', 'Other', 'Hispanic', 'Native American'),
          ylab = "Cholesterol PRS", xlab='Ethinic group')
dev.off()

# anova test per cluster
aov_cholesterol_all <- tidy(aov(SCORE ~ labels, data = cholesterol)) #0.000000551

tidy(TukeyHSD(aov(SCORE ~ labels, data = cholesterol))) # 3-1, 3-2, 4-2


cholesterol_summary <- as.data.frame(group_by(cholesterol, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  ))

write.csv(cholesterol_summary, 'cholesterol_summary.csv')

# per ethnicity
cholesterol_afr <- cholesterol[cholesterol$self_reported_race == 'African American',]
cholesterol_asian <- cholesterol[cholesterol$self_reported_race == 'Asian',]
cholesterol_eu <- cholesterol[cholesterol$self_reported_race == 'European American',]
cholesterol_his <- cholesterol[cholesterol$self_reported_race == 'Hispanic',]
cholesterol_nat <- cholesterol[cholesterol$self_reported_race == 'Native American',]
cholesterol_other <- cholesterol[cholesterol$self_reported_race == 'Other']

# AOV 
aov_cholesterol_african <- tidy(aov(SCORE ~ labels, data = cholesterol_afr )) #
aov_cholesterol_asian <- tidy(aov(SCORE ~ labels, data = cholesterol_asian ))
aov_cholesterol_european <- tidy(aov(SCORE ~ labels, data = cholesterol_eu ))
aov_cholesterol_native <- tidy(aov(SCORE ~ labels, data = cholesterol_nat )) #
aov_cholesterol_hispanic <- tidy(aov(SCORE ~ labels, data = cholesterol_his )) 
aov_cholesterol_other <- tidy(aov(SCORE ~ labels, data = cholesterol_other )) 

res_cholesterol <- aov_cholesterol_all %>% filter(term=='labels')%>%mutate(trait='CHOLESTEROL PRS', ancestry='ALL')


for(temp.dat in c('aov_cholesterol_african', 'aov_cholesterol_asian', 'aov_cholesterol_european', 'aov_cholesterol_native', 'aov_cholesterol_hispanic', 'aov_cholesterol_other')){
  temp <- get(temp.dat)
  temp <- temp %>% 
  filter(term=='labels')%>%
  mutate(trait=paste0(toupper(unlist(strsplit(temp.dat,"_"))[2]), " PRS"), 
  ancestry = toupper(unlist(strsplit(temp.dat,"_"))[3]))
  res_cholesterol <- bind_rows(res_cholesterol, temp)
}

res_cholesterol
write.csv(res_cholesterol, 'cholesterol_eth.csv')


# POSTHOC
post_cholesterol_african <- tidy(TukeyHSD(aov(SCORE ~ labels, data = cholesterol_afr ))) # 4-2
post_cholesterol_asian <- tidy(TukeyHSD(aov(SCORE ~ labels, data = cholesterol_asian )))
post_cholesterol_european <- tidy(TukeyHSD(aov(SCORE ~ labels, data = cholesterol_eu )))
post_cholesterol_native <- tidy(TukeyHSD(aov(SCORE ~ labels, data = cholesterol_nat ))) # 4-1, 4-3
post_cholesterol_hispanic <- tidy(TukeyHSD(aov(SCORE ~ labels, data = cholesterol_his ))) 
post_cholesterol_other <- tidy(TukeyHSD(aov(SCORE ~ labels, data = cholesterol_other ))) 


post_cholesterol_african <- post_cholesterol_african %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_cholesterol_african',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_cholesterol_african',"_"))[3]))

post_cholesterol_asian <- post_cholesterol_asian %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_cholesterol_asian',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_cholesterol_asian',"_"))[3]))

post_cholesterol_european <- post_cholesterol_european %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_cholesterol_european',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_cholesterol_european',"_"))[3]))

post_cholesterol_other <- post_cholesterol_other %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_cholesterol_other',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_cholesterol_other',"_"))[3]))

post_cholesterol_hispanic <- post_cholesterol_hispanic %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_cholesterol_hispanic',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_cholesterol_hispanic',"_"))[3]))

post_cholesterol_native <- post_cholesterol_native %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_cholesterol_native',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_cholesterol_native',"_"))[3]))

post_cholesterol <- bind_rows(post_cholesterol_african, post_cholesterol_asian, post_cholesterol_european,
                              post_cholesterol_hispanic, post_cholesterol_native, post_cholesterol_other)

write.csv(post_cholesterol, 'post_cholesterol_int.csv')




# creatinine
creatinine <- fread('creatinine.csv')
head(creatinine)

jpeg('creatinine_race_box.jpg')
ggboxplot(creatinine, x = "self_reported_race", y = "SCORE", 
          color = "self_reported_race", palette = c('green', 'blue', 'gray', 'brown','red','violet'),
          order = c('European American', 'Asian', 'African American', 'Other', 'Hispanic', 'Native American'),
          ylab = "Creatinine PRS", xlab='Ethinic group')
dev.off()

jpeg('creatinine_hist.jpg')
hist(creatinine$SCORE, xlab = "Creatinine PRS", main = "Histogram of creatinine PRS distribution")
dev.off()

# anova test per cluster
aov_creatinine_all <- tidy(aov(SCORE ~ labels, data = creatinine)) # 0.0000000409

tidy(TukeyHSD(aov(SCORE ~ labels, data = creatinine))) # 3-1, 3-2, 4-3

creatinine_summary <- group_by(creatinine, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  )

write.csv(creatinine_summary, 'creatinine_summary.csv')

# per ethnicity
creatinine_afr <- creatinine[creatinine$self_reported_race == 'African American',]
creatinine_asian <- creatinine[creatinine$self_reported_race == 'Asian',]
creatinine_eu <- creatinine[creatinine$self_reported_race == 'European American',]
creatinine_his <- creatinine[creatinine$self_reported_race == 'Hispanic',]
creatinine_nat <- creatinine[creatinine$self_reported_race == 'Native American',]
creatinine_other <- creatinine[creatinine$self_reported_race == 'Other']

# AOV 
aov_creatinine_african <- tidy(aov(SCORE ~ labels, data = creatinine_afr ))
aov_creatinine_asian <- tidy(aov(SCORE ~ labels, data = creatinine_asian ))
aov_creatinine_european <- tidy(aov(SCORE ~ labels, data = creatinine_eu ))
aov_creatinine_native <- tidy(aov(SCORE ~ labels, data = creatinine_nat )) 
aov_creatinine_hispanic <- tidy(aov(SCORE ~ labels, data = creatinine_his )) 
aov_creatinine_other <- tidy(aov(SCORE ~ labels, data = creatinine_other )) 

res_creatinine <- aov_creatinine_all %>% filter(term=='labels')%>%mutate(trait='CREATININE PRS', ancestry='ALL')


for(temp.dat in c('aov_creatinine_african', 'aov_creatinine_asian', 'aov_creatinine_european', 'aov_creatinine_native', 'aov_creatinine_hispanic', 'aov_creatinine_other')){
  temp <- get(temp.dat)
  temp <- temp %>% 
  filter(term=='labels')%>%
  mutate(trait=paste0(toupper(unlist(strsplit(temp.dat,"_"))[2]), " PRS"), 
  ancestry = toupper(unlist(strsplit(temp.dat,"_"))[3]))
  res_creatinine <- bind_rows(res_creatinine, temp)
}

res_creatinine
write.csv(res_creatinine, 'creatinine_eth.csv')

# POSTHOC
post_creatinine_african <- tidy(TukeyHSD(aov(SCORE ~ labels, data = creatinine_afr )))
post_creatinine_asian <- tidy(TukeyHSD(aov(SCORE ~ labels, data = creatinine_asian )))
post_creatinine_european <- tidy(TukeyHSD(aov(SCORE ~ labels, data = creatinine_eu )))
post_creatinine_native <- tidy(TukeyHSD(aov(SCORE ~ labels, data = creatinine_nat ))) 
post_creatinine_hispanic <- tidy(TukeyHSD(aov(SCORE ~ labels, data = creatinine_his ))) 
post_creatinine_other <- tidy(TukeyHSD(aov(SCORE ~ labels, data = creatinine_other )))

post_creatinine_african <- post_creatinine_african %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_creatinine_african',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_creatinine_african',"_"))[3]))

post_creatinine_asian <- post_creatinine_asian %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_creatinine_asian',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_creatinine_asian',"_"))[3]))

post_creatinine_european <- post_creatinine_european %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_creatinine_european',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_creatinine_european',"_"))[3]))

post_creatinine_other <- post_creatinine_other %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_creatinine_other',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_creatinine_other',"_"))[3]))

post_creatinine_hispanic <- post_creatinine_hispanic %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_creatinine_hispanic',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_creatinine_hispanic',"_"))[3]))

post_creatinine_native <- post_creatinine_native %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_creatinine_native',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_creatinine_native',"_"))[3]))

post_creatinine <- bind_rows(post_creatinine_african, post_creatinine_asian, post_creatinine_european,
                              post_creatinine_hispanic, post_creatinine_native, post_creatinine_other)

write.csv(post_creatinine, 'post_creatinine_int.csv')






# depression
depression <- fread('depression.csv')
head(depression)

jpeg('depression_race_box.jpg')
ggboxplot(depression, x = "self_reported_race", y = "SCORE", 
          color = "self_reported_race", palette = c('green', 'blue', 'gray', 'brown','red','violet'),
          order = c('European American', 'Asian', 'African American', 'Other', 'Hispanic', 'Native American'),
          ylab = "Depressive disorder PRS", xlab='Ethinic group')
dev.off()

jpeg('depression_hist.jpg')
hist(depression$SCORE, xlab = "Depressive disorder PRS", main = "Histogram of depressive disorder PRS distribution")
dev.off()

# anova test per cluster
aov_depression_all <- tidy(aov(SCORE ~ labels, data = depression)) #3.24e-10 , integrative model: 8.74e-12

tidy(TukeyHSD(aov(SCORE ~ labels, data = depression))) # 3-1, 3-2, 4-2, 4-1



# per ethnicity
depression_afr <- depression[depression$self_reported_race == 'African American',]
depression_asian <- depression[depression$self_reported_race == 'Asian',]
depression_eu <- depression[depression$self_reported_race == 'European American',]
depression_his <- depression[depression$self_reported_race == 'Hispanic',]
depression_nat <- depression[depression$self_reported_race == 'Native American',]
depression_other <- depression[depression$self_reported_race == 'Other']

hist(depression_other$SCORE)
max(depression_other$SCORE) - min(depression_other$SCORE)

dep_sum <- as.data.frame(group_by(depression, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  ))
  
write.csv(dep_sum,'depression_summary.csv') # highest in 4,5

# AOV 
aov_depression_african <- tidy(aov(SCORE ~ labels, data = depression_afr ))
aov_depression_asian <- tidy(aov(SCORE ~ labels, data = depression_asian ))
aov_depression_european <- tidy(aov(SCORE ~ labels, data = depression_eu ))
aov_depression_native <- tidy(aov(SCORE ~ labels, data = depression_nat )) 
aov_depression_hispanic <- tidy(aov(SCORE ~ labels, data = depression_his )) #
aov_depression_other <- tidy(aov(SCORE ~ labels, data = depression_other ))

res_depression <- aov_depression_all %>% filter(term=='labels')%>%mutate(trait='DEPRESSION PRS', ancestry='ALL')


for(temp.dat in c('aov_depression_african', 'aov_depression_asian', 'aov_depression_european', 'aov_depression_native', 'aov_depression_hispanic', 'aov_depression_other')){
  temp <- get(temp.dat)
  temp <- temp %>% 
  filter(term=='labels')%>%
  mutate(trait=paste0(toupper(unlist(strsplit(temp.dat,"_"))[2]), " PRS"), 
  ancestry = toupper(unlist(strsplit(temp.dat,"_"))[3]))
  res_depression <- bind_rows(res_depression, temp)
}

res_depression
write.csv(res_depression, 'depression_eth.csv')

jpeg('depression_box_str.jpg')
ggplot(depression, aes(x=self_reported_race, y=SCORE)) + 
    geom_boxplot(aes(fill=labels)) + 
    xlab('') + ylab('Polygenic risk score') + ggtitle('') +
    guides(fill=guide_legend(title="clusters")) +theme_bw()
dev.off()

jpeg('depression_box.jpg')
ggplot(depression, aes(x=labels, y=SCORE)) + 
    geom_boxplot(aes(fill=labels)) + 
    xlab('') + ylab('Polygenic risk score') + ggtitle('') +
    guides(fill=guide_legend(title="clusters")) +theme_bw()
dev.off()

# POSTHOC
post_depression_african <- tidy(TukeyHSD(aov(SCORE ~ labels, data = depression_afr )))
post_depression_asian <- tidy(TukeyHSD(aov(SCORE ~ labels, data = depression_asian )))
post_depression_european <- tidy(TukeyHSD(aov(SCORE ~ labels, data = depression_eu )))
post_depression_native <- tidy(TukeyHSD(aov(SCORE ~ labels, data = depression_nat ))) 
post_depression_hispanic <- tidy(TukeyHSD(aov(SCORE ~ labels, data = depression_his ))) 
post_depression_other <- tidy(TukeyHSD(aov(SCORE ~ labels, data = depression_other )))

post_depression_african <- post_depression_african %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_depression_african',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_depression_african',"_"))[3]))

post_depression_asian <- post_depression_asian %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_depression_asian',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_depression_asian',"_"))[3]))

post_depression_european <- post_depression_european %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_depression_european',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_depression_european',"_"))[3]))

post_depression_other <- post_depression_other %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_depression_other',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_depression_other',"_"))[3]))

post_depression_hispanic <- post_depression_hispanic %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_depression_hispanic',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_depression_hispanic',"_"))[3]))

post_depression_native <- post_depression_native %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_depression_native',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_depression_native',"_"))[3]))

post_depression <- bind_rows(post_depression_african, post_depression_asian, post_depression_european,
                              post_depression_hispanic, post_depression_native, post_depression_other)

write.csv(post_depression, 'post_depression.csv')



# diabetes
diabetes <- fread('diabetes2.csv')
head(diabetes)

jpeg('diabetes_hist.jpg')
hist(diabetes$SCORE, xlab = "Type 2 diabetes PRS", main = "Histogram of type 2 diabetes PRS distribution")
dev.off()

jpeg('diabetes_race_box.jpg')
ggboxplot(diabetes, x = "self_reported_race", y = "SCORE", 
          color = "self_reported_race", palette = c('green', 'blue', 'gray', 'brown','red','violet'),
          order = c('European American', 'Asian', 'African American', 'Other', 'Hispanic', 'Native American'),
          ylab = "Type 2 diabetes PRS", xlab='Ethinic group')
dev.off()

# anova test per cluster
aov_diabetes_all <- tidy(aov(SCORE ~ labels, data = diabetes)) #4.39e-10

tidy(TukeyHSD(aov(SCORE ~ labels, data = diabetes))) # 3-1, 3-2, 4-3

diab_summary <- as.data.frame(group_by(diabetes, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  ))
  
write.csv(diab_summary, 'diab_summary.csv')# highest in 4,5

# per ethnicity
diabetes_afr <- diabetes[diabetes$self_reported_race == 'African American',]
diabetes_asian <- diabetes[diabetes$self_reported_race == 'Asian',]
diabetes_eu <- diabetes[diabetes$self_reported_race == 'European American',]
diabetes_his <- diabetes[diabetes$self_reported_race == 'Hispanic',]
diabetes_nat <- diabetes[diabetes$self_reported_race == 'Native American',]
diabetes_other <- diabetes[diabetes$self_reported_race == 'Other']

# AOV 
aov_diabetes_african <- tidy(aov(SCORE ~ labels, data = diabetes_afr )) 
aov_diabetes_asian <- tidy(aov(SCORE ~ labels, data = diabetes_asian ))
aov_diabetes_european <- tidy(aov(SCORE ~ labels, data = diabetes_eu ))
aov_diabetes_native <- tidy(aov(SCORE ~ labels, data = diabetes_nat )) 
aov_diabetes_hispanic <- tidy(aov(SCORE ~ labels, data = diabetes_his )) #
aov_diabetes_other <- tidy(aov(SCORE ~ labels, data = diabetes_other )) 

res_diabetes <- aov_diabetes_all %>% filter(term=='labels')%>%mutate(trait='TYPE 2 DIABETES PRS', ancestry='ALL')


for(temp.dat in c('aov_diabetes_african', 'aov_diabetes_asian', 'aov_diabetes_european', 'aov_diabetes_native', 'aov_diabetes_hispanic', 'aov_diabetes_other')){
  temp <- get(temp.dat)
  temp <- temp %>% 
  filter(term=='labels')%>%
  mutate(trait=paste0(toupper(unlist(strsplit(temp.dat,"_"))[2]), " PRS"), 
  ancestry = toupper(unlist(strsplit(temp.dat,"_"))[3]))
  res_diabetes <- bind_rows(res_diabetes, temp)
}

res_diabetes
write.csv(res_diabetes, 'diabetes_eth.csv')

jpeg('diabetes_box_str.jpg')
ggplot(diabetes, aes(x=self_reported_race, y=SCORE)) + 
    geom_boxplot(aes(fill=labels)) + 
    xlab('') + ylab('Polygenic risk score') + ggtitle('') +
    guides(fill=guide_legend(title="clusters")) +theme_bw()
dev.off()

jpeg('depression_box.jpg')
ggplot(depression, aes(x=labels, y=SCORE)) + 
    geom_boxplot(aes(fill=labels)) + 
    xlab('') + ylab('Polygenic risk score') + ggtitle('') +
    guides(fill=guide_legend(title="clusters")) +theme_bw()
dev.off()



# POSTHOC
post_diabetes_african <- tidy(TukeyHSD(aov(SCORE ~ labels, data = diabetes_afr )))
post_diabetes_asian <- tidy(TukeyHSD(aov(SCORE ~ labels, data = diabetes_asian )))
post_diabetes_european <- tidy(TukeyHSD(aov(SCORE ~ labels, data = diabetes_eu )))
post_diabetes_native <- tidy(TukeyHSD(aov(SCORE ~ labels, data = diabetes_nat ))) 
post_diabetes_hispanic <- tidy(TukeyHSD(aov(SCORE ~ labels, data = diabetes_his ))) # 2-1, 3-2, 4-3
post_diabetes_other <- tidy(TukeyHSD(aov(SCORE ~ labels, data = diabetes_other ))) 

post_diabetes_african <- post_diabetes_african %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_diabetes_african',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_diabetes_african',"_"))[3]))

post_diabetes_asian <- post_diabetes_asian %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_diabetes_asian',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_diabetes_asian',"_"))[3]))

post_diabetes_european <- post_diabetes_european %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_diabetes_european',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_diabetes_european',"_"))[3]))

post_diabetes_other <- post_diabetes_other %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_diabetes_other',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_diabetes_other',"_"))[3]))

post_diabetes_hispanic <- post_diabetes_hispanic %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_diabetes_hispanic',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_diabetes_hispanic',"_"))[3]))

post_diabetes_native <- post_diabetes_native %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_diabetes_native',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_diabetes_native',"_"))[3]))

post_diabetes <- bind_rows(post_diabetes_african, post_diabetes_asian, post_diabetes_european,
                              post_diabetes_hispanic, post_diabetes_native, post_diabetes_other)

write.csv(post_diabetes, 'post_diabetes.csv')


group_by(diabetes_his, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  ) # highest in 5



# eGFR
egfr <- fread('egfr.csv')
head(egfr)

jpeg('egfr_hist.jpg')
hist(egfr$SCORE, xlab = "eGFR PRS", main = "Histogram of eGFR PRS distribution")
dev.off()

jpeg('egfr_race_box.jpg')
ggboxplot(egfr, x = "self_reported_race", y = "SCORE", 
          color = "self_reported_race", palette = c('green', 'blue', 'gray', 'brown','red','violet'),
          order = c('European American', 'Asian', 'African American', 'Other', 'Hispanic', 'Native American'),
          ylab = "eGFR PRS", xlab='Ethinic group')
dev.off()

# anova test per cluster
aov_egfr_all <- tidy(aov(SCORE ~ labels, data = egfr))

tidy(TukeyHSD(aov(SCORE ~ labels, data = egfr)))

egfr_sum <- as.data.frame(group_by(egfr, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  ))


write.csv(egfr_sum, 'egfr_sum.csv')

# per ethnicity
egfr_afr <- egfr[egfr$self_reported_race == 'African American',]
egfr_asian <- egfr[egfr$self_reported_race == 'Asian',]
egfr_eu <- egfr[egfr$self_reported_race == 'European American',]
egfr_his <- egfr[egfr$self_reported_race == 'Hispanic',]
egfr_nat <- egfr[egfr$self_reported_race == 'Native American',]
egfr_other <- egfr[egfr$self_reported_race == 'Other']

# AOV 
aov_egfr_african <- tidy(aov(SCORE ~ labels, data = egfr_afr ))
aov_egfr_asian <- tidy(aov(SCORE ~ labels, data = egfr_asian ))
aov_egfr_european <- tidy(aov(SCORE ~ labels, data = egfr_eu ))
aov_egfr_native <- tidy(aov(SCORE ~ labels, data = egfr_nat )) 
aov_egfr_hispanic <- tidy(aov(SCORE ~ labels, data = egfr_his )) 
aov_egfr_other <- tidy(aov(SCORE ~ labels, data = egfr_other )) 

res_egfr <- aov_egfr_all %>% filter(term=='labels')%>%mutate(trait='eGFR PRS', ancestry='ALL')


for(temp.dat in c('aov_egfr_african', 'aov_egfr_asian', 'aov_egfr_european', 'aov_egfr_native', 'aov_egfr_hispanic', 'aov_egfr_other')){
  temp <- get(temp.dat)
  temp <- temp %>% 
  filter(term=='labels')%>%
  mutate(trait=paste0(toupper(unlist(strsplit(temp.dat,"_"))[2]), " PRS"), 
  ancestry = toupper(unlist(strsplit(temp.dat,"_"))[3]))
  res_egfr <- bind_rows(res_egfr, temp)
}

res_egfr
write.csv(res_egfr, 'egfr_eth.csv')

# POSTHOC
post_egfr_african <- tidy(TukeyHSD(aov(SCORE ~ labels, data = egfr_afr )))
post_egfr_asian <- tidy(TukeyHSD(aov(SCORE ~ labels, data = egfr_asian )))
post_egfr_european <- tidy(TukeyHSD(aov(SCORE ~ labels, data = egfr_eu )))
post_egfr_native <- tidy(TukeyHSD(aov(SCORE ~ labels, data = egfr_nat ))) 
post_egfr_hispanic <- tidy(TukeyHSD(aov(SCORE ~ labels, data = egfr_his ))) 
post_egfr_other <- tidy(TukeyHSD(aov(SCORE ~ labels, data = egfr_other ))) 

post_egfr_african <- post_egfr_african %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_egfr_african',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_egfr_african',"_"))[3]))

post_egfr_asian <- post_egfr_asian %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_egfr_asian',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_egfr_asian',"_"))[3]))

post_egfr_european <- post_egfr_european %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_egfr_european',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_egfr_european',"_"))[3]))

post_egfr_other <- post_egfr_other %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_egfr_other',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_egfr_other',"_"))[3]))

post_egfr_hispanic <- post_egfr_hispanic %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_egfr_hispanic',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_egfr_hispanic',"_"))[3]))

post_egfr_native <- post_egfr_native %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_egfr_native',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_egfr_native',"_"))[3]))

post_egfr <- bind_rows(post_egfr_african, post_egfr_asian, post_egfr_european,
                              post_egfr_hispanic, post_egfr_native, post_egfr_other)

write.csv(post_egfr, 'post_egfr_int.csv')




# liver
liver <- fread('liver.csv')
head(liver)

jpeg('liver_hist.jpg')
hist(liver$SCORE, xlab = "Liver disease PRS", main = "Histogram of liver disease PRS distribution")
dev.off()

jpeg('liver_race_box.jpg')
ggboxplot(liver, x = "self_reported_race", y = "SCORE", 
          color = "self_reported_race", palette = c('green', 'blue', 'gray', 'brown','red','violet'),
          order = c('European American', 'Asian', 'African American', 'Other', 'Hispanic', 'Native American'),
          ylab = "Liver disease PRS", xlab='Ethinic group')
dev.off()

# anova test per cluster
aov_liver_all <- tidy(aov(SCORE ~ labels, data = liver)) # 0.0125

tidy(TukeyHSD(aov(SCORE ~ labels, data = liver))) # 4-1, 4-3

group_by(liver, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  )

# per ethnicity
liver_afr <- liver[liver$self_reported_race == 'African American',]
liver_asian <- liver[liver$self_reported_race == 'Asian',]
liver_eu <- liver[liver$self_reported_race == 'European American',]
liver_his <- liver[liver$self_reported_race == 'Hispanic',]
liver_nat <- liver[liver$self_reported_race == 'Native American',]
liver_other <- liver[liver$self_reported_race == 'Other']

# AOV 
aov_liver_african <- tidy(aov(SCORE ~ labels, data = liver_afr ))
aov_liver_asian <- tidy(aov(SCORE ~ labels, data = liver_asian ))
aov_liver_european <- tidy(aov(SCORE ~ labels, data = liver_eu )) #
aov_liver_native <- tidy(aov(SCORE ~ labels, data = liver_nat )) 
aov_liver_hispanic <- tidy(aov(SCORE ~ labels, data = liver_his )) #
aov_liver_other <- tidy(aov(SCORE ~ labels, data = liver_other )) 

res_liver <- aov_liver_all %>% filter(term=='labels')%>%mutate(trait='LIVER DISEASE PRS', ancestry='ALL')


for(temp.dat in c('aov_liver_african', 'aov_liver_asian', 'aov_liver_european', 'aov_liver_native', 'aov_liver_hispanic', 'aov_liver_other')){
  temp <- get(temp.dat)
  temp <- temp %>% 
  filter(term=='labels')%>%
  mutate(trait=paste0(toupper(unlist(strsplit(temp.dat,"_"))[2]), " PRS"), 
  ancestry = toupper(unlist(strsplit(temp.dat,"_"))[3]))
  res_liver <- bind_rows(res_liver, temp)
}

res_liver
write.csv(res_liver, 'liver_eth_int.csv')



# POSTHOC
post_liver_african <- tidy(TukeyHSD(aov(SCORE ~ labels, data = liver_afr )))
post_liver_asian <- tidy(TukeyHSD(aov(SCORE ~ labels, data = liver_asian )))
post_liver_european <- tidy(TukeyHSD(aov(SCORE ~ labels, data = liver_eu ))) #3-2
post_liver_native <- tidy(TukeyHSD(aov(SCORE ~ labels, data = liver_nat ))) 
post_liver_hispanic <- tidy(TukeyHSD(aov(SCORE ~ labels, data = liver_his ))) #4-2
post_liver_other <- tidy(TukeyHSD(aov(SCORE ~ labels, data = liver_other )))

post_liver_african <- post_liver_african %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_liver_african',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_liver_african',"_"))[3]))

post_liver_asian <- post_liver_asian %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_liver_asian',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_liver_asian',"_"))[3]))

post_liver_european <- post_liver_european %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_liver_european',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_liver_european',"_"))[3]))

post_liver_other <- post_liver_other %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_liver_other',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_liver_other',"_"))[3]))

post_liver_hispanic <- post_liver_hispanic %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_liver_hispanic',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_liver_hispanic',"_"))[3]))

post_liver_native <- post_liver_native %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_liver_native',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_liver_native',"_"))[3]))

post_liver <- bind_rows(post_liver_african, post_liver_asian, post_liver_european,
                              post_liver_hispanic, post_liver_native, post_liver_other)

write.csv(post_liver, 'post_liver_int.csv')







# parkinson
parkinson <- fread('parkinson.csv')
head(parkinson)

jpeg('parkinson_hist.jpg')
hist(parkinson$SCORE, xlab = "Parkinson's disease PRS", main = "Histogram of Parkinson's disease PRS distribution")
dev.off()

jpeg('parkinson_race_box.jpg')
ggboxplot(parkinson, x = "self_reported_race", y = "SCORE", 
          color = "self_reported_race", palette = c('green', 'blue', 'gray', 'brown','red','violet'),
          order = c('European American', 'Asian', 'African American', 'Other', 'Hispanic', 'Native American'),
          ylab = "Parkinson's disease PRS", xlab='Ethinic group')
dev.off()

# anova test per cluster
aov_parkinson_all <- tidy(aov(SCORE ~ labels, data = parkinson)) # 0.0000135, #9.83e-10

tidy(TukeyHSD(aov(SCORE ~ labels, data = parkinson))) # 3-1, 3-2, 4-3, 5-3

park_sum <- as.data.frame(group_by(parkinson, labels) %>%
  summarise(
    count = n(),
    mean = mean(SCORE, na.rm = TRUE),
    sd = sd(SCORE, na.rm = TRUE),
    max = max(SCORE, na.rm = TRUE),
    min = min(SCORE, na.rm = TRUE),
    median = median(SCORE, na.rm = TRUE)
  ))
write.csv(park_sum, 'parkinson_summary.csv') # highest in 1

# per ethnicity
parkinson_afr <- parkinson[parkinson$self_reported_race == 'African American',]
parkinson_asian <- parkinson[parkinson$self_reported_race == 'Asian',]
parkinson_eu <- parkinson[parkinson$self_reported_race == 'European American',]
parkinson_his <- parkinson[parkinson$self_reported_race == 'Hispanic',]
parkinson_nat <- parkinson[parkinson$self_reported_race == 'Native American',]
parkinson_other <- parkinson[parkinson$self_reported_race == 'Other']

# AOV 
aov_parkinson_african <- tidy(aov(SCORE ~ labels, data = parkinson_afr ))
aov_parkinson_asian <- tidy(aov(SCORE ~ labels, data = parkinson_asian ))
aov_parkinson_european <- tidy(aov(SCORE ~ labels, data = parkinson_eu ))
aov_parkinson_native <- tidy(aov(SCORE ~ labels, data = parkinson_nat )) 
aov_parkinson_hispanic <- tidy(aov(SCORE ~ labels, data = parkinson_his )) 
aov_parkinson_other <- tidy(aov(SCORE ~ labels, data = parkinson_other ))

res_parkinson <- aov_parkinson_all %>% filter(term=='labels')%>%mutate(trait='PARKINSON PRS', ancestry='ALL')


for(temp.dat in c('aov_parkinson_african', 'aov_parkinson_asian', 'aov_parkinson_european', 'aov_parkinson_native', 'aov_parkinson_hispanic', 'aov_parkinson_other')){
  temp <- get(temp.dat)
  temp <- temp %>% 
  filter(term=='labels')%>%
  mutate(trait=paste0(toupper(unlist(strsplit(temp.dat,"_"))[2]), " PRS"), 
  ancestry = toupper(unlist(strsplit(temp.dat,"_"))[3]))
  res_parkinson <- bind_rows(res_parkinson, temp)
}

res_parkinson
write.csv(res_parkinson, 'parkinson_eth.csv')


jpeg('parkinson_box_str.jpg')
ggplot(parkinson, aes(x=self_reported_race, y=SCORE)) + 
    geom_boxplot(aes(fill=labels)) + 
    xlab('') + ylab('Polygenic risk score') + ggtitle('') +
    guides(fill=guide_legend(title="clusters")) +theme_bw()
dev.off()

jpeg('parkinson_box.jpg')
ggplot(parkinson, aes(x=labels, y=SCORE)) + 
    geom_boxplot(aes(fill=labels)) + 
    xlab('') + ylab('Polygenic risk score') + ggtitle('') +
    guides(fill=guide_legend(title="clusters")) +theme_bw()
dev.off()

# POSTHOC
post_parkinson_african <- tidy(TukeyHSD(aov(SCORE ~ labels, data = parkinson_afr )))
post_parkinson_asian <- tidy(TukeyHSD(aov(SCORE ~ labels, data = parkinson_asian )))
post_parkinson_european <- tidy(TukeyHSD(aov(SCORE ~ labels, data = parkinson_eu )))
post_parkinson_native <- tidy(TukeyHSD(aov(SCORE ~ labels, data = parkinson_nat ))) 
post_parkinson_hispanic <- tidy(TukeyHSD(aov(SCORE ~ labels, data = parkinson_his ))) 
post_parkinson_other <- tidy(TukeyHSD(aov(SCORE ~ labels, data = parkinson_other ))) # 2-1, 3-2, 5-2

post_parkinson_african <- post_parkinson_african %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_parkinson_african',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_parkinson_african',"_"))[3]))

post_parkinson_asian <- post_parkinson_asian %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_parkinson_asian',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_parkinson_asian',"_"))[3]))

post_parkinson_european <- post_parkinson_european %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_parkinson_european',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_parkinson_european',"_"))[3]))

post_parkinson_other <- post_parkinson_other %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_parkinson_other',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_parkinson_other',"_"))[3]))

post_parkinson_hispanic <- post_parkinson_hispanic %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_parkinson_hispanic',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_parkinson_hispanic',"_"))[3]))

post_parkinson_native <- post_parkinson_native %>% filter(term=='labels') %>% 
        mutate(trait=paste0(toupper(unlist(strsplit('post_parkinson_native',"_"))[2]), " PRS"), 
               ancestry = toupper(unlist(strsplit('post_parkinson_native',"_"))[3]))

post_parkinson <- bind_rows(post_parkinson_african, post_parkinson_asian, post_parkinson_european,
                              post_parkinson_hispanic, post_parkinson_native, post_parkinson_other)

write.csv(post_parkinson, 'post_parkinson_int.csv')



# triglycerides
triglycerides <- fread('triglycerides.csv')
head(triglycerides)


jpeg('tri_hist.jpg')
hist(triglycerides$SCORE, xlab = "Triglycerides PRS", main = "Histogram of triglycerides PRS distribution")
dev.off()

jpeg('tri_race_box.jpg')
ggboxplot(triglycerides, x = "self_reported_race", y = "SCORE", 
          color = "self_reported_race", palette = c('green', 'blue', 'gray', 'brown','red','violet'),
          order = c('European American', 'Asian', 'African American', 'Other', 'Hispanic', 'Native American'),
          ylab = "Triglycerides PRS", xlab='Ethinic group')
dev.off()

# anova test per cluster
aov_triglycerides_all <- tidy(aov(SCORE ~ labels, data = triglycerides))

tidy(TukeyHSD(aov(SCORE ~ labels, data = triglycerides)))

# per ethnicity
triglycerides_afr <- triglycerides[triglycerides$self_reported_race == 'African American',]
triglycerides_asian <- triglycerides[triglycerides$self_reported_race == 'Asian',]
triglycerides_eu <- triglycerides[triglycerides$self_reported_race == 'European American',]
triglycerides_his <- triglycerides[triglycerides$self_reported_race == 'Hispanic',]
triglycerides_nat <- triglycerides[triglycerides$self_reported_race == 'Native American',]
triglycerides_other <- triglycerides[triglycerides$self_reported_race == 'Other']

# AOV 
aov_triglycerides_afr <- tidy(aov(SCORE ~ labels, data = triglycerides_afr ))
aov_triglycerides_asian <- tidy(aov(SCORE ~ labels, data = triglycerides_asian ))
aov_triglycerides_eu <- tidy(aov(SCORE ~ labels, data = triglycerides_eu ))
aov_triglycerides_nat <- tidy(aov(SCORE ~ labels, data = triglycerides_nat )) 
aov_triglycerides_his <- tidy(aov(SCORE ~ labels, data = triglycerides_his )) 
aov_triglycerides_other <- tidy(aov(SCORE ~ labels, data = triglycerides_other )) 

# POSTHOC
post_triglycerides_afr <- tidy(TukeyHSD(aov(SCORE ~ labels, data = triglycerides_afr )))
post_triglycerides_asian <- tidy(TukeyHSD(aov(SCORE ~ labels, data = triglycerides_asian )))
post_triglycerides_eu <- tidy(TukeyHSD(aov(SCORE ~ labels, data = triglycerides_eu )))
post_triglycerides_nat <- tidy(TukeyHSD(aov(SCORE ~ labels, data = triglycerides_nat ))) 
post_triglycerides_his <- tidy(TukeyHSD(aov(SCORE ~ labels, data = triglycerides_his ))) 
post_triglycerides_other <- tidy(TukeyHSD(aov(SCORE ~ labels, data = triglycerides_other ))) 

















aov_sbp_all <- tidy(aov(SCORE1_AVG ~ labels, data = prs_sbp))
res <- aov_sbp_all %>% filter(term=='labels')%>%mutate(trait='SBP PRS', ancestry='ALL')
aov_dbp_all <- tidy(aov(SCORE1_AVG ~ labels, data = prs_dbp))
aov_pp_all <- tidy(aov(SCORE1_AVG ~ labels, data = prs_pp))

for(temp.dat in c('aov_dbp_all','aov_pp_all', 'aov_alzheimer_all', 'aov_asthma_all', 'aov_bmi_all',
                  'aov_cad_all', 'aov_cholesterol_all', 'aov_depression_all', 'aov_egfr_all', 'aov_liver_all',
                   'aov_parkinson_all', 'aov_triglycerides_all', 'aov_diabetes_all', 'aov_creatinine_all')){
  temp <- get(temp.dat)
  temp <- temp %>% 
  filter(term=='labels')%>%
  mutate(trait=paste0(toupper(unlist(strsplit(temp.dat,"_"))[2]), ' PRS'), 
  ancestry = toupper(unlist(strsplit(temp.dat,"_"))[3]))
  res <- bind_rows(res,temp)
}
res <- as.data.frame(res)

write.csv(res, file = 'all_traits.csv')



########################################################################################################
# blood pressure
bp <- fread('blood_pressure_median_perperson.csv')
head(bp)

bp$labels[bp$labels == 0] <- 'cluster_1'
bp$labels[bp$labels == 1] <- 'cluster_2'
bp$labels[bp$labels == 2] <- 'cluster_3'
bp$labels[bp$labels == 3] <- 'cluster_4'
bp$labels[bp$labels == 4] <- 'cluster_5'
bp$labels[bp$labels == 5] <- 'cluster_6'

bp_afr <- bp[bp$self_reported_race == 'African American',]
bp_his <- bp[bp$self_reported_race == 'Hispanic',]
bp_eu <- bp[bp$self_reported_race == 'European American',]

head(bp_afr)


str(bp)
levels(as.factor(bp$labels))
table(bp$labels)
bp$labels <- as.factor(bp$labels)



summary <- as.data.frame(group_by(bp, labels) %>%
  summarise(
    count = n(),
    mean = mean(sbp_median, na.rm = TRUE),
    sd = sd(sbp_median, na.rm = TRUE),
    max = max(sbp_median, na.rm = TRUE),
    min = min(sbp_median, na.rm = TRUE),
    median = median(sbp_median, na.rm = TRUE)
  ))
summary

write.csv(summary, 'sbp_stats.csv')

bp$labels <- as.factor(bp$labels)
head(bp)

aov_sbp <- tidy(aov(sbp_median ~ labels, data = bp)) 

write.csv(aov_sbp, 'aov_sbp.csv')

post_sbp <- tidy(TukeyHSD(aov(sbp_median ~ labels, data = bp )))

write.csv(post_sbp, 'post_sbp.csv')



summary_dbp <- as.data.frame(group_by(bp, labels) %>%
  summarise(
    count = n(),
    mean = mean(dbp_median, na.rm = TRUE),
    sd = sd(dbp_median, na.rm = TRUE),
    max = max(dbp_median, na.rm = TRUE),
    min = min(dbp_median, na.rm = TRUE),
    median = median(dbp_median, na.rm = TRUE)
  ))
summary_dbp

write.csv(summary_dbp, 'dbp_stats.csv')


aov_dbp <- tidy(aov(dbp_median ~ labels, data = bp)) 

write.csv(aov_dbp, 'aov_dbp.csv')

post_dbp <- tidy(TukeyHSD(aov(dbp_median ~ labels, data = bp )))

write.csv(post_dbp, 'post_dbp.csv')



summary_dbp_afr <- as.data.frame(group_by(bp_afr, labels) %>%
  summarise(
    count = n(),
    mean = mean(dbp_median, na.rm = TRUE),
    sd = sd(dbp_median, na.rm = TRUE),
    max = max(dbp_median, na.rm = TRUE),
    min = min(dbp_median, na.rm = TRUE),
    median = median(dbp_median, na.rm = TRUE)
  ))
summary_dbp_afr

