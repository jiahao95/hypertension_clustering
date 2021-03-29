#%%
import pandas as pd
import numpy as np
from scipy import stats
from statistics import median
import os
path = ''

os.chdir('path')

clusters = pd.read_csv('clusters.csv')

#%%
def get_subgroups(df,n):
    dfs = []
    for i in range(1,n+1):
        cluster_+str(i) = df.loc[df.labels == i]
    return dfs

# %%
def analyze_sex_race(df):
    tot = len(df['rgnid'].unique())
    male_percent = round((len(df.loc[df['sex']==0])/tot)*100,1)
    female_percent = round((len(df.loc[df['sex']==1])/tot)*100,1)
    male = len(df.loc[df['sex']==0])
    female = len(df.loc[df['sex']==1])
    Hispanic = len(df.loc[df['self_reported_race']=='Hispanic'])
    African_American = len(df.loc[df['self_reported_race']=='African American'])
    European_American = len(df.loc[df['self_reported_race']=='European American'])
    Asian = len(df.loc[df['self_reported_race']=='Asian'])
    Native_American = len(df.loc[df['self_reported_race']=='Native American'])
    Other = len(df.loc[df['self_reported_race']=='Other'])
    row = {'total':tot, 'male_in_%':male_percent, 'female_in_%': female_percent, 'male':male, 'female':female, 'Hispanic': Hispanic, 'African_American': African_American, 'Asian': Asian, 'European_American':European_American, 'Native_American': Native_American, 'Other':Other}
    return row
# %%
def create_sex_race_table(dfs, n):
    l = []
    for df, i in zip(dfs, range(1,n+1):
        row + str(i) = analyze_sex_race(df)
    df = pd.concat(l).T
    return df
# %%
sex_df = create_sex_race_table(dfs)
sex_df = sex_df.rename(columns={0:'cluster_1',1:'cluster_2',2:'cluster_3',3:'cluster_4',4:'cluster_5', 5:'cluster_6'})
sex_df.to_csv('gender_race_table.csv')

# %%
# load labs
labs_num = pd.read_csv('filtered_labs.csv')
# %%
def analyze_lab_res(df, label, path):
    cluster_labs_num = pd.merge(labs_num,df, on='rgnid', how='inner').drop(columns=['reference_unit ','delta','sex', 'birth_date'])
    labs =  cluster_labs_num.groupby(["rgnid","test"])['value'].median().unstack().reset_index().rename_axis(None, axis=1)
    converted_labs_median = df[['rgnid', 'self_reported_race']].merge(labs, on='rgnid')
    converted_labs_median  = converted_labs_median[['SODIUM-BLD','TRIGLYCERIDES','TROPONIN-I','TSH','U-CREATININE (CONC)', 'VENTRICULAR RATE', 'WB POTASSIUM , VEN', 'WB GLUCOSE , VEN','WHITE BLOOD CELL','WB SODIUM , VEN', 'WB UREA NITROGEN, VEN', 'WB NA , VEN (POCT)', 'WB LACTATE-VEN (POCT)', 'WB K , VEN (POCT)', 'WB GLUCOSE-VEN (POCT)', 'WB CO2, VEN', 'WB CHLORIDE, VEN', 'WB CA++ , VEN(POCT)', 'VITAMIN D, 25 HYDROXY', 'VITAMIN B12, SERUM', 'UROBILINOGEN', 'UREA NITROGEN-BLD', 'U-SPECIFIC GRAVITY', 'U-PH', 'TRANSFERRIN SAT', 'TOT CO2 , VEN(POCT)', 'TIBC', 'self_reported_race']]
    converted_labs_median['labels'] = label
    col_medians = converted_labs_median.median()
    converted_labs_median = converted_labs_median.fillna(col_medians)
    converted_labs_median.to_csv(path+ label +'_labs.csv', index=False)
    lab_summary = converted_labs_median.describe().T
    lab_summary.to_csv(path + label +'_summary.csv')
    return converted_labs_median
#%%
def prepare_for_anova(dfs, labels, path):
    lab_final = []
    for df,label in zip(dfs, labels):
        res = analyze_lab_res(df = df, path = path, label = label)
        lab_final.append(res)
    df_anova = pd.concat(lab_final)
    df_anova.to_csv(path+'lab_numerical_all_for_anova_w_etn.csv')

#%%
dfs = [cluster_1, cluster_2, cluster_3, cluster_4, cluster_5, cluster_6]
labels = ['cluster_1', 'cluster_2', 'cluster_3', 'cluster_4', 'cluster_5', 'cluster_6']
prepare_for_anova(dfs = dfs, labels = labels, path = path)

# %%
import scipy.stats as stats
from statistics import mean 
from sklearn import preprocessing
import pickle
from pickle import dump
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import StandardScaler
import numpy as np
from scipy.stats import chi2_contingency
# %%
diag = pd.read_csv('diagnoses_w_age.csv')
diag = pd.merge(diag,clusters, on='rgnid', how='inner')
diag = diag[['rgnid','dx_name','labels','phecode', 'self_reported_race']]
diag = diag.drop_duplicates(subset=['rgnid', 'phecode'])
diag.to_csv('disease_cluster_table_w_etn.csv', index=False)

# medications
# %%
medication = pd.read_csv('/sc/arion/projects/rg_HPIMS/user/jiahao01/data_july/BioMe/BRSPD/meds_final.csv')

meds = pd.merge(medication,clusters, on='rgnid', how='inner')

meds = meds[['rgnid','event', 'labels', 'description']]

meds = meds.drop_duplicates(subset=['rgnid', 'event'])

meds.to_csv('medication_cluster_table_w_etn.csv', index=False)



# procedures
# %%
procedures = pd.read_csv('/sc/arion/projects/rg_HPIMS/user/jiahao01/data_july/BioMe/BRSPD/procedures.csv')

procedures = pd.merge(procedures,clusters, on='rgnid', how='inner')

procedures = procedures[['rgnid', 'proc_code', 'proc_description','labels','self_reported_race']]

procedures = procedures.drop_duplicates(subset=['rgnid','proc_code','proc_description'])

procedures.to_csv('procedure_table_w_etn.csv', index=False)


#blood pressure
# %%
bp = pd.read_csv('/sc/arion/projects/rg_HPIMS/user/jiahao01/data_july/BioMe/BRSPD/bp_w_age.csv')
#%%
def analyze_bp(df, path, i):
    bp_num = pd.merge(bp,df, on='rgnid', how='inner').drop(columns=['diff','sex','birth_date'])
    bp_num['sbp_median'] =  bp_num.groupby(["rgnid"])['sbp'].transform('median')
    bp_num['dbp_median'] =  bp_num.groupby(["rgnid"])['dbp'].transform('median')
    bp_num = bp_num[['rgnid', 'labels', 'sbp_median', 'dbp_median', 'self_reported_race']]
    bp_num = bp_num.drop_duplicates()
    bp_summary = bp_num.describe().T.rename(index={'sbp_median':'sbp_'+ str(i), 'dbp_median':'dbp_'+ str(i)})
    bp_summary.to_csv(path +'/bp_summary_'+ str(i) +'.csv')
    return bp_num
#%%
def prepare_bp_anova(dfs, path):
    bps = []
    for df, i in zip(dfs, range(1,(len(dfs)+1))):
        bp_num = analyze_bp(df = df, path = path, i = i)
        bps.append(bp_num)
    df_anova = pd.concat(bps)
    df_anova.to_csv(path+'blood_pressure_median_perperson.csv') 
#%%
prepare_bp_anova(dfs=dfs, path = path)


#%%
from datetime import date
#%%
clusters['combined'] = pd.to_datetime('2020-07-15',format='%Y-%m-%d')
clusters['birth_date'] = pd.to_datetime(clusters['birth_date'], format='%Y-%m-%d')
clusters['delta'] = ((clusters['combined'] - clusters['birth_date'])/np.timedelta64(1,'D'))/365
age = clusters[['rgnid', 'labels', 'self_reported_race', 'delta']]
age.to_csv('age_wt_etn.csv', index = False)


# %%
bmi = pd.read_csv('/sc/arion/projects/rg_HPIMS/user/jiahao01/convae_architecture/recluster/bmi_reclustered.csv')
#%%
bmi_ = bmi[['rgnid', 'bmi', 'self_reported_race']].merge(clusters, on = 'rgnid')
#%%
bmi_.to_csv('/bmi.csv', index=False)

