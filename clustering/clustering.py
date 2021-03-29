# %%
import pandas as pd
import os
import numpy as np
import sklearn
from sklearn.cluster import DBSCAN
import matplotlib.pyplot as plt
import scipy.cluster.hierarchy as sch
from sklearn.cluster import AgglomerativeClustering
from sklearn.preprocessing import StandardScaler, MinMaxScaler
from sklearn.decomposition import PCA, KernelPCA
from sklearn.manifold import TSNE
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import StandardScaler
import json
from sklearn import metrics
from sklearn.decomposition import FastICA, TruncatedSVD
from sklearn.metrics import pairwise_distances
from sklearn.metrics import davies_bouldin_score
from sklearn.cluster import KMeans, OPTICS
from sklearn.mixture import GaussianMixture
from mpl_toolkits.mplot3d import Axes3D
from sklearn.manifold import TSNE
import seaborn as sns
from scipy.cluster.hierarchy import dendrogram
from sklearn.cluster import AgglomerativeClustering
import umap.umap_ as umap
import scipy.cluster.hierarchy as sch
from sklearn.neighbors import NearestNeighbors
# %%
test_embeddings = pd.read_csv('TSconvae-avg_vect.csv', header=None)

# %%
data = test_embeddings.iloc[,1:65].values

# %%
Y = StandardScaler().fit_transform(data)
X = StandardScaler().fit_transform(data)
#%%
pca = PCA(n_components=16, random_state=42)

pca_components = pca.fit_transform(X)

print(pca.explained_variance_ratio_.cumsum())


# %%
# pass normalized data
def apply_umap(transformed_df,dimension):
    clusterable_embedding = umap.UMAP(
        n_neighbors=50,
        min_dist=0.01,
        n_components=dimension,
        random_state=42
        #learning_rate=0.001
    )
    X_transformed=clusterable_embedding.fit_transform(transformed_df)
    return X_transformed
# %%
def scatter_plot(df,labels):
    sns.set(style='white', rc={'figure.figsize':(10,8)})
    plt.scatter(df[:, 0], df[:, 1], c=labels, s=5, cmap='Spectral')
    plt.show()
# %%
def apply_kmeans(transformed_sample,ellbow_method,cluster,initialization,n):
    if ellbow_method==True:
        elbow_method(transformed_sample)
    kmeans = KMeans(n_clusters=cluster, init=initialization, max_iter=300, n_init=n, random_state=42)
    pred_y = kmeans.fit_predict(transformed_sample)
    return kmeans.labels_
# %%
def elbow_method(transformed_sample): 
    wcss = []
    for i in range(1, 11):
        kmeans = KMeans(n_clusters=i, init='random', max_iter=300, n_init=10, random_state=0)
        kmeans.fit(transformed_sample)
        wcss.append(kmeans.inertia_)
    plt.plot(range(1, 11), wcss)
    plt.title('Elbow Method')
    plt.xlabel('Number of clusters')
    plt.ylabel('WCSS')
    plt.show()

# %%
def get_silhouette_Coefficient(labels,df):
    m=metrics.silhouette_score(df, labels, metric='euclidean')
    print('silhouette_score:',m)
    return m

#%%
labels_kmeans = apply_kmeans(Y, False, cluster=6, initialization="k-means++", n=1000)
# %%
# prepare data for plotting, reduce to 2 dimensions
df_dim_red_plot = apply_umap(Y, 2)
#%%
# build dataframe
df = pd.DataFrame(data=df_dim_red_plot,  columns=["dim1", "dim2"])
df['labels'] = labels_kmeans
#%%
df = df.replace({'labels': {5:'cluster_6',0:'cluster_1', 3:'cluster_4', 4:'cluster_5',1:'cluster_2', 2:'cluster_3'}})
#%%
# use reduced data to compute scatterplot
facet = sns.lmplot(data=df, x='dim1', y='dim2', hue='labels', 
                   fit_reg=False, legend=True, legend_out=True, scatter_kws={"s": 1})
#%%
get_silhouette_Coefficient(labels_kmeans, df_dim_red_plot)
#%%
# 3d plot
df_3d = pd.DataFrame(data=df_dim_red_plot,  columns=["dim1", "dim2","dim3"])
df_3d['labels'] = labels
#%%
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import pandas as pd
import numpy as np


fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
x = np.array(df_3d['dim1'])
y = np.array(df_3d['dim2'])
z = np.array(df_3d['dim3'])

ax.scatter(x,y,z, marker="s", c=df_3d["labels"], s=0.05, cmap="RdBu")

plt.show()

# %%
tsne = TSNE(n_components=2, verbose=0, perplexity=20, n_iter=2000, learning_rate = 2000)
tsne_pca_results = tsne.fit_transform(X)

# %%
df_tsne = pd.DataFrame(data=tsne_pca_results, columns=['dim_1','dim_2'])
df_tsne['labels'] = labels
#%%
facet = sns.lmplot(data=df_tsne, x='dim_1', y='dim_2', hue='labels', 
                   fit_reg=False, legend=True, legend_out=True, scatter_kws={"s": 1})

