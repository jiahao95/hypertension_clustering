# EHR-based Stratification of Hypertensive Patients Powered by Integration of Genetic Variants
This project proposes a multi-modal deep representation learning model, which extracts latent feature vectors from high-dimensional EHR and genetic data. On top of it, we perform the K-means clustering to derive hypertension subtypes.

The pipeline is adapted from [Landi, I., Glicksberg, B. S., Lee, H. C., Cherng, S., Landi, G., Danieletto, M., Dudley, J. T., Furlanello, C., & Miotto, R. Deep representation learning of electronic health records to unlock patient stratification at scale. npj Digit. Med. 3, 96 (2020)]. The original repository is found in this [link](https://github.com/landiisotta/convae_architecture)<br/>
The project is structured as follows:


## EHR-based model
Here, only the EHR data is used. <br/> 
<br/>
Following hyperparameters are changed to the original pipeline: <br/>
batch-size = 32, epoch = 30, learning rate = 0.00001, weight-decay = 0.0001, kernel = 3.


## Multimodal model
In this part, we combine both EHR data and genetic data to create a fused representation.


## Clustering
The K-means clustering algorithm is applied to derive patient clusters.

_**Please refer to README file in each subdirectory for detailed description.**_
