##  System requirements
Python > 3.7

## Parameters setting:
* deep learning archicture is presented in net.py

* other hyperparameters can be tuned in utils.py and learn-patient-representaion.sh

## Training:
run following bash script to start training
```
sh learn-patient-representation.sh
```


## Extract embeddings:
Once the training is completed, run the following to extract the embeddings
```
python evaluation.py
```

Follow this [link](https://github.com/jiahao95/hypertension_clustering/tree/main/clustering) for the clustering part.
