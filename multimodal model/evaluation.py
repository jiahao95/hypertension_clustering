import pandas as pd
import os
from torch.utils.data import Dataset
import random
import torch
import os
import csv
import torch.nn as nn
import torch.nn.functional as F
import numpy as np
import math
import utils as ut
import utils as ut
from torch.utils.data import DataLoader
import csv
import pandas as pd
import ast

ehr_len_padded = 8
prs_len_padded = 15

class EHRdata(Dataset):

    def __init__(self, datadir, ehr_file, sampling):
        self.ehr = {}
        df = pd.read_csv(os.path.join(datadir, ehr_file), converters={'unique_concept_x':ast.literal_eval, 'unique_concept_y':ast.literal_eval})
        for row in df.itertuples():
            seq = row.unique_concept_x
            prs = row.unique_concept_y
            ids = row.rgnid

            if len(seq) < ehr_len_padded:
                self.ehr[ids] = [seq + [0] * (ehr_len_padded - len(seq)),
                                      prs + [0] * (prs_len_padded - len(prs))]

            elif len(seq) % ehr_len_padded != 0:
                nseq, nleft = divmod(len(seq), ehr_len_padded)
                self.ehr[ids] = [seq + [0] * \
                                     (ehr_len_padded - nleft),
                                     prs + [0] * (prs_len_padded - len(prs))]
            else:
                self.ehr[ids] = [seq,prs + [0] * (prs_len_padded - len(prs))]
        # sub-sample dataset
        if sampling is not None:
            mrns = list(self.ehr.keys())
            random.shuffle(mrns)
            ehr = {}
            for k in mrns[:sampling]:
                ehr[k] = self.ehr[k]
            self.ehr = ehr

        self.ehr_list = [[mrn, term] for mrn, term in self.ehr.items()]

    def __getitem__(self, index):
        prs_seq = self.ehr_list[index][1][1]
        ehr_seq = self.ehr_list[index][1][0]
        pat = self.ehr_list[index][0]
        return pat, ehr_seq, prs_seq

    def __len__(self):
        return len(self.ehr)


def ehr_collate(batch):
    ehr_data = []
    prs_data = []
    mrn = []
    for pat, ehr_seq, prs_seq in batch:
        mrn.append(pat)
        if len(ehr_seq) == ehr_len_padded and len(prs_seq) == prs_len_padded:
            ehr_data.append(torch.tensor(
                [ehr_seq], dtype=torch.long).view(-1, ehr_len_padded))
            prs_data.append(torch.tensor([prs_seq], dtype=torch.long).view(-1, prs_len_padded))
   

        elif len(ehr_seq) > ehr_len_padded and len(prs_seq) == prs_len_padded:
            ps = []
            
            for i in range(0, len(ehr_seq) - ehr_len_padded + 1,      
                           ehr_len_padded + 1):
                ps.append(ehr_seq[i:i + ehr_len_padded])
            ehr_data.append(torch.tensor(
                ps, dtype=torch.long).view(-1, ehr_len_padded))
            prs_data.append(torch.tensor([prs_seq], dtype=torch.long).view(-1, prs_len_padded))
        
        elif len(ehr_seq) == ehr_len_padded and len(prs_seq) > prs_len_padded:
            pr = []
            for j in range(0, len(prs_seq) - prs_len_padded + 1, prs_len_padded +1):
                pr.append(prs_seq[j:j + prs_len_padded])
            prs_data.append(torch.tensor(pr, dtype=torch.long).view(-1, prs_len_padded))
            ehr_data.append(torch.tensor([ehr_seq], dtype=torch.long).view(-1, ehr_len_padded))

        elif len(ehr_seq) > ehr_len_padded and len(prs_seq) > prs_len_padded:
            ps = []
            pr = []
            for i in range(0, len(ehr_seq) - ehr_len_padded + 1, ehr_len_padded + 1):
                ps.append(ehr_seq[i:i + ehr_len_padded])
            ehr_data.append(torch.tensor(ps, dtype=torch.long).view(-1, ehr_len_padded))
            for j in range(0, len(prs_seq) - prs_len_padded + 1, prs_len_padded +1):
                pr.append(prs_seq[j:j + prs_len_padded])
            prs_data.append(torch.tensor(pr, dtype=torch.long).view(-1, prs_len_padded))

        else:
            raise Warning(
                'Not all sequences have length multiple than %d' % ehr_len_padded)
    return mrn, ehr_data, prs_data


class ehrEncoding(nn.Module):

    def __init__(self, EHR_vocab_size, PRS_vocab_size, max_seq_len_ehr, max_seq_len_prs,
                 ehr_emb_size, prs_emb_size, kernel_size, pre_embs_ehr=None, pre_embs_prs=None, 
                 ehr_vocab=None, prs_vocab=None):
                 
                 
                 super(ehrEncoding, self).__init__()
        
        
                 self.EHR_vocab_size = EHR_vocab_size
                 self.PRS_vocab_size = PRS_vocab_size
                 self.max_seq_len_ehr = max_seq_len_ehr
                 self.max_seq_len_prs = max_seq_len_prs
                 self.ehr_emb_size = ehr_emb_size
                 self.prs_emb_size = prs_emb_size
                 self.kernel_size = kernel_size
                 self.ch_l1 = int(ehr_emb_size/2)
                 self.ch_l2 = int(prs_emb_size/2)

                 self.padding = int((kernel_size - 1) / 2)
                 self.features_ehr = math.floor(
                                 max_seq_len_ehr + 2 * self.padding - kernel_size + 1) \
                                 + 2 * self.padding - kernel_size + 1
        #self.padding = math.floor(max_seq_len + 2 * self.padding - kernel_size + 1) + 1
                 self.features_prs = math.floor(
                                 max_seq_len_prs + 2 * self.padding - kernel_size + 1) \
                                 + 2 * self.padding - kernel_size + 1

                 self.EHR_embedding = nn.Embedding(
                     EHR_vocab_size, self.ehr_emb_size, padding_idx=0)
        
                 self.PRS_embedding = nn.Embedding(
                     PRS_vocab_size, self.prs_emb_size, padding_idx=0)

        # load pre-computed embeddings
                 cnd_emb = pre_embs_ehr is not None
                 cnd_vocab = ehr_vocab is not None
                 if cnd_emb and cnd_vocab:
                     weight_mtx = np.zeros((EHR_vocab_size, ehr_emb_size))
                     wfound = 0
                     for i in range(EHR_vocab_size):
                         if i != 0:
                             try:
                                 weight_mtx[i] = pre_embs_ehr[ehr_vocab[i]]
                                 wfound += 1
                             except KeyError:
                                weight_mtx[i] = np.random.normal(scale=0.6, size=(ehr_emb_size,))
                     print('Found pre-computed embeddings for {0} concepts'.format(wfound))
                     self.EHR_embedding.from_pretrained(torch.FloatTensor(weight_mtx))
        
        # load pre-computed embeddings
                 cnd_emb_ = pre_embs_prs is not None
                 cnd_vocab_ = prs_vocab is not None
                 if cnd_emb_ and cnd_vocab_:
                     weight_mtx_ = np.zeros((PRS_vocab_size, prs_emb_size))
                     wfound_ = 0
                     for i in range(PRS_vocab_size):
                         if i != 0:
                             try:
                                 weight_mtx_[i] = pre_embs_prs[prs_vocab[i]]
                                 wfound_ += 1
                             except KeyError:
                                 weight_mtx_[i] = np.random.normal(scale=0.6, size=(prs_emb_size,))
                     print('Found pre-computed embeddings for {0} concepts'.format(wfound_))
                     self.PRS_embedding.from_pretrained(torch.FloatTensor(weight_mtx_))

                 self.cnn_l1 = nn.Conv1d(self.ehr_emb_size, self.ch_l1,
                                kernel_size=kernel_size, padding=self.padding)
                 self.bn1 = nn.BatchNorm1d(self.ch_l1, momentum=0.9)
                 self.bn2 = nn.BatchNorm1d(200)
                 self.bn3 = nn.BatchNorm1d(300)

                 self.cnn_l2 = nn.Conv1d(self.prs_emb_size, self.ch_l2,
                                kernel_size=kernel_size, padding=self.padding)
                 self.bn2 = nn.BatchNorm1d(self.ch_l2, momentum=0.9)


                 self.linear1 = nn.Linear(self.ch_l1 * self.features_ehr, 64)
                 self.linear3 = nn.Linear(64,32)
                 self.linear7 = nn.Linear(32, 64)
                 self.linear8 = nn.Linear(64, EHR_vocab_size * max_seq_len_ehr)

                 self.sigm = nn.Sigmoid()
                 self.softplus = nn.Softplus()

        # instantiate layers for PRS
                 self.linear9 = nn.Linear(64, 64)
                 self.linear10 = nn.Linear(64,32)
                 self.linear11 = nn.Linear(32,64)
                 self.linear13 = nn.Linear(64, PRS_vocab_size * max_seq_len_prs)
        
        # intermediate fully      
                 self.linear15 = nn.Linear(32,32)
                 self.linear16 = nn.Linear(32,16)



    def forward(self, x, y):
        # embedding
        embeds_ehr = self.EHR_embedding(x)
        embeds_ehr = embeds_ehr.permute(0, 2, 1)

        
        embeds_prs = self.PRS_embedding(y)

        
        # first CNN layer
        out_ehr = F.relu(self.cnn_l1(embeds_ehr))
        out_ehr = F.max_pool1d(out_ehr, kernel_size=self.kernel_size,
                           stride=1, padding=self.padding)
        

        out_ehr = out_ehr.view(-1, out_ehr.shape[2] * out_ehr.shape[1])
 
        out_ehr = self.linear1(out_ehr)
        out_ehr = torch.relu(out_ehr)
        out_ehr = F.dropout(out_ehr)
        out_ehr = self.linear3(out_ehr)

        out_prs = torch.mean(embeds_prs,1)
      
        out_prs = self.linear9(out_prs)
        out_prs = F.dropout(out_prs, 0.3)
        out_prs = torch.relu(out_prs)
        out_prs = self.linear10(out_prs)



        # encoded representation
        encoded_ehr = out_ehr.view(-1, out_ehr.shape[1])
        encoded_prs = out_prs.view(-1, out_prs.shape[1])
        encoded_concat = torch.cat((encoded_ehr, encoded_prs), 0)

        encoded_concat = self.linear15(encoded_concat)
        encoded_concat = torch.relu(encoded_concat)
        encoded_concat = F.dropout(encoded_concat)
        encoded_merged = self.linear16(encoded_concat)

        encoded_vec = encoded_merged.view(-1, encoded_merged.shape[1])

        out_ehr = self.linear7(out_ehr)
        out_ehr = F.softplus(out_ehr)
        out_ehr = self.linear8(out_ehr)

        out_ehr = out_ehr.view(-1, self.EHR_vocab_size, x.shape[1])

        
        # prs decoder
        out_prs = self.linear11(out_prs)

        out_prs = F.softplus(out_prs)
        out_prs = self.linear13(out_prs)

        out_prs = out_prs.view(-1, self.PRS_vocab_size, y.shape[1])

        return out_ehr, out_prs, encoded_vec


model = ehrEncoding(EHR_vocab_size = 11732,
                        PRS_vocab_size = 76,
                        max_seq_len_ehr = 8,
                        max_seq_len_prs = 15,
                        ehr_emb_size = 64,
                        prs_emb_size = 64,
                        kernel_size = 3)



# use map_location cause the model was trained on gpu
checkpoint = torch.load('best_model.pt', map_location=torch.device('cpu'))



model.load_state_dict(checkpoint['model_state_dict'])


indir = ''


data_ts = EHRdata(os.path.join(indir), ehr_file= 'ehr_prs.csv', sampling=None)


data_generator_ts = DataLoader(data_ts,
                                       ut.model_param['batch_size'],
                                       shuffle=False,
                                       collate_fn=ehr_collate)
print("Test cohort size: {0}".format(len(data_ts)))

# load loss
loss = checkpoint['loss']


loss_fn = nn.CrossEntropyLoss()

def accuracy(out, target):
    logsoft = F.log_softmax(out, dim=1)
    pred = torch.argmax(logsoft, dim=1)
    return torch.sum((pred == target).float()) / (out.shape[2] * out.shape[0])

metrics = {'accuracy': accuracy}



"""
Evaluate representation model
"""
def evaluate(model, loss_fn, data_iter_ts, metrics, best_eval=False):
    model.eval()
    summ = []
    encoded_list = []
    encoded_list_avg = []
    mrn_list = []

    with torch.no_grad():
        for idx, (list_mrn, list_batch_ehr, list_batch_prs) in enumerate(data_iter_ts):
            for batch, mrn in zip(zip(list_batch_ehr,list_batch_prs), list_mrn):
                batch_ehr, batch_prs = batch[0], batch[1]
                out_ehr, out_prs, encoded = model(batch_ehr, batch_prs)
                loss_1 = loss_fn(out_ehr, batch_ehr)
                loss_2 = loss_fn(out_prs, batch_prs)
                loss = loss_1 + loss_2
                out_ehr.cpu()
                out_prs.cpu()
                encoded.cpu()
                summary_batch = {metric: metrics[metric](out_ehr, batch_ehr).item()
                                 for metric in metrics}
                summary_batch['loss'] = loss.item()
                summ.append(summary_batch)
                if best_eval:
                    encoded_list_avg.append(
                        np.mean(encoded.tolist(), axis=0).tolist())
                    encoded_list.append(encoded.tolist())
                    mrn_list.append(mrn)
        metrics_mean = {metric: np.mean(
            [x[metric] for x in summ]) for metric in summ[0]}
        metrics_string = " -- ".join("{}: {:05.3f}".format(k.capitalize(), v)
                                     for k, v in sorted(metrics_mean.items()))
        print(metrics_string)

        return mrn_list, encoded_list, encoded_list_avg, metrics_mean

mrn_list, encoded_list, encoded_list_avg, metrics_mean = evaluate(model, loss_fn, data_generator_ts, metrics, best_eval=True)

outfile = os.path.join(indir, 'TSconvae-avg_vect_16_03_b.csv')
with open(outfile, 'w') as f:
    wr = csv.writer(f)
    for m, e in zip(mrn_list, encoded_list_avg):
        print(m)
        wr.writerow([m] + list(e))

