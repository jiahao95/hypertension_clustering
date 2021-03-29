"""
Define the data to feed the deep learning model.

If batch_size = 1, each sequence is padded to reach length multiple of
"padded_seq_len"; each sequence is tehn trimmed in subsequences of
length "padded_seq_len".
"""

from torch.utils.data import Dataset
from utils import ehr_len_padded, prs_len_padded
import random
import torch
import os
import csv
import pandas as pd
import ast


class EHRdata(Dataset):

    def __init__(self, datadir, ehr_file, sampling):
        self.ehr = {}
        df = pd.read_csv(os.path.join(datadir, ehr_file), converters={'unique_concept_x':ast.literal_eval, 'unique_concept_y':ast.literal_eval})
        df = df.sample(frac=1).reset_index(drop=True)
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
        # print('length:      ', len(ehr_seq))
        # print(pat)
        # print('ehr_seq:   ', ehr_seq)
        
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
    #print('prs_data    ',prs_data)
    #print('ehr_data    ', ehr_data)
    #print('mrn    ', mrn)
    return mrn, ehr_data, prs_data
