import torch
import torch.nn as nn
import torch.nn.functional as F
import numpy as np
import math


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
        embeds_ehr = self.EHR_embedding(x)
        embeds_ehr = embeds_ehr.permute(0, 2, 1)

        embeds_prs = self.PRS_embedding(y)
   
        
        # first CNN layer
        out_ehr = F.selu(self.cnn_l1(embeds_ehr))
        out_ehr = F.max_pool1d(out_ehr, kernel_size=self.kernel_size,
                           stride=1, padding=self.padding)

        out_ehr = out_ehr.view(-1, out_ehr.shape[2] * out_ehr.shape[1])

        out_ehr = self.linear1(out_ehr)
        out_ehr = torch.relu(out_ehr)
        out_ehr = F.dropout(out_ehr)
        out_ehr = self.linear3(out_ehr)

        out_prs = torch.mean(embeds_prs,1)
        out_prs = self.linear9(out_prs)
        out_prs = torch.relu(out_ehr)
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

        

        out_prs = self.linear11(out_prs)
        out_prs = F.softplus(out_prs)
        out_prs = self.linear13(out_prs)

        out_prs = out_prs.view(-1, self.PRS_vocab_size, y.shape[1])

        return out_ehr, out_prs, encoded_vec


def accuracy(out, target):
    logsoft = F.log_softmax(out, dim=1)
    pred = torch.argmax(logsoft, dim=1)
    return torch.sum((pred == target).float()) / (out.shape[2] * out.shape[0])


criterion = nn.CrossEntropyLoss()

metrics = {'accuracy': accuracy}
