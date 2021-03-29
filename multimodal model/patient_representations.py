from model.data_loader import EHRdata, ehr_collate
from gensim.models import Word2Vec
from train import train_and_evaluate
from time import time
from torch.utils.data import DataLoader
import model.net as net
import torch.nn as nn
import torch
import utils as ut
import argparse
import sys
import csv
import os

"""
Learn patient representations from the EHRs using an autoencoder of CNNs
"""


def learn_patient_representations(indir,
                                  test_set=False,
                                  sampling=None,
                                  emb_ehr=None,
                                  emb_prs=None):
    exp_dir = os.path.join(indir, 'encodings')
    os.makedirs(exp_dir, exist_ok=True)

    # get the vocabulary size
    fvocab_ehr = os.path.join(os.path.join(indir),
                          ut.dt_files['vocab_ehr'])
    with open(fvocab_ehr) as f:
        rd_ehr = csv.reader(f)
        next(rd_ehr)
        vocab_ehr = {}
        for r in rd_ehr:
            vocab_ehr[int(r[1])] = r[0]
        vocab_ehr_size = len(vocab_ehr) + 1
    print('EHR Vocabulary size: {0}'.format(vocab_ehr_size))

    fvocab_prs = os.path.join(os.path.join(indir),
                          ut.dt_files['vocab_prs'])
    with open(fvocab_prs) as d:
        rd_prs = csv.reader(d)
        next(rd_prs)
        vocab_prs = {}
        for l in rd_prs:
            vocab_prs[int(l[1])] = l[0]
        vocab_prs_size = len(vocab_prs) + 1
    print('PRS Vocabulary size: {0}'.format(vocab_prs_size))


    # load pre-computed embeddings
    if emb_ehr is not None:
        model = Word2Vec.load(emb_ehr)
        embs = model.wv
        del model
        print('Loaded pre-computed EHR embeddings for {0} concepts'.format(
            len(embs.vocab)))
    else:
        embs = None
    
    # load prs embeddings
    if emb_prs is not None:
        model_prs = Word2Vec.load(emb_prs)
        embeddings = model_prs.wv
        del model_prs
        print('Loaded pre-computed PRS embeddings for {0} concepts'.format(
            len(embeddings.vocab)))
    else:
        embeddings = None

    # set random seed for experiment reproducibility
    torch.manual_seed(123)
    torch.cuda.manual_seed(123)

    # load data
    data_tr = EHRdata(os.path.join(indir), ut.dt_files['ehr-file'], sampling)
    data_generator_tr = DataLoader(data_tr,
                                   ut.model_param['batch_size'],
                                   shuffle=True,
                                   collate_fn=ehr_collate)
    if test_set:
        data_ts = EHRdata(os.path.join(indir), ut.dt_files['ehr-file-test'],
                          sampling)

        data_generator_ts = DataLoader(data_ts,
                                       ut.model_param['batch_size'],
                                       shuffle=True,
                                       collate_fn=ehr_collate)
        print("Test cohort size: {0}".format(len(data_ts)))
    else:
        data_generator_ts = data_generator_tr

    print('Training cohort size: {0}\n'.format(len(data_tr)))
    print('Max Sequence Length EHR: {0}\n'.format(ut.ehr_len_padded))
    print('Max Sequence Length PRS: {0}\n'.format(ut.prs_len_padded))
    # define model and optimizer
    print('Learning rate: {0}'.format(ut.model_param['learning_rate']))
    print('Batch size: {0}'.format(ut.model_param['batch_size']))
    print('Kernel size: {0}\n'.format(ut.model_param['kernel_size']))

    model = net.ehrEncoding(EHR_vocab_size = vocab_ehr_size,
                            PRS_vocab_size = vocab_prs_size,
                            max_seq_len_ehr = ut.ehr_len_padded,
                            max_seq_len_prs = ut.prs_len_padded,
                            ehr_emb_size = ut.model_param['ehr_embedding_size'],
                            prs_emb_size = ut.model_param['prs_embedding_size'],
                            kernel_size = ut.model_param['kernel_size'],
                            pre_embs_ehr = embs,
                            ehr_vocab = vocab_ehr,
                            pre_embs_prs = embeddings,
                            prs_vocab = vocab_prs)

    optimizer = torch.optim.Adam(model.parameters(),
                                 lr=ut.model_param['learning_rate'],
                                 weight_decay=ut.model_param['weight_decay'])

    # training and evaluation
    if torch.cuda.device_count() > 1:
        print('No. of GPUs: {0}\n'.format(torch.cuda.device_count()))
        model = nn.DataParallel(model)
    else:
        model.cuda()
        print('No. of GPUs: 1\n')

    # model.cuda()
    loss_fn = net.criterion
    print('Training for {} epochs\n'.format(ut.model_param['num_epochs']))

    mrn, encoded, encoded_avg, metrics_avg = train_and_evaluate(model,
                                                                data_generator_tr,
                                                                data_generator_ts,
                                                                loss_fn,
                                                                optimizer,
                                                                net.metrics,
                                                                exp_dir)

    # save results

    # encoded vectors (representations)
    outfile = os.path.join(exp_dir, 'convae-avg_vect.csv')
    with open(outfile, 'w') as f:
        wr = csv.writer(f)
        wr.writerow(["MRN", "ENCODED-AVG"])
        for m, e in zip(mrn, encoded_avg):
            wr.writerow([m] + list(e))

    outfile = os.path.join(exp_dir, 'convae_vect.csv')
    with open(outfile, 'w') as f:
        wr = csv.writer(f)
        wr.writerow(["MRN", "ENCODED-SUBSEQ"])
        for m, evs in zip(mrn, encoded):
            for e in evs:
                wr.writerow([m] + e)

    # metrics (loss and accuracy)
    outfile = os.path.join(exp_dir, 'metrics.txt')
    with open(outfile, 'w') as f:
        f.write('Mean loss: %.3f\n' % metrics_avg['loss'])
        f.write('Accuracy: %.3f\n' % metrics_avg['accuracy'])

    #ehr subseq with age in days
    outfile = os.path.join(exp_dir, 'cohort-ehr-subseq{0}.csv'.format(ut.ehr_len_padded))
    with open(os.path.join(os.path.join(indir), 'cohort-ehrseq.csv')) as f:
        rd = csv.reader(f)
        next(rd)
        ehr = {}
        for r in rd:
            ehr.setdefault(r[0], list()).extend(r[1:])
    ehr_subseq = {}
    for list_m, batch in data_generator_tr:
        for b, m in zip(batch, list_m):
            if len(b) == 1:
                ehr_subseq[m] = b.tolist()
            else:
                seq = []
                for vec in b.tolist():
                    seq.extend(vec)
                nseq, nleft = divmod(len(seq), ut.ehr_len_padded)
                if nleft > 0:
                    seq = seq + [0] * \
                          (ut.ehr_len_padded - nleft)
                for i in range(0, len(seq) - ut.ehr_len_padded + 1,
                               ut.ehr_len_padded):
                    ehr_subseq.setdefault(m, list()).append(seq[i:i + ut.ehr_len_padded])
    with open(outfile, 'w') as f:
        wr = csv.writer(f)
        wr.writerow(["MRN", "EHRsubseq"])
        for m, subseq in ehr_subseq.items():
            for seq in subseq:
                wr.writerow([m] + list(filter(lambda x: x != 0, seq)))

    if test_set:
        outfile = os.path.join(exp_dir, 'cohort_test-ehr-subseq{0}.csv'.format(ut.ehr_len_padded))
        ehr_subseq = {}
        for list_m, batch in data_generator_ts:
            for b, m in zip(batch, list_m):
                if len(b) == 1:
                    ehr_subseq[m] = b.tolist()
                else:
                    seq = []
                    for vec in b.tolist():
                        seq.extend(vec)
                    nseq, nleft = divmod(len(seq), ut.ehr_len_padded)
                    if nleft > 0:
                        seq = seq + [0] * \
                              (ut.ehr_len_padded - nleft)
                    for i in range(0, len(seq) - ut.ehr_len_padded + 1,
                                   ut.ehr_len_padded):
                        ehr_subseq.setdefault(m, list()).append(seq[i:i + ut.ehr_len_padded])
        with open(outfile, 'w') as f:
            wr = csv.writer(f)
            wr.writerow(["MRN", "EHRsubseq"])
            for m, subseq in ehr_subseq.items():
                for seq in subseq:
                    wr.writerow([m] + list(filter(lambda x: x != 0, seq)))

    return


# main function

def _process_args():
    parser = argparse.ArgumentParser(
        description='EHR Patient Stratification: derive patient '
                    'representations from longitudinal EHRs')
    parser.add_argument(dest='indir', help='EHR dataset directory')
    parser.add_argument('--test_set', dest='test_set',
                        default=False)
    parser.add_argument('-s', default=None, type=int,
                        help='Enable sub-sampling with data size '
                             '(default: None)')
    parser.add_argument('-e_EHR', default=None,
                        help='Pre-computed EHR embeddings (default: None)')
    parser.add_argument('-e_PRS', default=None,
                        help='Pre-computed PRS embeddings (default: None)')
    return parser.parse_args(sys.argv[1:])


if __name__ == '__main__':
    args = _process_args()
    print('')

    start = time()
    learn_patient_representations(indir=args.indir,
                                  test_set=args.test_set,
                                  sampling=args.s,
                                  emb_ehr=args.e_EHR,
                                  emb_prs=args.e_PRS)

    print('\nProcessing time: %s seconds\n' % round(time() - start, 2))

    print('Task completed\n')
