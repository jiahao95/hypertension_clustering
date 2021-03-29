"""
Model training and evaluation.

The model is evaluated when (1) loss < 0.001 or (2) the number of
epochs is reached. The best model is saved in the experiment folder.
"""

from evaluate import evaluate
from time import time
import utils as ut
import numpy as np
import csv
import os
#from tqdm import tqdm

def train(model, optimizer, loss_fn, data_iter_tr):
    model.train()
    encoded_list = []
    encoded_avg_list = []
    loss_list = []
    mrn_list = []

    for idx, (list_mrn, list_batch_ehr, list_batch_prs) in enumerate(data_iter_tr):
        loss_batch = []
        for batch, mrn in zip(zip(list_batch_ehr,list_batch_prs), list_mrn):
            # print(batch[0])
            batch_ehr, batch_prs = batch[0], batch[1]
            batch_ehr = batch_ehr.cuda()
            batch_prs = batch_prs.cuda()
            #print('batch_ehr: ', batch_ehr)
            #print('batch_prs: ', batch_prs)
            optimizer.zero_grad()
            out_ehr, out_prs, encoded_vect = model(batch_ehr, batch_prs)
            loss_1 = loss_fn(out_ehr, batch_ehr)
            loss_2 = loss_fn(out_prs, batch_prs)
            loss = loss_1 + loss_2
            loss.backward()
            optimizer.step()
            loss_batch.append(loss.item())
            encoded_avg_list.append(
                np.mean(encoded_vect.tolist(), axis=0).tolist())
            encoded_list.append(encoded_vect.tolist())
            mrn_list.append(mrn)

        loss_list.append(np.mean(loss_batch))
        # print progress bar
        if idx % 10 == 0:
            print('Training in progress: [{}/{} ({:.0f}%)]\tLoss: {:.6f}'.
                format(idx * len(list_mrn), len(data_iter_tr.dataset),
                       100. * idx / len(data_iter_tr), loss))
    loss_mean = np.mean(loss_list)

    return mrn_list, encoded_list, encoded_avg_list, loss_mean


def train_and_evaluate(model, data_iter_tr, data_iter_ts,
                       loss_fn, optimizer, metrics, exp_dir):
    loss_vect = []
    n_epoch = ut.model_param['num_epochs']
    for epoch in range(1, n_epoch + 1):
        print('Epoch {0} of {1}'.format(epoch, n_epoch))

        start = time()
        mrn, encoded, encoded_avg, loss_mean = train(
            model, optimizer, loss_fn, data_iter_tr)
        print('-- time = ', round(time() - start, 3))
        print('-- mean loss: {0}'.format(round(loss_mean, 3)))
        loss_vect.append(loss_mean)

        is_best_1 = loss_mean < 0.1
        is_best_2 = epoch == n_epoch
        if is_best_1 or is_best_2:

            outfile = os.path.join(exp_dir, 'TRconvae-avg_vect.csv')
            with open(outfile, 'w') as f:
                wr = csv.writer(f)
                for m, e in zip(mrn, encoded_avg):
                    wr.writerow([m] + list(e))

            outfile = os.path.join(exp_dir, 'TRconvae_vect.csv')
            with open(outfile, 'w') as f:
                wr = csv.writer(f)
                for m, evs in zip(mrn, encoded):
                    for e in evs:
                        wr.writerow([m] + e)

            outfile = os.path.join(exp_dir, 'TRmetrics.txt')
            with open(outfile, 'w') as f:
                f.write('Mean Loss: %.3f\n' % loss_mean)

            outfile = os.path.join(exp_dir, 'TRlosses.csv')
            with open(outfile, 'w') as f:
                wr = csv.writer(f)
                wr.writerow(['Epoch', 'Loss'])
                for idx, l in enumerate(loss_vect):
                    wr.writerow([idx, l])

            print('\nFound new best model at epoch {0}'.format(epoch))
            ut.save_best_model(epoch, model, optimizer, loss_mean, exp_dir)

            print('\nEvaluating the model')
            mrn, encoded, encoded_avg, test_metrics = evaluate(
                model, loss_fn, data_iter_ts, metrics, best_eval=True)

            return mrn, encoded, encoded_avg, test_metrics
