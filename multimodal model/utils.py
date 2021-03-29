import torch
import os

# dataset filenames
dt_files = {'ehr-file': 'ehr_prs_merged.csv',
            'ehr-file-test': 'testing_subset.csv',
            'vocab_ehr': 'dictionary_EHR.csv',
            'vocab_prs': 'dictionary_PRS.csv'}

# model parameters
model_param = {'num_epochs': 30,
               'batch_size': 32, 
               'ehr_embedding_size': 64,
               'prs_embedding_size': 64,
               'kernel_size': 3,
               'learning_rate': 0.0001,
               'weight_decay': 1e-5
               }

# length of padded sub-sequences
ehr_len_padded = 8
prs_len_padded = 15

# save the best model
def save_best_model(epoch, model, optimizer, loss, outdir):
    torch.save({'epoch': epoch,
                'model_state_dict': model.state_dict(),
                'optimizer_state_dict': optimizer.state_dict(),
                'loss': loss}, os.path.join(outdir, 'best_model.pt'))
