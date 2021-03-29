#! /bin/zsh

clear
virtualenvdir=/sc/arion/projects/rg_HPIMS/user/jiahao01/anaconda3/envs/torch-g/bin/python
indir=./data_example


test_set=$1

if [ ! -z "$test_set" ]
then
    test_set="--test_set $test_set"
fi


$virtualenvdir -u ./patient_representations.py $indir $test_set -e_EHR 'ehr_word_embeddings.bin' -e_PRS 'word_embeddings_prs.bin'
