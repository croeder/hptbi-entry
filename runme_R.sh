#!/usr/bin/env bash

FAILURE=0

mkdir -p output
echo "language | r" > ./output/evaluation.txt

if [ -z "$1" ];
then
  echo "Version | dev" >> ./output/evaluation.txt
else
  echo "Version | $1" >> ./output/evaluation.txt
fi


if ! R CMD BATCH --vanilla '--args mortality' training.R output/training_mortality.Rout
then
    echo "Training mortality failed. See output/training_mortality.Rout"
    FAILURE=1
fi

if ! R CMD BATCH --vanilla '--args fss'       training.R output/training_fss.Rout
then
    echo "Training fss failed. See output/training_fss.Rout"
    FAILURE=1
fi

if ! R CMD BATCH --vanilla "--args $1" testing.R output/testing.Rout
then
    echo "Testing failed. See output/testing.Rout"
    FAILURE=1
fi

if ! R CMD BATCH --vanilla "--args $1" evaluate.R output/evaluate.Rout
then
    echo "Evaluation failed. See output/evaluate.Rout"
    FAILURE=1
fi

if [[ $FAILURE == 0 ]]
then
    echo "Success!"
    echo ""
    grep -v "seconds elapsed" output/evaluation.txt
fi

