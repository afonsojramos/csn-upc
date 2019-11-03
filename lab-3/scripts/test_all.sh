#!/bin/bash
make clean
make
FILES=./data/tests/*
echo "" > ./results/baseline_new.txt
for f in $FILES
do
  echo "Processing $f file..."
  name=${f/#"./data/tests/"}
  name=${name/%"_test.txt"}
  echo $name >> ./results/baseline_new.txt
  ./closeness_centrality $f >> ./results/baseline_new.txt
done