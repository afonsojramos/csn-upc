#!/bin/bash
make clean
make

echo "Arabic" > ./results/ER.txt
for f in "./data/ER/Arabic/"*.txt; do
  echo "Processing $f file..."
  ./closeness_centrality $f 21532 68743 >> ./results/ER.txt
done

echo "Basque" >> ./results/ER.txt
for f in "./data/ER/Basque/"*.txt; do
  echo "Processing $f file..."
  ./closeness_centrality $f 12207 25541 >> ./results/ER.txt
done

echo "Catalan" >> ./results/ER.txt
for f in "./data/ER/Catalan/"*.txt; do
  echo "Processing $f file..."
  ./closeness_centrality $f 36865 197074 >> ./results/ER.txt
done

echo "Chinese" >> ./results/ER.txt
for f in "./data/ER/Chinese/"*.txt; do
  echo "Processing $f file..."
  ./closeness_centrality $f 40298 180925 >> ./results/ER.txt
done

echo "Czech" >> ./results/ER.txt
for f in "./data/ER/Czech/"*.txt; do
  echo "Processing $f file..."
  ./closeness_centrality $f 69303 257222 >> ./results/ER.txt
done

echo "English" >> ./results/ER.txt
for f in "./data/ER/English/"*.txt; do
  echo "Processing $f file..."
  ./closeness_centrality $f 29634 193067 >> ./results/ER.txt
done

echo "Greek" >> ./results/ER.txt
for f in "./data/ER/Greek/"*.txt; do
  echo "Processing $f file..."
  ./closeness_centrality $f 13283 43961 >> ./results/ER.txt
done

echo "Hungarian" >> ./results/ER.txt
for f in "./data/ER/Hungarian/"*.txt; do
  echo "Processing $f file..."
  ./closeness_centrality $f 36126 106681 >> ./results/ER.txt
done

echo "Italian" >> ./results/ER.txt
for f in "./data/ER/Italian/"*.txt; do
  echo "Processing $f file..."
  ./closeness_centrality $f 14726 55954 >> ./results/ER.txt
done

echo "Turkish" >> ./results/ER.txt
for f in "./data/ER/Turkish/"*.txt; do
  echo "Processing $f file..."
  ./closeness_centrality $f 20409 45620 >> ./results/ER.txt
done

python3 ./scripts/to_csv.py