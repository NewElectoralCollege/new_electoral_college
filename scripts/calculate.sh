rm ../data/2020/*.json
rm ../data/2016/*.json
rm ../data/2012/*.json
rm ../data/2008/*.json
rm ../data/2004/*.json
rm ../data/2000/*.json
rm ../data/1996/*.json
rm ../data/1992/*.json
rm ../data/1988/*.json
rm ../data/1984/*.json
rm ../data/1980/*.json
rm ../data/1976/*.json

cd ../

python -u makeResults.py

cd scripts

sh format.sh