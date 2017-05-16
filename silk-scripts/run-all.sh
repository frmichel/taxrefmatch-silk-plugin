#!/bin/bash

DS=taxonconcept
./run-silk.sh ${DS} | tee run-silk-${DS}.log
./merge-links.sh ${DS} | tee merge-links-${DS}.log &

DS=geospecies
./run-silk.sh ${DS} | tee run-silk-${DS}.log
./merge-links.sh ${DS} | tee merge-links-${DS}.log &

DS=vto
./run-silk.sh ${DS} | tee run-silk-${DS}.log
./merge-links.sh ${DS} | tee merge-links-${DS}.log &

DS=ncbi
./run-silk.sh ${DS} | tee run-silk-${DS}.log
./merge-links.sh ${DS} | tee merge-links-${DS}.log

