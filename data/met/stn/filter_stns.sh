#!/bin/bash

cp ghcnd-stations.txt relevant-stations.txt

# by country
sed -ni '/^US/p; /^CA*/p' relevant-stations.txt

# by lat/lon
awk '$2 >= 39.5 && $2<=51.6 && $3 <= -75.5 && $3 >= -93.5' relevant-stations.txt | sponge relevant-stations.txt 

# if you don't have sponge, then comment above and use this instead
#awk $2 >= 39.5 && $2<=51.6 && $3 <= -75.5 && $3 >= -93.5 relevant-stations.txt > relevant.tmp && mv -f relevant.tmp relevant-stations.txt
