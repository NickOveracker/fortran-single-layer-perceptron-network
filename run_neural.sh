#!/bin/bash
./neural | tr -s " " | cut -d" " -f2,3,4,5 --output-delimiter="," > output.csv
