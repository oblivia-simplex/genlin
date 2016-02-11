#! /bin/bash

cat $1 | cut -d, -f2- | sed s:2$:BENIGN: | sed s:4$:MALIGNANT:
