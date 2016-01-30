#! /bin/sh
cat shuttle.csv | sed s:1$:RAD-FLOW: | sed s:2$:FPV-CLOSE: | sed s:3$:FPV-OPEN: | sed s:4$:HIGH: | sed s:5$:BYPASS: | sed s:6$:BPV-CLOSE: | sed s:7$:BPV-OPEN:

