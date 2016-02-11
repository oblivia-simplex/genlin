#! /bin/sh
cat $1 | sed s:1$:CLASS-1: | sed s:2$:CLASS-2: | sed s:3$:NEITHER:

