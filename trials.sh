#! /bin/bash

./GENLIN --parallel t \
         --packs nil \
         --opcode-bits 3 \
         --dataset :thyroid \
         --data-path \"./datasets/thyroid/ann-train.lbl.csv\" \
         --testing-data-path \"./datasets/thyroid/ann-test.lbl.csv\" \
         --sex nil \
         --mutation-rate 1 \
         --selection-method :lexicase \
         --fitfunc-name :detection-rate \
         --rounds 4000 \
         --save-every 1000 \
         --stat-interval 100 \
         --migration-rate 100 \
         --migration-size 1/5 \
         --greedy-migration 1/2 \
         --target .99 \
         --number-of-islands 8 \
         --population-size 1200 \
         --max-cal-depth 2 \
         --sampling-policy :balanced \
         --sampling-ratio 1/3 \
    | tee logs/thyroid-lexicase-asexual-balanced.lab.log

./GENLIN --parallel t \
         --packs nil \
         --opcode-bits 3 \
         --dataset :thyroid \
         --data-path \"./datasets/thyroid/ann-train.lbl.csv\" \
         --testing-data-path \"./datasets/thyroid/ann-test.lbl.csv\" \
         --sex :1pt \
         --mutation-rate 1/3 \
         --selection-method :lexicase \
         --fitfunc-name :detection-rate \
         --rounds 4000 \
         --save-every 1000 \
         --stat-interval 100 \
         --migration-rate 100 \
         --migration-size 1/5 \
         --greedy-migration 1/2 \
         --target .99 \
         --number-of-islands 8 \
         --population-size 1200 \
         --max-cal-depth 2 \
         --sampling-policy :balanced \
         --sampling-ratio 1/3 \
    | tee logs/thyroid-lexicase-sex1pt-balanced.lab.log

./GENLIN --parallel t \
         --packs nil \
         --opcode-bits 3 \
         --dataset :thyroid \
         --data-path \"./datasets/thyroid/ann-train.lbl.csv\" \
         --testing-data-path \"./datasets/thyroid/ann-test.lbl.csv\" \
         --sex nil \
         --mutation-rate 1 \
         --selection-method :tournement \
         --fitfunc-name :detection-rate \
         --rounds 4000 \
         --save-every 1000 \
         --stat-interval 100 \
         --migration-rate 100 \
         --migration-size 1/5 \
         --greedy-migration 1/2 \
         --target .99 \
         --number-of-islands 8 \
         --population-size 1200 \
         --max-cal-depth 2 \
         --sampling-policy :balanced \
         --sampling-ratio 1/3 \
    | tee logs/thyroid-tournement-asexual.lab.log

./GENLIN --parallel t \
         --packs nil \
         --opcode-bits 3 \
         --dataset :thyroid \
         --data-path \"./datasets/thyroid/ann-train.lbl.csv\" \
         --testing-data-path \"./datasets/thyroid/ann-test.lbl.csv\" \
         --sex :1pt \
         --mutation-rate 1/3 \
         --selection-method :lexicase \
         --fitfunc-name :detection-rate \
         --rounds 4000 \
         --save-every 1000 \
         --stat-interval 100 \
         --migration-rate 100 \
         --migration-size 1/5 \
         --greedy-migration 1/2 \
         --target .99 \
         --number-of-islands 8 \
         --population-size 1200 \
         --max-cal-depth 2 \
         --sampling-policy :balanced \
         --sampling-ratio 1/3 \
    | tee logs/thyroid-tournement-sex1pt.lab.log


./GENLIN --parallel t \
         --packs nil \
         --opcode-bits 3 \
         --dataset :thyroid \
         --data-path \"./datasets/thyroid/ann-train.lbl.csv\" \
         --testing-data-path \"./datasets/thyroid/ann-test.lbl.csv\" \
         --sex :1pt \
         --mutation-rate 1/3 \
         --selection-method :lexicase \
         --fitfunc-name :n-ary-prop-vote \
         --rounds 4000 \
         --save-every 1000 \
         --stat-interval 100 \
         --migration-rate 100 \
         --migration-size 1/5 \
         --greedy-migration 1/2 \
         --target .99 \
         --number-of-islands 8 \
         --population-size 1200 \
         --max-cal-depth 2 \
         --sampling-policy :balanced \
         --sampling-ratio 1/3 \
    | tee logs/thyroid-tournement-sex1pt-propvote.lab.log

./GENLIN --parallel t \
         --packs nil \
         --opcode-bits 3 \
         --dataset :shuttle \
         --data-path \"./datasets/shuttle/shuttle.trn.csv\" \
         --testing-data-path \"./datasets/shuttle/shuttle.tst.csv\" \
         --sex :1pt \
         --mutation-rate 1/3 \
         --selection-method :tournement \
         --fitfunc-name :n-ary-prop-vote \
         --rounds 4000 \
         --save-every 1000 \
         --stat-interval 100 \
         --migration-rate 100 \
         --migration-size 1/5 \
         --greedy-migration 1/2 \
         --target .99 \
         --number-of-islands 8 \
         --population-size 1200 \
         --max-cal-depth 2 \
         --sampling-policy :balanced \
         --sampling-ratio 1/3 \
    | tee logs/shuttle-tournement-sex1pt-propvote.lab.log

./GENLIN --parallel t \
         --packs nil \
         --opcode-bits 3 \
         --dataset :shuttle \
         --data-path \"./datasets/shuttle/shuttle.trn.csv\" \
         --testing-data-path \"./datasets/shuttle/shuttle.tst.csv\" \
         --sex :1pt \
         --mutation-rate 1/3 \
         --selection-method :tournement \
         --fitfunc-name :detection-rate \
         --rounds 4000 \
         --save-every 1000 \
         --stat-interval 100 \
         --migration-rate 100 \
         --migration-size 1/5 \
         --greedy-migration 1/2 \
         --target .99 \
         --number-of-islands 8 \
         --population-size 1200 \
         --max-cal-depth 2 \
         --sampling-policy :balanced \
         --sampling-ratio 1/3 \
    | tee logs/shuttle-tournement-sex1pt-dr.lab.log



 
./GENLIN --parallel t \
         --packs nil \
         --opcode-bits 3 \
         --dataset :shuttle \
         --data-path \"./datasets/shuttle/shuttle.trn.csv\" \
         --testing-data-path \"./datasets/shuttle/shuttle.tst.csv\" \
         --sex :1pt \
         --mutation-rate 1/3 \
         --selection-method :lexicase \
         --fitfunc-name :detection-rate \
         --rounds 4000 \
         --save-every 1000 \
         --stat-interval 100 \
         --migration-rate 100 \
         --migration-size 1/5 \
         --greedy-migration 1/2 \
         --target .99 \
         --number-of-islands 8 \
         --population-size 1200 \
         --max-cal-depth 2 \
         --sampling-policy :balanced \
         --sampling-ratio 1/3 \
    | tee logs/shuttle-lexicase-sex1pt-dr.lab.log

 
./GENLIN --parallel t \
         --packs nil \
         --opcode-bits 3 \
         --dataset :shuttle \
         --data-path \"./datasets/shuttle/shuttle.trn.csv\" \
         --testing-data-path \"./datasets/shuttle/shuttle.tst.csv\" \
         --sex nil \
         --mutation-rate 1 \
         --selection-method :lexicase \
         --fitfunc-name :detection-rate \
         --rounds 4000 \
         --save-every 1000 \
         --stat-interval 100 \
         --migration-rate 100 \
         --migration-size 1/5 \
         --greedy-migration 1/2 \
         --target .99 \
         --number-of-islands 8 \
         --population-size 1200 \
         --max-cal-depth 2 \
         --sampling-policy :balanced \
         --sampling-ratio 1/3 \
    | tee logs/shuttle-lexicase-asexual-dr.lab.log










