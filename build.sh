cd psv && python table_gen.py && cd .. &&

clang *.c -march=native -o engine -O3
