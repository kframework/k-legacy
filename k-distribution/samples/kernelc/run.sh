#!/bin/bash -x

kompile kernelc.k

krun -v tests/binary_search_tree/find.c     --prove tests/binary_search_tree/find_spec.k    --smt_prelude ../../include/z3/search_tree.smt2
krun -v tests/binary_search_tree/insert.c   --prove tests/binary_search_tree/insert_spec.k  --smt_prelude ../../include/z3/search_tree.smt2
krun -v tests/binary_search_tree/delete.c   --prove tests/binary_search_tree/delete_spec.k  --smt_prelude ../../include/z3/search_tree.smt2

krun -v tests/avl_tree/find.c               --prove tests/avl_tree/find_spec.k              --smt_prelude ../../include/z3/search_tree.smt2
krun -v tests/avl_tree/insert.c             --prove tests/avl_tree/insert_spec.k            --smt_prelude ../../include/z3/search_tree.smt2
krun -v tests/avl_tree/delete.c             --prove tests/avl_tree/delete_spec.k            --smt_prelude ../../include/z3/search_tree.smt2

krun -v tests/list/reverse.c                --prove tests/list/reverse_spec.k               --smt_prelude ../../include/z3/list.smt2
krun -v tests/list/append.c                 --prove tests/list/append_spec.k                --smt_prelude ../../include/z3/list.smt2

krun -v tests/sorting/bubble_sort.c         --prove tests/sorting/bubble_sort_spec.k        --smt_prelude ../../include/z3/sorted_list.smt2
krun -v tests/sorting/insertion_sort.c      --prove tests/sorting/insertion_sort_spec.k     --smt_prelude ../../include/z3/sorted_list.smt2
krun -v tests/sorting/quicksort.c           --prove tests/sorting/quicksort_spec.k          --smt_prelude ../../include/z3/sorted_list.smt2
krun -v tests/sorting/merge_sort.c          --prove tests/sorting/merge_sort_spec.k         --smt_prelude ../../include/z3/sorted_list.smt2
