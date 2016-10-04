#!/bin/bash
bash run.sh | diff - run.sh.out >>dummy.k
