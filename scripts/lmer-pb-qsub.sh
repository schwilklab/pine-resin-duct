#!/bin/bash

## Run the full landscape historical temperature reconstruction

# load modules
module load intel
module load R
# module load gdal # not yet working, link errors. Not needed

# necessary for R to use BLAS libraries on quanah:
export MKL_NUM_THREADS=36
export OPM_NUM_THREADS=36

#$ -V
#$ -N pblmer
#$ -o ../results/$JOB_NAME.o$JOB_ID
#$ -e ../results/$JOB_NAME.e$JOB_ID
#$ -cwd
#$ -S /bin/bash
#$ -P quanah
#$ -pe fill 36
#$ -q omni

R --slave < ~/projects/pine-resin-duct/scripts/pb-lmer.R

