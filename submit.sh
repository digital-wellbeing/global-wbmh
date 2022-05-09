#!/bin/bash

# Runs the R script models.R with arguments from --array
# Use sbatch --array=1-24 submit.sh to submit

#SBATCH --constraint='cpu_frq:2.90GHz'
#SBATCH --cpus-per-task=12
#SBATCH --mem-per-cpu=3G
#SBATCH --time=7-00:00:00
#SBATCH --job-name=DWG-Global

module purge
module load R/4.1.2-foss-2021b-ARC

Rscript models.R $SLURM_ARRAY_TASK_ID
