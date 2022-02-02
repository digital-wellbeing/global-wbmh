#!/bin/bash

# Runs the R script models.R with arguments from --array
# It requests 12 cores per model
# Use sbatch --array=1-6 submit.sh to use a job array to submit 6 tasks

#SBATCH --constraint='cpu_frq:2.90GHz'
#SBATCH --cpus-per-task=12
#SBATCH --mem-per-cpu=2G
#SBATCH --time=24:00:00
#SBATCH --job-name=DWG-Global

module purge
module load R/4.1.2-foss-2021b-ARC

Rscript models.R $SLURM_ARRAY_TASK_ID
