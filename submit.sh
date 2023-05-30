#!/bin/bash

# Runs the R script models.R with arguments from --array
# Submit on cluster with: sbatch --array=1-18 submit.sh

#SBATCH --constraint='cpu_frq:2.90GHz'
#SBATCH --cpus-per-task=4
#SBATCH --mem-per-cpu=3G
#SBATCH --time=7-00:00:00
#SBATCH --job-name=DWG-Global

module purge
module load R/4.2.2-foss-2022a-ARC

Rscript models.R $SLURM_ARRAY_TASK_ID
