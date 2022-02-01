#!/bin/bash

# Runs the R script serially
# It requests 12 cores per model

#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=12
#SBATCH --mem-per-cpu=2G
#SBATCH --time=48:00:00
#SBATCH --job-name=Global-WBMH
#SBATCH --output=global-wbmh-slurm.out

module purge
module load R/4.1.2-foss-2021b-ARC

Rscript models.R