#!/bin/bash -l

#SBATCH -J Marek_GAMM_maya_shannon
#SBATCH --account=project_2003211
#SBATCH -o my_output_%j
#SBATCH -e my_output_err_%j
#SBATCH -t 71:15:00
#SBATCH --cpus-per-task=40
#SBATCH -p serial 
#SBATCH --partition=large
#SBATCH --mem-per-cpu=2G 
#SBATCH -N 2
#SBATCH -n 2
#SBATCH --mail-type=END
#SBATCH --mail-type=ALL
#SBATCH --mail-user=mpotterf@jyu.fi

module load r-env-deprecated
srun -J "M13z" --nodes 1 --ntasks 1 Rscript gamm_m1_3.R &
srun -J "M1z" --nodes 1 --ntasks 1 Rscript gamm_m1z.R &
wait
