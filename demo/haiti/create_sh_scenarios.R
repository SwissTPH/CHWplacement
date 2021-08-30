
batch_CHW_scenario <- function( radius, buffer, capaurb, caparur,
                                rururb_cutoff=300, minurbsize =2000, isinside=0,gurobi_gap=5,
                                name="myname",
                                dpt_init=1, dpt_end=14
                       , time = "1-00:00:00"
                       , mem = "50000"
                       , qos_sub = "1day", mypath, workingdir
){
  capacity_name=paste0(capaurb,caparur,rururb_cutoff,"in",isinside,"urbs",minurbsize) 
  repo=paste0("clscp_",radius,"_buffer",buffer,"_capa",capacity_name)
  
  submitFileName <-file.path("batchfiles",paste0(name,"batch.sh"))
  cat( 
  "#!/bin/bash
#SBATCH --job-name=\"",name,"\"
#SBATCH --mem=",mem,"# memory pool for all cores
#SBATCH --output=",name,".LOG
#SBATCH -e=",name,".err
#SBATCH --time=",time,"
#SBATCH --qos=",qos_sub,"
#SBATCH --cpus-per-task=20
#SBATCH --array=",dpt_init,"-",dpt_end,"

dpt_name=$(head -$SLURM_ARRAY_TASK_ID dept.list | tail -1)

buffer=",buffer,"
isinside=",isinside,"
capaurb=",capaurb,"
caparur=",caparur,"
rururb_cutoff=",rururb_cutoff,"
radius=",radius,"
minurbsize=",minurbsize,"
capacity_name=\"",capacity_name,"\"
repo=\"",repo,"\"
workingdir=\"",workingdir,"\"


module purge
module load R
module load GDAL

Rscript ../calculate_placement_haiti.R $workingdir \"write\" $dpt_name $capacity_name $buffer $isinside $capaurb $caparur $rururb_cutoff $minurbsize $radius

module purge
module load Gurobi/8.1.0_linux64
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

gurobi_cl MIPGapAbs=",gurobi_gap,"  ResultFile=",mypath,"/$repo/$dpt_name.sol ",mypath,"/$repo/$dpt_name.mps


module purge
module load R
module load GDAL

Rscript ../calculate_placement_haiti.R $workingdir \"read\" $dpt_name $capacity_name $buffer $isinside $capaurb $caparur $rururb_cutoff $minurbsize $radius
"    , sep = "", file = submitFileName
  )
  
  system(paste0("cd batchfiles ; sbatch   ", name,"batch.sh"))

}#end function
