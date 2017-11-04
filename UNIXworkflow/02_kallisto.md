# Kallisto: Quantifying RNA Transcript Abundances

To quantify transcripts, we first need to have a reference genome or transciptome to which the reads can be mapped. I'm a big fan of the kallisto program because its super fast and easy to use! Its also becoming more widely used and trusted.

## Navigate to directory raw reads. Create director for kallisto output

~~~ {.bash}
ssh <username>@stampede.tacc.utexas.edu
cd $SCRATCH/FMR1/00_rawreads
mkdir ../02_kallistoquant
~~~


## 02_kallistoquant

Now, we will use the `kallistoquant` function to quantify reads.

This process has taken me a long time to perfect. The challenge is that critical information is sent to standard output and I don't know how to catptuer that information well if I run all the jobs at once in one script. 

So, I make a commands file and slurm file for every script. This way I get an standard output and error file for each sample as well. These files are needed for MultiQC and for stats about reads mapped. 

### Create the commands files

~~~ {.bash}
for R1 in *R1_001.fastq.gz
do
    R2=$(basename $R1 R1_001.fastq.gz)R2_001.fastq.gz
    samp=$(basename $R1 _R1_001.fastq.gz)
    echo $R1 $R2 $samp
    echo "kallisto quant -b 100 -t 16 -i /work/02189/rmharris/SingleNeuronSeq/data/reference_genomes/gencode.vM7.transcripts.idx  -o ../03_kallistoquant/${samp} $R1 $R2" > ${samp}_03_kallistoquant.cmds
done
~~~

### Create the slurm files and a bash file to execute them

~~~ {.bash}
rm 03_kallistoquant.sh
for cmd in *03_kallistoquant.cmds
do
	samp=$(basename $cmd _L002_03_kallistoquant.cmds)
	slurm=$(basename $cmd _L002_03_kallistoquant.cmds).slurm
	echo $cmd $samp
	echo "launcher_creator.py -t 0:30:00 -j $cmd -n $samp -l $slurm -A NeuroEthoEvoDevo -q normal -m 'module use -a /work/03439/wallen/public/modulefiles; module load gcc/4.9.1; module load hdf5/1.8.15; module load zlib/1.2.8; module load kallisto/0.42.3'" >> 03_kallistoquant.sh
	echo "sbatch $slurm" >> 03_kallistoquant.sh
done
cat 03_kallistoquant.sh		
~~~

### Launch all the jobs

~~~ {.bash}
chmod a+x 03_kallistoquant.sh
bash a+x 03_kallistoquant.sh
~~~ 

## Optional - sample name cleanup

You may need to remove the uninformative bits of the "sample name" so they match up with the actual sample name. 

~~~ {.bash}
for file in *
do
    sample=${file//_S*/}
    echo $file $sample
    mv $file $sample
done
~~~

Then, replace the `_` with `-`

~~~ {.bash}
for file in *
do
    sample=${file//_/-}
    echo $file $sample
    mv $file $sample
done
~~~

## MultiQC

Setup MultiQC on Stampede and run for all files in working directory. Use scp to save the `multiqc_report.html` file to your local computer.

~~~ {.bash}
module load python
export PATH="/work/projects/BioITeam/stampede/bin/multiqc-1.0:$PATH"
export PYTHONPATH="/work/projects/BioITeam/stampede/lib/python2.7/annab-packages:$PYTHONPATH"
multiqc .
~~~

## Now, save the data locally

Copy the kallisto directories and all it's subdirectories in a folder named 02_kallisto quant locally using scp -r. Copy the multiqc_report.html locally and rename it multiqc_report_02.html.


## References
- Kallisto: https://pachterlab.github.io/kallisto/
- MultiQC: http://multiqc.info
- MultiQC tutorial for Stampede: https://wikis.utexas.edu/display/bioiteam/Using+MultiQC
