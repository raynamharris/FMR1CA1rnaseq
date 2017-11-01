# Kallisto: Quantifying RNA Transcript Abundances

To quantify transcripts, we first need to have a reference genome or transciptome to which the reads can be mapped. I'm a big fan of the kallisto program because its super fast and easy to use! Its also becoming more widely used and trusted.

## Navigate to directory raw reads. Create director for kallisto output

~~~ {.bash}
ssh <username>@stampede.tacc.utexas.edu
cd $SCRATCH/FMR1/00_rawreads
mkdir ../02_kallistoquant
~~~


## 02_kallistoquant

Now, we will use the `kallistoquant` function to quantify reads! Again, we use a for loop to create the commands file. The output for each pair of samples will be stored in a subdirectory. The kallsito index from the M7 mouse transcriptome is stored in a different directory. 

~~~ {.bash}
rm 02_kallistoquant.cmds
for R1 in *R1_001.fastq.gz
do
    R2=$(basename $R1 R1_001.fastq.gz)R2_001.fastq.gz
    samp=$(basename $R1 _R1_001.fastq.gz)
    echo $R1 $R2 $samp
    echo "kallisto quant -b 100 -i /work/02189/rmharris/SingleNeuronSeq/data/reference_genomes/gencode.vM7.transcripts.idx  -o ../02_kallistoquant/${samp} $R1 $R2" >> 02_kallistoquant.cmds
done
~~~

### Option 1: Submit a job on Stampede.
Then create the launcher script. Kallisto is not a TACC supported module, so we must use the version of Kallisto that was build by TACC user "wallen" and stored in his public directory. I use the `largemem` node on TACC because kallisto need more memory than the normal nodes support.

~~~ {.bash}
launcher_creator.py -t 1:00:00 -j 02_kallistoquant.cmds -n 02_kallistoquant -l 02_kallistoquant.slurm -A NeuroEthoEvoDevo -q largemem -m 'module use -a /work/03439/wallen/public/modulefiles; module load gcc/4.9.1; module load hdf5/1.8.15; module load zlib/1.2.8; module load kallisto/0.42.3'
sbatch 02_kallistoquant.slurm
~~~

Note: The largemem node has compute limitations. If you have two many samples, the job may need to be split in two. One can use the lane identifiers (like L002 and L003) to subset the data. 

### Option 2: Use an interactive compute node
Request compute time, makde cmd file executable, load modules, run commands. Note: Kallisto is not a TACC supported module, so we must use the version of Kallisto that was build by TACC user "wallen" and stored in his public directory.

~~~ {.bash}
idev -m 120
module use -a /work/03439/wallen/public/modulefiles
module load gcc/4.9.1
module load hdf5/1.8.15
module load zlib/1.2.8
module load kallisto/0.42.3
chmod a+x 04_kallistoquant.cmds
bash 04_kallistoquant.cmds
~~~


## Some quick summary stats
One of the output files contains information about the number of reads that survied trimming and filtering, number of reads mapped, and the average read lenght. We can view that with this command. We can extract that information with grep and awk commands and then save it to a tsv file.

~~~{.bash}echo 'totalreads, pseudoaligned, avelenght' > readsprocessed.csv
grep -A 1 'processed' 04_kallistoquant.e* | awk 'BEGIN {RS = "--"; OFS="\t"}; {print $3, $5, $13}' > readsprocessed.tsv
~~~

## Now, save the data locally

Save the data locally using scp.


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


## References
- Kallisto: https://pachterlab.github.io/kallisto/
