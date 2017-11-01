# Get, Store, and/or Retrieve Raw Data

Includes the process and commands needed to download the data to scratch on Stampede with `00_gsaf_download`. In theory, this only has to be done once, but it could be done again if data are lost or compromised. 


## 00_gsaf_download 

The first step in the bioinformatics pipeline is to download the data from the sequencing facility, in this case GSAF. The GSAF provides the script for downloading the data from there amazon web server. 

1. Setup project and data directories on Stampede.
2. Copy script to download data: Save a script to download the data (called "00_gsaf_download.sh") in each subdirector  
3. Download the data.
4. Clean up


### Setup project and RNAseq job directories 

Login to TACC using ssh with your password and authentication credentials. Replace "<username>" with your TACC user name. 

~~~ {.bash}
ssh <username>@stampede.tacc.utexas.edu
~~~

On scratch, create the project directory using the job number (e.g. JA16444) and subsubdirectory called 00_rawdata. The argument `-p` will create the parent and subdirectories if they do not already exist.

~~~ {.bash}
mkdir -p $SCRATCH/FMR1/00_rawdata
cd $SCRATCH/FMR1/00_rawdata
~~~

### Copy script to download data 
Copy the text for the gsaf download script found here: https://wikis.utexas.edu/display/GSAF/How+to+download+your+data. Use nano to save it as `00_gsaf_download.sh`. Then, make the file executable.

~~~ {.bash}
chmod a+x 00_gsaf_download.sh
~~~


### Download the raw data (~10 min on the login node)

~~~ {.bash}
00_gsaf_download.sh "<PATH_TO_AMAZON_READS>"
~~~

### Make all fastq.gz files READ ONLY!

To protect our data from accidental deletion or overriding, let's make all the files read only!

~~~ {.bash}
chmod a-w *fastq.gz
ls -l
~~~

### Clean up a little

Create a new directory called `wget_log` and move all the output files associated with downloading the data.

~~~ {.bash}
mkdir wget_log
mv *.wget.log wget_log
ls
~~~


