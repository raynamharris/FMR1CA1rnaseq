NCBI

For NCBI, I need to send them a zipped folder of all my fastqc and mdsums files. 

For workflow purposes, I keep launcher scripts and output files in the directory where they are run and created. I know its againsts best practices, but I've tried other organizations and failed. 

So, first I'll move these extranous files to a temporary folder. Then, I'll zip the raw files. Then I move the extracous files back into the un-zipped director of raw data.

~~~{.bash
cds
cd FMR1
mv 00_rawdata/*.slurm temp/
mv 00_rawdata./*.e8* temp/
mv 00_rawdata/*.o8* temp/
mv 00_rawdata/*.cmds temp/
mv 00_rawdata/*.sh temp/
mv 00_rawdata/multiqc* temp/
~~~

Then zip

~~~{.bash
tar -cvpf 00_rawdata.zip 00_rawdata
~~~

~~~{.bash
mv temp/* 00_rawdata
rm -r temp
~~~