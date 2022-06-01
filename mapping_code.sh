#!/bin/sh

#### Create a Genome Index ####
STAR --runThreadN 50 \
--runMode genomeGenerate \
--genomeDir /home/boltonp17/manakins/pfil/index/ \
--genomeFastaFiles /home/boltonp17/manakins/pfil/GCF_003945595.1_ASM394559v1_genomic.fna \
--sjdbGTFfile /home/boltonp17/manakins/pfil/GCF_003945595.1_ASM394559v1_genomic.gtf \
--sjdbOverhang 99 ;


#### Map the files using STAR ####
## these more stringent parameters were devised by as part of a project by Rasband et al on expression of growth hormone paralogs.

INDIR=/home/boltonp17/manakins/rawdata/fastqs/
cd $INDIR 
READS=*.fastq 

for j in $READS
        do
        sample=$(echo $j | sed 's/.fastq//')
        STAR --genomeDir /home/boltonp17/manakins/pfil/index/ \
				--sjdbGTFfile /home/boltonp17/manakins/pfil/GCF_003945595.1_ASM394559v1_genomic.gtf \
				--sjdbOverhang 99 \
                --runThreadN 16 \
                --readFilesIn $j \
                --outFileNamePrefix /home/boltonp17/manakins/alignments_2pass/$sample.stringent \
                --outSAMtype BAM SortedByCoordinate \
                --outSAMattributes Standard \
                --quantMode TranscriptomeSAM GeneCounts \
				--twopassMode Basic \
				--quantMode TranscriptomeSAM GeneCounts \
				--outFilterMismatchNoverReadLmax 0.04 \
				--outFilterMultimapNmax 5 \
				--outFilterType BySJout
		done ;





--sjdbOverhang 99 \
--runThreadN 16 \
--outSAMtype BAM SortedByCoordinate \
--outSAMattributes Standard \
--quantMode TranscriptomeSAM GeneCounts \
--twopassMode Basic \
--quantMode TranscriptomeSAM GeneCounts \
--outFilterMismatchNoverReadLmax 0.04 \
--outFilterMultimapNmax 5 \
--outFilterType BySJout

#### Generate Counts ####
# Then, I used featureCounts to count the transcripts on the antisense strand.

featureCounts -T 16 -s 2 -t exon -g gene_id -a /home/boltonp17/manakins/pfil/GCF_003945595.1_ASM394559v1_genomic.gtf  -o stringent.exonCounts.txt *stringentAligned.sortedByCoord.out.bam

