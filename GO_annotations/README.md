This document describes how Peri Bolton took Maggie Ko's list of gene-names and assigned GO IDs.
Briefly, Maggie did multiple BLASTs of human, avian and reptilian databases to assign gene symbols to the Pipra IDs.
Peri took these Pipra Gene Symbols and in GO_annotations.R associated the best_annotations - the consensus gene symbols from multiple taxa - to human GO terms.
Of approximately 18065 Pipra Gene Symbols, Peri was able to assign GO terms to 14752. Of these 18k terms 5406 are LOC IDs. 

The human GO GAF file was generated 2021-02-01
Pipra genome: RefSeq GCA_003945595.1 ASM394559v1


The data packet here includes the following files:
File	Contents
GO_annotations.R	R code used to append human GO terms
pfil_GO_key_raw.csv	raw results, 1 line per GO per gene.
pfil_GO_key_BP.csv	Biological Process IDs. includes multiple gene ID combinations, 1 row per Pipra gene
pfil_GO_V2_terms_human_BP_MWU-format.tab PipraIDs in GO_MWU format for Biological Process

#### pfil_GO_key_raw.csv
Master file for changing formats and filtering according to evidence codes and different GO categories.
###
column	description
best_annot	The gene_symbol used to assign GO_terms.
GO_ID	GO accession code from GeneOntology.org goa_human.gaf
Aspect	Categorical variable for GO category: P=Biological Process, C=Cellular Component, F=Molecular Function
Evidence_Code	GO evidence code (see http://geneontology.org/docs/guide-go-evidence-codes/)
dummy_name	Unique identifier for best annotation when gene symbols are repeated, indicating pp=presumed paralog, and ah=assumed homology (no gene symbol in Pipra that matches the best_annot).
GeneID	Original Pipra gene_symbol



#### pfil_GO_key_BP.csv ####
One-line per Pipra Gene with Biological Process IDs ; delimited, with different possible gene symbols.
###
column	description 
gene	Pipra Gene ID
best_annot The gene_symbol used to assign GO_terms.
dummy_name	Unique identifier for best annotation when gene symbols are repeated, indicating pp=presumed paralog, and ah=assumed homology (no gene symbol in Pipra that matches the best_annot).
GO	Biological Processes GO Accessions semi-colon ; delimited.


#### pfil_GO_V2_terms_human_BP_MWU-format.tab ####
GO MWU format, tab delimited. One-line per Pipra Gene with Biological Process IDs ; delimited. 
Similar to the topGO input format.
###
column	description 
gene	Pipra Gene ID
GO	Biological Processes GO Accessions semi-colon ; delimited.





