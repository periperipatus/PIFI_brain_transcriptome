#### Making GO annotations ####

## Maggie Ko blasted Pipra genes against other birds, and other genomes such as human to generate a list of possible homologs.
## I had done something similar for all LOC names, and James Pease also blasted the Pipra genome to the human using tBlastX
## Maggie then annotated the gene names with the most frequent annotation in all these experiments OR the next most frequent if the most frequent was still a LOC.
## For more details see Baldwin et al (In Prep). 


#this file contains the final list of Pipra gene names against the final list of annotations generated by Maggie Ko
#this version came direct from the 'Combine_3rounds...' .csv file with some columns removed.
final<-readLines("Combine_3rounds_anno_James_Peri_final_hsap_manakins_v3.txt")
final<- data.frame(do.call('rbind', strsplit(as.character(final),'\t',fixed=TRUE)))
colnames(final)<- final[1,]
final<- final[-1,]
#final<- read.table("Combine_3rounds_anno_James_Peri_final_hsap_manakins_v3.txt", header = T, sep="\t")
final<- subset(final, select=c("best_anno","most_freq_anno","anno_score","was_loc","GeneID"))
#change the SEPT7 type genes to SEPTIN7

final$best_anno<- sub("(^SEPT)([0-9]{1,2}$)","\\1IN\\2", final$best_anno)
#SEPT genes need to be renamed to SEPTIN to conform with HGNC
#remove gene ids that were duplicated. This is because transcripts were used so there are multiple isoforms included.
key<- final[!duplicated(final$GeneID),] #18065 


#write.csv(key, file="Maggies_annotations_modifiedR.csv", row.names=FALSE) #this one will include only single entry per gene.


#### --- Gene Ontology.org ----####
#http://geneontology.org/docs/go-annotation-file-gaf-format-2.2/

key<- read.csv("Maggies_annotations_modifiedR.csv")
# make it so duplicated names are indicated with a unique suffix
keynames<- unique(key$best_anno[duplicated(key$best_anno)])
singletons<- key[!key$best_anno %in% keynames,]
out<- list()
#i<- "ACR"
#i<- "A2ML1"
for(i in keynames){
  sub<- key[key$best_anno==i,]
  presum_ortholog<- sub$best_anno==sub$GeneID
  if(is.element(TRUE, presum_ortholog)){
    sub_ortho<- sub[presum_ortholog,]
    sub_ortho$dummy_name<- sub_ortho$GeneID
    sub_para<- sub[!presum_ortholog,]
    len<- 1:nrow(sub_para)
    sub_para$dummy_name<- paste0(sub_para$best_anno,"_pp_",len) ## pp is presumed paralog.
    sub2<- rbind(sub_para, sub_ortho)
    out[[i]]<- sub2
  }else{
    len<- 1:nrow(sub)
    sub$dummy_name<- paste0(sub$best_anno,"_ah_",len) ## ah is assumed/unknown homology
    out[[i]]<- sub
  }
}
paralogs<- do.call("rbind",out)
singletons$dummy_name<- singletons$best_anno

key<- rbind(singletons, paralogs)
key<- plyr::rename(key, replace=c("best_anno"="DB_Object_Symbol"))


human<-readLines("goa_human_noheader.gaf")
human_gaf<- data.frame(do.call('rbind', strsplit(as.character(human),'\t',fixed=TRUE)))
colnames(human_gaf)<- c("DB","DB_Object_ID","DB_Object_Symbol", "Qualifier","GO_ID","DB_Reference","Evidence_Code","With_or_From","Aspect","DB_Object_Name", "DB_Object_Synonym","DB_Object_Type","Taxon","Date","Assigned_By","Annotation_Extension")
##DB_Object_Symbol==Gene Symbol
## Aspect is GO Category: F (function), P (process), C (component)
## GO_ID is the GO_ID

shared_GO<- merge(x=human_gaf, y=key, by="DB_Object_Symbol")

no_GO<- key[!key$DB_Object_Symbol %in% shared_GO$DB_Object_Symbol,]
#how many of the original set merged?
length(unique(key$DB_Object_Symbol))
length(unique(shared_GO$DB_Object_Symbol))
16446-13182 # annotations were not in the GO database. 
#of those that didn't merge, how many were LOC names?

nrow(key[grep("^LOC[0-9]+$", key$DB_Object_Symbol),])
#nrow(shared_GO[grepl("CYP2C23A", shared_GO$DB_Object_Symbol),])


all_GO<- merge(x=human_gaf, y=key, by="DB_Object_Symbol", all.y=TRUE)

all_GO$GO_ID[is.na(all_GO$GO_ID)]<- "unknown"

#keep the relevant bits
all_GO<- subset(all_GO, select=c("DB_Object_Symbol", "GO_ID", "Aspect", "Evidence_Code", "dummy_name","GeneID"))
all_GO<- plyr::rename(all_GO, replace=c("DB_Object_Symbol"="best_annot"))
write.csv(all_GO, "pfil_GO_key_raw.csv", row.names=FALSE)
all_GO<- read.csv("pfil_GO_key_raw.csv")

#Convert Biological Process annotations to GO_MWU format (very similar to what topGO wants)
all_GO$Aspect[all_GO$GO_ID=="unknown"]<- "P"
human_bp<- all_GO[which(all_GO$Aspect=="P"),]
#set up the human_go database in a format that GO_MWU can read. 
genes<- unique(human_bp$dummy_name)
go_out<- list()
for(g in genes){
  sub<- human_bp[human_bp$dummy_name==g,]
  #combine GO terms. 
  sub<- sub[!duplicated(sub$GO_ID),]
  go<- sub$GO_ID
  go<-paste(go, collapse=";")
  go_out[[g]]<- data.frame(pfil_name=sub$GeneID[1],best_annot=sub$best_annot[1], dummy_name=g, GO=go)
}
go_out_bp<- do.call("rbind", go_out)

write.csv(go_out, "pfil_GO_key_BP.csv", row.names=FALSE)
go_out<- plyr::rename(go_out, replace=c("pfil_name"="gene"))
write.csv(go_out[,c("gene","GO")], "pfil_GO_V2_terms_human_BP_MWU-format.tab", row.names=FALSE)



#set up the human_go database in a format that GO_MWU can read. 
genes<- unique(all_GO$dummy_name)
go_out<- list()
for(g in genes){
  sub<- all_GO[all_GO$dummy_name==g,]
  #combine GO terms. 
  sub<- sub[!duplicated(sub$GO_ID),]
  go<- sub$GO_ID
  go<-paste(go, collapse=";")
  go_out[[g]]<- data.frame(pfil_name=sub$GeneID[1],best_annot=sub$best_annot[1], dummy_name=g, GO=go)
}
go_out_all<- do.call("rbind", go_out)
write.csv(go_out_all,"pfil_GO_key_all_short_format.csv", row.names=FALSE)



