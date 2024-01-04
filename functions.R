#remove outliers from continuous datasets in DESeq2
rm_continuous_outliers<- function(deseq=x, results=y, pct=0.98){
  cooks<- as.data.frame(assays(deseq)[["cooks"]])
  p=ncol(model.matrix(design(deseq), colData(deseq)))
  n=nrow(colData(deseq))
  #n=nrow(deseq)
  cd_cutoff<- qf(pct,p,n-p)
  out<- as.data.frame(t(apply(cooks, 1, function(x){ x > cd_cutoff })))
  out$outlier<- apply(out,1, any)
  outliers<- rownames(out)[out$outlier==TRUE]
  results$padj[rownames(results) %in% outliers]<-NA
  results$pvalue[rownames(results) %in% outliers]<-NA
  return(results)
}


### functions to plot stuff DESEQ2:


gene_plots<- function(resobj, grps, x, set_colours){
  #resobj is the results object sortedby pvalue and includes gene names
  #grps is the names of the variables to extract
  #x = number of genes to show
  # vector of colors
  #resobj<- out_res
  #x=6
  #grps=c("mean_T","Class",varname)
  #set_colours=status_cols
  plots<- list()
  #i=1
  for(i in 1:x){
    dat<- plotCounts(dd, gene=resobj$gene[i], intgroup=grps, returnData=TRUE)
    gene<- resobj$gene[i]
    lfc<- round(resobj$log2FoldChange[i],1)
    p<- ifelse(resobj$pvalue[i]>=0.001,signif(resobj$pvalue[i],2),formatC(resobj$pvalue[i],1, format="e"))
    padj<- ifelse(resobj$padj[i]>=0.001, signif(resobj$padj[i],2),formatC(resobj$padj[i],1, format="e"))
      
      pl<- ggplot(dat, aes_string(x=grps[1], y="count", color=grps[2])) + geom_point(size=2)
      pl<- pl  + labs(title=resobj$display_gene_ID[i],subtitle=paste0("LFC=",lfc,", p=",p,", q=", padj),x="", y="") + peri_geneplots
      pl<- pl + scale_colour_manual(values=set_colours, name="Age x Status")
      
      plots[[i]]<- pl
  }
  
  g<- ggarrange(plotlist=plots, common.legend=TRUE)
  g<- annotate_figure(g, top=textGrob(paste0("Top ",x ," DEGs with ",gsub("_"," ",varname)," in ", tissue),gp=gpar(fontsize=7)), bottom=textGrob(gsub("_"," ",varname),gp=gpar(fontsize=7)), left=textGrob("Normalized Expression", rot = 90, vjust = 1, gp=gpar(fontsize=7)))
  .GlobalEnv$g <- g
}

res_gsea_bp<- function(resobj){
  require(viridis)
  require(ggplot2)
  require(clusterProfiler)
  #resobj<- out_res
  res_go<- resobj
  res_go$logP<- -log(res_go$pvalue, 10)
  res_go$logP<- ifelse(res_go$log2FoldChange<0, -(res_go$logP), res_go$logP)

  res_go<- as.data.frame(res_go)
  #res_go<- merge(res_go, go_key, by="gene")
  res_go<- res_go[order(res_go$logP, decreasing=TRUE),]
  res_go<- res_go[!is.na(res_go$logP),]
  res_go$pvalue<- ifelse(is.na(res_go$padj), NA, as.numeric(res_go$pvalue))
  
  geneList<- res_go$logP
  names(geneList)<- res_go$gene
  
  
  y<- GSEA(geneList, TERM2GENE = go2gene_bp, TERM2NAME = go2name_bp)
  y@result$Description<- ifelse(str_count(y@result$Description, '\\w+') >= 10,
                                gsub("(^[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+)\\s","\\1\n",y@result$Description, perl=TRUE),
                                ifelse(str_count(y@result$Description, '\\w+') >= 6,
                                gsub("(^[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+)\\s","\\1\n",y@result$Description, perl=TRUE),
                                as.character(y@result$Description)))
            
  #test<- head(y@result$Description)
  
  gseaplot<- ridgeplot(y, fill="pvalue", showCategory=20) + peri_figure_supp + labs(title=paste0("Top 20 GO BP terms from GSEA\n in ",tissue," according to ",varname), x="direction scaled log10(p-value)") + scale_fill_viridis(option="G") + theme(legend.key.size=unit(3,"mm"))
  .GlobalEnv$gseaplot <- gseaplot
  
  gsearesult<- y@result
  
  .GlobalEnv$gsearesult<- gsearesult
  
}

res_go_bp<- function(resobj){
  #resobj<- out_res
  res_go<- resobj
  if(length(res_go$gene[which(res_go$padj<0.1)])>40){
    #For genes with FDR q<0.1
    go_results<- enricher(res_go$gene[which(res_go$padj<0.1)], universe=res_go$gene, TERM2GENE = go2gene_bp, TERM2NAME = go2name_bp)
    
    ego2<- go_results@result
    ego2$n_annotated<- word(ego2$GeneRatio, 2,2, sep="/")
    ego2$n_annotated<- as.numeric(ego2$n_annotated)
    test<- ego2[1:10,]
    #if the top 10 genes have a category with <3 genes, then do another test with the p<0.05 set.
    if(any(test$Count <= 3, na.rm=TRUE)){
      #for genes with <40 sig FDR, take genes with raw p<0.05
      
      go_results<- enricher(res_go$gene[which(res_go$pvalue<0.05)], universe=res_go$gene, TERM2GENE = go2gene_bp, TERM2NAME = go2name_bp)
      
      ego2<- go_results@result
      ego2$n_annotated<- word(ego2$GeneRatio, 2,2, sep="/")
      ego2$n_annotated<- as.numeric(ego2$n_annotated)
      ego2$Description<- ifelse(str_count(ego2$Description, '\\w+') >= 10,
                                gsub("(^[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+)\\s","\\1\n",ego2$Description, perl=TRUE),
                                ifelse(str_count(ego2$Description, '\\w+') >= 6,
                                       gsub("(^[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+)\\s","\\1\n",ego2$Description, perl=TRUE),
                                       as.character(ego2$Description)))
      
      go_plot_result<- ggplot(ego2[1:10,], aes(x=Description, y=Count, fill=pvalue)) + geom_bar(stat="identity", colour="black", lwd=0.25) + theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) + coord_flip() + peri_figure_supp + scale_y_continuous(expand=c(0,0)) + labs(x="", y="No. genes enriched", title=paste0("Top 10 GO BP terms from foreground/background\n in ",tissue," according to ",varname), subtitle=paste0("foreground is",ego2$n_annotated[1]," GO annotated genes of ",length(res_go$gene[which(res_go$pvalue<0.05)])," genes with p<0.05")) + scale_fill_viridis(option="G") + theme(legend.key.size=unit(3,"mm"))
      .GlobalEnv$go_plot_result <- go_plot_result
      .GlobalEnv$enrich_results<- ego2
      .GlobalEnv$main.title<- paste0("Top 10 GO BP terms from foreground/background in ",tissue," according to ",varname, ". \nForeground is ",ego2$n_annotated[1]," GO annotated genes of ",length(res_go$gene[which(res_go$pvalue<0.05)])," genes with p<0.05")
      
    }else{
      ego2$Description<- ifelse(str_count(ego2$Description, '\\w+') >= 10,
                                gsub("(^[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+)\\s","\\1\n",ego2$Description, perl=TRUE),
                                ifelse(str_count(ego2$Description, '\\w+') >= 6,
                                       gsub("(^[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+)\\s","\\1\n",ego2$Description, perl=TRUE),
                                       as.character(ego2$Description)))
      go_plot_result<- ggplot(ego2[1:10,], aes(x=Description, y=Count, fill=pvalue)) + geom_bar(stat="identity", colour="black", lwd=0.25) + theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) + coord_flip() + peri_figure_supp + scale_y_continuous(expand=c(0,0)) + labs(x="", y="No. genes enriched", title=paste0("Top 10 GO BP terms from foreground/background\n in ",tissue," according to ",varname), subtitle=paste0("foreground is ",ego2$n_annotated[1]," GO annotated genes of ",length(res_go$gene[which(res_go$padj<0.1)])," genes with q<0.1")) + scale_fill_viridis(option="G") + theme(legend.key.size=unit(3,"mm"))
    .GlobalEnv$go_plot_result <- go_plot_result
    .GlobalEnv$enrich_results<- ego2
    .GlobalEnv$main.title<- paste0("Top 10 GO BP terms from foreground/background in ",tissue," according to ",varname, ". \nForeground is ",ego2$n_annotated[1]," GO annotated genes of ",length(res_go$gene[which(res_go$padj<0.1)])," genes with q<0.1")
    
    }
    
  }else if(length(res_go$gene[which(res_go$padj<0.1)])<40){
    
    #for genes with <40 sig FDR, take genes with raw p<0.05
    
    go_results<- enricher(res_go$gene[which(res_go$pvalue<0.05)], universe=res_go$gene, TERM2GENE = go2gene_bp, TERM2NAME = go2name_bp)
    
    ego2<- go_results@result
    ego2$n_annotated<- word(ego2$GeneRatio, 2,2, sep="/")
    ego2$n_annotated<- as.numeric(ego2$n_annotated)
    ego2$Description<- ifelse(str_count(ego2$Description, '\\w+') >= 10,
                              gsub("(^[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+)\\s","\\1\n",ego2$Description, perl=TRUE),
                              ifelse(str_count(ego2$Description, '\\w+') >= 6,
                                     gsub("(^[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+)\\s","\\1\n",ego2$Description, perl=TRUE),
                                     as.character(ego2$Description)))
    go_plot_result<- ggplot(ego2[1:20,], aes(x=Description, y=Count, fill=pvalue)) + geom_bar(stat="identity", colour="black", lwd=0.25) + theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) + coord_flip() + peri_figure_supp + scale_y_continuous(expand=c(0,0)) + labs(x="", y="No. genes enriched", title=paste0("Top 10 GO BP terms from foreground/background\n in ",tissue," according to ",varname), subtitle=paste0("foreground is",ego2$n_annotated[1]," GO annotated genes of ",length(res_go$gene[which(res_go$pvalue<0.05)])," genes with p<0.05")) + scale_fill_viridis(option="G") + theme(legend.key.size=unit(3,"mm"))
    .GlobalEnv$go_plot_result <- go_plot_result
    .GlobalEnv$enrich_results<- ego2
    .GlobalEnv$main.title<- paste0("Top 10 GO BP terms from foreground/background in ",tissue," according to ",varname, ". \nForeground is ",ego2$n_annotated[1]," GO annotated genes of ",length(res_go$gene[which(res_go$pvalue<0.05)])," genes with p<0.05")
  }
}

volcano_plots<- function(resobj, set_colours){
  #resobj<- out_res
  #set_colours=tissue_cols[["GON"]]
  resobj<- resobj[!is.na(resobj$pvalue) & !is.na(resobj$padj),]
  resobj<- droplevels(resobj)
  topgenes<- resobj$gene[which(!grepl("LOC[0-9]+",resobj$gene))][1:5]
  toplfc<- resobj[which(resobj$pvalue<0.05 & !grepl("LOC[0-9]+",resobj$gene)),]
  toplfc<- toplfc$gene[order(toplfc$log2FoldChange)][1:5]
  resobj$interesting_genes<- ifelse((resobj$gene %in% candidates2 & resobj$pvalue<0.05) | resobj$gene %in% topgenes | resobj$gene %in% toplfc, as.character(resobj$gene), "")
  resobj$sig<- ifelse(resobj$padj<0.1,"q<0.1",ifelse(resobj$pvalue<0.05,"p<0.05","NS"))
  resobj$sig<- factor(resobj$sig, levels=c("q<0.1","p<0.05","NS"))
  
  vp<- ggplot(resobj, aes(x=log2FoldChange, y=-log10(pvalue), color=sig)) + geom_point(size=2)
  vp<- vp + geom_text_repel(aes(label=interesting_genes), color="grey20",size=geom.text.size, seed=42, force=3, point.padding = 0.25) 
  vp<- vp + peri_figure_supp + labs(title=paste0(tissue," exp ", resobj$des[1], " n(sample)=",nrow(colData(dd))),subtitle=paste0(nrow(resobj),"/",nrow(resobj[which(resobj$pvalue<0.05),]),"/",nrow(resobj[which(resobj$padj<0.1),])),y="-log10(pvalue)", x="Log2FoldChange") 
  vp<- vp + geom_point(data=resobj[resobj$interesting_genes!="",], aes(fill=sig), pch=21,color="grey40", show.legend=FALSE, size=2.5, stroke=0.25) 
  vp<- vp + scale_colour_manual(values=set_colours, name="") + scale_fill_manual(values=set_colours[1:2])
  #return(vp)
  .GlobalEnv$vp<- vp
}

GO_WGCNA<- function(resobj,module,go2name,go2gene){
  require(stringr)
  require(ggplot2)
  require(viridis)
  #resobj<- out_res
      #for genes with <40 sig FDR, take genes with raw p<0.05
    
    module_genes<- resobj$gene[resobj$moduleColor==module]
  
  
    go_results<- enricher(module_genes, universe=resobj$gene, TERM2GENE = go2gene, TERM2NAME = go2name)
    
    ego2<- go_results@result
    ego2$n_annotated<- word(ego2$GeneRatio, 2,2, sep="/")
    ego2$n_annotated<- as.numeric(ego2$n_annotated)
    ego2$Description<- ifelse(str_count(ego2$Description, '\\w+') >= 10,
                              gsub("(^[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+)\\s","\\1\n",ego2$Description, perl=TRUE),
                              ifelse(str_count(ego2$Description, '\\w+') >= 6,
                                     gsub("(^[^\\s]+\\s[^\\s]+\\s[^\\s]+\\s[^\\s]+)\\s","\\1\n",ego2$Description, perl=TRUE),
                                     as.character(ego2$Description)))
    go_plot_result<- ggplot(ego2[1:20,], aes(x=Description, y=Count, fill=pvalue)) + geom_bar(stat="identity", colour="black", lwd=0.25) + theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) + coord_flip() + peri_figure_supp + scale_y_continuous(expand=c(0,0)) + labs(x="", y="No. genes enriched", title=paste0("Top 10 GO BP terms from foreground/background\n in ",module," module"), subtitle=paste0("foreground is",ego2$n_annotated[1]," GO annotated genes of ",length(module_genes)," in module")) + scale_fill_viridis(option="G") + theme(legend.key.size=unit(3,"mm"))
    .GlobalEnv$go_plot_result <- go_plot_result
    .GlobalEnv$enrich_results<- ego2
    .GlobalEnv$main.title<- paste0("Top 10 GO BP terms from foreground/background in ",module," module.", ". \nForeground is ",ego2$n_annotated[1]," GO annotated genes of ",length(module_genes)," genes in module")
  
}

