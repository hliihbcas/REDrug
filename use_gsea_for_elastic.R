#!/home/hli/anaconda3/envs/rp48/bin/Rscript
library(clusterProfiler)
library(org.Hs.eg.db)  # 人类基因注释
library(org.Mm.eg.db)
library(enrichplot)     # 可视化
library(dplyr)
library(ggplot2)
library(enrichplot)
library(gridExtra)
args = commandArgs(T)
print(args[1])
pathway <- read.gmt(args[2])

OrgDbd = args[3]

data = read.delim(args[1],stringsAsFactors=F)

my_gseaplot2 <- function(x, geneSetID, title = "", color = "green", base_size = 11, 
                         rel_heights = c(1.5, 0.5, 1), subplots = 1:3, 
                         pvalue_table = FALSE, ES_geom = "line") {
  
  # 1. 调用原函数生成基础图（不带原表格）
  p <- enrichplot::gseaplot2(x, geneSetID, title, color, base_size, 
                             rel_heights, subplots, pvalue_table = FALSE, ES_geom)
  
  # 2. 自定义大字号表格
  if (pvalue_table) {
    # ==== 修改点：提取数据时，只提取 Description 和 p.adjust ====
    pd <- x[geneSetID, c("Description", "p.adjust"), drop = FALSE]
    
    # 自动换行处理过长文本
    pd$Description <- stringr::str_wrap(pd$Description, width = 35)
    rownames(pd) <- pd$Description
    
    # 删掉第一列文字（因为已经变成行名了），只保留 p.adjust 这个数值列
    pd <- pd[, -1, drop = FALSE] 
    pd <- round(pd, 4)
    
    # 获取折线对应的颜色
    if (length(geneSetID) == 1) {
      line_colors <- color[1]
    } else {
      cols <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
      line_colors <- cols[1:length(geneSetID)]
      if (length(color) == length(geneSetID)) {
        line_colors <- color
      }
    }
    
    # 自定义表格主题
    custom_theme <- ttheme_default(
      core = list(fg_params = list(fontsize = 14, fontface = "bold")),    
      colhead = list(fg_params = list(fontsize = 15, fontface = "bold")), 
      rowhead = list(fg_params = list(col = line_colors, fontsize = 14, fontface = "bold")) 
    )
    
    tp <- tableGrob(pd, theme = custom_theme)
	tp$widths <- unit(c(11.5, 4), "cm")     
    # ==== 彻底消除自带的背景图例 ====
    p[[1]] <- p[[1]] + theme(legend.position = "none")
    
    # ==== 把新表格放到右上角 ====
    p[[1]] <- p[[1]] + annotation_custom(
      tp, 
      xmin = quantile(p[[1]]$data$x, .45), 
      xmax = quantile(p[[1]]$data$x, .9), 
      ymin = quantile(p[[1]]$data$runningScore, .7), 
      ymax = quantile(p[[1]]$data$runningScore, .85)
    )
  }
  return(p)
}



geneList <- data$log2FoldChange+rnorm(length(data$log2FoldChange), sd = 1e-10)
#print(str(data[[1]]))
#print(str(geneList))
names(geneList) <-data[[1]]

gene_map <- bitr(names(geneList),
                 fromType = "SYMBOL",
                 toType = "ENTREZID",
                 OrgDb = OrgDbd)
gene_map <- gene_map[!duplicated(gene_map$SYMBOL), ]
geneList_entrez <- geneList[gene_map$SYMBOL]
names(geneList_entrez) <- gene_map$ENTREZID
geneList_entrez <- na.omit(geneList_entrez) %>% sort(decreasing = TRUE)
#geneList_entrez <- na.omit(geneList_entrez)

#gsea_result <- GSEA(geneList =geneList_entrez, TERM2GENE = pathway,minGSSize=1,pvalueCutoff = 1,pAdjustMethod = "BH",eps = 0,nPermSimple = 10000)
gsea_result <- GSEA(geneList =geneList_entrez, TERM2GENE = pathway,minGSSize=1,pvalueCutoff = 1,pAdjustMethod = "BH",eps = 0,nPermSimple = 100000)

#print(head(gsea_result))
print(str(gsea_result))

pre_title = args[1]
result_fs = paste0(args[1],'_result.txt')

			real_len = length(gsea_result@result$ID)

			if (real_len==0){
				next
			}else{
    			#p = gseaplot2(gsea_result, geneSetID=1:real_len,pvalue_table = TRUE)+theme(legend.position = "inside",legend.position.inside = c(1, 0.5), legend.justification = c(1, 1),legend.box.margin = margin(t = 20, r = 0, b = 0, l = 0, unit = "pt"))
    			p = gseaplot2(gsea_result, geneSetID=1:real_len,pvalue_table = TRUE)
    			ggsave(paste0(pre_title,'all_REDrug.png'),p,width=15,height=24)
				write.table(gsea_result@result,result_fs,sep='\t',row.names=F,col.names=T,quote=FALSE)
				print(gsea_result@result)
				#p = my_gseaplot2(gsea_result,c("Lymphoid and a non-Lymphoid cell","PD-1 signaling","Class A/1 (Rhodopsin-like receptors)"),pvalue_table = TRUE)
				#p = my_gseaplot2(gsea_result,c("C1_Activate","C2_Activate","C3_Activate","C4_Activate","C5_Activate","C6_Activate","C7_Activate"),pvalue_table = TRUE)
				p = my_gseaplot2(gsea_result,c('Cytokine Signaling in Immune system','Neutrophil degranulation','DAP12 interactions','TCR signaling','Hemostasis',"Lymphoid and a non-Lymphoid cell","Class A/1 (Rhodopsin-like receptors)"),pvalue_table = TRUE)
				ggsave(paste0(pre_title,'selected_REDrug.png'),p,width=10,height=15)

			}

