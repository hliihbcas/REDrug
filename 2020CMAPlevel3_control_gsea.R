#!/home/hli/anaconda3/envs/rp48/bin/Rscript
library(clusterProfiler)
library(org.Hs.eg.db)  # 人类基因注释
library(org.Mm.eg.db)
library(enrichplot)     # 可视化
library(dplyr)
library(ggplot2)
library(stringr)
args = commandArgs(T)

dataf=args[1]
samplef=args[2]
gmtf=args[3]
density_f=args[4]
times = args[5]
outfold=args[6]

dataf_control = args[7]
#samplef_control = args[8]



times=as.numeric(times)

pathway <- read.gmt(gmtf)

data_l5 = read.delim(dataf,stringsAsFactors=F)
print(str(data_l5))

data_l5_control = read.delim(dataf_control,stringsAsFactors=F,row.names=1)
print(str(data_l5_control))

sample_l5=read.delim(samplef,stringsAsFactors=F,head=F)

data_l5_genenames = rownames(data_l5)
print(str(data_l5_genenames))
print(str(na.omit(data_l5_genenames)))

data_l5_genenames_control = rownames(data_l5_control)
if (mean(data_l5_genenames == data_l5_genenames_control)==1){
	print('yes')
}else{
	 stop("gene not match between control and treatment")	
}



pprs = unique(subset(sample_l5,V22=="trt_cp",V20,drop=T))

density_d = read.delim(density_f,stringsAsFactors=F,head=F)
density_dc = density_d[[1]]
print(str(density_dc))
for (si in pprs){
	for (i in density_dc){
		#i=as.numeric(i)
		
		sample_ids = subset(sample_l5,V20==si & V5 ==i & V6==times,V21,drop=T)
		if (length(sample_ids)<3){
			fflage = paste0(si,"_",i,": next")
			print(fflage)
			next
		}else{
			fflage = paste0(si,"_",i,": pass")
			print(fflage)

    		lines = paste0("mkdir ",outfold,'/',si)
    		result_f = system(lines,intern = TRUE)

			dir_i = i
			dir_i = strsplit(dir_i,' ')[[1]][1]
    		lines = paste0("mkdir ",outfold,'/',si,"/",dir_i)
    		result_f = system(lines,intern = TRUE)
			print('aa')

    		lines = paste0("mkdir ",outfold,'/',si,"/",dir_i,'/',times)
    		result_f = system(lines,intern = TRUE)
    		pre_title = paste0(outfold,'/',si,"/",dir_i,'/',times,"/")

    		sample_idss = str_replace_all(sample_ids, ":", ".")
    		print(str(sample_idss))
    		data_l5_temp = data_l5[,sample_idss]
    		row_means_vector <- rowMeans(data_l5_temp)
    		print(str(row_means_vector))

    		controls_ids = subset(sample_l5,V6==times & V22=="ctl_vehicle",V21,drop=T)
			
    		controls_idss = str_replace_all(controls_ids, ":", ".")
    		print(str(controls_idss))
    		data_l5_temp_control = data_l5_control[,controls_idss]
    		row_means_vector_control = rowMeans(data_l5_temp_control)
    		print(str(row_means_vector_control))



    		aa = row_means_vector > 0
    		bb = row_means_vector_control > 0
    		cc = aa & bb
    		row_means_vector = row_means_vector[cc]
    		row_means_vector_control = row_means_vector_control[cc]
    		data_l5_genenames_temp = data_l5_genenames[cc]
			print('bb')
			print(str(data_l5_genenames_temp))
    		data_fc = log(row_means_vector/row_means_vector_control,2)

    		geneList=data_fc+rnorm(length(data_fc), sd = 1e-12)
    		geneList=data_fc
    		print(str(geneList))
    		names(geneList)=data_l5_genenames_temp
    		print(head(geneList))
    		geneList_entrez <- na.omit(geneList) %>% sort(decreasing = TRUE)
    		print(head(geneList_entrez))
    		gsea_result <- GSEA(geneList =geneList_entrez, TERM2GENE = pathway,minGSSize=1,maxGSSize=1000,pvalueCutoff = 1,pAdjustMethod = "BH",eps = 0,verbose = TRUE)
    		print(str(gsea_result))

    		result_fs = paste0(pre_title,'gsea_result.txt')
			result_fs_rds = paste0(pre_title,'gsea_result.rds')
			saveRDS(gsea_result,file=result_fs_rds)

			if ("Cell surface interactions" %in% gsea_result@result$ID){
    			p = gseaplot2(gsea_result, "Cell surface interactions",pvalue_table = TRUE)
    			ggsave(paste0(pre_title,'2.png'),p,width=10)
			}
			if ("Antigen Processing and Presentation" %in% gsea_result@result$ID){
    			p = gseaplot2(gsea_result, "Antigen Processing and Presentation",pvalue_table = TRUE)
    			ggsave(paste0(pre_title,'1.png'),p,width=10)
			}
			real_len = length(gsea_result@result$ID)

			if (real_len==0){
				next
			}else{
    			p = gseaplot2(gsea_result, geneSetID=1:real_len,pvalue_table = TRUE)
    			ggsave(paste0(pre_title,'all.png'),p,width=17,height=20)
				write.table(gsea_result@result,result_fs,sep='\t',row.names=F,col.names=T,quote=FALSE)
			}

		}
	}


}

