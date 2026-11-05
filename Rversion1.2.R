#!/home/hli/anaconda3/envs/magicRV2/bin/Rscript
library(ggplot2)
library(rjson)
#./Rversion1.R ordination 11 ./100100100/*.data 0 ./100100100/feature 2> ./100100100/err_11.txt 
#./Rversion1.R 0 01 0 ./100100100/user.conf 0 2> ./100100100/err_01.txt 

main <- function(){
    argstr<- commandArgs(T)
    type <- argstr[1]#top type
	
	subtype <- argstr[2]#sub type
	
    datafile <- argstr[3]
    plot.conf <- argstr[4]
	feature.conf <- argstr[5]
	print('aa')
	#feature <- fromJSON(file=feature.conf)
    if (subtype == "11"){
		if(type=="Vientiane"){
			tryCatch({
			feature <- fromJSON(file=feature.conf)
			flage_env <- check_env(datafile)
			if(!flage_env){
				p <- ggplot(mtcars,aes(gear,carb)) + theme_void() + annotate("text",x=range(mtcars$gear)[1],y=range(mtcars$carb)[1],label="Detected abnormal input data and parameter reading,\n please refer to the standard input file for proofreading.\nPlease browse the Err report panel at the bottom of the page, \nwhich contains detailed error information.",size=6)
				plot_continue(p,feature)
				return("1")
			}
			temp_str = load(datafile)
			p <- get(temp_str)
			},error=function(e){
				p <- ggplot(mtcars,aes(gear,carb)) + theme_void() + annotate("text",x=range(mtcars$gear)[1],y=range(mtcars$carb)[1],label="Detected abnormal input data and parameter reading,\n please refer to the standard input file for proofreading.\nPlease browse the Err report panel at the bottom of the page, \nwhich contains detailed error information.",size=6)
				plot_continue(p,feature)
				return("1")
			})
			vuomics_flage = FALSE
			if(length(attr(p,'class'))==3 || length(attr(p,'class'))==2){
				if (length(attr(p,'class'))==3){
					class_str = unlist(strsplit(attr(p,'class')[3],","))
					if(length(class_str)==4 && class_str[1] == 'visomics'){
						vuomics_flage = TRUE
					}
				}else{
					class_str = unlist(strsplit(attr(p,'class')[2],","))
					if(length(class_str)==4 && class_str[1] == 'visomics'){
						vuomics_flage = TRUE
					}
				}
			}

			if(vuomics_flage){
				
				
				if(length(class_str)==4 && class_str[1] == 'visomics'){
					if (class_str[4]!="0"){
						if(class_str[4] == "bar_en_group"){
							feature$mutiformat="bar"
						}else{
						feature$mutiformat = class_str[4]
						}
					}
					out_txt = paste(class_str[2:4],collapse="\t")
					filename_out = paste(feature$out,"/config",sep="")
					write(out_txt,file=filename_out)
					plot_continue(p,feature)
				}
			}else{
				plot_continue(p,feature)
			}

		}
		if (type == "ordinations"){
			tryCatch({
			feature <- fromJSON(file=feature.conf)

			data <- read.delim(datafile,stringsAsFactors=F,blank.lines.skip = TRUE,comment.char = '',row.names=1)
			},error=function(e){
				p <- ggplot(mtcars,aes(gear,carb)) + theme_void() + annotate("text",x=range(mtcars$gear)[1],y=range(mtcars$carb)[1],label="Detected abnormal input data and parameter reading,\n please refer to the standard input file for proofreading.\nPlease browse the Err report panel at the bottom of the page, \nwhich contains detailed error information.",size=6)
				plot_continue(p,feature)
				return("1")
			})
			#print(str(data))			
			#write.table(df,scole_file,header=T)
			if (feature$method1 == "pcoa"){
				pcoa_main(data,feature,subtype)
			}
		}
		if(type == 'barplots'){
			tryCatch({
			feature <- fromJSON(file=feature.conf)
			data <- read.delim(datafile,stringsAsFactors=F)
			data[[feature$group]]=factor(data[[feature$group]],levels=unique(data[[feature$group]]))
			},error=function(e){
				p <- ggplot(mtcars,aes(gear,carb)) + theme_void() + annotate("text",x=range(mtcars$gear)[1],y=range(mtcars$carb)[1],label="Detected abnormal input data and parameter reading,\n please refer to the standard input file for proofreading.\nPlease browse the Err report panel at the bottom of the page, \nwhich contains detailed error information.",size=6)
				plot_continue(p,feature)
				return("1")
			})
			if(feature$bar_type == 'mean'){
				#data[[feature$group]]=factor(data[[feature$group]],levels=unique(data[[feature$group]]))
				#print(levels(data[[feature$group]]))
				bar_mean(data,feature,subtype)
			}
			else{
				data[[feature$subgroup]]=factor(data[[feature$subgroup]],levels=unique(data[[feature$subgroup]]))
				bar_charts(data,feature,subtype)
			}
		}
		if(type == 'boxplots'){
			tryCatch({
			print(feature.conf)
			feature <- fromJSON(file=feature.conf)
			print(feature)
			data <- read.delim(datafile,stringsAsFactors=F)
			data[[feature$group]]=factor(data[[feature$group]],levels=unique(data[[feature$group]]))
			},error=function(e){
				p <- ggplot(mtcars,aes(gear,carb)) + theme_void() + annotate("text",x=range(mtcars$gear)[1],y=range(mtcars$carb)[1],label="Detected abnormal input data and parameter reading,\n please refer to the standard input file for proofreading.\nPlease browse the Err report panel at the bottom of the page, \nwhich contains detailed error information.",size=6)
				plot_continue(p,feature)
				return("1")
			})

			if(feature$bar_type == 'mean'){
				box_mean(data,feature,subtype)
			}
			else{
				data[[feature$subgroup]]=factor(data[[feature$subgroup]],levels=unique(data[[feature$subgroup]]))
				box_charts(data,feature,subtype)
			}
		}

		if(type == 'heatmap'){
			tryCatch({
			feature <- fromJSON(file=feature.conf)
			#data <- read.delim(datafile,stringsAsFactors=F,row.names = 1,check.names=F)
			data <- read.delim(datafile,stringsAsFactors=F,row.names = 1)
			},error=function(e){
				p <- ggplot(mtcars,aes(gear,carb)) + theme_void() + annotate("text",x=range(mtcars$gear)[1],y=range(mtcars$carb)[1],label="Detected abnormal input data and parameter reading,\n please refer to the standard input file for proofreading.\nPlease browse the Err report panel at the bottom of the page, \nwhich contains detailed error information.",size=6)
				plot_continue(p,feature)
				return("1")
			})

			if(feature[['r_method']]=='ggplot2'){
				print(str(data))
				heatmap_all(data,feature)
			}
			else{
				heatmap_pheatmap(data,feature)
			}
		}
		if(type == 'vendiagram'){
			tryCatch({
			feature <- fromJSON(file=feature.conf)

			predata <- read.delim(datafile,header=T,stringsAsFactors=F)
			colnames_pre <- colnames(predata)
			other_colname <- colnames_pre[-which(colnames_pre==feature$group)]
			uniq_set <- unique(predata[[feature$group]])

			uniq_value <- unique(predata[[other_colname]])

			data <- list()
			
			for (sets in uniq_set){
				#print(sets)
				data[[sets]] = subset(predata,get(feature$group)==sets,get(other_colname),drop=T)
			}


			},error=function(e){
				p <- ggplot(mtcars,aes(gear,carb)) + theme_void() + annotate("text",x=range(mtcars$gear)[1],y=range(mtcars$carb)[1],label="Detected abnormal input data and parameter reading,\n please refer to the standard input file for proofreading.\nPlease browse the Err report panel at the bottom of the page, \nwhich contains detailed error information.",size=6)
				plot_continue(p,feature)
				return("1")
			})

			if (feature$type_venn=="ggupset"){

				vendiagram_plot(data,feature)

			}else{
				temo_data = sapply(data,t_new,uniq_value)
				rownames(temo_data) = uniq_value
				temo_data = as.data.frame(temo_data)
				upset_plot(temo_data,feature)
			}
			
		}
		if(type == 'Volcanomap'){
			tryCatch({
			feature <- fromJSON(file=feature.conf)
			resdata <- read.delim(datafile,header=T,stringsAsFactors=F)
			},error=function(e){
				p <- ggplot(mtcars,aes(gear,carb)) + theme_void() + annotate("text",x=range(mtcars$gear)[1],y=range(mtcars$carb)[1],label="Detected abnormal input data and parameter reading,\n please refer to the standard input file for proofreading.\nPlease browse the Err report panel at the bottom of the page, \nwhich contains detailed error information.",size=6)
				plot_continue(p,feature)
				return("1")
			})
			
			if(feature$group_vm_change == "False"){
				resdata$change <- as.factor(
    				ifelse(
        			resdata[[feature$group_vm_p]]<(as.numeric(feature$padjust_de_vm)) & abs(resdata[[feature$group_vm_fc]])>abs(as.numeric(feature$fc_de_vm)),
        				ifelse(resdata[[feature$group_vm_fc]]>abs(as.numeric(feature$fc_de_vm)), "Up", "Down"),
        				"Equally"
    				)
				)
				feature$group_vm_change = "change"
			}
			resdata <- na.omit(resdata)
			#print(str(resdata))
			valcannoone_plot(resdata,feature)
			
		}
		if(type == 'bubble_chart'){
			
			feature <- fromJSON(file=feature.conf)
			tryCatch({
			resdata <- read.delim(datafile,header=T,stringsAsFactors=F)
			#print(str(resdata))
			
			
			resdata <- na.omit(resdata)
			},error=function(e){
				p <- ggplot(mtcars,aes(gear,carb)) + theme_void() + annotate("text",x=range(mtcars$gear)[1],y=range(mtcars$carb)[1],label="Detected abnormal input data and parameter reading,\n please refer to the standard input file for proofreading.\nPlease browse the Err report panel at the bottom of the page, \nwhich contains detailed error information.",size=6)
				plot_continue(p,feature)
				return("1")
			})


			if(feature$group_bc_change_type == 'continuous'){
				resdata <- resdata[order(resdata[[feature$group_bc_change]]),]
				#print("order")
				#print(str(resdata))
			}

			if(feature$group_bc_p_type == 'discrete'){
				resdata[[feature$group_bc_p]] = factor(resdata[[feature$group_bc_p]],levels=unique(resdata[[feature$group_bc_p]]))
				#print(str(resdata))
			}

			
			bubble_plot(resdata,feature)
			
		}
		if(type == 'de_analyse'){
			print('bb')
			feature <- fromJSON(file=feature.conf)
		
			table_path = paste0(feature$out,'/sample')
			table <- read.table(table_path,header = TRUE,sep = "\t",row.name=1)
			print(table)

			tryCatch({
			print('cc')
			data <- read.table(datafile, header=T, row.names=1, com='', quote='',check.names=F, sep="\t")
			table_path = paste0(feature$out,'/sample')
			print(table_path)
			table <- read.table(table_path,header = TRUE,sep = "\t",row.name=1)
			print('dd')
			print(str(table))
			table[,1] = factor(table[,1],levels=unique(table[,1]))
			},error=function(e){
				p <- ggplot(mtcars,aes(gear,carb)) + theme_void() + annotate("text",x=range(mtcars$gear)[1],y=range(mtcars$carb)[1],label="Detected abnormal input data and parameter reading,\n please refer to the standard input file for proofreading.\nPlease check whether you upload the necessary group files\nIf it still does not work properly after checking, \nPlease browse the Err report panel at the bottom of the page, \nwhich contains detailed error information.",size=6)
				feature$mutiformat='heat'
				feature$r_method = 'ggplot2'
				plot_continue(p,feature)

				feature$mutiformat='valcano'
				plot_continue(p,feature)

				feature$mutiformat='pca'
				plot_continue(p,feature)

				return("1")
			})
			#data <- read.delim(datafile,stringsAsFactors=F,row.names = 1)
			deanalyse(data,table,feature)
		}
		if(type == 'enrich_gsea'){
			feature <- fromJSON(file=feature.conf)
			data <- read.delim(datafile,stringsAsFactors=F)
			gsea_analyse(data,feature)


		}

		if(type == 'enrich_analyse'){
			
			feature <- fromJSON(file=feature.conf)
			tryCatch({
				data <- read.delim(datafile,stringsAsFactors=F)
			},error=function(e){
				p <- ggplot(mtcars,aes(gear,carb)) + theme_void() + annotate("text",x=range(mtcars$gear)[1],y=range(mtcars$carb)[1],label="Detected abnormal input data and parameter reading,\n please refer to the standard input file for proofreading.\nPlease browse the Err report panel at the bottom of the page, \nwhich contains detailed error information.",size=6)
				feature$mutiformat='bar'
				plot_continue(p,feature)
				feature$mutiformat='dot'
				plot_continue(p,feature)
				feature$mutiformat='cnet'
				plot_continue(p,feature)
				return
			})
			enrich_analyse(data,feature)
		}
		if(type == 'ppi'){
			#library('R.utils')
			#R.utils::setOption("clusterProfiler.download.method",'auto')
			feature <- fromJSON(file=feature.conf)
			tryCatch({
			data <- read.delim(datafile,stringsAsFactors=F)
			},error=function(e){
				p <- ggplot(mtcars,aes(gear,carb)) + theme_void() + annotate("text",x=range(mtcars$gear)[1],y=range(mtcars$carb)[1],label="Detected abnormal input data and parameter reading,\n please refer to the standard input file for proofreading.\nPlease browse the Err report panel at the bottom of the page, \nwhich contains detailed error information.",size=6)
				plot_continue(p,feature)
				return("1")
			})
			ppi_analyse(data,feature)
		}

		if(type == 'domain'){
			
			library(drawProteins)
			feature <- fromJSON(file=feature.conf)
			tryCatch({
			input_sub_data = read.table(datafile,head=T,stringsAsFactors=F,sep='\t')
			},error=function(e){
				p <- ggplot(mtcars,aes(gear,carb)) + theme_void() + annotate("text",x=range(mtcars$gear)[1],y=range(mtcars$carb)[1],label="Detected abnormal input data and parameter reading,\n please refer to the standard input file for proofreading.\nPlease browse the Err report panel at the bottom of the page, \nwhich contains detailed error information.",size=6)
				plot_continue(p,feature)
				return("1")
			})
			#input_data = subset(input_sub_data,select=c('molecule','gene','start','end','strand','orientation'),drop=F)
			#input_data = input_data[!duplicated(input_data),]
			#print(str(input_data))
			#print(str(input_sub_data))
			#a = apply(input_data,1,gene_plot,out=feature,gene=input_data,sub_gene=input_sub_data)			
			domain_gene_plot(input_sub_data,feature)
		}

	}

	if (subtype == "01"){
		detail_plot(plot.conf)
	}

}

check_args_env <- function(test_vec){
	red_line = c(".First",".Last",".onLoad",".onAttach",".onUnload",".onDetach",".Last.lib")
	result_end = test_vec %in% red_line
	if (mean(result_end)!=0){
		return(FALSE)
	}
	return(TRUE)
}

LoadToEnvironment <- function(RData, env = new.env()){
    load(RData, env)
    return(env) 
}

check_env <- function(RData){
	#print("amin")
	test_env <- LoadToEnvironment(RData)
	if (length(ls(all.names=TRUE,envir=test_env))!=1){
		rm(list=ls(all.names=TRUE,envir=test_env),envir=test_env)
		return(FALSE)
	}
		
	flage_args <- check_args_env(ls(all.names=TRUE, envir=test_env))
	if(!flage_args){
		rm(list=ls(all.names=TRUE,envir=test_env),envir=test_env)
		return(FALSE)
	}
	
	test_p <- get(ls(all.names=TRUE,envir=test_env),envir=test_env)

	if(attr(test_p,'class')[1]=="aplot"){
		if(names(test_p)[1]=="plotlist"){
		test_p = test_p$plotlist[[1]]
		print('hello')
		}else{
			rm(list=ls(all.names=TRUE,envir=test_env),envir=test_env)
			return(FALSE)
		}
	}

	if(length(attr(test_p,'class'))<2 || length(attr(test_p,'class'))>3){
		
		rm(list=ls(all.names=TRUE,envir=test_env),envir=test_env)
		return(FALSE)
	}

	if(attr(test_p,'class')[1:2]!=c('gg','ggplot') && attr(test_p,'class')[2:3]!=c('gg','ggplot')){
		rm(list=ls(all.names=TRUE,envir=test_env),envir=test_env)
		return(FALSE)
	}

	#print("so")
	#print(names(test_p))
	#print(identical(names(test_p),c("data","layers","scales","mapping","theme","coordinates","facet","plot_env","labels")))
	if(!identical(names(test_p),c("data","layers","scales","mapping","theme","coordinates","facet","plot_env","labels")) && !identical(names(test_p),c("data","layers","scales","mapping","theme","coordinates","facet","plot_env","labels","guides"))){
		print("wrong ggplot2 object file")
		rm(list=ls(all.names=TRUE,envir=test_env),envir=test_env)
		return(FALSE)
	}
	return(TRUE)
}

t_new <- function(x,y){
	as.numeric(y %in% x)

}

bubble_plot <- function(data,feature){
	library(viridis)
	if (feature$group_bc_color_type == 'continuous'){
		p <- ggplot(data,aes(x=get(feature$group_bc_change),y=get(feature$group_bc_p),size=get(feature$group_bc_size),color=get(feature$group_bc_color))) +
			geom_point(alpha=0.5)+
			scale_size(range=c(1.5,10),name=feature$group_bc_size)+
			scale_color_viridis(discrete=FALSE)
	}else{
		p <- ggplot(data,aes(x=get(feature$group_bc_change),y=get(feature$group_bc_p),size=get(feature$group_bc_size),color=get(feature$group_bc_color))) +
			geom_point(alpha=0.5)+
			scale_size(range=c(1.5,10),name=feature$group_bc_size)+
			scale_color_viridis(discrete=TRUE)
	}
	p <- p + labs(x=feature$group_bc_change,y=feature$group_bc_p,color=feature$group_bc_color)+ theme_light()

	temp_bp = paste(feature$group_bc_change_type,feature$group_bc_p_type,sep="|")
	if(feature$group_bc_color_type == "continuous"){
		temp_bp = paste("bubble_chart|continuous",temp_bp,sep="|")
		temp_bp_end = paste('visomics',temp_bp,'1','0',sep=",")
		attr(p,'class') = c(attr(p,'class'),temp_bp_end)
	}else{
		temp_bp = paste("bubble_chart|discrete",temp_bp,sep="|")
		temp_bp_end = paste('visomics',temp_bp,'1','0',sep=",")
		attr(p,'class') = c(attr(p,'class'),temp_bp_end)
	}
	plot_continue(p,feature)

}

valcannoone_plot <- function(resdata,feature){
	valcano <- ggplot(data=resdata, aes(x=get(feature$group_vm_fc), y=-log10(get(feature$group_vm_p)), color=get(feature$group_vm_change))) +
    geom_point(alpha=0.8, size=1) +
    theme_bw(base_size=15) +
    theme(
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()
    ) +
	labs(color=feature$group_vm_change,x="log2FoldChange",y="-log10(padj)")+
    scale_colour_brewer(palette="Set1") +
    geom_vline(xintercept=c(-abs(as.numeric(feature$fc_de_vm)), abs(as.numeric(feature$fc_de_vm))), lty=2, col="gray", lwd=0.5) +
    geom_hline(yintercept=-log10(as.numeric(feature$padjust_de_vm)), lty=2, col="gray", lwd=0.5)
	
	
	attr(valcano,'class') = c(attr(valcano,'class'),'visomics,Volcanomap,1,0')
	plot_continue(valcano,feature)
}


domain_gene_plot <- function(data,feature){
	
	p <- draw_canvas(data)
	p<- draw_chains(p,data)
	#p <- draw_domains(p,data)
	draw_type = unique(data$type)
    for(i in draw_type){
		if(i !='CHAIN'){
			p <- draw_domains(p,data,type=i)
		}
	}
	p <- p + theme_bw(base_size = 20) + theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank()) +theme(axis.ticks =element_blank(),axis.text.y =element_blank()) + theme(panel.border =element_blank())
	#attr(p,'class')=c(attr(p,'class'),'visomics,domain,1,0')
	plot_continue(p,feature)
	
	
}

gene_plot <- function(x,out,gene,sub_gene){

    name = strsplit(x[1],':')[[1]]
    name = paste(name,collapse='__')
    #outpdf_name = paste(c(name,x[2]),collapse='__')
    #outpdf_name = paste(c(out,outpdf_name,'.png'),collapse='')
    #print(outpdf_name)
    #png(outpdf_name,width=1000,height=600,units='px',res=150)
    #png(outpdf_name)
        p = ggplot(subset(gene, molecule == x[1] & gene == x[2]),
        aes(xmin = start, xmax = end, y = strand)) +
        geom_gene_arrow(arrow_body_height = grid::unit(6, "mm"),arrowhead_width = grid::unit(7, "mm"), arrowhead_height = grid::unit(7, "mm")) +
		#geom_gene_arrow()+
        #geom_gene_label(aes(label = gene)) +
        geom_subgene_arrow(
        data = subset(sub_gene, molecule == x[1] & gene == x[2]),
        aes(xsubmin = from, xsubmax = to, fill = subgene),
		arrow_body_height = grid::unit(6, "mm")
        ) +
        geom_subgene_label(
        data = subset(sub_gene, molecule == x[1] & gene == x[2]),
        aes(xsubmin = from, xsubmax = to, label = subgene),
        min.size = 0
        )+
		labs(title=x[2])
		plot_continue(p,out)
        #print(plotpng)
    #dev.off()

}



enrich_last_plot <- function(term,feature){
	#enrich_analyse
	group_flage = '0'
	if(length(feature$y_method)!=0){
		newterm = term@result
		#newterm = newterm[-order(newterm[[feature$y_method]]),]
		#newterm = factor(newterm$Description,levels=unique(newterm$Description))
		if(length(feature$group)==0){
			
			#print(str(newterm))
			newterm = tail(newterm[order(-newterm[[feature$y_method]]),],10)
			#print(newterm)
			newterm$Description = factor(newterm$Description,levels=unique(newterm$Description))
			#print(newterm)
			p <- ggplot(newterm,aes(Description,-log10(get(feature$y_method)))) + geom_bar(stat='identity',fill='red',position=position_dodge(2),width=0.4)  + labs(title=feature$plot_type,y=quote(-log[10](p-value)))+ coord_flip() 
		}
		else{
			#print(str(newterm))
			group_flage = '1'
			newterm = tail(newterm[order(-newterm[[feature$y_method]],newterm[['ONTOLOGY']]),],10)
			newterm$Description = factor(newterm$Description,levels=unique(newterm$Description))
			p <- ggplot(newterm,aes(Description,-log10(get(feature$y_method)),fill=get(feature$group))) + geom_bar(stat='identity',width=0.4) + scale_fill_brewer(palette='Set1') + labs(title=feature$plot_type,y=quote(-log[10](p-value)),fill='Type') + coord_flip() 
		}
	}
	else{
		#if(length(feature$group)!=0){
		#	newterm = term@result
		#	newterm = tail(newterm[order(-newterm[['Count']],newterm[['ONTOLOGY']]),],10)
		#	newterm$Description = factor(newterm$Description,levels=unique(newterm$Description))
		#	p <- ggplot(newterm,aes(Description,Count,fill=get(feature$group))) + geom_bar(stat='identity',width=0.4) + scale_fill_brewer(palette='Set1') + labs(title=feature$plot_type,fill='Type') + theme(plot.margin = unit(c(2,3,3,4),"cm")) +coord_flip() 

		#}else{
			if(feature$plot_type=='enanalyse_go'){
				if(feature$method == "none"){
					p <- barplot(term,showCategory=10,title='GO',color = "pvalue")
				}else{
					p <- barplot(term,showCategory=10,title='GO')
				}
			}
			else{
				if(feature$method == "none"){
					p <- barplot(term,showCategory=10,title='KEGG',color = "pvalue")
				}else{
				p <- barplot(term,showCategory=10,title='KEGG')
				}
			}
		#}
	}

	feature$mutiformat='bar'
	if(group_flage == '0'){
		attr(p,'class') = c(attr(p,'class'),'visomics,enrichment analysis,1,bar')
	}else{
		#print('bar_en_group')
		attr(p,'class') = c(attr(p,'class'),'visomics,enrichment analysis,1,bar_en_group')
	}
	plot_continue(p,feature)

	if(feature$method == "none"){
		p <- dotplot(term,color = "pvalue")
	}else{
		p <- dotplot(term)
	}

	feature$mutiformat='dot'
	attr(p,'class') = c(attr(p,'class'),'visomics,enrichment analysis,2,dot')
	plot_continue(p,feature)

	p <- cnetplot(term)
	attr(p,'class') = c(attr(p,'class'),'visomics,enrichment analysis,3,cnet')
	feature$mutiformat='cnet'


	plot_continue(p,feature)

}

go_noref <- function(data,feature){
	table_path = paste0(feature$out,'/sample')
	table <- read.table(table_path,header = TRUE,sep = "\t",stringsAsFactors=F)
	#print(str(table))
	
	genes <- data[[feature$gene_name]]
	gene_GOID <- data.frame(table[[feature$background_gkid]],table[[feature$background_gene]],stringsAsFactors=F)
	GO_names <- go2term(table[[feature$background_gkid]])
	enrich_go <- enricher(gene=genes,pvalueCutoff=feature$pvalue,pAdjustMethod=feature$method,minGSSize=10,maxGSSize=500,TERM2GENE=gene_GOID,TERM2NAME=GO_names)


	enrich_file = paste0(feature$out,'/go_enrich_noref_result.txt')
    write.table(enrich_go,enrich_file,sep='\t',quote=F,row.names=F)

	enrich_last_plot(enrich_go,feature)
}	

go_ref <- function(data,feature){
	#library('org.Dr.eg.db')
	genes <- data[[feature$gene_name]]
	
	if(feature$gene_name_type == 'symbol'){
		print('bbb')
		print(str(genes))
		enrich.go <- enrichGO(gene = genes,OrgDb = feature$ref_db,keyType='SYMBOL',ont = 'ALL',pAdjustMethod = feature$method,pvalueCutoff=feature$pvalue,qvalueCutoff = 0.2)
	}
	else if(feature$gene_name_type == 'ebi'){
		enrich.go <- enrichGO(gene = genes,OrgDb = feature$ref_db,keyType='ENSEMBL',ont = 'ALL',pAdjustMethod = feature$method,pvalueCutoff=feature$pvalue,qvalueCutoff = 0.2)
	}	
	else{
		enrich.go <- enrichGO(gene = genes,OrgDb = feature$ref_db,keyType='ENTREZID',ont = 'ALL',pAdjustMethod = feature$method,pvalueCutoff=feature$pvalue,qvalueCutoff = 0.2)
	}

	enrich_file = paste0(feature$out,'/go_enrich_ref_result.txt')
    write.table(enrich.go,enrich_file,sep='\t',quote=F,row.names=F)
	#print(str(enrich.go))
	print(str(enrich.go))
	enrich_last_plot(enrich.go,feature)
}

kegg_ref <- function(data,feature){
	print('e')
	#library('dbplyr')
	#library(AnnotationHub)		
	print('b')
	if(feature$ref_db[1]=='org.Dr.eg.db'){
		library(org.Dr.eg.db)
	}

	if(feature$ref_db[1]=='org.Hs.eg.db'){
		library(org.Hs.eg.db)
	}

	if(feature$ref_db[1]=='org.Mm.eg.db'){
		library(org.Mm.eg.db)
	}

	if(feature$ref_db[1]=='org.Rn.eg.db'){
		library(org.Rn.eg.db)
	}

	if(feature$ref_db[1]=='org.Dm.eg.db'){
		library(org.Dm.eg.db)
	}

	if(feature$ref_db[1]=='org.Ce.eg.db'){
		library(org.Ce.eg.db)
	}



if(feature$ref_db[1]=='org.At.tair.db'){
		library(org.At.tair.db)
	}

	if(feature$ref_db[1]=='org.Sc.sgd.db'){
		library(org.Sc.sgd.db)
	}

	if(feature$ref_db[1]=='org.Gg.eg.db'){
		library(org.Gg.eg.db)
	}

	if(feature$ref_db[1]=='org.Pt.eg.db'){
		library(org.Pt.eg.db)
	}

	if(feature$ref_db[1]=='org.Ss.eg.db'){
		library(org.Ss.eg.db)
	}

	
	genes <- data[[feature$gene_name]]
	
	if(feature$gene_name_type == 'symbol'){
		#entrez_id = na.omit(mapIds(x=get(feature$ref_db[1]),keys=genes,keytype='SYMBOL',column='ENTREZID'))
		id <-bitr(genes, fromType = "SYMBOL", 
                 toType = c("ENTREZID"),
                 OrgDb = get(feature$ref_db[1]))
				 
		entrez_id = na.omit(id$ENTREZID)

	}
	else if(feature$gene_name_type == 'ebi'){
		#entrez_id = na.omit(mapIds(x=get(feature$ref_db[1]),keys=genes,keytype='ENSEMBL',column='ENTREZID'))
		id <-bitr(genes, fromType = "ENSEMBL", 
                 toType = c("ENTREZID"),
                 OrgDb = get(feature$ref_db[1]))
				 
		entrez_id = na.omit(id$ENTREZID)
	}
	else{
		entrez_id = na.omit(genes)
	}
	print('a')
	print(entrez_id)
	
	
	kegg <- enrichKEGG(gene=entrez_id,keyType='kegg',organism=feature$ref_db[2],pAdjustMethod=feature$method,pvalueCutoff=feature$pvalue,qvalueCutoff=0.2)
	test_temp = paste0(feature$out,'/test')
	save(kegg,file=test_temp)

	enrich_file = paste0(feature$out,'/kegg_enrich_ref_result.txt')
	#print(enrich_file)
    write.table(kegg,enrich_file,sep='\t',quote=F,row.names=F)
	#print(str(kegg))
	enrich_last_plot(kegg,feature)
}

kegg_noref <- function(data,feature){
	

	table_path = paste0(feature$out,'/sample')
	table <- read.table(table_path,header = TRUE,sep = "\t",stringsAsFactors=F)
	#print(str(table))
	
	genes <- data[[feature$gene_name]]
	gene_KeID <- data.frame(table[[feature$background_gkid]],table[[feature$background_gene]],stringsAsFactors=F)
	#ko_id <- bitr_kegg(table[[feature$background_gkid]], "kegg", "Path", "ko")
	#ko_name <- ko2name(ko_id$Path)
	k_name <- ko2name(table[[feature$background_gkid]])
	#ko_k_name <- merge(ko_id,ko_name,by.ko_id ='Path',by.ko_name ='ko')
	#k_name <- ko_k_name[c(1,4)]
	

	
	enrich_ke <- enricher(gene=genes,pvalueCutoff=feature$pvalue,pAdjustMethod=feature$method,minGSSize=10,maxGSSize=500,TERM2GENE=gene_KeID,TERM2NAME=k_name)
	#print(str(enrich_ke))
	enrich_file = paste0(feature$out,'/kegg_enrich_ref_result.txt')
    write.table(enrich_ke,enrich_file,sep='\t',quote=F,row.names=F)

	enrich_last_plot(enrich_ke,feature)
}

gsea_analyse <- function(data,feature){
	if(feature$plot_type == 'enanalyse_go'){
		
		gsea_ref(data,feature)


	}
}

gsea_ref <- function(data,feature){
	
library(ReactomePA)
library(org.Hs.eg.db)  # 人类基因注释
library(org.Mm.eg.db)
library(clusterProfiler)
library(enrichplot)     # 可视化
library(dplyr)
set.seed(123)
print(str(data))
geneList <- data$log2FoldChange+rnorm(length(data$log2FoldChange), sd = 1e-10)
print(str(data[[1]]))
print(str(geneList))
names(geneList) <-data[[1]]

gene_map <- bitr(names(geneList), 
                 fromType = "SYMBOL", 
                 toType = "ENTREZID", 
                 OrgDb = get(feature$ref_db[1]))
gene_map <- gene_map[!duplicated(gene_map$SYMBOL), ] 
geneList_entrez <- geneList[gene_map$SYMBOL]
names(geneList_entrez) <- gene_map$ENTREZID
geneList_entrez <- na.omit(geneList_entrez) %>% sort(decreasing = TRUE)
if (feature$ref_db[1]=="org.Mm.eg.db"){
	d_organism = "mouse"
}else{
	d_organism = "human"
}
gsea_reactome <- gsePathway(
  geneList = geneList_entrez,     # 排序后的命名向量
  organism = d_organism,             # 可选"mouse"、"rat"
  minGSSize = 10,                 # 最小基因集大小（过滤过小通路）
  maxGSSize = 500,                 # 最大基因集大小（过滤过大通路）
  pvalueCutoff = 0.1,             # p值阈值
  pAdjustMethod = "BH",            # 多重检验校正方法（FDR）
  eps = 0,                         # 避免零除错误
  verbose = FALSE                  # 关闭冗长输出
)
print('what happen')
print(str(gsea_reactome))

sig_pathways <- gsea_reactome %>%
  as.data.frame() %>%
  filter(p.adjust < 0.05 & NES > 0)  # FDR<0.05且|NES|>1

down_pathways <- gsea_reactome %>%
  as.data.frame() %>%
  filter(p.adjust < 0.05 & NES < 0)


p <- dotplot(gsea_reactome,
#p <- dotplot(sig_pathways,
        showCategory = 10,
        color = "p.adjust",   # 按FDR值着色
        title = "Reactome GSEA")

pp = dotplot(gsea_reactome,split=".sign")+facet_wrap(~.sign,scales = "free")


ggsave(paste0(feature$out,'/gsea_reactome.png'),pp,width=13)
data_mapped_file = paste0(feature$out,'/sig_pathways.txt')
data_mapped_file4 = paste0(feature$out,'/down_sig_pathways.txt')

data_mapped_file1 = paste0(feature$out,'/all_pathways.txt')
data_mapped_file2 = paste0(feature$out,'/all_pathways_col.txt')
write.table(sig_pathways,data_mapped_file,sep='\t',row.names=F,col.names=F,quote=FALSE)
write.table(down_pathways,data_mapped_file4,sep='\t',row.names=F,col.names=F,quote=FALSE)
write.table(gsea_reactome@result,data_mapped_file1,sep='\t',row.names=F,col.names=F,quote=FALSE)
write.table(gsea_reactome@result,data_mapped_file2,sep='\t',row.names=F,col.names=T,quote=FALSE)


#write.csv(gsea_reactome@result, "gsea_reactome_results.csv", row.names = FALSE)


}


enrich_analyse <- function(data,feature){
	library(clusterProfiler)
	print('a')
	#library('R.utils')
	#R.utils::setOption("clusterProfiler.download.method",'wget')
	if(feature$plot_type == 'enanalyse_go'){
		
		if(length(feature$ref_db)==0){
			
				go_noref(data,feature)
		}
		else{
			go_ref(data,feature)
		}
	}
	else{
		if(length(feature$ref_db)==0){
			kegg_noref(data,feature)		
		}
		else{
			print('c')
			kegg_ref(data,feature)
			
		}
	}
}


ppi_trans_id <- function(data,feature){
	
	

	genes <- data[[feature$gene_name]]
	
	
	
	string_db <- STRINGdb$new(version="11.5", species=as.numeric(feature$stringdb),score_threshold=feature$sth[1], input_directory="")
	data_mapped <- data %>% string_db$map(my_data_frame_id_col_names = feature$gene_name, removeUnmappedRows = TRUE)
	data_mapped_file = paste0(feature$out,'/idmapping')

	python_path_pre = strsplit(feature$out,"/")[[1]]
	#print(python_path_pre)
	#print(python_path_pre[1:(length(python_path_pre)-2)])
	python_path = c(python_path_pre[1:(length(python_path_pre)-2)],'ppi.py')
	
	python_sc = paste(python_path,collapse="/")
	#print(python_sc)
	backgroud_file = paste(paste(python_path_pre[1:(length(python_path_pre)-2)],collapse="/"),"/stringdb/v11.5/",feature$stringdb,".*",sep='')
	comond_line = paste(python_sc,data_mapped_file,backgroud_file,feature$sth[1])
	print(comond_line)
	print(str(data_mapped))
    write.table(data_mapped[c('STRING_id',feature$gene_name)],data_mapped_file,sep='\t',row.names=F,col.names=F,quote=FALSE)

	system(comond_line)

	out_file_name = data_mapped_file = paste0(feature$out,'/ppi_result.txt')
	ppi_data = read.delim(out_file_name,stringsAsFactors=F,header=T)
	#print(str(ppi_data))
	return(ppi_data)
	#return(ensembl_id)
	
}

ppi_trans_id_1 <- function(data,feature){
	
	library(AnnotationHub)		

	if(feature$ref_db[1]=='org.Dr.eg.db'){
		library(org.Dr.eg.db)
	}

	if(feature$ref_db[1]=='org.Hs.eg.db'){
		library(org.Hs.eg.db)
	}

	if(feature$ref_db[1]=='org.Mm.eg.db'){
		library(org.Mm.eg.db)
	}

	if(feature$ref_db[1]=='org.Rn.eg.db'){
		library(org.Rn.eg.db)
	}

	if(feature$ref_db[1]=='org.Dm.eg.db'){
		library(org.Dm.eg.db)
	}

	if(feature$ref_db[1]=='org.Ce.eg.db'){
		library(org.Ce.eg.db)
	}



	if(feature$ref_db[1]=='org.At.tair.db'){
		library(org.At.tair.db)
	}

	if(feature$ref_db[1]=='org.Sc.sgd.db'){
		library(org.Sc.sgd.db)
	}

	if(feature$ref_db[1]=='org.Gg.eg.db'){
		library(org.Gg.eg.db)
	}

	if(feature$ref_db[1]=='org.Pt.eg.db'){
		library(org.Pt.eg.db)
	}

	if(feature$ref_db[1]=='org.Ss.eg.db'){
		library(org.Ss.eg.db)
	}

	genes <- data[[feature$gene_name]]
	print(genes);
	print(feature$ref_db[1])
	if(feature$gene_name_type == 'symbol'){
		entre_id = na.omit(select(x=get(feature$ref_db[1]),keys=genes,keytype='SYMBOL',column='ENTREZID'))
		#entre_id = na.omit(mapIds(x=get(feature$ref_db[1]),keys=genes,keytype='SYMBOL',column='ENTREZID'))
	}
	else if(feature$gene_name_type == 'ebi'){
		entre_id = na.omit(select(x=get(feature$ref_db[1]),keys=genes,keytype='ENSEMBL',column='ENTREZID'))
	}
	else{
		entre_id = na.omit(select(x=get(feature$ref_db[1]),keys=genes,keytype='ENTREZID',column='SYMBOL'))
	}
	#print(entre_id)
	#entre_id = as.vector(entre_id)
	print(str(entre_id))
	string_db <- STRINGdb$new(version="11.5", species=as.numeric(feature$stringdb),score_threshold=feature$sth[1], input_directory="")
	data_mapped <- entre_id %>% string_db$map(my_data_frame_id_col_names = "ENTREZID", removeUnmappedRows = TRUE)
	data_mapped_file = paste0(feature$out,'/idmapping')

	python_path_pre = strsplit(feature$out,"/")[[1]]
	#print(python_path_pre)
	#print(python_path_pre[1:(length(python_path_pre)-2)])
	python_path = c(python_path_pre[1:(length(python_path_pre)-2)],'ppi.py')
	
	python_sc = paste(python_path,collapse="/")
	#print(python_sc)
	backgroud_file = paste(paste(python_path_pre[1:(length(python_path_pre)-2)],collapse="/"),"/stringdb/v11.5/",feature$stringdb,".*",sep='')
	comond_line = paste(python_sc,data_mapped_file,backgroud_file)
	print(comond_line)

    write.table(data_mapped[c('STRING_id','SYMBOL')],data_mapped_file,sep='\t',row.names=F,col.names=F,quote=FALSE)

	system(comond_line)

	out_file_name = data_mapped_file = paste0(feature$out,'/ppi_result.txt')
	ppi_data = read.delim(out_file_name,stringsAsFactors=F,header=T)
	#print(str(ppi_data))
	return(ppi_data)
	#return(ensembl_id)
	
}

ppi_ggraph <- function(data,feature){
	data_new <- t(apply(data,1,sort,decreasing=T))
	colnames(data_new) = colnames(data)
	data_new <- as.data.frame(data_new,stringsAsFactors=F)
	data_new$combined_score <- as.numeric(data_new$combined_score)
	data <- data_new %>% distinct()

	data <- subset(data,combined_score>= feature$sth[1],drop=F)
	if(feature$removed == 'yes'){
		
		
		data <- data %>% mutate(from_c = count(., from)$n[match(from, count(., from)$from)]) %>%
  			mutate(to_c = count(., to)$n[match(to, count(., to)$to)]) %>%
  			filter(!(from_c == 1 & to_c == 1)) %>%
  			dplyr::select(1,2,3)
	}
	
	#print(data)
	#print(str(data))
	data_note <- data %>% { data.frame(gene = c(.$from, .$to)) } %>% distinct()
	net_ppi <- igraph::graph_from_data_frame(d=data,vertices=data_note,directed = F)
	igraph::V(net_ppi)$deg <- igraph::degree(net_ppi)
	igraph::V(net_ppi)$size <- igraph::degree(net_ppi)/5
	igraph::E(net_ppi)$width <- igraph::E(net_ppi)$combined_score/10
	if( length(feature$repel)!=0 && feature$repel=='yes'){
		feature$repel=T
	}else{
		feature$repel=F
	}
	if(feature$node_text=='yes'){
		if(feature$layout == 'linear'){
			p <- ggraph(net_ppi,layout = feature$layout, circular = TRUE)+
  				get(feature$edge_style)(aes(edge_width=width), color = feature$edge_color, show.legend = F)+
  				geom_node_point(aes(size=size), color=feature$node_color, alpha=feature$node_alpha[1])+
  				geom_node_text(aes(filter=deg>feature$node_text_greater[1], label=name), size = feature$node_text_size[1], repel = feature$repel)+
  				scale_edge_width(range = c(0.2,1))+
  				scale_size_continuous(range = c(1,10) )+
  				guides(size=F)+
  				theme_graph()
		}else{
			if(feature$layout == 'centrality'){
			p <- ggraph(net_ppi,layout = feature$layout, cent = deg)+
  				get(feature$edge_style)(aes(edge_width=width), color = feature$edge_color, show.legend = F)+
  				geom_node_point(aes(size=size), color=feature$node_color, alpha=feature$node_alpha[1])+
  				geom_node_text(aes(filter=deg>feature$node_text_greater[1], label=name), size = feature$node_text_size[1], repel = feature$repel)+
  				scale_edge_width(range = c(0.2,1))+
  				scale_size_continuous(range = c(1,10) )+
  				guides(size=F)+
  				theme_graph()
			}else{
				p <- ggraph(net_ppi,layout = feature$layout)+
  				get(feature$edge_style)(aes(edge_width=width), color = feature$edge_color, show.legend = F)+
  				geom_node_point(aes(size=size), color=feature$node_color, alpha=feature$node_alpha[1])+
  				geom_node_text(aes(filter=deg>feature$node_text_greater[1], label=name), size = feature$node_text_size[1], repel = feature$repel)+
  				scale_edge_width(range = c(0.2,1))+
  				scale_size_continuous(range = c(1,10) )+
  				guides(size=F)+
  				theme_graph()
			}	  
		}
	}else{

		if(feature$layout == 'linear'){
			p <- ggraph(net_ppi,layout = feature$layout, circular = TRUE)+
  				get(feature$edge_style)(aes(edge_width=width), color = feature$edge_color, show.legend = F)+
  				geom_node_point(aes(size=size), color=feature$node_color, alpha=feature$node_alpha[1])+  				
  				scale_edge_width(range = c(0.2,1))+
  				scale_size_continuous(range = c(1,10) )+
  				guides(size=F)+
  				theme_graph()
		}else{
			if(feature$layout == 'centrality'){
			p <- ggraph(net_ppi,layout = feature$layout, cent = deg)+
  				get(feature$edge_style)(aes(edge_width=width), color = feature$edge_color, show.legend = F)+
  				geom_node_point(aes(size=size), color=feature$node_color, alpha=feature$node_alpha[1])+  				
  				scale_edge_width(range = c(0.2,1))+
  				scale_size_continuous(range = c(1,10) )+
  				guides(size=F)+
  				theme_graph()
			}else{
				p <- ggraph(net_ppi,layout = feature$layout)+
  				get(feature$edge_style)(aes(edge_width=width), color = feature$edge_color, show.legend = F)+
  				geom_node_point(aes(size=size), color=feature$node_color, alpha=feature$node_alpha[1])+  				
  				scale_edge_width(range = c(0.2,1))+
  				scale_size_continuous(range = c(1,10) )+
  				guides(size=F)+
  				theme_graph()
			}	  
		}

	}
	return(p)
}

ppi_analyse <- function(data,feature){
	
	library(tidyverse)
	#library(clusterProfiler)
	library(STRINGdb)
	library(igraph)
	library(ggraph)
	ppi_data <- ppi_trans_id(data,feature)
	p <- ppi_ggraph(ppi_data,feature)
	attr(p,'class') = c(attr(p,'class'),'visomics,ppi,1,ppi_01')
	plot_continue(p,feature)
	

}

deanalyse_pca <- function(dds,feature){
	
	rld <- rlog(dds)
	p <- plotPCA(rld, intgroup=c("condition"))
	feature$mutiformat='pca'


	attr(p,'class') = c(attr(p,'class'),'visomics,de analyse,1,pca')
	plot_continue(p,feature)
	
}


deanalyse_valcannoone <- function(resdata,feature){
	
	valcano <- ggplot(data=resdata, aes(x=log2FoldChange, y=-log10(padj), color=change)) +
    geom_point(alpha=0.8, size=1) +
    theme_bw(base_size=15) +
    theme(
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()
    ) +
    ggtitle("DEmRNA") +
    scale_color_manual(name="", values=c("red", "green", "black"), limits=c("Up", "Down", "Equally")) +
    #geom_vline(xintercept=c(-1, 1), lty=2, col="gray", lwd=0.5) +
	geom_vline(xintercept=c(-abs(feature$fc), abs(feature$fc)), lty=2, col="gray", lwd=0.5) +
    #geom_hline(yintercept=-log10(0.05), lty=2, col="gray", lwd=0.5)
	geom_hline(yintercept=-log10(abs(feature$p)), lty=2, col="gray", lwd=0.5)
	feature$mutiformat='valcano'

	attr(valcano,'class') = c(attr(valcano,'class'),'visomics,de analyse,2,valcano')
	plot_continue(valcano,feature)
	
}

denanlyse_heatmap <- function(resdata,table,feature){

	library(pheatmap)
	
	end <- 8+nrow(table)-1

	if(nrow(resdata) < abs(feature$top)){
		feature$top = nrow(resdata)
	}

	all_diff_gene_deseq2_expr_50 <- resdata[1:feature$top,8:end]
	
	rownames(all_diff_gene_deseq2_expr_50) <- resdata[1:feature$top,]$Row.names
	#print(str(all_diff_gene_deseq2_expr_50))
	#write.table(all_diff_gene_deseq2_expr_50,"/home/hli/Djangotest/GBTP/static/magicRversion1/data/heatmap",sep='\t',row.names=T)
	#print(str(table))
	#print(table)
	#p<- pheatmap(all_diff_gene_deseq2_expr_50, cluster_rows=TRUE, scale="row", annotation_col=table)
	feature$mutiformat='heat'
	feature$r_method = 'ggplot2'
	heatmap_all(all_diff_gene_deseq2_expr_50,feature)
	#plot_continue(p,feature)
	
}


deanalyse <- function(data,table,feature){
	library("DESeq2")
	library("BiocParallel")
	#print(table)
	condition <- table[,1]
	#print(condition)
	#print(table)
	#print(condition)
	#print(str(table))
	#print(str(data))

	print(str(table))
	print(str(data))

	data <- DESeqDataSetFromMatrix(data, colData=table, design= ~ condition)
	keep <- rowSums(counts(data) >= feature$reads_number_de) >= feature$filter_samples_de
	#dds <- data[ rowSums(counts(data)) > 1, ]
	dds <- data[keep, ]
	#print(str(keep))
	#print(nrow(dds))
	print(str(dds))
	if (length(intersect(c('deanalyse_pca','deanalyse_all'),feature$format))!=0){ 
		deanalyse_pca(dds,feature)
	}
	print("before DESeq")
	dds <- DESeq(dds)
	print("after DESeq")
	resultsNames(dds)
	res <- results(dds)
	print("after result")
	res <- res[order(res$padj),]
	resdata <- merge (as.data.frame(res),as.data.frame(counts(dds,normalize=TRUE)),by="row.names",sort=FALSE)
	

	#print(str(resdata))
	#print(tail(resdata))
	resdata$change <- as.factor(
    	ifelse(
        	resdata$padj<(feature$p) & abs(resdata$log2FoldChange)>abs(feature$fc),
        	ifelse(resdata$log2FoldChange>abs(feature$fc), "Up", "Down"),
        	"Equally"
    	)
	)
	resdata <- na.omit(resdata)
	all_file = paste0(feature$out,'/all_gene_result.txt')
    write.table(resdata,all_file,sep='\t',row.names=FALSE,quote=FALSE)
	
	all_file = paste0(feature$out,'/up_gene_result.txt')
    write.table(subset(resdata,change=='Up',drop=F),all_file,sep='\t',row.names=FALSE,quote=FALSE)
	
	all_file = paste0(feature$out,'/down_gene_result.txt')
    write.table(subset(resdata,change=='Down',drop=F),all_file,sep='\t',row.names=FALSE,quote=FALSE)

	if (length(intersect(c('deanalyse_valcannoone','deanalyse_all'),feature$format))!=0){ 
		deanalyse_valcannoone(resdata,feature)
	}

	if(feature$de_module == 'up'){
		resdata = subset(resdata,change=='Up',drop=F)
	}
	else if(feature$de_module == 'down'){
		resdata = subset(resdata,change=='Down',drop=F)
	}
	else{
		print('i am all')
	}

	if (length(intersect(c('denanlyse_heatmap','deanalyse_all'),feature$format))!=0){ 
		denanlyse_heatmap(resdata,table,feature)
	}
	
	resdata_sorted = resdata[order(resdata$log2FoldChange,decreasing = TRUE), ]
	all_file_g = paste0(feature$out,'/gsea_data.txt')
	write.table(subset(resdata_sorted,select=c("Row.names","log2FoldChange"),drop=F),all_file_g,sep='\t',row.names=FALSE,quote=FALSE)

}





pheatmap_plot <- function(data,feature,group){
	library(pheatmap)
	library(ggplotify)
	#NA/NaN/Inf in foreign function call (arg 10)
	if(feature$legend == 'yes'){
		feature$legend=T
	}else{
		feature$legend=F
	}

	if(feature$number == 'no'){
		feature$number = F
	}else{
		feature$number=T
	}

	if(feature$title == ''){
		feature$title=NA
	}

	if(feature$process != 'scale' ){
		feature$process='none'
	}
	else{
		feature$process='row'

	}
	

	data = data[apply(data, 1, function(x) sd(x)!=0),] 

	data = data[apply(data, 2, function(x) sd(x)!=0),]


	if(length(feature$distance)!=0){
		if(length(feature$sample)!=0){
			if(length(feature$median) == 0){
				feature$median = 'complete'
			}
			
			
			
			tablefile = paste0(feature$out,'/sample')
			#print(tablefile)
			table = read.delim(tablefile,stringsAsFactors=F,row.name=1)
			#print(table)
			#print(colnames(data))
			#print(str(data))
			p <- pheatmap(data, cluster_rows=T,clustering_distance_rows=feature$distance,clustering_method=feature$method,annotation_col=table,scale=feature$process,legend=feature$legend,display_numbers=feature$number,main=feature$title,angle_col=feature$angle,fontsize=feature$size)
		}
		else{	
			p <- pheatmap(data, cluster_rows=T,clustering_distance_rows=feature$distance,clustering_method=feature$method,scale=feature$process,legend=feature$legend,display_numbers=feature$number,main=feature$title,angle_col=feature$angle,fontsize=feature$size)
		}
	}
	else{
		if(length(feature$sample)!=0){
			#print(feature$out)
			tablefile = paste0(feature$out,'/sample')
			#print(tablefile)
			table = read.delim(tablefile,stringsAsFactors=F,row.name=1)
			#print(str(table))
			p <- pheatmap(data, cluster_rows=T,annotation_col=table,scale=feature$process,legend=feature$legend,display_numbers=feature$number,main=feature$title,angle_col=feature$angle,fontsize=feature$size)

		}		
		else{	
			p <- pheatmap(data, cluster_rows=F,cluster_cols=F,legend=feature$legend,display_numbers=feature$number,scale=feature$process,main=feature$title,angle_col=feature$angle,fontsize=feature$size)
		}	
	}
	p <- as.ggplot(p)
	return (p)

}

heatmap_pheatmap <- function(data,feature){
	

	
	if(feature$process !='0' ){
		data <- heatmap_process(data,feature)
	}	
	group = 0
	

	p <- pheatmap_plot(data,feature,group)
	plot_continue(p,feature)
	

}



upset_plot <- function(data,feature){
	library(UpSetR)
	library(ggplotify)
	mb_ratio = as.numeric(feature$upset_mb)
	mb_ratio_c = c(1-(mb_ratio/(1+mb_ratio)),mb_ratio/(1+mb_ratio))
	
	conf = feature
	main_file = paste0(feature$out,"/ordination_",conf$count+1,".png")
	
	
	png(file=main_file,res = as.numeric(feature$upset_dpi),width=as.numeric(feature$upset_width),height=as.numeric(feature$upset_height))
	
	print(upset(data,sets=colnames(data),mb.ratio=mb_ratio_c,order.by=feature$upset_order,number.angles=as.numeric(feature$upset_angle),point.size=as.numeric(feature$upset_point),line.size=as.numeric(feature$upset_line),mainbar.y.label=feature$upset_yaxis,sets.x.label=feature$upset_xaxis))
	
	
	dev.off()

	main_file = paste0(feature$out,"/ordination_",conf$count+1,".pdf")
	pdf(file=main_file)
	print(upset(data,sets=colnames(data),mb.ratio=mb_ratio_c,order.by=feature$upset_order,number.angles=as.numeric(feature$upset_angle),point.size=as.numeric(feature$upset_point),line.size=as.numeric(feature$upset_line),mainbar.y.label=feature$upset_yaxis,sets.x.label=feature$upset_xaxis))
	dev.off()

	main_file = paste0(feature$out,"/ordination_",conf$count+1,".jpg")
	jpeg(file=main_file,res = as.numeric(feature$upset_dpi),width=as.numeric(feature$upset_width),height=as.numeric(feature$upset_height))
	print(upset(data,sets=colnames(data),mb.ratio=mb_ratio_c,order.by=feature$upset_order,number.angles=as.numeric(feature$upset_angle),point.size=as.numeric(feature$upset_point),line.size=as.numeric(feature$upset_line),mainbar.y.label=feature$upset_yaxis,sets.x.label=feature$upset_xaxis))
	dev.off()

	main_file = paste0(feature$out,"/ordination_",conf$count+1,".svg")
	svg(file=main_file)
	print(upset(data,sets=colnames(data),mb.ratio=mb_ratio_c,order.by=feature$upset_order,number.angles=as.numeric(feature$upset_angle),point.size=as.numeric(feature$upset_point),line.size=as.numeric(feature$upset_line),mainbar.y.label=feature$upset_yaxis,sets.x.label=feature$upset_xaxis))
	dev.off()

	main_file = paste0(feature$out,"/ordination_",conf$count+1,".bmp")
	bmp(file=main_file,res = as.numeric(feature$upset_dpi),width=as.numeric(feature$upset_width),height=as.numeric(feature$upset_height))
	print(upset(data,sets=colnames(data),mb.ratio=mb_ratio_c,order.by=feature$upset_order,number.angles=as.numeric(feature$upset_angle),point.size=as.numeric(feature$upset_point),line.size=as.numeric(feature$upset_line),mainbar.y.label=feature$upset_yaxis,sets.x.label=feature$upset_xaxis))
	dev.off()

	main_file = paste0(feature$out,"/ordination_",conf$count+1,".tiff")
	tiff(file=main_file,res = as.numeric(feature$upset_dpi),width=as.numeric(feature$upset_width),height=as.numeric(feature$upset_height))
	print(upset(data,sets=colnames(data),mb.ratio=mb_ratio_c,order.by=feature$upset_order,number.angles=as.numeric(feature$upset_angle),point.size=as.numeric(feature$upset_point),line.size=as.numeric(feature$upset_line),mainbar.y.label=feature$upset_yaxis,sets.x.label=feature$upset_xaxis))
	dev.off()

}

log10del <- function(x){
	x <- x+1
	return(log10(x))
}

heatmap_process <- function(df,feature){
	print(str(df))
	library(reshape2)
	library(ggtree)
	library(scales)
		if(feature$process == 'log10'){
			df <- apply(df,1,log10del)
			df <- as.data.frame(t(df),stringsAsFactors=FALSE)
			return(df)
		}
		if(feature$process == 'scale' && feature[['r_method']] == 'pheatmap'){
			return (df)
		}

		df <- apply(df,1,get(feature$process))
		df <- as.data.frame(t(df),stringsAsFactors=FALSE)
		return (df)
	
}

counstruct_group <- function(feature){
	#library(dplyr)
	samplefile <- paste0(feature$out,'/sample')
	#samplefile = paste0("/home/hli/Djangotest/GBTP/static/magicRversion1/data/",feature$out,"/sample")
	sample_data <- read.delim(samplefile,stringsAsFactors=F)
	#print(str(sample_data))
	groups <- unique(sample_data[[2]])
	rowlen = length(rownames(sample_data))
	sample_data$p = replicate(rowlen,colnames(sample_data)[2])
	#print(str(sample_data))
	temp_second_col <- colnames(sample_data)[2]
	#print(temp_second_col)
	colnames(sample_data) = c('sample','condition','p')
	#group <- sample_data[[2]] %>% data.frame(x=c(1:rowlen),y=rep(1,rowlen)) %>% ggplot() + geom_tile(aes(x,y,fill=.)) +theme(panel.background = element_blank(),axis.line = element_blank(),axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),plot.title = element_blank())
	#group <- ggplot(sample_data,aes(get(colnames(sample_data)[1]),y=p,fill=get(colnames(sample_data)[2]))) + geom_tile(color='black') + scale_y_discrete(position="right") + theme_minimal() + xlab(NULL) + ylab(NULL) + theme(axis.text.x = element_blank()) + labs(fill=colnames(sample_data)[2])
	#group <- ggplot(sample_data,aes(sample,y=p,fill=condition)) + geom_tile(color='black') + scale_y_discrete(position="right",labels=c(temp_second_col)) + theme_minimal() + xlab(NULL) + ylab(NULL) + theme(axis.text.x = element_blank(),legend.margin=margin(l=8)) + labs(fill=temp_second_col)
	group <- ggplot(sample_data,aes(sample,y=p,fill=condition)) + geom_tile(color='black') + scale_y_discrete(position="right",labels=c(temp_second_col),expand=c(0,0)) + theme_minimal() + xlab(NULL) + ylab(NULL) + theme(axis.text.x = element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank()) + labs(fill=temp_second_col)
	
	return (group)

}

counstruct_tree <- function(data,feature){
	library(ggtree)
	library(vegan)
	data <- na.omit(data)
	gg_row <- ggtree(hclust(dist(data,method=feature$distance),method=feature$method),layout="rectangular",branch.length="none")
	gg_col <-  ggtree(hclust(dist(t(data),method=feature$distance),method=feature$method)) + layout_dendrogram()
	
	
	#gg_row <- ggtree(hclust(p_a,method=feature$method),layout="rectangular",branch.length="none")
	#gg_col <-  ggtree(hclust(vegdist(t(data),method=feature$distance),method=feature$method)) + layout_dendrogram()
	return_p = list(a=gg_col,b=gg_row)
	return(return_p)
}

heatmap_plot <- function(df,feature,group,gg_tree){

	library(reshape2)
	library(ggtree)
	library(scales)
	library(aplot)
	#library(ggplotify)
	#library(patchwork)
	
	#print(str(df))
	rowlen = length(rownames(df))
	collen = length(colnames(df))
	df <- cbind(name=row.names(df),df,stringsAsFactors=FALSE)

	df <- melt(df,id.vars='name')
	df$variable = as.vector(df$variable)
	#print(str(df))
	#df$x <- rep(c(1:collen),each=rowlen)
	#df$y <- rep(c(1:rowlen),collen)		
	#print(str(df))
	#heatmap_p <- ggplot(df,aes(x,y,fill=value)) + geom_tile(color='black') + scale_fill_gradient(low='blue',high='red')+theme(plot.margin = unit(c(0,0,0,0), unit="mm"))

	df$name = factor(df$name,levels=unique(df$name))
	df$variable = factor(df$variable,levels=unique(df$variable))
	
	heatmap_p <- ggplot(df,aes(variable,name,fill=value)) + geom_raster() + scale_fill_gradient2(low="#003366", high="#990033", mid="white") + geom_tile(color='black') + theme_minimal() + xlab(NULL) + ylab(NULL)
	#heatmap_p <- ggplot(df,aes(variable,name,fill=value)) + geom_point(aes(size=value,color=value)) + scale_color_gradient2(low="#003366", high="#990033", mid="white") + theme_minimal() + theme(axis.text.x =element_text(angle =90,hjust =0.5,vjust = 0.5)) + xlab(NULL) + ylab(NULL)

	

	if(length(feature$distance)!=0){
		if(length(feature$sample)!=0){
			samplefile <- paste0(feature$out,'/sample')
	#samplefile = paste0("/home/hli/Djangotest/GBTP/static/magicRversion1/data/",feature$out,"/sample")
			sample_data <- read.delim(samplefile,stringsAsFactors=F)
			if(feature$hclust == 'both'){
				p <- heatmap_p %>% insert_top(group, height = .05) %>% insert_top(gg_tree$a,height=.1)  %>% insert_left(gg_tree$b,width=.2)
			}
			if(feature$hclust == 'row'){
				p <- heatmap_p %>% insert_top(group, height = .05) %>% insert_left(gg_tree$b,width=.2)
			}
			if(feature$hclust == 'col'){
				p <- heatmap_p %>% insert_top(group, height = .05) %>% insert_top(gg_tree$a,height=.1)
			}
		}
		else{
			if(feature$hclust == 'both'){
				p <- heatmap_p %>% insert_left(gg_tree$b,width=.2) %>% insert_top(gg_tree$a,height=.2)
			}
			if(feature$hclust == 'row'){
				p <- heatmap_p %>% insert_left(gg_tree$b,width=.2)
			}
			if(feature$hclust == 'col'){
				p <- heatmap_p %>% insert_top(gg_tree$a,height=.2)
			}

			#p <- heatmap_p %>% insert_left(gg_tree$b,width=.2) %>% insert_top(gg_tree$a,height=.2)
			#p <- as.ggplot(p)
			#print(str(p))
			#p <- gg_tree$b + heatmap_p
		}
	}
	else{
		
		if(length(feature$sample)!=0){
			#p <- group
			p <- heatmap_p %>% insert_top(group, height = .05)
			#p <- as.ggplot(p)
			#p <- group / plot_spacer() /heatmap_p + plot_layout(heights=c(1,-1.6,35),guides = 'collect')
		}else{
			p <- heatmap_p
		}			
		
		#scale_fill_gradient(low='steelblue',high='red')
		#scale_fill_gradient2(low=muted('blue'),midpoint=0.5,high=muted('red'),mid='yellow')
		#scale_fill_gradient(low='blue',high='red')
		#scale_fill_gradient2(low=muted('blue'),midpoint=(max(df$value)+min(df$value))/2,high=muted('red'),mid='white')
		#scale_fill_gradient2(low=muted('green'),high=muted('red'),mid='white',midpoint=(max(df$value)+min(df$value))/2)
		#scale_fill_gradient2(low=muted('blue'),midpoint=median(df$value),high=muted('red'),mid='white')
		#scale_fill_gradientn(values = seq(-2,2,0.2),colours = c('cyan','blue','green','orange','red'))
		#scale_fill_gradient2(low=muted('blue'),midpoint=median(df$value),high=muted('red'))
		#scale_fill_gradient(low='blue',high='red')
		#scale_fill_gradient2(low=muted('blue'),midpoint=0,high=muted('red'))		
	}
	return (p)
}

heatmap_all <- function(data,feature){
	
	if(feature$process !='0'){
		data_old <- heatmap_process(data,feature)
		colnames(data_old) = colnames(data)
		data = data_old
	}	
	group = 0
	if(length(feature$sample)!=0){
		
		group <- counstruct_group(feature)
	}
	gg_tree = 0
	if(length(feature$distance)!=0){
		gg_tree <- counstruct_tree(data,feature)
	}
	print(str(data))
	p <- heatmap_plot(data,feature,group,gg_tree)	
	#p <- group
	if (length(feature$mutiformat) == 1){
		attr(p,'class') = c(attr(p,'class'),'visomics,de analyse,3,heat')
	}else{
		attr(p,'class') = c(attr(p,'class'),'visomics,heatmap,1,0')
	}
	plot_continue(p,feature)
	
}


user_css <- function(p,conf){
	if(length(conf[[1]]$theme)!=0){
		if(conf[[1]]$theme %in% c("theme_tufte","theme_solarized","theme_excel","theme_hc")){
			library(ggthemes)			
		}		
		p <- p + get(conf[[1]]$theme)()
		conf = conf[-1]
	}
	#p <- p + get(conf[[1]]$theme)()
	#ggsave("scale.png",plot=p)
	#conf = conf[-1]
	for (css in conf){
		css <- construct_theme(css)	
		#print(css)	
		p <- p+css
	}
	return(p)
}

counstruct_text <- function(item){
#b <- list(plot.title=list(colour="red",hjust=NULL,vjust=NULL,size=NULL,margin=NULL))
	temp= item[[1]]
	a <- list(family=temp$family,face=temp$face,colour=temp$colour,size=temp$size,hjust=temp$hjust,vjust=temp$vjust,angle=temp$angle,lineheight=temp$lineheight,margin=temp$margin,debug=temp$debug)
	item[[1]] = a
	return(item)
}

construct_line <- function(item){
	temp = item[[1]]
	a <- list(colour=temp$colour,size=temp$size,linetype=temp$linetype)
	item[[1]] = a
	return(item)
}

construct_rect <- function(item){
    temp = item[[1]]
    a <- list(colour=temp$colour,size=temp$size,fill=temp$fill)
    item[[1]] = a
    return(item)
}



counstruct_margin <- function(item){
	temp_int = item[[1]]$rbl
	temp_unit = item[[1]]$unit
	a <- get(item$subclass[1])(temp_int[1],temp_int[2],temp_int[3],temp_int[4],unit=temp_unit)
	item[[1]]=a
	item$subclass=NULL
	attr(item,"class")=c("theme","gg")
	return(item)
}

construct_ticks <- function(item){
	temp_int = item[[1]]$number
	temp_unit = item[[1]]$unit
	a <- unit(temp_int,temp_unit)
	item[[1]] = a
	item$subclass=NULL
	attr(item,"class")=c("theme","gg")
	return(item)	
}

construct_theme <- function(item){
	#if(names(item)[1] %in% c('axis.ticks.length.x','axis.ticks.length.y')){
    #    item <- construct_ticks(item)
    #    return(item)
    #}

	if(length(names(item))==1){
        attr(item,"class")=c("theme","gg")
        return(item)
    }

    if(item$subclass[1] == "margin"){
        item <- counstruct_margin(item)
        return(item)
    }

	if(item$subclass[1] == "unit"){
        item <- construct_ticks(item)
        return(item)
    }

	if(item$subclass[[2]][1] == "element_text" ){
		item <- counstruct_text(item)		
	}

	if(item$subclass[[2]][1] == "element_line"){
		item <- construct_line(item)
	}

	if(item$subclass[[2]][1] == "element_rect"){
        item <- construct_rect(item)
    }	

	attr(item,"class")=c("theme","gg")
	#print(item[[names(item)[1]]])
	attr(item[[names(item)[1]]],item$subclass[[1]]) = item$subclass[[2]]
	item$subclass = NULL
	#print(str(item))
	return(item)
}

construct_continuous <- function(cp){
	subcp <- cp[[1]]
	if(length(subcp$breaks)==0){
		subcp$breaks = waiver()		
	}
	if (length(subcp$labels)==0){
		subcp$labels = waiver()
	}	
	if(length(subcp$expand)==0){
		subcp$expand = waiver()
	}
	if (length(subcp$limits)==0){
		subcp$limits = NULL
	}
	if (length(subcp$position)==0){
		if(names(cp) == "scale_x_continuous" || names(cp) == "scale_x_discrete"){
			subcp$position = "bottom"
		}
		else{
			subcp$position = "left"
		}
	}
	if (length(subcp$trans)==0){
		subcp$trans = "identity"
	}
	return(subcp)
}

user_scale <- function(p,conf){
	for(cp in conf){
		if (names(cp) %in% c("scale_x_continuous","scale_y_continuous")){
			subcp <- construct_continuous(cp)
			p <- p+get(names(cp))(breaks=subcp$breaks,labels=subcp$labels,position=subcp$position,expand = subcp$expand,limits = subcp$limits,trans = subcp$trans)
			next
		}
		
		if (names(cp) %in% c("scale_x_discrete","scale_y_discrete")){
			subcp <- construct_continuous(cp)
			p <- p+get(names(cp))(breaks=subcp$breaks,labels=subcp$labels,position=subcp$position,expand = subcp$expand)
			next
		}

		if (names(cp) == "labs"){
			temp <- cp$labs
			attr(temp,'class')="labels"
			#print(temp)
			p <- p+temp	
			next
		}

        if(names(cp) == "coord_cartesian"){
            #if (length(cp$coord_cartesian$xlim) == 0){
			#	cp$coord_cartesian$xlim = NULL
            #}
            #if (length(cp$coord_cartesian$ylim) == 0){
			#	cp$coord_cartesian$ylim = NULL
            #}
			p <- p+coord_cartesian(xlim=cp$coord_cartesian$xlim,ylim=cp$coord_cartesian$ylim)
            next
        }

		if(names(cp) == "geom_text"){
			if(length(cp$geom_text$vjust) == 0){
				cp$geom_text$vjust="middle"
			}
			if(length(cp$geom_text$hjust) == 0){
				cp$geom_text$hjust="center"
			}
			if(length(cp$geom_text$nudge_y) == 0){
				cp$geom_text$nudge_y=0
			}
			if(length(cp$geom_text$nudge_x) == 0){
				cp$geom_text$nudge_x=0
			}
			if(length(cp$geom_text$size) == 0){
				cp$geom_text$size=4
			}
			if(length(cp$geom_text$family) == 0){
				cp$geom_text$family="sans"
			}
			if(length(cp$geom_text$check_overlap) == 0){
				cp$geom_text$check_overlap = F
			}
			else{
				cp$geom_text$check_overlap = T
			}

			temp=cp[[1]]
			#print(temp)
			p <- p+geom_text(aes(label=text),family=temp$family,size=temp$size,vjust=temp$vjust,check_overlap=temp$check_overlap,hjust=temp$hjust,nudge_y=temp$nudge_y,nudge_x=temp$nudge_x,show.legend=F)	
			next
		}

		

		if(names(cp) == "geom_dl"){
			if(length(cp$geom_dl$label)==0){
				cp$geom_dl$label="group"
			}
			if(length(cp$geom_dl$method)==0){
				cp$geom_dl$method="smart.grid"
			}
			library(directlabels)
			#print(cp$geom_dl$label)
			a <- cp$geom_dl$label		
			p <- p+directlabels::geom_dl(aes(label=get(a)),method=cp$geom_dl$method)
			next
		}

		if(names(cp) == "annotate"){
			#if(length(cp$geom_dl$label)==0){
			#	cp$geom_dl$label=""
			#}
			if(length(cp$annotate$size)==0){
				cp$annotate$size=4
			}
			if(length(cp$annotate$colour)==0){
				cp$annotate$colour="black"
				
			}

			p <- p+ annotate('text',x=cp$annotate$x,y=cp$annotate$y,label=cp$annotate$label,size=cp$annotate$size,colour=cp$annotate$colour)			
			next
		}

		if(names(cp) %in% c("scale_fill_brewer","scale_colour_brewer")){
			#print(cp[[1]]$palette)
			p <- p + get(names(cp))(palette=(cp[[1]]$palette))
			next
		}
		
		
		if(names(cp) == 'scale_fill_gradient'){
			#print(names(cp))
			p <- p + get(names(cp))(low=cp[[1]]$low,high=cp[[1]]$high) + guides(fill=guide_colorbar(reverse=T))
			next
		}

		if(names(cp) == 'scale_colour_gradient'){
			p <- p + get(names(cp))(low=cp[[1]]$low,high=cp[[1]]$high) + guides(color=guide_colorbar(reverse=T)) + scale_size()
			next
		}

		if(names(cp) == 'scale_size'){
			p <- p + scale_size(range=c(cp[[1]][1],cp[[1]][2]))
			next
		}



		if(names(cp) == "scale_fill_grey"){
			if(length(cp[[1]]$start) == 0){
				p <- p + scale_fill_grey()
			}
			else{
				p <- p + scale_fill_grey(start=cp[[1]]$start,end=cp[[1]]$end)
			}
			next
		}
		
		if(names(cp) %in% c("scale_fill_hue","scale_colour_hue")){
			if(length(cp[[1]]$h) == 0){
				cp[[1]]$h = c(0,360) + 15
			}
			if(length(cp[[1]]$c) == 0){
				cp[[1]]$c = 100	
			}
			if(length(cp[[1]]$l) == 0){
				cp[[1]]$l = 65
			}
			p <- p + get(names(cp))(h=cp[[1]]$h,c=cp[[1]]$c,l=cp[[1]]$l)
			next
		}

		if(names(cp) %in% c("scale_fill_manual","scale_colour_manual")){
			library(wesanderson)
			if (length(cp[[1]]$wes_palette) == 0){
				p <- p+get(names(cp))(values=cp[[1]]$values)		
				next
			}
			else{
				
				print(wes_palette('Zissou'))
				p <- p+get(names(cp))(values = wes_palette(cp[[1]]$wes_palette))
				next
			}
		}
		if(names(cp) %in% c('geom_point')){
			p <- p + get(names(cp))(size=cp[[1]]$size)
		}

		if(names(cp) %in% c('coord_fixed','coord_flip')){
			p <- p+get(names(cp))()
		}

		if(names(cp) %in% c('geom_signif')){
			library(ggsignif)
			if(cp[[1]]$asterisk == 'no'){
				cp[[1]]$asterisk=F
			}
			else{
				cp[[1]]$asterisk=T
			}
			compaired = combn(cp[[1]]$groups,2,simplify=F)
			#print(cp[[1]])
			#
			p <- p + geom_signif(comparisons = compaired,map_signif_level=cp[[1]]$asterisk,test=cp[[1]]$method,size=cp[[1]]$size,textsize=cp[[1]]$textsize,y_position=cp[[1]]$position,annotations=cp[[1]]$annote)
		}


	}
	return(p)		
}

detail_plot <- function(plot.conf){
   	user.conf <- fromJSON(file=plot.conf)
	if(length(user.conf$flage_type)==0){
		plotitem_pathy <- paste0(user.conf$out,"/plot_",user.conf$count+user.conf$checkedbutton)
	}
	else{
		plotitem_pathy <- paste0(user.conf$out,"/plot_",user.conf$count+user.conf$checkedbutton,'_',user.conf$flage_type)
	}
   	
	flage_env <- check_env(plotitem_pathy)
	if(!flage_env){
		p <- ggplot(mtcars,aes(gear,carb)) + theme_void() + annotate("text",x=range(mtcars$gear)[1],y=range(mtcars$carb)[1],label="Detected abnormal input data and parameter reading,\n please refer to the standard input file for proofreading.\nPlease browse the Err report panel at the bottom of the page, \nwhich contains detailed error information.",size=6)
		print("detail plot check error")
		plot_continue(p,user.conf)
		return("1")
	}
	temp_str = load(plotitem_pathy)
	p <- get(temp_str)

   	#load(plotitem_pathy)
	if((length(user.conf$plottype_temp)!=0 && user.conf$plottype_temp == 'heatmap' && attr(p,'class')[1]!='gg') || (length(user.conf$flage_type)!=0 && user.conf$flage_type == 'heat' && attr(p,'class')[1]!='gg')){
		library('aplot')
		#print('hello')
		#p_ori <- p
		p_main <- p$plotlist[[1]]
		p_new <- user_scale(p_main,user.conf$scale)
		p_new_new <- user_css(p_new,user.conf$css)
		p$plotlist[[1]] <- p_new_new
		plot_continue(p,user.conf)
	}else{
   		p <- user_scale(p,user.conf$scale)
	#ggsave("scale.png",plot=p)
   		p <- user_css(p,user.conf$css)
	#print(str(p))
	#ggsave("scale.png",plot=p)
	#if((length(user.conf$plottype_temp)!=0 && user.conf$plottype_temp == 'heatmap' && attr(p,'class')[1]!='gg') || (length(user.conf$flage_type)!=0 && user.conf$flage_type == 'heat' && attr(p,'class')[1]!='gg')){
	#	print('hello02')
		
	#	p_ori$plotlist[[1]] <- p
	#	p <- p_ori
	#}

   		plot_continue(p,user.conf)
	}
}

plot_continue <- function(p,conf){
	if(length(conf$maxdetail)==0){
		if(length(conf$mutiformat) == 0){
        	main_file = paste0(conf$out,"/ordination_",conf$count+1,".png")
    #print(main_file)
    		ggsave(main_file,plot=p)
    		save_plot <- paste0(conf$out,"/plot_",conf$count+1)
    		save(p,file=save_plot)

			save_plot_new <- paste0(conf$out,"/plot_",conf$count+1,".RData")
    		save(p,file=save_plot_new)

		}
		else{
			main_file = paste0(conf$out,"/ordination_",conf$count+1,'_',conf$mutiformat,".png")
			ggsave(main_file,plot=p)
			save_plot <- paste0(conf$out,"/plot_",conf$count+1,'_',conf$mutiformat)
			save(p,file=save_plot)

			save_plot_new <- paste0(conf$out,"/plot_",conf$count+1,'_',conf$mutiformat,".RData")
			save(p,file=save_plot_new)
		}	
    }
	else{
		#ggsave(main_file,plot=p)
		#save_plot <- paste0(conf$out,"/plot_",conf$count+conf$maxdetail)
		#save(p,file=save_plot)
		#print("i am here")
		if(length(conf$download$width)==0){
			conf$download$width=NA
		}
		if(length(conf$download$height)==0){
            conf$download$height=NA
        }
		if(length(conf$download$res)==0){
			conf$download$res=300
		}
		
		if(length(conf$flage_type)==0){
			main_file = paste0(conf$out,"/ordination_",conf$count+conf$maxdetail,".png")
			save_plot <- paste0(conf$out,"/plot_",conf$count+conf$maxdetail)
			save_plot_new <- paste0(conf$out,"/plot_",conf$count+conf$maxdetail,".RData")
		}
		else{
			main_file = paste0(conf$out,"/ordination_",conf$count+conf$maxdetail,'_',conf$flage_type,".png")
			save_plot <- paste0(conf$out,"/plot_",conf$count+conf$maxdetail,'_',conf$flage_type)
			save_plot_new <- paste0(conf$out,"/plot_",conf$count+conf$maxdetail,'_',conf$flage_type,".RData")
		}	

		
		
		
        ggsave(main_file,plot=p,width=conf$download$width,height=conf$download$height,dpi=conf$download$res)
		
       
        save(p,file=save_plot)
		save(p,file=save_plot_new)


		for (type in conf$download$format){	
			if(length(conf$flage_type)==0){
				main_file = paste0(conf$out,"/ordination_",conf$count+conf$maxdetail,".",type)
		#print(main_file)	
			}else{
				main_file = paste0(conf$out,"/ordination_",conf$count+conf$maxdetail,'_',conf$flage_type,".",type)
			}	
			ggsave(main_file,width=conf$download$width,height=conf$download$height,dpi=conf$download$res,plot=p)
		}
	}
}

err_min <- function(x){
    mean(x)-sd(x)
}

err_max <- function(x){
    mean(x)+sd(x)
}


barmean_plot <- function(df,feature,comparison){
	
	library(ggsignif)
	nam_all <- names(df)
	name_leave = nam_all[which(nam_all!=feature$group)]
	p <- ggplot(data=df,aes(get(feature$group),get(name_leave[1])))
	if (feature$fill == '--- no mapping ---'){
		p <- p + stat_summary(geom="bar",fun=mean,color='black') 
	}
	else{
		p <- p + stat_summary(geom="bar",fun=mean,aes(fill=get(feature$group)),color='black')
		#p <- p + stat_summary(geom="bar",fun=mean,aes(linetype=get(feature$group))) 
	}

	if(feature$err_bar != 'no'){
		#p <- p + stat_summary(geom="errorbar",fun.min=err_min,fun.max=err_max,width=0.2)
		p <- p + stat_summary(geom="errorbar",fun.min=mean,fun.max=err_max,width=0.2)
	}


	if(feature$markers == 'no'){
		p <- p + labs(x=feature$group,y=name_leave,fill=feature$group)
		return (p)
	}
	else{
		if(feature$asterisk == 'no'){
			feature$asterisk=F
		}
		else{
			feature$asterisk=T
		}
		#print(feature)
		#print(comparison)
		#print(str(df))
		p <- p + geom_signif(comparisons=comparison,test=feature$smethod,step_increase=feature$interval,map_signif_level =feature$asterisk,size=feature$size,textsize=feature$textsize)
		p <- p + labs(x=feature$group,y=name_leave,fill=feature$group)
		return(p)		

	}

}

boxmean_plot <- function(df,feature,comparison){
	library(ggpubr)
	nam_all <- names(df)
	name_leave = nam_all[which(nam_all!=feature$group)]
	#palette aaas,lancet not yet test
	#best ucscgb
	palette_multicolor = "ucscgb"
	if (feature$fill == 'no' && feature$err_bar=='no'){
		p <- ggboxplot(df,x=(feature$group),y=(name_leave),palette=palette_multicolor) 
	}
	else if(feature$fill != 'no' && feature$err_bar=='no'){
		p <- ggboxplot(df,x=(feature$group),y=(name_leave),palette=palette_multicolor,color=feature$group)
		#print("yes")
	}
	else if(feature$fill != 'no' && feature$err_bar=='yes'){
		p <- ggboxplot(df,x=(feature$group),y=(name_leave),palette=palette_multicolor,color=feature$group,add='jitter')
	}
	else{
		p <- ggboxplot(df,x=(feature$group),y=(name_leave),palette=palette_multicolor,add='jitter')
	}

	if(feature$markers == 'no'){
		return (p)
	}
	else{
		#p <- p + stat_compare_means()

		if(feature$asterisk == 'no'){
			p <- p+ stat_compare_means(comparisons=comparison,method=feature$smethod)
		}
		else{
			p <- p + stat_compare_means(comparisons=comparison,label='p.signif',method=feature$smethod)
		}

	}
}

barcharts_plot <- function(df,feature,comparison){
	library(ggsignif)
	nam_all <- names(df)
	name_leave = nam_all[which(nam_all!=feature$group)]
	name_leave = name_leave[which(name_leave!=feature$subgroup)]

	if(feature$markers == 'no'){
		

		
			p <- ggplot(data=df,aes(get(feature$group),get(name_leave[1]),fill=get(feature$subgroup))) + geom_bar(stat = "summary", fun = mean, color = "black", position = position_dodge())

		if(feature$err_bar != 'no'){
			#p <- p + stat_summary(fun.min=err_min,fun.max=err_max, geom = "errorbar", colour = "black",width = 0.25,position = position_dodge( .9))
			p <- p + stat_summary(fun.max=err_max, fun.min=mean,geom = "errorbar", colour = "black",width = 0.25,position = position_dodge( .9))
		}
		p <- p + labs(x=feature$group,y=name_leave,fill=feature$subgroup)
		return(p)
	}
	else{
		p <- ggplot(data=df,aes(get(feature$subgroup),get(name_leave[1])))
		if (feature$fill == 'no'){
			p <- p + stat_summary(geom="bar",fun=mean,color='black') 
		}
		else{
			p <- p + stat_summary(geom="bar",fun=mean,aes(fill=get(feature$fill)),color='black')
		}

		if(feature$err_bar != 'no'){
			#p <- p + stat_summary(fun.min=err_min,fun.max=err_max, geom = "errorbar", colour = "black",width = 0.25,position = position_dodge( .9))
			p <- p + stat_summary(fun.max=err_max, fun.min=mean,geom = "errorbar", colour = "black",width = 0.25,position = position_dodge( .9))
		}

		p <- p + facet_wrap(~get(feature$group),nrow=1,strip.position='bottom')
		if(feature$asterisk == 'no'){
			feature$asterisk=F
		}
		else{
			feature$asterisk=T
		}
		#print('yes we can')
		#print(str(comparison))
		#print('am i')
		#print(str(df))
		p <- p + geom_signif(comparisons=comparison,step_increase=feature$interval,map_signif_level =feature$asterisk,size=feature$size,textsize=feature$textsize,test=feature$smethod) + theme(axis.text.x=element_blank(),panel.spacing.x=unit(0,'pt'),strip.background=element_blank(),axis.ticks.length =unit(0,'pt')) + labs(x=feature$group,y=name_leave,fill=feature$subgroup)
		return(p)
	}
}

boxcharts_plot <- function(df,feature,comparison){
	library(ggpubr)
	nam_all <- names(df)
	name_leave = nam_all[which(nam_all!=feature$group)]
	name_leave = name_leave[which(name_leave!=feature$subgroup)]
	if(feature$markers == 'no'){
		if(feature$fill == 'no' && feature$err_bar=='no'){
			

			p <- ggboxplot(df,x=(feature$group),y=name_leave,palette='jco')
		}
		else if(feature$fill != 'no' && feature$err_bar=='no'){
			p <- ggboxplot(df,x=(feature$group),y=name_leave,palette='jco',color=(feature$fill))
		}
		else if(feature$fill != 'no' && feature$err_bar=='yes'){
			p <- ggboxplot(df,x=(feature$group),y=name_leave,palette='jco',color=(feature$fill),add='jitter')
		}
		else{
			p <- ggboxplot(df,x=(feature$group),y=name_leave,palette='jco',add='jitter')
		}			
	}
	else{
		if(feature$fill == 'no' && feature$err_bar=='no'){
			p <- ggboxplot(df,x=(feature$subgroup),y=name_leave,palette='jco') + facet_wrap(~get(feature$group),nrow=1,strip.position='bottom')+ theme_grey() +theme(axis.text.x=element_blank(),panel.spacing.x=unit(0,'pt'),strip.background=element_blank(),axis.ticks.length =unit(0,'pt'))
			if(feature$asterisk == 'no'){
				p <- p+ stat_compare_means(comparisons=comparison,method=feature$smethod)
			}
			else{
				p <- p + stat_compare_means(comparisons=comparison,label='p.signif',method=feature$smethod)
			}
			
		}
		else if(feature$fill != 'no' && feature$err_bar=='no'){
			p <- ggboxplot(df,x=(feature$subgroup),y=name_leave,palette='jco',color=(feature$fill))+ facet_wrap(~get(feature$group),nrow=1,strip.position='bottom')+ theme_grey() +theme(axis.text.x=element_blank(),panel.spacing.x=unit(0,'pt'),strip.background=element_blank(),axis.ticks.length =unit(0,'pt'))
			if(feature$asterisk == 'no'){
				p <- p+ stat_compare_means(comparisons=comparison,method=feature$smethod)
			}
			else{
				p <- p + stat_compare_means(comparisons=comparison,label='p.signif',method=feature$smethod)
			}
		}
		else if(feature$fill != 'no' && feature$err_bar=='yes'){
			p <- ggboxplot(df,x=(feature$subgroup),y=name_leave,palette='jco',color=(feature$fill),add='jitter')+ facet_wrap(~get(feature$group),nrow=1,strip.position='bottom')+ theme_grey() +theme(axis.text.x=element_blank(),panel.spacing.x=unit(0,'pt'),strip.background=element_blank(),axis.ticks.length =unit(0,'pt'))
			#p <- ggboxplot(df,x=(feature$subgroup),y=name_leave,palette='jco',color=(feature$fill),add='jitter')
			if(feature$asterisk == 'no'){
				p <- p+ stat_compare_means(comparisons=comparison,method=feature$smethod)
			}
			else{
				p <- p + stat_compare_means(comparisons=comparison,label='p.signif',method=feature$smethod)
			}
		}
		else{
			p <- ggboxplot(df,x=(feature$subgroup),y=name_leave,palette='jco',add='jitter')+ facet_wrap(~get(feature$group),nrow=1,strip.position='bottom')+ theme_grey() +theme(axis.text.x=element_blank(),panel.spacing.x=unit(0,'pt'),strip.background=element_blank(),axis.ticks.length =unit(0,'pt'))
			if(feature$asterisk == 'no'){
				p <- p+ stat_compare_means(comparisons=comparison,method=feature$smethod)
			}
			else{
				p <- p + stat_compare_means(comparisons=comparison,label='p.signif',method=feature$smethod)
			}
		}			
	}	
}

construct_allcomp <- function(data,feature){
	group <- as.character(data[[feature$group]])
	uniq_group <- group[!duplicated(group)]
	if(length(feature$compare_first) != 0){
		compaired=list()
		for( i in 1:length(uniq_group)){
			if(i != 1){
				compaired <- c(compaired,list(c(uniq_group[1],uniq_group[i])))
				
			}
		}
		#print(str(compaired))
		return(compaired)
	}
	
	compaired <- combn(uniq_group,2,simplify=F)
	#print(str(compaired))
	return (compaired)
}

bar_mean <- function(data,feature,subtype){
	
	comparisons <- construct_allcomp(data,feature)
	p <- barmean_plot(data,feature,comparisons)	
	
	attr(p,'class')=c(attr(p,'class'),'visomics,barplots,1,0')
	plot_continue(p,feature)
	
}

box_mean <- function(data,feature,subtype){
	
		comparisons <- construct_allcomp(data,feature)
		p <- boxmean_plot(data,feature,comparisons)
	
		attr(p,'class') = c(attr(p,'class'),'visomics,boxplots,1,0')
		plot_continue(p,feature)
	
}

vendiagram_plot <- function(data,feature){
	library('ggvenn')
	if(feature$show_elements == 'False'){
		if(feature$show_percentage == 'True'){
			p <- ggvenn(data,names(data),show_elements=FALSE,show_percentage=T,label_sep=feature$label_sep,digits=as.numeric(feature$digits),stroke_color=feature$stroke_color,stroke_alpha=as.numeric(feature$stroke_alpha),stroke_size=as.numeric(feature$stroke_size),stroke_linetype=feature$stroke_linetype,set_name_color=feature$set_name_color,set_name_size=as.numeric(feature$set_name_size),text_color=feature$text_color,text_size=as.numeric(feature$text_size))
		}else{
			p <- ggvenn(data,names(data),show_elements=FALSE,show_percentage=F,label_sep=feature$label_sep,digits=as.numeric(feature$digits),stroke_color=feature$stroke_color,stroke_alpha=as.numeric(feature$stroke_alpha),stroke_size=as.numeric(feature$stroke_size),stroke_linetype=feature$stroke_linetype,set_name_color=feature$set_name_color,set_name_size=as.numeric(feature$set_name_size),text_color=feature$text_color,text_size=as.numeric(feature$text_size))

		}
	}else{
		if(feature$show_percentage == 'True'){
			p <- ggvenn(data,names(data),show_elements=TRUE,show_percentage=T,label_sep=feature$label_sep,digits=as.numeric(feature$digits),stroke_color=feature$stroke_color,stroke_alpha=as.numeric(feature$stroke_alpha),stroke_size=as.numeric(feature$stroke_size),stroke_linetype=feature$stroke_linetype,set_name_color=feature$set_name_color,set_name_size=as.numeric(feature$set_name_size),text_color=feature$text_color,text_size=as.numeric(feature$text_size))

		}else{
			p <- ggvenn(data,names(data),show_elements=TRUE,show_percentage=F,label_sep=feature$label_sep,digits=as.numeric(feature$digits),stroke_color=feature$stroke_color,stroke_alpha=as.numeric(feature$stroke_alpha),stroke_size=as.numeric(feature$stroke_size),stroke_linetype=feature$stroke_linetype,set_name_color=feature$set_name_color,set_name_size=as.numeric(feature$set_name_size),text_color=feature$text_color,text_size=as.numeric(feature$text_size))
		}
	}
	p <- p + scale_fill_brewer(palette="Set2")
	
	attr(p,'class') = c(attr(p,'class'),'visomics,vendiagram,1,0')
	plot_continue(p,feature)
}

construct_charts <- function(data,feature){
	#全部都做
	sub <- unique(data[[feature$subgroup]])
	#print(str(sub))
	if(length(feature$compare_first) != 0){
		compaired=list()
		for( i in 1:length(sub)){
			if(i != 1){
				compaired <- c(compaired,list(c(as.numeric(sub[1]),as.numeric(sub[i]))))
				#print(str(compaired))
			}
		}
		
		return(compaired)
	}

	compaired <- combn(sub,2,simplify=F)
	compaired <- lapply(compaired,as.numeric)
	return(compaired)
}

bar_charts <- function(data,feature,subtype){
	
		comparisons <- construct_charts(data,feature)
		p <- barcharts_plot(data,feature,comparisons)
		attr(p,'class') = c(attr(p,'class'),'visomics,barplots,1,0')
		plot_continue(p,feature)
		
}

box_charts <- function(data,feature,subtype){
	
		comparisons <- construct_charts(data,feature)
		p <- boxcharts_plot(data,feature,comparisons)
		attr(p,'class') = c(attr(p,'class'),'visomics,boxplots,1,0')
		plot_continue(p,feature)
	
}

pcoa_main <- function(data,feature,subtype){
	
	res <- pcoa_count(data,feature,subtype)
	download_save <- pca_save_file(res,feature,data)
	p <- plotpcoa(download_save,feature)

	attr(p,'class') = c(attr(p,'class'),'visomics,ordinations,1,0')
	#feature$maxdetail=0
	plot_continue(p,feature)
	
	
	#main_file = paste0(feature$out,"/ordination.png")
	#ggsave(main_file,plot=p)
	#save_plot <- paste0(feature$out,"/plotmain")
	#save(p,file=save_plot)
}

pca_save_file <- function(res,feature,data){
    res_file = paste0(feature$out,"/res.Data")
    save(res,file=res_file)

    sre_file = paste0(feature$out,'/sre.txt')
    write.table(res$values,sre_file,sep='\t')

    score_file = paste0(feature$out,'/score.txt')
    write.table(as.data.frame(res$vectors),score_file,sep='\t')

    if(feature$prior_eip=='no'){
    xlab_new <- paste0("PC1(",round(res$values$Relative_eig[1]*100,2),"%)")
    ylab_new <- paste0("PC2(",round(res$values$Relative_eig[2]*100,2),"%)")
	}else{
		if(is.null(res$values$Rel_corr_eig)){
			print("There were no negative eigenvalues. No correction was applied")
			xlab_new <- paste0("PC1(",round(res$values$Relative_eig[1]*100,2),"%)")
    		ylab_new <- paste0("PC2(",round(res$values$Relative_eig[2]*100,2),"%)")
		}else{
			print("correction")
			xlab_new <- paste0("PC1(",round(res$values$Rel_corr_eig[1]*100,2),"%)")
    		ylab_new <- paste0("PC2(",round(res$values$Rel_corr_eig[2]*100,2),"%)")
		}
	}
    lab_new <- c(xlab_new,ylab_new)

    download_save = res$vectors[,1:2]
    download_save = as.data.frame(download_save)
    colnames(download_save)=lab_new
    rownames(download_save)= rownames(data)
    download_save$text = rownames(data)


    if (length(feature$group) != 0){
        download_save$group = data[[feature$group]]
		download_save$group = as.factor(download_save$group)
    }

#temp
	if (length(feature$double) != 0){

		download_save <- rbind(download_save,download_save)
		#print("double")
	}
#temp


    download_file = paste0(feature$out,"/out.txt")
    write.table(download_save,download_file,sep='\t')

	return(download_save)
}

plotpcoa <- function(df,feature){
	if (length(df$group)!=0 && length(feature$typed)!=0){
		p1 <- ggplot(data=df,aes(get(colnames(df)[1]),get(colnames(df)[2]),color=group)) + stat_ellipse(aes(colour=group),type=feature$typed,level = as.numeric(feature$level),show.legend=F) + geom_point() + scale_colour_brewer(palette="Set1") + labs(x=colnames(df)[1],y=colnames(df)[2])
	}
	if (length(df$group)!=0 && length(feature$typed)==0){
		p1 <- ggplot(data=df,aes(get(colnames(df)[1]),get(colnames(df)[2]),color=group)) + geom_point() + scale_colour_brewer(palette="Set1") + labs(x=colnames(df)[1],y=colnames(df)[2])
	}
	if(length(df$group)==0){
		p1 <- ggplot(data=df,aes(get(colnames(df)[1]),get(colnames(df)[2]))) + geom_point() + labs(x=colnames(df)[1],y=colnames(df)[2])
	}	
	return(p1)
}

exgroup <- function(data,feature){
	#print(feature)
	if (length(feature$group) != 0){
		data <- data[,which(colnames(data)!=feature$group)]
	}
	return (data)
}

standard <- function(data,feature){
	if (feature$stand == "yes"){
		data <- apply(data,2,scale,center=T,scale=T)
	}
	return (data)
}


pcoa_count <- function(data,feature,subtype){
	library(vegan)
	library(ape)
	#print(feature)
	
	data <- exgroup(data,feature)
	print(str(data))
	data <- standard(data,feature)

	#print(feature$distance)
	#dis <- vegdist(data,feature$distance)
	#missing value are not allowed with argument 'na.rm=FALSE'
	dis <- vegdist(data,feature$distance,na.rm=TRUE)
	if(feature$prior_eip=='yes'){
		print("prior")
		res <- pcoa(dis,correction =feature$corr_meth)
	}else{
		res <- pcoa(dis)
	}
	if(subtype=="11"){
		sre <- paste0(feature$out,"/sreplot.png")
		bio <- paste0(feature$out,"/biplot.png")

		x_num <- length(colnames(res$vectors))
		print(str(res))
		data_bar <- data.frame(x=colnames(res$vectors),y=res$values$Eigenvalues[1:x_num])
		data_bar$x <- factor(data_bar$x,levels=data_bar$x)
		sre_plot <- ggplot(data_bar,aes(x,y)) + geom_bar(stat="identity",fill="black") + scale_y_continuous(expand=c(0,0)) + theme_classic()
		ggsave(sre,plot=sre_plot)
		
		#print(str(res))
		#print(str(data))

		png(bio)
		bio_plot = biplot(res,data)
		#print(bio_plot)
		dev.off()
		#ggsave(bio,plot=bio_plot)
	}

	return (res)
}

main()
