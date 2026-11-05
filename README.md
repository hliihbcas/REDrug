# REDrug
###REDrug: a framework for screening immunotherapy-sensitizing compounds in CRC using immunotherapy-potentiating transcriptional signatures

### Difference Analysis
$ ./Rversion1.2.R de_analyse 11 ./deanalysis/GSE174252/data_deanalysis 0 ./deanalysis/GSE174252/feature_deanalysis > ./deanalysis/GSE174252/err.txt 2>&1 &

### Enrichment Analysis
$ ./Rversion1.2.R enrich_analyse 11 ./deanalysis/GSE174252/up_gene_result.txt 0 ./enrich/GSE174252/feature_enrich > ./enrich/GSE174252/err.txt 2>&1 & 

### GSEA
$ ./Rversion1.2.R enrich_gsea 11 ./deanalysis/GSE112252_untreated/gsea_data.txt 0 ./enrich/GSE112252_untreated/feature_enrich > ./enrich/GSE112252_untreated/err.txt 2>&1 &

### Enrichment Analysis of a Specified Gene Set
$ ./use_gsea_for_elastic.R ./enrich/msi_PRJNA727917_new/gsea_data.txt /home/hli/NetGNN/visomics-1.2/core_gene_new/human_all.gmt org.Hs.eg.db

### Screening of Core Gene Sets
$ python select_core_geneset.py mm_hs_fold.txt Hemostasis ./core_gene_new/Hemostasis_human human

### Compound Screening Using the L1000 Dataset (2017 level3)
$ ./2017level3_control_gsea.R ./small_crispr_Phase2_l3_GSE70138_level3_2017/LDS-1483/Data/HT29/GSE70138_Broad_LINCS_Level3_INF_mlr12k_n345976x12328_2017-03-06.gctx_HT29.tsv ./small_crispr_Phase2_l3_GSE70138_level3_2017/LDS-1483/Data/HT29/GSE70138_Broad_LINCS_inst_info_2017-03-06_HT29.txt /home/hli/NetGNN/visomics-1.2/core_gene_new/human_all.gmt ./small_crispr_Phase2_l3_GSE70138_level3_2017/LDS-1483/Data/HT29/density_ht29_zeroT.txt 24.0 ./small_crispr_Phase2_l3_GSE70138_level3_2017/LDS-1483/Data/HT29

### Compound Screening Using the L1000 Dataset (2020 level3)
$ ./2020CMAPlevel3_control_gsea.R ./CMAP_level3_2020/HT29/level3_beta_trt_cp_n1805898x12328.gctx_HT29.tsv ./CMAP_level3_2020/HT29/instinfo_beta_HT29.txt /home/hli/NetGNN/visomics-1.2/core_gene_new/human_all.gmt ./CMAP_level3_2020/HT29/density_ht29_zeroT.txt 96 ./CMAP_level3_2020/HT29 ./CMAP_level3_2020/controls/level3_beta_ctl_n188708x12328.txt

### Results Sorted by Batch
$ python count_GSEAresult_nes.py ./small_crispr_Phase2_l3_GSE70138_level3_2017/LDS-1483/Data/HT29 ./small_crispr_Phase2_l3_GSE70138_level3_2017/LDS-1483/Data/HT29/GSE70138_Broad_LINCS_inst_info_2017-03-06_HT29.txt 24 ./small_crispr_Phase2_l3_GSE70138_level3_2017/LDS-1483/Data/HT29/GSEAresult_nes_test.txt

### Integration Results
$ python result_excavation_combination.py ./REDValue/formu08

### Elastic model
$ ./Elastic.R ./small_crispr_Phase2_l3_GSE70138_level3_2017/LDS-1483/Data/HT29/GSEAresult_nes_test.txt /home/hli/NetGNN/visomics-1.2/LASSO_positive/GSEAresult_nes_test_simple.txt 5000

###**Before you start the process, here are a few things you need to know**  
1. rp48_environment.yaml is the main Conda environment configuration file.
2. The above primarily runs on an Ubuntu Server environment (Ubuntu 24.04.2 LTS). 
3. If you need detailed sample data files, please contact us at m17715629787@163.com | liheng19@mails.ucas.ac.cn | duanyou@outlook.com
