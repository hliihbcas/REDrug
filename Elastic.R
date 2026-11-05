#!/home/hli/anaconda3/envs/rp48/bin/Rscript
library(glmnet)
library(dplyr)
set.seed(123)
args = commandArgs(T)
L1000_datasets_f = args[1]
real_positive_f = args[2]
runs = args[3]
runs = as.numeric(runs)
L1000_datasets = read.delim(L1000_datasets_f,stringsAsFactors=F)
real_positive = read.delim(real_positive_f,stringsAsFactors=F)
print(nrow(L1000_datasets))
#f9
positive_L1000_datasets = subset(L1000_datasets,DAP12.interactions.NES>0 & DAP12.interactions.P.adjust<0.05 & TCR.signaling.NES>0 & TCR.signaling.P.adjust<0.05 & Hemostasis.NES>0 & Hemostasis.P.adjust<0.05 & Cytokine.Signaling.in.Immune.system.NES >0 & Cytokine.Signaling.in.Immune.system.P.adjust<0.05 & Neutrophil.degranulation.NES>0 & Neutrophil.degranulation.P.adjust<0.05 & Lymphoid.and.a.non.Lymphoid.cell.NES >0 & Lymphoid.and.a.non.Lymphoid.cell.P.adjust<0.05 & Class.A.1..Rhodopsin.like.receptors..NES > 0 & Class.A.1..Rhodopsin.like.receptors..P.adjust<0.05 ,c(DAP12.interactions.NES,TCR.signaling.NES,Hemostasis.NES,Cytokine.Signaling.in.Immune.system.NES,Neutrophil.degranulation.NES,Class.A.1..Rhodopsin.like.receptors..NES,Lymphoid.and.a.non.Lymphoid.cell.NES),drop=F)
print(paste0('候选样本: ',nrow(positive_L1000_datasets)))

#f9
L1000_datasets = subset(L1000_datasets,DAP12.interactions.NES<0 & DAP12.interactions.P.adjust<0.05 & TCR.signaling.NES<0 & TCR.signaling.P.adjust<0.05 & Hemostasis.NES<0 & Hemostasis.P.adjust<0.05 & Cytokine.Signaling.in.Immune.system.NES <0 & Cytokine.Signaling.in.Immune.system.P.adjust<0.05 & Neutrophil.degranulation.NES<0 & Neutrophil.degranulation.P.adjust<0.05 & Lymphoid.and.a.non.Lymphoid.cell.NES <0 & Lymphoid.and.a.non.Lymphoid.cell.P.adjust<0.05 & Class.A.1..Rhodopsin.like.receptors..NES < 0 & Class.A.1..Rhodopsin.like.receptors..P.adjust<0.05 ,c(DAP12.interactions.NES,TCR.signaling.NES,Hemostasis.NES,Cytokine.Signaling.in.Immune.system.NES,Neutrophil.degranulation.NES,Class.A.1..Rhodopsin.like.receptors..NES,Lymphoid.and.a.non.Lymphoid.cell.NES),drop=F)

L1000_datasets$label = rep(0,nrow(L1000_datasets))
print(nrow(L1000_datasets))
print(nrow(real_positive))

real_positive = subset(real_positive, DAP12.interactions.NES>0 & DAP12.interactions.P.adjust<0.05 & TCR.signaling.NES>0 & TCR.signaling.P.adjust<0.05 & Hemostasis.NES>0 & Hemostasis.P.adjust<0.05 & Cytokine.Signaling.in.Immune.system.NES >0 & Cytokine.Signaling.in.Immune.system.P.adjust<0.05 & Neutrophil.degranulation.NES>0 & Neutrophil.degranulation.P.adjust<0.05 & Lymphoid.and.a.non.Lymphoid.cell.NES >0 & Lymphoid.and.a.non.Lymphoid.cell.P.adjust<0.05 & Class.A.1..Rhodopsin.like.receptors..NES > 0 & Class.A.1..Rhodopsin.like.receptors..P.adjust<0.05 ,c(DAP12.interactions.NES,TCR.signaling.NES,Hemostasis.NES,Cytokine.Signaling.in.Immune.system.NES,Neutrophil.degranulation.NES,Class.A.1..Rhodopsin.like.receptors..NES,Lymphoid.and.a.non.Lymphoid.cell.NES),drop=F)

real_positive$label = rep(1,nrow(real_positive))
print(nrow(real_positive))
#print(ncol(real_positiv))
result=rep(0.0,ncol(real_positive))
for (i in rep(1,runs)){

L1000_datasets_small = L1000_datasets[sample(nrow(L1000_datasets),nrow(real_positive)),]


if (mean(names(real_positive) == names(L1000_datasets_small))!=1){
	stop('names not same')
}else{
	mean(c(1,2))
}
nes_data = rbind(real_positive, L1000_datasets_small)
X <- as.matrix(nes_data %>% select(-label))
y <- nes_data$label

lasso_model <- cv.glmnet(X, y, 
                         family = "binomial",  # 二分类问题
                         alpha = 0.9,           # alpha=1表示LASSO
                         nfolds = 10)         # 10折交叉验证

optimal_lambda <- lasso_model$lambda.min
coefficients <- coef(lasso_model, s = optimal_lambda)
coefficients_results = as.vector(coefficients)
}
print(coefficients)
result_ave = result/runs
print(result_ave)
