import sys,os
fold_names = sys.argv[1]
sample_f = sys.argv[2]
timesf = sys.argv[3]
outf = sys.argv[4]
all_items = os.listdir(fold_names)
subdirectories = [item for item in all_items if os.path.isdir(os.path.join(fold_names, item))]

sample_fs = open(sample_f).readlines()
sample_fs_dict={}
for i in sample_fs:
    i_list = i.strip().split('\t')
    sample_fs_dict[i_list[7]] = i_list[8]

counts_results = ["GSE_id\tpert_iname\tpert_dose\ttime\tAntigen Processing and Presentation NES\tCell surface interactions NES\tChemokine receptors bind chemokines NES\tClass A/1 (Rhodopsin-like receptors) NES\tCostimulation by the CD28 family NES\tCytokine Signaling in Immune system NES\tDAP12 interactions NES\tDAP12 signaling NES\tG alpha (i) signalling events NES\tGeneration of second messenger molecules NES\tGPCR downstream signalling NES\tGPCR ligand binding NES\tHemostasis NES\tLymphoid and a non-Lymphoid cell NES\tInterleukin-2 family signaling NES\tNeutrophil degranulation NES\tPD-1 signaling NES\tPeptide ligand-binding receptors NES\tPhosphorylation of CD3 and TCR zeta chains NES\tSignaling by GPCR NES\tSignaling by Interleukins NES\tTCR signaling NES\tAntigen Processing and Presentation P-adjust\tCell surface interactions P-adjust\tChemokine receptors bind chemokines P-adjust\tClass A/1 (Rhodopsin-like receptors) P-adjust\tCostimulation by the CD28 family P-adjust\tCytokine Signaling in Immune system P-adjust\tDAP12 interactions P-adjust\tDAP12 signaling P-adjust\tG alpha (i) signalling events P-adjust\tGeneration of second messenger molecules P-adjust\tGPCR downstream signalling P-adjust\tGPCR ligand binding P-adjust\tHemostasis P-adjust\tLymphoid and a non-Lymphoid cell P-adjust\tInterleukin-2 family signaling P-adjust\tNeutrophil degranulation P-adjust\tPD-1 signaling P-adjust\tPeptide ligand-binding receptors P-adjust\tPhosphorylation of CD3 and TCR zeta chains P-adjust\tSignaling by GPCR P-adjust\tSignaling by Interleukins P-adjust\tTCR signaling P-adjust"]


for i in subdirectories:
    fold_i = os.path.join(fold_names, i)
    second_items = os.listdir(fold_i)
    real_paths = [second_item for second_item in second_items if os.path.isdir(os.path.join(fold_i, second_item))]

    for real_path in real_paths:
        fold_i_real = os.path.join(fold_i,real_path)
        fold_i_real_f = os.path.join(fold_i_real, timesf)
        
        results = os.path.join(fold_i_real_f,'gsea_result.txt')
        resultss = open(results).readlines()[1:]
        temp_dict = {}
        for x in resultss:            
            x_s = x.strip().split('\t')
            temp_dict[x_s[0]]=[x_s[4],x_s[6]]

        if temp_dict.get('Antigen Processing and Presentation')==None:
            temp_dict['Antigen Processing and Presentation']=['NA','NA']
        if temp_dict.get('Cell surface interactions')==None:
            temp_dict['Cell surface interactions']=['NA','NA']
        if temp_dict.get('Chemokine receptors bind chemokines')==None:
            temp_dict['Chemokine receptors bind chemokines']=['NA','NA']
        if temp_dict.get('Class A/1 (Rhodopsin-like receptors)')==None:
            temp_dict['Class A/1 (Rhodopsin-like receptors)']=['NA','NA']
        if temp_dict.get('Costimulation by the CD28 family')==None:
            temp_dict['Costimulation by the CD28 family']=['NA','NA']
        if temp_dict.get('Cytokine Signaling in Immune system')==None:
            temp_dict['Cytokine Signaling in Immune system']=['NA','NA']
        if temp_dict.get('DAP12 interactions')==None:
            temp_dict['DAP12 interactions']=['NA','NA']
        if temp_dict.get('DAP12 signaling')==None:
            temp_dict['DAP12 signaling']=['NA','NA']
        if temp_dict.get('G alpha (i) signalling events')==None:
            temp_dict['G alpha (i) signalling events']=['NA','NA']
        if temp_dict.get('Generation of second messenger molecules')==None:
            temp_dict['Generation of second messenger molecules']=['NA','NA']
        if temp_dict.get('GPCR downstream signalling')==None:
            temp_dict['GPCR downstream signalling']=['NA','NA']
        if temp_dict.get('GPCR ligand binding')==None:
            temp_dict['GPCR ligand binding']=['NA','NA']
        if temp_dict.get('Hemostasis')==None:
            temp_dict['Hemostasis']=['NA','NA']
        if temp_dict.get('Lymphoid and a non-Lymphoid cell')==None:
            temp_dict['Lymphoid and a non-Lymphoid cell']=['NA','NA']
        if temp_dict.get('Interleukin-2 family signaling')==None:
            temp_dict['Interleukin-2 family signaling']=['NA','NA']
        if temp_dict.get('Neutrophil degranulation')==None:
            temp_dict['Neutrophil degranulation'] = ['NA','NA']
        if temp_dict.get('PD-1 signaling')==None:
            temp_dict['PD-1 signaling'] = ['NA','NA']
        if temp_dict.get('Peptide ligand-binding receptors')==None:
            temp_dict['Peptide ligand-binding receptors'] = ['NA','NA']
        if temp_dict.get('Phosphorylation of CD3 and TCR zeta chains')==None:
            temp_dict['Phosphorylation of CD3 and TCR zeta chains'] = ['NA','NA']
        if temp_dict.get('Signaling by GPCR')==None:
            temp_dict['Signaling by GPCR'] = ['NA','NA']
        if temp_dict.get('Signaling by Interleukins')==None:
            temp_dict['Signaling by Interleukins'] = ['NA','NA']
        if temp_dict.get('TCR signaling')==None:
            temp_dict['TCR signaling'] = ['NA','NA']

        temp_listss=[i,sample_fs_dict[i],real_path,timesf,temp_dict.get('Antigen Processing and Presentation')[0],temp_dict.get('Cell surface interactions')[0],temp_dict.get('Chemokine receptors bind chemokines')[0],temp_dict.get('Class A/1 (Rhodopsin-like receptors)')[0],temp_dict.get('Costimulation by the CD28 family')[0],temp_dict.get('Cytokine Signaling in Immune system')[0],temp_dict.get('DAP12 interactions')[0],temp_dict.get('DAP12 signaling')[0],temp_dict.get('G alpha (i) signalling events')[0],temp_dict.get('Generation of second messenger molecules')[0],temp_dict.get('GPCR downstream signalling')[0],temp_dict.get('GPCR ligand binding')[0],temp_dict.get('Hemostasis')[0],temp_dict.get('Lymphoid and a non-Lymphoid cell')[0],temp_dict.get('Interleukin-2 family signaling')[0],temp_dict.get('Neutrophil degranulation')[0],temp_dict.get('PD-1 signaling')[0],temp_dict.get('Peptide ligand-binding receptors')[0],temp_dict.get('Phosphorylation of CD3 and TCR zeta chains')[0],temp_dict.get('Signaling by GPCR')[0],temp_dict.get('Signaling by Interleukins')[0],temp_dict.get('TCR signaling')[0],temp_dict.get('Antigen Processing and Presentation')[1],temp_dict.get('Cell surface interactions')[1],temp_dict.get('Chemokine receptors bind chemokines')[1],temp_dict.get('Class A/1 (Rhodopsin-like receptors)')[1],temp_dict.get('Costimulation by the CD28 family')[1],temp_dict.get('Cytokine Signaling in Immune system')[1],temp_dict.get('DAP12 interactions')[1],temp_dict.get('DAP12 signaling')[1],temp_dict.get('G alpha (i) signalling events')[1],temp_dict.get('Generation of second messenger molecules')[1],temp_dict.get('GPCR downstream signalling')[1],temp_dict.get('GPCR ligand binding')[1],temp_dict.get('Hemostasis')[1],temp_dict.get('Lymphoid and a non-Lymphoid cell')[1],temp_dict.get('Interleukin-2 family signaling')[1],temp_dict.get('Neutrophil degranulation')[1],temp_dict.get('PD-1 signaling')[1],temp_dict.get('Peptide ligand-binding receptors')[1],temp_dict.get('Phosphorylation of CD3 and TCR zeta chains')[1],temp_dict.get('Signaling by GPCR')[1],temp_dict.get('Signaling by Interleukins')[1],temp_dict.get('TCR signaling')[1]]

        counts_results.append('\t'.join(temp_listss))
counts_results_str = '\n'.join(counts_results)
outff = open(outf,'w')
outff.write(counts_results_str)
outff.close()
