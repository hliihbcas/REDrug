import sys
fold_name = sys.argv[1]
out_f = fold_name+"/combination_result.txt"

result_dict={}

file_Phase2= fold_name+"/result.txt"
file_Phase2_f = open(file_Phase2).readlines()
for iterm in file_Phase2_f[1:]:
    iterm_list = iterm.strip().split('\t')
    key = iterm_list[0]+":"+iterm_list[1]
    if result_dict.get(key)==None:
        result_dict[key]={'Phase2':{'infos':[[str(iterm_list[2]),str(iterm_list[3]),str(iterm_list[-1])]],'RED':float(iterm_list[-1])}}
    else:
        temp_list = [iterm_list[2],iterm_list[3],iterm_list[-1]]
        tRED=float(iterm_list[-1])
        result_dict[key]['Phase2']['infos'].append(temp_list)
        if result_dict[key]['Phase2']['RED'] < tRED:
            result_dict[key]['Phase2']['RED']=tRED

#print(result_dict["BRD-K91315211:betahistine"])

Phase3_lists_all=[]

file_Phase3_6 = fold_name+"/result_CMAP_6.txt"
file_Phase3_6_f = open(file_Phase3_6).readlines()

file_Phase3_24 = fold_name+"/result_CMAP_24.txt"
file_Phase3_24_f = open(file_Phase3_24).readlines()

file_Phase3_6_HCT116 = fold_name+"/result_CMAP_HCT116_6.txt"
file_Phase3_6_HCT116_f = open(file_Phase3_6_HCT116).readlines()

file_Phase3_6_LOVO = fold_name+"/result_CMAP_LOVO_6.txt"
file_Phase3_6_LOVO_f = open(file_Phase3_6_LOVO).readlines()

Phase3_lists_all= file_Phase3_6_f[1:]+file_Phase3_24_f[1:]

for iterm in Phase3_lists_all:
    iterm_list = iterm.strip().split('\t')
    key = iterm_list[0]+":"+iterm_list[1]
    if result_dict.get(key)==None:
        result_dict[key]={'Phase3':{'infos':[[str(iterm_list[2]),str(iterm_list[3]),str(iterm_list[-1])]],'RED':float(iterm_list[-1])}}
    else:
        if result_dict[key].get('Phase3')!=None:
            temp_list = [iterm_list[2],iterm_list[3],iterm_list[-1]]
            tRED=float(iterm_list[-1])
            result_dict[key]['Phase3']['infos'].append(temp_list)
            if result_dict[key]['Phase3']['RED'] < tRED:
                result_dict[key]['Phase3']['RED']=tRED
        else:
            result_dict[key]['Phase3']={'infos':[[str(iterm_list[2]),str(iterm_list[3]),str(iterm_list[-1])]],'RED':float(iterm_list[-1])}

for iterm in file_Phase3_6_HCT116_f[1:]:
    iterm_list = iterm.strip().split('\t')
    key = iterm_list[0]+":"+iterm_list[1]
    if result_dict.get(key)==None:
        result_dict[key]={'Phase3':{'infos':[[str(iterm_list[2]),str(iterm_list[3]),str(iterm_list[-1]),'HCT116']],'RED':float(iterm_list[-1])}}
    else:
        if result_dict[key].get('Phase3')!=None:
            temp_list = [iterm_list[2],iterm_list[3],iterm_list[-1],'HCT116']
            tRED=float(iterm_list[-1])
            result_dict[key]['Phase3']['infos'].append(temp_list)
            if result_dict[key]['Phase3']['RED'] < tRED:
                result_dict[key]['Phase3']['RED']=tRED
        else:
            result_dict[key]['Phase3']={'infos':[[str(iterm_list[2]),str(iterm_list[3]),str(iterm_list[-1]),'HCT116']],'RED':float(iterm_list[-1])}


for iterm in file_Phase3_6_LOVO_f[1:]:
    iterm_list = iterm.strip().split('\t')
    key = iterm_list[0]+":"+iterm_list[1]
    if result_dict.get(key)==None:
        result_dict[key]={'Phase3':{'infos':[[str(iterm_list[2]),str(iterm_list[3]),str(iterm_list[-1]),'LOVO']],'RED':float(iterm_list[-1])}}
    else:
        if result_dict[key].get('Phase3')!=None:
            temp_list = [iterm_list[2],iterm_list[3],iterm_list[-1],'LOVO']
            tRED=float(iterm_list[-1])
            result_dict[key]['Phase3']['infos'].append(temp_list)
            if result_dict[key]['Phase3']['RED'] < tRED:
                result_dict[key]['Phase3']['RED']=tRED
        else:
            result_dict[key]['Phase3']={'infos':[[str(iterm_list[2]),str(iterm_list[3]),str(iterm_list[-1]),'LOVO']],'RED':float(iterm_list[-1])}

result_dict_lists = []

print(len(result_dict))
print(result_dict["BRD-K91315211:betahistine"])
for keys in result_dict:
    temp_keys = [keys.split(":")[0],keys.split(":")[1]]
    if result_dict[keys].get('Phase2')!=None:
        tp2_counts = str(len(result_dict[keys]['Phase2']['infos']))
        tp2_RED = str(result_dict[keys]['Phase2']['RED'])
        tp2_infos = ['/'.join(i) for i in result_dict[keys]['Phase2']['infos']]
        tp2_infos_str = "; ".join(tp2_infos)
    else:
        tp2_counts='0'
        tp2_RED="NA"
        tp2_infos_str="NA"
    temp_keys.append(tp2_counts)
    temp_keys.append(tp2_RED)
    temp_keys.append(tp2_infos_str)

    if result_dict[keys].get('Phase3')!=None:
        tp3_counts = str(len(result_dict[keys]['Phase3']['infos']))
        tp3_RED = str(result_dict[keys]['Phase3']['RED'])
        tp3_infos = ['/'.join(i) for i in result_dict[keys]['Phase3']['infos']]
        tp3_infos_str = "; ".join(tp3_infos)
    else:
        tp3_counts='0'
        tp3_RED="NA"
        tp3_infos_str="NA"
    temp_keys.append(tp3_counts)
    temp_keys.append(tp3_RED)
    temp_keys.append(tp3_infos_str)
    tep_sum = int(temp_keys[2])+int(temp_keys[5])
    temp_keys.append(tep_sum)
    result_dict_lists.append(temp_keys)

sorted_result_dict_lists = sorted(result_dict_lists, key=lambda x: x[-1], reverse=True)

for i in range(len(sorted_result_dict_lists)):
    sorted_result_dict_lists[i][-1]=str(sorted_result_dict_lists[i][-1])
    sorted_result_dict_lists[i] = "\t".join(sorted_result_dict_lists[i])

sorted_result_dict_lists_str = "\n".join(sorted_result_dict_lists)
out_ff = open(out_f,'w')
out_ff.write(sorted_result_dict_lists_str)
out_ff.close()
