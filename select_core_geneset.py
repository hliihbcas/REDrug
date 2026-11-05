import sys
from collections import Counter

fold_file = sys.argv[1]
quesry_term = sys.argv[2]
out_f = sys.argv[3]
flagef = sys.argv[4]

result_genes=[]
fold_files = open(fold_file).readlines()
print(quesry_term)
colock_d= 0
for i in fold_files:
    i_f = i.strip()
    if flagef=="mouse":
        if "human" in i:
            file_temp = "/home/hli/NetGNN/visomics-1.2/enrich/"+i_f+"/sig_pathways_mouse.txt"
        else:
            file_temp = "/home/hli/NetGNN/visomics-1.2/enrich/"+i_f+"/sig_pathways.txt"
    else:
        if "human" in i:
            file_temp = "/home/hli/NetGNN/visomics-1.2/enrich/"+i_f+"/sig_pathways.txt"
        else:
            file_temp = "/home/hli/NetGNN/visomics-1.2/enrich/"+i_f+"/sig_pathways_human.txt"

    file_temps = open(file_temp).readlines()
    for n in file_temps:
        n_list = n.strip().split('\t')
        back_term = n_list[1]
        if quesry_term!=back_term:continue
        colock_d+=1
        str_ids = n_list[-1].strip().split(" ")[-1]
        #print(str_ids)
        str_ids_lists = str_ids.split("/")
        result_genes+=str_ids_lists

print(colock_d)

result=Counter(result_genes)
print(result)
result_n = []
for key in result:
    if float(result[key])/float(colock_d) >= 0.7:
        result_n.append(str(key))

end_str = "\n".join(result_n)
out_ff = open(out_f+".txt",'w')
out_ff.write(end_str)
out_ff.close()

result_n_str_new= "\t".join(result_n)
end_str_new = quesry_term+"\tNA\t"+result_n_str_new+"\n"
out_ff_new = open(out_f+".gmt",'w')
out_ff_new.write(end_str_new)
out_ff_new.close()


