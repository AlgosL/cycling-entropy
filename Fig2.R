lat_esq<-rep("ESQ",100)
lat_dir<-rep("DIR",100)
mov_rep<-rep("REP",100)
mov_ped<-rep("PED",100)
olho_oa<-rep("OA",100)
olho_of<-rep("OF",100)
pos_c<-rep("C",100)
pos_f<-rep("F",100)
pos_p<-rep("P",100)
pos_o<-rep("O",100)
idd<-c("I1","I2","I3","I4","I5","I6","I7","I8","I12","I13","I14","I15","I17","I18","I20","I21","I22","I24")
sd_cri<-10; NN<-18; Ncol<-6;

oiPOA2<-read.table("SMax_2_col.dat")
oiROA2<-read.table("SMax_2_col_ROA.dat")
oiPOF2<-read.table("SMax_2_col_POF.dat")
oiROF2<-read.table("SMax_2_col_ROF.dat")

for(i in 1:NN){
  indiv<-rep(idd[i],100)
  
  pli2poa<-data.frame(indiv,olho_oa,mov_ped,pos_o,lat_dir,oiPOA2[,i])
  names(pli2poa)<-c("ind","olho","MOV","POS","LAT","dados")
  pli2pof<-data.frame(indiv,olho_of,mov_ped,pos_o,lat_dir,oiPOF2[,i])
  names(pli2pof)<-c("ind","olho","MOV","POS","LAT","dados")
  pli2roa<-data.frame(indiv,olho_oa,mov_rep,pos_o,lat_dir,oiROA2[,i])
  names(pli2roa)<-c("ind","olho","MOV","POS","LAT","dados")
  pli2rof<-data.frame(indiv,olho_of,mov_rep,pos_o,lat_dir,oiROF2[,i])
  names(pli2rof)<-c("ind","olho","MOV","POS","LAT","dados")
  
  pli<-rbind(pli2poa,pli2pof,pli2roa,pli2rof)
  #print(c(i,mean(pli[,Ncol]),sd(pli[,Ncol])))
  
  if(sd(pli[,Ncol])>sd_cri){pli[,Ncol]<-NA}
  
  if(i==1){pli2<-pli}else{ 
    pli2<-rbind(pli2,pli)}
}

oiPOA3<-read.table("SMax_3_col.dat")
oiROA3<-read.table("SMax_3_col_ROA.dat")
oiPOF3<-read.table("SMax_3_col_POF.dat")
oiROF3<-read.table("SMax_3_col_ROF.dat")

for(i in 1:NN){
  indiv<-rep(idd[i],100)
  
  pli3poa<-data.frame(indiv,olho_oa,mov_ped,pos_o,lat_esq,oiPOA3[,i])
  names(pli3poa)<-c("ind","olho","MOV","POS","LAT","dados")
  pli3pof<-data.frame(indiv,olho_of,mov_ped,pos_o,lat_esq,oiPOF3[,i])
  names(pli3pof)<-c("ind","olho","MOV","POS","LAT","dados")
  pli3roa<-data.frame(indiv,olho_oa,mov_rep,pos_o,lat_esq,oiROA3[,i])
  names(pli3roa)<-c("ind","olho","MOV","POS","LAT","dados")
  pli3rof<-data.frame(indiv,olho_of,mov_rep,pos_o,lat_esq,oiROF3[,i])
  names(pli3rof)<-c("ind","olho","MOV","POS","LAT","dados")
  
  pli<-rbind(pli3poa,pli3pof,pli3roa,pli3rof)
  #print(c(i,mean(pli[,Ncol]),sd(pli[,Ncol])))
  
  if(sd(pli[,Ncol])>sd_cri){pli[,Ncol]<-NA}
  
  if(i==1){pli3<-pli}else{ 
    pli3<-rbind(pli3,pli)}
}

oiPOA4<-read.table("SMax_4_col.dat")
oiROA4<-read.table("SMax_4_col_ROA.dat")
oiPOF4<-read.table("SMax_4_col_POF.dat")
oiROF4<-read.table("SMax_4_col_ROF.dat")

for(i in 1:NN){
  indiv<-rep(idd[i],100)
  
  pli4poa<-data.frame(indiv,olho_oa,mov_ped,pos_p,lat_dir,oiPOA4[,i])
  names(pli4poa)<-c("ind","olho","MOV","POS","LAT","dados")
  pli4pof<-data.frame(indiv,olho_of,mov_ped,pos_p,lat_dir,oiPOF4[,i])
  names(pli4pof)<-c("ind","olho","MOV","POS","LAT","dados")
  pli4roa<-data.frame(indiv,olho_oa,mov_rep,pos_p,lat_dir,oiROA4[,i])
  names(pli4roa)<-c("ind","olho","MOV","POS","LAT","dados")
  pli4rof<-data.frame(indiv,olho_of,mov_rep,pos_p,lat_dir,oiROF4[,i])
  names(pli4rof)<-c("ind","olho","MOV","POS","LAT","dados")
  
  pli<-rbind(pli4poa,pli4pof,pli4roa,pli4rof)
  #print(c(i,mean(pli[,Ncol]),sd(pli[,Ncol])))
  
  if(sd(pli[,Ncol])>sd_cri){pli[,Ncol]<-NA}
  
  if(i==1){pli4<-pli}else{ 
    pli4<-rbind(pli4,pli)}
}


oiPOA5<-read.table("SMax_5_col.dat")
oiROA5<-read.table("SMax_5_col_ROA.dat")
oiPOF5<-read.table("SMax_5_col_POF.dat")
oiROF5<-read.table("SMax_5_col_ROF.dat")

for(i in 1:NN){
  indiv<-rep(idd[i],100)
  
  pli5poa<-data.frame(indiv,olho_oa,mov_ped,pos_p,lat_esq,oiPOA5[,i])
  names(pli5poa)<-c("ind","olho","MOV","POS","LAT","dados")
  pli5pof<-data.frame(indiv,olho_of,mov_ped,pos_p,lat_esq,oiPOF5[,i])
  names(pli5pof)<-c("ind","olho","MOV","POS","LAT","dados")
  pli5roa<-data.frame(indiv,olho_oa,mov_rep,pos_p,lat_esq,oiROA5[,i])
  names(pli5roa)<-c("ind","olho","MOV","POS","LAT","dados")
  pli5rof<-data.frame(indiv,olho_of,mov_rep,pos_p,lat_esq,oiROF5[,i])
  names(pli5rof)<-c("ind","olho","MOV","POS","LAT","dados")
  
  pli<-rbind(pli5poa,pli5pof,pli5roa,pli5rof)
  #print(c(i,mean(pli[,Ncol]),sd(pli[,Ncol])))
  
  if(sd(pli[,Ncol])>sd_cri){pli[,Ncol]<-NA}
  
  if(i==1){pli5<-pli}else{ 
    pli5<-rbind(pli5,pli)}
}

oiPOA6<-read.table("SMax_6_col.dat")
oiROA6<-read.table("SMax_6_col_ROA.dat")
oiPOF6<-read.table("SMax_6_col_POF.dat")
oiROF6<-read.table("SMax_6_col_ROF.dat")

for(i in 1:NN){
  indiv<-rep(idd[i],100)
  
  pli6poa<-data.frame(indiv,olho_oa,mov_ped,pos_c,lat_dir,oiPOA6[,i])
  names(pli6poa)<-c("ind","olho","MOV","POS","LAT","dados")
  pli6pof<-data.frame(indiv,olho_of,mov_ped,pos_c,lat_dir,oiPOF6[,i])
  names(pli6pof)<-c("ind","olho","MOV","POS","LAT","dados")
  pli6roa<-data.frame(indiv,olho_oa,mov_rep,pos_c,lat_dir,oiROA6[,i])
  names(pli6roa)<-c("ind","olho","MOV","POS","LAT","dados")
  pli6rof<-data.frame(indiv,olho_of,mov_rep,pos_c,lat_dir,oiROF6[,i])
  names(pli6rof)<-c("ind","olho","MOV","POS","LAT","dados")
  
  pli<-rbind(pli6poa,pli6pof,pli6roa,pli6rof)
  #print(c(i,mean(pli[,Ncol]),sd(pli[,Ncol])))
  
  if(sd(pli[,Ncol])>sd_cri){pli[,Ncol]<-NA}
  
  if(i==1){pli6<-pli}else{ 
    pli6<-rbind(pli6,pli)}
}

oiPOA7<-read.table("SMax_7_col.dat")
oiROA7<-read.table("SMax_7_col_ROA.dat")
oiPOF7<-read.table("SMax_7_col_POF.dat")
oiROF7<-read.table("SMax_7_col_ROF.dat")

for(i in 1:NN){
  indiv<-rep(idd[i],100)
  
  pli7poa<-data.frame(indiv,olho_oa,mov_ped,pos_c,lat_esq,oiPOA7[,i])
  names(pli7poa)<-c("ind","olho","MOV","POS","LAT","dados")
  pli7pof<-data.frame(indiv,olho_of,mov_ped,pos_c,lat_esq,oiPOF7[,i])
  names(pli7pof)<-c("ind","olho","MOV","POS","LAT","dados")
  pli7roa<-data.frame(indiv,olho_oa,mov_rep,pos_c,lat_esq,oiROA7[,i])
  names(pli7roa)<-c("ind","olho","MOV","POS","LAT","dados")
  pli7rof<-data.frame(indiv,olho_of,mov_rep,pos_c,lat_esq,oiROF7[,i])
  names(pli7rof)<-c("ind","olho","MOV","POS","LAT","dados")
  
  pli<-rbind(pli7poa,pli7pof,pli7roa,pli7rof)
  #print(c(i,mean(pli[,Ncol]),sd(pli[,Ncol])))
  
  if(sd(pli[,Ncol])>sd_cri){pli[,Ncol]<-NA}
  
  if(i==1){pli7<-pli}else{ 
    pli7<-rbind(pli7,pli)}
}

oiPOA8<-read.table("SMax_8_col.dat")
oiROA8<-read.table("SMax_8_col_ROA.dat")
oiPOF8<-read.table("SMax_8_col_POF.dat")
oiROF8<-read.table("SMax_8_col_ROF.dat")

for(i in 1:NN){
  indiv<-rep(idd[i],100)
  
  pli8poa<-data.frame(indiv,olho_oa,mov_ped,pos_f,lat_dir,oiPOA8[,i])
  names(pli8poa)<-c("ind","olho","MOV","POS","LAT","dados")
  pli8pof<-data.frame(indiv,olho_of,mov_ped,pos_f,lat_dir,oiPOF8[,i])
  names(pli8pof)<-c("ind","olho","MOV","POS","LAT","dados")
  pli8roa<-data.frame(indiv,olho_oa,mov_rep,pos_f,lat_dir,oiROA8[,i])
  names(pli8roa)<-c("ind","olho","MOV","POS","LAT","dados")
  pli8rof<-data.frame(indiv,olho_of,mov_rep,pos_f,lat_dir,oiROF8[,i])
  names(pli8rof)<-c("ind","olho","MOV","POS","LAT","dados")
  
  pli<-rbind(pli8poa,pli8pof,pli8roa,pli8rof)
  #print(c(i,mean(pli[,Ncol]),sd(pli[,Ncol])))
  
  if(sd(pli[,Ncol])>sd_cri){pli[,Ncol]<-NA}
  
  if(i==1){pli8<-pli}else{ 
    pli8<-rbind(pli8,pli)}
}

oiPOA9<-read.table("SMax_9_col.dat")
oiROA9<-read.table("SMax_9_col_ROA.dat")
oiPOF9<-read.table("SMax_9_col_POF.dat")
oiROF9<-read.table("SMax_9_col_ROF.dat")

for(i in 1:NN){
  indiv<-rep(idd[i],100)
  
  pli9poa<-data.frame(indiv,olho_oa,mov_ped,pos_f,lat_esq,oiPOA9[,i])
  names(pli9poa)<-c("ind","olho","MOV","POS","LAT","dados")
  pli9pof<-data.frame(indiv,olho_of,mov_ped,pos_f,lat_esq,oiPOF9[,i])
  names(pli9pof)<-c("ind","olho","MOV","POS","LAT","dados")
  pli9roa<-data.frame(indiv,olho_oa,mov_rep,pos_f,lat_esq,oiROA9[,i])
  names(pli9roa)<-c("ind","olho","MOV","POS","LAT","dados")
  pli9rof<-data.frame(indiv,olho_of,mov_rep,pos_f,lat_esq,oiROF9[,i])
  names(pli9rof)<-c("ind","olho","MOV","POS","LAT","dados")
  
  pli<-rbind(pli9poa,pli9pof,pli9roa,pli9rof)
  #print(c(i,mean(pli[,Ncol]),sd(pli[,Ncol])))
  
  if(sd(pli[,Ncol])>sd_cri){pli[,Ncol]<-NA}
  
  if(i==1){pli9<-pli}else{ 
    pli9<-rbind(pli9,pli)}
}

pla<-rbind(pli2,pli3,pli4,pli5,pli6,pli7,pli8,pli9)
attach(pla)

u6<-as.vector(unique(ind))


names2<-c("Open Eye","Close Eye")
pdf("fig2.pdf")
boxplot(dados[POS=="O"&MOV=="REP"&olho=="OA"&LAT=="DIR"],dados[POS=="O"&MOV=="PED"&olho=="OF"&LAT=="DIR"], ylab="Entropy",names=names2,ylim=c(3,6),outline=FALSE,cex.lab=1.3,cex.axis=1.2)
dev.off()





