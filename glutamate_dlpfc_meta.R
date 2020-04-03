#load all necessary packages
library(readxl)
library(tidyverse)
library(meta)
library(metafor)

data_glu<-read_excel("data_glutamate.xlsx")
######random and fixed effects meta-analysis with between study factor, including test for effect of medication status
resul_by_subgroup<-data_glu%>%
  metacont(SCZn, SCZmean, SCZsd, HCn, HCmean, HCsd, studylab, data = ., sm="SMD", method.smd = "Cohen", byvar=medication_status)

#for info on I squared
#https://wiki.joannabriggs.org/display/MANUAL/3.3.10.2+Quantification+of+the+statistical+heterogeneity%3A+I+squared

#create pdf
pdf("Fig1_forest_subgroup_med.pdf", width=12, height=12) 
#create plot
forest(resul_by_subgroup, col.square = "lightblue", col.diamond = "darkblue", col.diamond.lines = "darkblue", lab.e = "Patients with schizophrenia", lab.c = "Healthy controls")
# Close the pdf file
dev.off() 


#calculate Coefficient of Variance Ratio
cvar_ratio<-data_glu%>%
  escalc("CVR", n1i=SCZn, m1i=SCZmean, sd1i=SCZsd, n2i=HCn, m2i=HCmean, sd2i=HCsd, data=., slab=studylab, append = T)

#calcualte random effects meta-analysis with between study factor medication status
resul_cvar_ratio<-rma.uni(cvar_ratio, measure = "CVR", slab=data_glu$studylab, mods = ~ medication_status)

summary(resul_cvar_ratio)

resul_cvar_ratio<-rma.uni(cvar_ratio, measure = "CVR", slab=data_glu$studylab)

summary(resul_cvar_ratio)


#creating Figure 2
pdf("Fig2_forestCVR_subgroup_med.pdf", width=10, height=6) 
# 2. Create a plot

forest_plot<-forest.rma(resul_cvar_ratio, xlim=c(-8, 6),ylim=c(-1, 29),
                        order=order(data_glu$medication_status, decreasing = T), cex=0.9,rows=c(1,18:16,25:23,11:6), psize=0,top=2, addfit = F, col = "darkblue", xlab = "")
#for getting rid of the black squares, psize=0 is added
#now, add nice blue squares
points(resul_cvar_ratio$yi[1:13][order(data_glu$medication_status)],rev(c(1,16:18,23:25,6:11)), pch=15, col="lightblue")
points(resul_cvar_ratio$yi[1:13][order(data_glu$medication_status)],rev(c(1,16:18,23:25,6:11)), pch=3, col="black")

text(0,length(forest_plot[["rows"]])+17, "Log Coefficient of Variation Ratio", pos=1, font = 2)

text(forest_plot[["xlim"]][1],length(forest_plot[["rows"]])+16, "Study", pos=4, font = 2)
text(forest_plot[["xlim"]][2],length(forest_plot[["rows"]])+16, "Log CVR [95% CI]", pos=2, font = 2)
text(-2,-1.5, cex=0.7, "greater variability in controls")
text(2,-1.5, cex=0.7, "greater variability in patients")

### add text with Q-value, dfs, p-value, and I^2 statistic
text(forest_plot[["xlim"]][1], -1.8, pos=4, cex=0.75, col = "darkgrey", bquote(paste("overall (Q = ",
                                                                                     .(formatC(resul_cvar_ratio$QE, digits=2, format="f")), ", df = ", .(resul_cvar_ratio$k - resul_cvar_ratio$p),
                                                                                     ", p = ", .(formatC(resul_cvar_ratio$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                                                     .(formatC(resul_cvar_ratio$I2, digits=1, format="f")), "%)")))



### add text for the subgroups
text(forest_plot[["xlim"]][1], c(26.2,19.2,12.2,2.2), pos=4, c("medicated and unmedicated",
                                                               "naïve",
                                                               "medicated", "unclear"), col = "darkgrey",cex=1.1)


### fit random-effects model in the subgroups
data_glu$medication_status
res.um<-rma.uni(cvar_ratio, measure = "CVR", slab=data_glu$studylab, subset=(medication_status=="medicated and unmedicated") )
res.n<-rma.uni(cvar_ratio, measure = "CVR", slab=data_glu$studylab, subset=(medication_status=="naïve") )
res.m<-rma.uni(cvar_ratio, measure = "CVR", slab=data_glu$studylab, subset=(medication_status=="medicated") )
res.u<-rma.uni(cvar_ratio, measure = "CVR", slab=data_glu$studylab, subset=(medication_status=="unclear") )

### add summary polygons for thesubgroups
addpoly(res.um, row=22, cex=1, mlab="",col = "darkblue")
addpoly(res.n, row= 15, cex=1, mlab="",col = "darkblue")
addpoly(res.m, row= 5, cex=1, mlab="",col = "darkblue")


### add text with Q-value, dfs, p-value, and I^2 statistic for subgroups
text(forest_plot[["xlim"]][1], 22, pos=4, cex=0.70, col = "darkgrey", bquote(paste("RE model for subgroup (Q = ",
                                                                                   .(formatC(res.um$QE, digits=2, format="f")), ", df = ", .(res.um$k - res.um$p),
                                                                                   ", p = ", .(formatC(res.um$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                                                   .(formatC(res.um$I2, digits=1, format="f")), "%)")))
text(forest_plot[["xlim"]][1], 15, pos=4, cex=0.70,col = "darkgrey", bquote(paste("RE model for subgroup (Q = ",
                                                                                  .(formatC(res.n$QE, digits=2, format="f")), ", df = ", .(res.n$k - res.n$p),
                                                                                  ", p = ", .(formatC(res.n$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                                                  .(formatC(res.n$I2, digits=1, format="f")), "%)")))
text(forest_plot[["xlim"]][1], 5, pos=4, cex=0.70, col = "darkgrey",bquote(paste("RE model for subgroup (Q = ",
                                                                                 .(formatC(res.m$QE, digits=2, format="f")), ", df = ", .(res.m$k - res.m$p),
                                                                                 ", p = ", .(formatC(res.m$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                                                 .(formatC(res.m$I2, digits=1, format="f")), "%)")))

# Close the pdf file
dev.off() 



###Testing for seperate subgroups depending on reported medication status

data_glu_sep_med_group<-read_excel("data_glutamate_sep_med_group.xlsx")

resul_by_subgroup<-data_glu_sep_med_group%>%
  metacont(SCZn, SCZmean, SCZsd, HCn, HCmean, HCsd, studylab, data = ., sm="SMD", method.smd = "Cohen", byvar=medication_status)

resul_by_subgroup$zval.fixed.w
resul_by_subgroup$pval.fixed.w


resul_by_subgroup$lower.fixed.w

#create SFig2
pdf("SFig2rplot_forest_subgroup_medication.pdf", width=12, height=12) 
# 2. Create a plot
forest(resul_by_subgroup, col.square = "lightblue", col.diamond ="darkblue", lab.e = "Patients with schizophrenia", lab.c = "Healthy controls")

# Close the pdf file
dev.off() 



##analysis by patient group chronic vs FEP
resul_by_subgroup<-data_glu%>%
  metacont(SCZn, SCZmean, SCZsd, HCn, HCmean, HCsd, studylab, data = ., sm="SMD", method.smd = "Cohen", byvar=`patient group`)
resul_by_subgroup

#Create SFig3
pdf("SFig3_rplot_forest_patient_group.pdf", width=12, height=8) 
# 2. Create a plot
forest(resul_by_subgroup, col.square = "lightblue", col.diamond = "darkblue", col.diamond.lines = "darkblue", lab.e = "Patients with schizophrenia", lab.c = "Healthy controls")
# Close the pdf file
dev.off() 


##analysis by metabolite extraxcted
resul_by_subgroup<-data_glu%>%
  metacont(SCZn, SCZmean, SCZsd, HCn, HCmean, HCsd, studylab, data=., sm="SMD", method.smd = "Cohen", byvar=metabolite)
resul_by_subgroup


pdf("SFig4_rplot_forest_subgroup_metabolite.pdf", width=12, height=12) 
# 2. Create a plot
forest(resul_by_subgroup, col.square = "lightblue", col.diamond = "darkblue", col.diamond.lines = "darkblue", lab.e = "Patients with schizophrenia", lab.c = "Healthy controls")
# Close the pdf file
dev.off() 


#Egger's regression test (regtest() function),
resul<-data_glu%>%
  metacont(SCZn, SCZmean, SCZsd, HCn, HCmean, HCsd, studylab, data = ., sm="SMD", method.smd = "Cohen")

metabias(resul, method.bias ='linreg')

regtest(x=resul$TE, sei=resul$seTE, model="lm")

#funnelplot
pdf("SFig5rplot_funnel.pdf", width=12, height=12) 
funnel(resul)
dev.off() 


#Testing effects of 'file drawer studies' 

trimfill(resul)


##funnnel plot with imputation
pdf("SFig6rplot_funnel_trimfill.pdf", width=12, height=12) 
funnel(trimfill(resul))
dev.off() 

##metaregression

metareg(resul, year)
pdf("SFig7rplot_bubble_pubyear.pdf", width=12, height=12) 
bubble(metareg(resul, year),xlim = c(2010,2020), ylim=c(-0.9,1), bg = "transparent", ylab = "effect size", xlab="year of publication", cex.lab=1.5)
dev.off() 

metareg(resul, age)

pdf("SFig8rplot_bubble_age.pdf", width=12, height=12) 
bubble(metareg(resul, age), xlim = c(20,45), ylim=c(-0.7,1), bg = "transparent", ylab = "effect size", xlab="mean age", cex.lab=1.5)
dev.off() 


#Outlier detection
#function from https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/detecting-outliers-influential-cases.html
spot.outliers.random<-function(data){
  data<-data
  Author<-data$studlab
  lowerci<-data$lower
  upperci<-data$upper
  m.outliers<-data.frame(Author,lowerci,upperci)
  te.lower<-data$lower.random
  te.upper<-data$upper.random
  dplyr::filter(m.outliers,upperci < te.lower)
  dplyr::filter(m.outliers,lowerci > te.upper)
}
#Outlier Testing for effect in medication naive patients
resul_med_naive<-data_glu%>%filter(medication_status=="naïve")%>%
  metacont(SCZn, SCZmean, SCZsd, HCn, HCmean, HCsd, studylab, data = ., sm="SMD", method.smd = "Cohen")
forest(resul_med_naive)

spot.outliers.random(resul_med_naive)



###meta analysis of variability ratio

var_ratio<-data_glu%>%
  escalc("VR", n1i=SCZn, m1i=SCZmean, sd1i=SCZsd, n2i=HCn, m2i=HCmean, sd2i=HCsd, data=., slab=studylab, append = T)

sum_var_ratio<-summary(var_ratio)

resul_var_ratio<-rma.uni(var_ratio, measure = "VR", slab=data_glu$studylab)
##double check: calculation of VR manually
log(lit_Glut$SCZsd[lit_Glut$include=="yes"]/lit_Glut$HCsd[lit_Glut$include=="yes"])+((1/((2*lit_Glut$SCZn[lit_Glut$include=="yes"])-1))-(1/((2*lit_Glut$HCn[lit_Glut$include=="yes"])-1)))


pdf("SFig9rplot_forestVR.pdf", width=10, height=6) 
# 2. Create a plot
forest_plot<-forest.rma(resul_var_ratio, slab=data_glu$studylab, showweights=TRUE, top=2)
text(forest_plot[["xlim"]][1],length(forest_plot[["rows"]])+2, "Study", pos=4)
text(forest_plot[["xlim"]][2]-1.5,length(forest_plot[["rows"]])+2,"Weight", pos=2)
text(forest_plot[["xlim"]][2],length(forest_plot[["rows"]])+2, "Log VR [95% CI]", pos=2)
text(-1.9,-1.7,"greater variability in controls")
text(1.9,-1.7,"greater variability in patients")
### add text with Q-value, dfs, p-value, and I^2 statistic
text(forest_plot[["xlim"]][1]+1, -1, pos=4, cex=1, bquote(paste("(Q = ",
                                                                .(formatC(resul_var_ratio$QE, digits=2, format="f")), ", df = ", .(resul_var_ratio$k - resul_var_ratio$p),
                                                                ", p = ", .(formatC(resul_var_ratio$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                                .(formatC(resul_var_ratio$I2, digits=1, format="f")), "%)")))
# Close the pdf file
dev.off() 

###meta analysis of cvariability ratio

##
cvar_ratio<-data_glu%>%
  escalc("CVR", n1i=SCZn, m1i=SCZmean, sd1i=SCZsd, n2i=HCn, m2i=HCmean, sd2i=HCsd, data=., slab=studylab, append = T)

sum_cvar_ratio<-summary(cvar_ratio)

resul_cvar_ratio<-rma.uni(cvar_ratio, measure = "CVR", slab=data_glu$studylab)

pdf("SFig10rplot_forestCVR.pdf", width=10, height=6) 
# 2. Create a plot

resul_cvar_ratio<-rma.uni(cvar_ratio, measure = "CVR", slab=data_glu$studylab)
forest_plot<-forest.rma(resul_cvar_ratio, slab=data_glu$studylab, showweights=TRUE, top=2)
text(forest_plot[["xlim"]][1],length(forest_plot[["rows"]])+2, "Study", pos=4)
text(forest_plot[["xlim"]][2]-2,length(forest_plot[["rows"]])+2,"Weight", pos=2)
text(forest_plot[["xlim"]][2],length(forest_plot[["rows"]])+2, "Log CVR [95% CI]", pos=2)
text(-1.9,-1.7,"greater variability in controls")
text(1.9,-1.7,"greater variability in patients")
### add text with Q-value, dfs, p-value, and I^2 statistic
text(forest_plot[["xlim"]][1]+1, -1, pos=4, cex=1, bquote(paste("(Q = ",
                                                                .(formatC(resul_cvar_ratio$QE, digits=2, format="f")), ", df = ", .(resul_cvar_ratio$k - resul_cvar_ratio$p),
                                                                ", p = ", .(formatC(resul_cvar_ratio$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                                .(formatC(resul_cvar_ratio$I2, digits=1, format="f")), "%)")))
# Close the pdf file
dev.off() 


###moderator subgroup analysis var ratio
resul_var_ratio<-rma.uni(var_ratio, measure = "VR", slab=data_glu$studylab, mods = ~ medication_status)
forest.rma(resul_var_ratio, slab=data_glu$studylab, showweights=TRUE, top=2)
resul_var_ratio<-rma.uni(var_ratio, measure = "VR", slab=data_glu$studylab)

pdf("SFig11rplot_forestVR_subgroup.pdf", width=10, height=6) 
# 2. Create a plot
forest_plot<-forest.rma(resul_var_ratio, xlim=c(-8, 6),ylim=c(-1, 29),
                        order=order(data_glu$medication_status, decreasing = T), cex=1,rows=c(1,18:16,25:23,11:6), psize=1,top=2, addfit = T)

text(forest_plot[["xlim"]][1],length(forest_plot[["rows"]])+16, "Study", pos=4)
text(forest_plot[["xlim"]][2],length(forest_plot[["rows"]])+16, "Log VR [95% CI]", pos=2)
text(-2,-1.5, cex=0.7, "greater variability in controls")
text(2,-1.5, cex=0.7, "greater variability in patients")

### add text with Q-value, dfs, p-value, and I^2 statistic
text(forest_plot[["xlim"]][1], -1.8, pos=4, cex=0.75, col = "darkgrey", bquote(paste("overall (Q = ",
                                                                                     .(formatC(resul_var_ratio$QE, digits=2, format="f")), ", df = ", .(resul_var_ratio$k - resul_var_ratio$p),
                                                                                     ", p = ", .(formatC(resul_var_ratio$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                                                     .(formatC(resul_var_ratio$I2, digits=1, format="f")), "%)")))


### add text for the subgroups
text(forest_plot[["xlim"]][1], c(26.2,19.2,12.2,2.2), pos=4, c("medicated and unmedicated",
                                                               "naïve",
                                                               "medicated", "unclear"), col = "darkgrey",cex=1.1)


### fit random-effects model in the subgroups

res.um<-rma.uni(var_ratio, measure = "VR", slab=data_glu$studylab, subset=(medication_status=="medicated and unmedicated") )
res.n<-rma.uni(var_ratio, measure = "VR", slab=data_glu$studylab, subset=(medication_status=="naïve") )
res.m<-rma.uni(var_ratio, measure = "VR", slab=data_glu$studylab, subset=(medication_status=="medicated") )
res.u<-rma.uni(var_ratio, measure = "VR", slab=data_glu$studylab, subset=(medication_status=="unclear") )

### add summary polygons for the subgroups
addpoly(res.um, row=22, cex=1, mlab="")
addpoly(res.n, row= 15, cex=1, mlab="")
addpoly(res.m, row= 5, cex=1, mlab="")
#addpoly(res.u, row= 0, cex=1, mlab="")


### add text with Q-value, dfs, p-value, and I^2 statistic for subgroups
text(forest_plot[["xlim"]][1], 22, pos=4, cex=0.70, col = "darkgrey", bquote(paste("RE model for subgroup (Q = ",
                                                                                   .(formatC(res.um$QE, digits=2, format="f")), ", df = ", .(res.um$k - res.um$p),
                                                                                   ", p = ", .(formatC(res.um$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                                                   .(formatC(res.um$I2, digits=1, format="f")), "%)")))
text(forest_plot[["xlim"]][1], 15, pos=4, cex=0.70,col = "darkgrey", bquote(paste("RE model for subgroup (Q = ",
                                                                                  .(formatC(res.n$QE, digits=2, format="f")), ", df = ", .(res.n$k - res.n$p),
                                                                                  ", p = ", .(formatC(res.n$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                                                  .(formatC(res.n$I2, digits=1, format="f")), "%)")))
text(forest_plot[["xlim"]][1], 5, pos=4, cex=0.70, col = "darkgrey",bquote(paste("RE model for subgroup (Q = ",
                                                                                 .(formatC(res.m$QE, digits=2, format="f")), ", df = ", .(res.m$k - res.m$p),
                                                                                 ", p = ", .(formatC(res.m$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                                                 .(formatC(res.m$I2, digits=1, format="f")), "%)")))


# Close the pdf file
dev.off() 

