#load all necessary packages
library(readxl)
library(tidyverse)
library(meta)
library(metafor)

data_glu<-read_excel("data_glutamate.xlsx")
######random and fixed effects meta-analysis with between study factor, testing for effect of medication status
resul_by_subgroup<-data_glu%>%
  metacont(SCZn, SCZmean, SCZsd, HCn, HCmean, HCsd, studylab, data = ., sm="SMD", method.smd = "Cohen", byvar=medication_status)
resul_by_subgroup$zval.random.w
resul_by_subgroup$pval.random.w

resul_by_subgroup$zval.fixed.w
resul_by_subgroup$pval.fixed.w
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

rev(resul_cvar_ratio$yi[1:13])
m[nrow(m):1,]


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



resul_by_subgroup<-data_glu%>%
  metacont(SCZn, SCZmean, SCZsd, HCn, HCmean, HCsd, studylab, data=., sm="SMD", method.smd = "Cohen", byvar=metabolite)
resul_by_subgroup


pdf("SFig4_rplot_forest_subgroup_metabolite.pdf", width=12, height=12) 
# 2. Create a plot
forest(resul_by_subgroup, col.square = "lightblue", col.diamond = "darkblue", col.diamond.lines = "darkblue", lab.e = "Patients with schizophrenia", lab.c = "Healthy controls")
# Close the pdf file
dev.off() 


####for CVR
resul_cvar_ratio
data_glu%>%
  escalc("CVR", n1i=SCZn, m1i=SCZmean, sd1i=SCZsd, n2i=HCn, m2i=HCmean, sd2i=HCsd, data=., slab=studylab, append = T)

resul_cvar_ratio<-rma.uni(cvar_ratio, measure = "CVR", slab=data_glu$studylab)

res <- rma(resul_cvar_ratio$yi, resul_cvar_ratio$vi, mods = ~ resul$age)
res

metabias(resul_cvar_ratio, method.bias ='linreg')
funnel(resul_cvar_ratio)
regtest(x=resul$TE, sei=resul$seTE, model="lm")
metareg(resul, year)
bubble(metareg(resul, year),xlim = c(2010,2020), ylim=c(-0.9,1), bg = "transparent", ylab = "effect size", xlab="year of publication", cex.lab=1.5)

metareg(resul_cvar_ratio, age)
bubble(metareg(resul_cvar_ratio, age), xlim = c(20,45), ylim=c(-0.7,1), bg = "transparent", ylab = "effect size", xlab="mean age", cex.lab=1.5)


cvar_ratio<-data_glu%>%
  escalc("CVR", n1i=SCZn, m1i=SCZmean, sd1i=SCZsd, n2i=HCn, m2i=HCmean, sd2i=HCsd, data=., slab=studylab, append = T)

#calcualte random effects meta-analysis with between study factor age and year
resul_cvar_ratio<-rma.uni(cvar_ratio, measure = "CVR", slab=data_glu$studylab, mods = ~ year)

resul_cvar_ratio<-rma.uni(cvar_ratio, measure = "CVR", slab=data_glu$studylab, mods = ~ age)

resul_cvar_ratio<-rma(cvar_ratio, measure = "CVR", slab=data_glu$studylab, mods = ~ age, intercept = F)
#http://www.metafor-project.org/doku.php/tips:models_with_or_without_intercept

