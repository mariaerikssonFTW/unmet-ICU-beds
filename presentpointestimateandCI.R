library(boot)

load(file="repsret.rdata")
load(file="look_cont_titco_df.rdata")
load(file="look_cat_titco.rdata")
load(file="epilin.rdata")

#vilken fil ska du använda
ret=repsret

#unmet need i originaldatan
orig_means <- ret$t0
reps_means=data.frame(mean(ret$t[,1]),mean(ret$t[,2]),mean(ret$t[,3]))
reps_means=c(mean(ret$t[,1]),mean(ret$t[,2]),mean(ret$t[,3]))
bias=reps_means-orig_means

#std error variabeln
#t-vektorerna
t_df=data.frame(ret$t)
t_df=data.frame(t_df$X1*100,t_df$X2,t_df$X3*100)
colnames(t_df)<-data.frame("Unmet ICU bed need, in % of current resources","Area Under Curve in test data","Unnecessary ICU bed admissions, in % of current resources")

se=c(sd(ret$t[,1]),sd(ret$t[,2]),sd(ret$t[,3]))

unmet_mean_orig=ret$t0[1]
AUC_mean_orig=ret$t0[2]
UN_mean_orig=ret$t0[3]

R=ret[["R"]]
conf=0.95
alpha = 1-conf
degrees_of_freedom = R - 1
t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)

# Calculating lower bound and upper bound
lower_bound <- reps_means - t_score*se
upper_bound <- reps_means + t_score*se

print(cbind(lower_bound,upper_bound))


#make a table
variablenames=data.frame("Unmet ICU bed need, in % of current resources","Area Under Curve in test data","Unnecessary ICU bed admissions, in % of current resources")
variablenames=data.table::transpose(variablenames)
firstcolumn=c(orig_means[1]*100,orig_means[2],orig_means[3]*100)
seconddcolumn=c(reps_means[1]*100,reps_means[2],reps_means[3]*100)
thirdandfourth=cbind(lower_bound,upper_bound)

CItable2markdown=data.frame(variablenames,firstcolumn,seconddcolumn,thirdandfourth)

#så här får man boot.ci från multipla
CI_unmet=boot.ci(ret, type="basic", index=1)
CI_unmet_bootlow=CI_unmet$basic[4]
CI_unmet_boothigh=CI_unmet$basic[5]

CI_AUC=boot.ci(ret, type="basic", index=2)
CI_AUC_bootlow=CI_AUC$basic[4]
CI_AUC_boothigh=CI_AUC$basic[5]

colnames(CItable2markdown)<-data.frame("variable","mean all TITCO","mean bootstrap","95% CI low","95% CI high")
save(CItable2markdown,file="CItable2markdown")

#
tvec=ret$t[,1]
tvec_df=data.frame(tvec)
#boot.ci(boot.out,type="basic",var.t0=se[1],t0=unmet_mean_orig)


AUClow=round(lower_bound[2],digits=2)
AUChigh=round(upper_bound[2],digits=2)
AUCmean=round(reps_means[2],digits=2)

unmetlow=round(lower_bound[1]*100,digits=2)
unmethigh=round(upper_bound[1]*100,digits=2)
unmetmean=round(reps_means[1]*100,digits=2)

#pvärden
epilin_df=data.frame(epilin)

p_NISS=look_cont_titco_df$p[24]
p_NISS=epilin[44,4]
p_died=epilin[61,4]
p_blrec=epilin[23,4]
p_ot1=epilin[47,4]
p_schi=epilin[25,4]

meant=mean(ret$t[,1])
meant
meant0=orig_means
bootCI=boot.ci(ret, type="basic", index=1)
CI_low=bootCI[["basic"]][[4]]
CI_high=bootCI[["basic"]][[5]]

meanCI=CI_high-(CI_high-CI_low)/2

