###################################################################################
####  Unscrambling the drivers of egg production in Agassiz's desert tortoise: ####
#########  Climate and individual attributes predict reproductive output  #########
#########  C.Mitchell, D.Friend, L.Phillips, E.Hunter, J.Lovich, M.Agha  ##########
########  S.Puffer, K.Cummings, P.Medica, T.Esque, K.Nussear, K.Shoemaker #########
###################################################################################


####################################
##########  CLEAR WORKSPACE

rm(list=ls())

###################################
##########  SET WORKING DIRECTORY

#working directory must contain "EDM_FUNCTIONS_1_5.R","tortoise_egg_data.csv", "prism_egg_data.csv", and "dayment_data.csv"
getwd()

maindir <- getwd()

##################################
##########  LOAD PACKAGES AND FUNCTIONS

source("EDM_FUNCTIONS_1_5.R")

library(R2jags)
library(runjags)
library(coda)
library(ROCR)

###################################
##########   READ IN DATA

egg_dat = read.csv("Final data for paper/tortoise_egg_data.csv")
prism_dat = read.csv("Final data for paper/prism_data.csv")
daymet_dat = read.csv("Final data for paper/daymet_data.csv")

###################################
##########   REFORMAT DATA FOR JAGS

jags_dat = format_data_for_JAGS(egg_dat, prism_dat, daymet_dat)
jags_dat

#assign the data in jags_dat to different variable names
obs.numeggs = jags_dat$TE
did.reproduce = jags_dat$ReproStatus
n.inds = nrow(jags_dat$ReproStatus)
sp.temp = jags_dat$daymet_jdate
MCL = jags_dat$MCL
MTR = jags_dat$mtr_window
c.precip = jags_dat$precip_window
first = as.numeric(jags_dat$first_year)-1995     ### a vector of years corresponding to the FIRST time each tort reproduced
last = as.numeric(jags_dat$last_year)-1995    ### a vector of years corresponding to the LAST time each tort reproduced 
site.num <- as.numeric(as.factor(gsub("\\.","",gsub('[[:digit:]]+', '', rownames(obs.numeggs)))))

uniqtort<-rownames(obs.numeggs)

#############
# Write JAGS model
#############

BUGSfilename <- writeJAGSmod()  # write JAGS model
#BUGSfilename <- writeJAGSmod_siteeff()  # write JAGS model

######################
# PREPARE DATA FOR BUGS
######################

ndx <- numeric(0)   # for now, no left out for CV
datforjags <- prepareJAGSdat(indices=ndx,daymet = T,clim=T,internal=T)    # function prepares data for running JAGS
#datforjags <- prepareJAGSdat(indices=ndx,daymet = T,clim=T,internal=T,site=T)  # added site

#datforjags$data.for.bugs$did.reproduce

######################
# RUN JAGS 
######################

mod2 <- runJAGSmod(fn=BUGSfilename,indat=datforjags)   # run JAGS model

Mod.mcmc <- mod2$mcmc
Mod.mcmc.list<- coda::mcmc.list(Mod.mcmc)

### unlist each chain and combine into one df
mcmc.list <- as.data.frame(runjags::combine.mcmc(Mod.mcmc.list))  #  row.names=1:60000

# save(Mod.mcmc.list,mcmc.list,file="JAGSresults_forDerek.RData")    
save(mod2,jags_dat,file="JAGSresults_forDerek.RData")

str(mcmc.list)

#####################
# Diagnose convergence
#####################

varstotest <- c("beta.cp","beta.cp.pr","beta.MCLpr","beta.MCLel","beta.st","beta.MTRpr","beta.reproduce","b.numeggs","sitesd.rp","sitesd.ep")
myconv <- diagnoseJAGSconvergence(Mod.mcmc.list,varstotest)  # diagnose convergence


############################
#### Assess model performance and goodness-of-fit
###############################

pval <- computeJAGSpval(mcmc.list)   #compute bayesian p-value
pval        # 0.7026667


#############
# temp
############

mean(obs.numeggs,na.rm=T)   # 5.65 including zeroes
temp <- obs.numeggs
temp[temp==0] <- NA
mean(temp,na.rm=T)  # 6.870748 not including zeroes

sqrt(mean((obs.numeggs-mean(obs.numeggs,na.rm=T))^2,na.rm=T))   # overall null RMSE=3.69

############################
#### Perform k-fold cross-validation [KTS: change to look separately at prob and numeggs] [TODO: rsquared only for counts]
###############################

   ##### remove random individuals (standard 10-fold c-v)
n.folds <- 10
foldvec <- sample(1:n.folds,size=nrow(obs.numeggs),replace=T)

temp <- RunCrossVal(fn=BUGSfilename, foldvec,daymet = T,clim=T,internal=T,CV=T,
                    fullmod=mcmc.list,dr=did.reproduce,fst=first,lst=last,ut=uniqtort)    # takes a while to run
origcv <- temp

sedf <- temp$se
preddf1 <- temp$pred1
preddf2 <- temp$pred2
preddf3 <- preddf1*preddf2

  ### re-run without cross validation (performance metrics for full model)
temp <- RunCrossVal(fn=BUGSfilename,foldvec,daymet = T,clim=T,CV=F,
                    fullmod=mcmc.list,dr=did.reproduce,fst=first,lst=last,ut=uniqtort)
orig_nocv <- temp

sedf_nocv <- temp$se
preddf1_nocv <- temp$pred1
preddf2_nocv <- temp$pred2
preddf3_nocv <- preddf1_nocv*preddf2_nocv

rmse_cv_vector <- apply(sedf,1,function(t) sqrt(mean(t)) )
quantile(rmse_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%       5%      10%      25%      50%      75%      90%      95%    97.5% 
# 3.317170 3.327375 3.338861 3.358569 3.381838 3.406551 3.428415 3.442441 3.454916 

rmse_cv_vector <- apply(sedf_nocv,1,function(t) sqrt(mean(t)) )
quantile(rmse_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%       5%      10%      25%      50%      75%      90%      95%    97.5% 
# 3.186596 3.195914 3.207086 3.224981 3.244655 3.263403 3.278967 3.288406 3.296079

  ### compute R-squared for the model with cross-validation
allobs <- colnames(preddf3)
realobs <- numeric(length(allobs))
realdr <- numeric(length(allobs))

i=1
for(i in 1:length(allobs)){
  this <- allobs[i]
  this <- gsub("p.reproduce\\[","",this)
  this <- gsub("\\]","",this)
  temp <- as.numeric(strsplit(this,",")[[1]])
  ind=temp[1];yr=temp[2]
  realobs[i] <- datforjags$data.for.bugs$obs.numeggs[ind,yr]
  realdr[i] <- datforjags$data.for.bugs$did.reproduce[ind,yr]
}

meanobs <- mean(realobs)  # 5.65
meanobs

rmse_null <- sqrt(mean((realobs-meanobs)^2))
rmse_null    # 3.692332

    # rsquared- both rp and ep processes combined
rsquared_cv_vector <- sapply(1:nrow(preddf3),rsquaredfunc )
quantile(rsquared_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
# 0.1244652 0.1307763 0.1378447 0.1488061 0.1611112 0.1726159 0.1822974 0.1879135 0.1928873

rsquared_cv_vector <- sapply(1:nrow(preddf3_nocv),rsquaredfunc_nocv )
quantile(rsquared_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
# 0.2031186 0.2068245 0.2113711 0.2188400 0.2277895 0.2371259 0.2455681 0.2508156 0.2551777


# rsquared- just ep process
preddf3 <- preddf2[,realdr==1]
realobs <- realobs[realdr==1]
rsquared_cv_vector <- sapply(1:nrow(preddf3),rsquaredfunc )
quantile(rsquared_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
# 0.2817777 0.2848040 0.2880850 0.2935305 0.2993008 0.3048346 0.3093718 0.3119333 0.3142648

preddf3_nocv <- preddf2_nocv[,realdr==1]
#realobs <- realobs[realdr==1]
rsquared_cv_vector <- sapply(1:nrow(preddf3_nocv),rsquaredfunc_nocv )
quantile(rsquared_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
# 0.2979087 0.3015278 0.3051634 0.3102748 0.3143881 0.3172651 0.3190571 0.3197760 0.3202839

roc_cv_vector <- sapply(1:nrow(preddf1), rocfunc )
quantile(roc_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
# 0.6137606 0.6201092 0.6278195 0.6404762 0.6540399 0.6671560 0.6794725 0.6861827 0.6923863 

roc_cv_vector <- sapply(1:nrow(preddf1_nocv), rocfunc_nocv )
quantile(roc_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
# 0.7205911 0.7249314 0.7313021 0.7428900 0.7561583 0.7695101 0.7812519 0.7881018 0.7939316

####################################
  ##### Cross validation with all climate variables removed
####################################

BUGSfilename_noclim <- writeJAGSmod_noclim()  # write JAGS model
ndx <- numeric(0)   # for now, no left out for CV
datforjags_noclim <- prepareJAGSdat(indices=ndx,daymet = F,clim=F,internal=T)    # function prepares data for running JAGS

  ##### run full model with no climate
mod3 <- runJAGSmod(fn=BUGSfilename_noclim,indat=datforjags_noclim)   # run JAGS model
Mod.mcmc <- mod3$mcmc
Mod.mcmc.list<- coda::mcmc.list(Mod.mcmc)
### unlist each chain and combine into one df
mcmc.list_noclim <- as.data.frame(runjags::combine.mcmc(Mod.mcmc.list))  #  row.names=1:60000

temp <- RunCrossVal(fn=BUGSfilename_noclim,foldvec,daymet = F,clim=F,internal=T,CV=T,
                    fullmod=mcmc.list_noclim,dr=did.reproduce,fst=first,lst=last,ut=uniqtort)    # takes a while to run
sedf2 <- temp$se
preddf2_1 <- temp$pred1
preddf2_2 <- temp$pred2
preddf2_3 <- preddf2_1*preddf2_2

### re-run without cross validation (performance metrics for full model)
temp <- RunCrossVal(fn=BUGSfilename_noclim,foldvec,daymet = F,clim=F,internal=T,CV=F,
                    fullmod=mcmc.list_noclim,dr=did.reproduce,fst=first,lst=last,ut=uniqtort)
sedf_nocv2 <- temp$se
preddf_nocv2_1 <- temp$pred1
preddf_nocv2_2 <- temp$pred2
preddf_nocv2_3 <- preddf_nocv2_1*preddf_nocv2_2

rmse_cv_vector <- apply(sedf2,1,function(t) sqrt(mean(t)) )
quantile(rmse_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%       5%      10%      25%      50%      75%      90%      95%    97.5% 
# 3.554849 3.565057 3.578329 3.599796 3.623500 3.647884 3.671573 3.686015 3.697929


rmse_cv_vector <- apply(sedf_nocv2,1,function(t) sqrt(mean(t)) )
quantile(rmse_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%       5%      10%      25%      50%      75%      90%      95%    97.5% 
# 3.425914 3.436948 3.448751 3.468689 3.489372 3.509103 3.525422 3.534425 3.541588  

### compute R-squared for the model with cross-validation
allobs <- colnames(preddf2_3)
realobs <- numeric(length(allobs))
realdr <- numeric(length(allobs))

i=1
for(i in 1:length(allobs)){
  this <- allobs[i]
  this <- gsub("p.reproduce\\[","",this)
  this <- gsub("\\]","",this)
  temp <- as.numeric(strsplit(this,",")[[1]])
  ind=temp[1];yr=temp[2]
  realobs[i] <- datforjags$data.for.bugs$obs.numeggs[ind,yr]
  realdr[i] <- datforjags$data.for.bugs$did.reproduce[ind,yr]
}
meanobs <- mean(realobs)

rmse_null <- sqrt(mean((realobs-meanobs)^2))
rmse_null

preddf3 <- preddf2_3
rsquared_cv_vector <- sapply(1:nrow(preddf3),rsquaredfunc )
quantile(rsquared_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%           5%          10%          25%          50%          75%          90%          95%        97.5% 
# -0.003034514  0.003418336  0.011212494  0.023930553  0.036936092  0.049495173  0.060797859  0.067751423  0.073082884

preddf3_nocv = preddf_nocv2_3
rsquared_cv_vector <- sapply(1:nrow(preddf3_nocv),rsquaredfunc_nocv )
quantile(rsquared_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%         5%        10%        25%        50%        75%        90%        95%      97.5% 
# 0.07998554 0.08370334 0.08836532 0.09678579 0.10691413 0.11747070 0.12758662 0.13354795 0.13910215

# rsquared- just ep process
preddf3 <- preddf2_2[,realdr==1]
realobs <- realobs[realdr==1]
rsquared_cv_vector <- sapply(1:nrow(preddf3),rsquaredfunc )
quantile(rsquared_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
# 0.2223357 0.2242600 0.2267095 0.2302825 0.2338967 0.2374929 0.2402598 0.2419444 0.2431952

preddf3_nocv <- preddf_nocv2_2[,realdr==1]
#realobs <- realobs[realdr==1]
rsquared_cv_vector <- sapply(1:nrow(preddf3_nocv),rsquaredfunc_nocv )
quantile(rsquared_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
# 0.2302865 0.2328468 0.2353000 0.2384854 0.2409640 0.2426238 0.2435386 0.2439082 0.2441622

preddf1 <- preddf2_1
roc_cv_vector <- sapply(1:nrow(preddf1), rocfunc )
quantile(roc_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5%           # No better than random!
# 0.4768854 0.4863546 0.4976680 0.5166279 0.5382683 0.5585780 0.5765378 0.5881173 0.5976375   

preddf1_nocv <- preddf_nocv2_1
roc_cv_vector <- sapply(1:nrow(preddf1_nocv), rocfunc_nocv )
quantile(roc_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
# 0.6150182 0.6267681 0.6389593 0.6588972 0.6806779 0.6995346 0.7142034 0.7229144 0.7296160 

####################################
##### Cross validation with only climate variables
####################################

BUGSfilename_onlyclim <- writeJAGSmod_onlyclim()  # write JAGS model
ndx <- numeric(0)   # for now, no left out for CV
datforjags <- prepareJAGSdat(indices=ndx,daymet = T,clim=T,internal = F)    # function prepares data for running JAGS

##### run full model with only climate
mod3 <- runJAGSmod(fn=BUGSfilename_onlyclim,indat=datforjags)   # run JAGS model
Mod.mcmc <- mod3$mcmc
Mod.mcmc.list<- coda::mcmc.list(Mod.mcmc)
### unlist each chain and combine into one df
mcmc.list_onlyclim <- as.data.frame(runjags::combine.mcmc(Mod.mcmc.list))  #  row.names=1:60000

temp <- RunCrossVal(fn=BUGSfilename_onlyclim, foldvec=foldvec,daymet = T,clim=T,internal=F,CV=T,
                    fullmod=mcmc.list_onlyclim,dr=did.reproduce,fst=first,lst=last,ut=uniqtort)    # takes a while to run
sedf3 <- temp$se
preddf3_1 <- temp$pred1
preddf3_2 <- temp$pred2
preddf3_3 <- preddf3_1*preddf3_2

### re-run without cross validation (performance metrics for full model)
temp <- RunCrossVal(fn=BUGSfilename_onlyclim,foldvec=foldvec,daymet = T,clim=T,internal=F,CV=F,
                    fullmod=mcmc.list_onlyclim,dr=did.reproduce,fst=first,lst=last,ut=uniqtort)
sedf_nocv3 <- temp$se
preddf_nocv3_1 <- temp$pred1
preddf_nocv3_2 <- temp$pred2
preddf_nocv3_3 <- preddf_nocv3_1*preddf_nocv3_2

rmse_cv_vector <- apply(sedf3,1,function(t) sqrt(mean(t)) )
quantile(rmse_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))   

# 2.5%       5%      10%      25%      50%      75%      90%      95%    97.5% 
# 3.416394 3.420859 3.426424 3.436290 3.446961 3.457904 3.468566 3.475094 3.481386

rmse_cv_vector <- apply(sedf_nocv3,1,function(t) sqrt(mean(t)) )
quantile(rmse_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))   

# 2.5%       5%      10%      25%      50%      75%      90%      95%    97.5% 
# 3.391419 3.395015 3.399904 3.407823 3.416638 3.426351 3.436137 3.442465 3.448477 

### compute R-squared for the model with cross-validation
allobs <- colnames(preddf3_3)
realobs <- numeric(length(allobs))
realdr <- numeric(length(allobs))

i=1
for(i in 1:length(allobs)){
  this <- allobs[i]
  this <- gsub("p.reproduce\\[","",this)
  this <- gsub("\\]","",this)
  temp <- as.numeric(strsplit(this,",")[[1]])
  ind=temp[1];yr=temp[2]
  realobs[i] <- datforjags$data.for.bugs$obs.numeggs[ind,yr]
  realdr[i] <- datforjags$data.for.bugs$did.reproduce[ind,yr]
}

meanobs <- mean(realobs)
rmse_null <- sqrt(mean((realobs-meanobs)^2))
rmse_null

preddf3 <- preddf3_3
rsquared_cv_vector <- sapply(1:nrow(preddf),rsquaredfunc )
quantile(rsquared_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
# 0.1109974 0.1142082 0.1175327 0.1229500 0.1284919 0.1338794 0.1388460 0.1416412 0.1438799

preddf3_nocv = preddf_nocv3_3
rsquared_cv_vector <- sapply(1:nrow(preddf3_nocv),rsquaredfunc_nocv )
quantile(rsquared_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
# 0.1277252 0.1307638 0.1339567 0.1388827 0.1437579 0.1481704 0.1521248 0.1545619 0.1563514

# rsquared- just ep process
preddf3 <- preddf3_2[,realdr==1]
realobs <- realobs[realdr==1]
rsquared_cv_vector <- sapply(1:nrow(preddf3),rsquaredfunc )
quantile(rsquared_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
# 0.2225881 0.2253892 0.2284708 0.2334263 0.2387180 0.2438433 0.2483646 0.2508951 0.2529218

preddf3_nocv <- preddf_nocv3_2[,realdr==1]
#realobs <- realobs[realdr==1]
rsquared_cv_vector <- sapply(1:nrow(preddf3_nocv),rsquaredfunc_nocv )
quantile(rsquared_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
# 0.2381083 0.2415057 0.2450331 0.2495031 0.2530955 0.2553720 0.2565923 0.2570237 0.2573111


preddf1 <- preddf3_1
roc_cv_vector <- sapply(1:nrow(preddf1), rocfunc )
quantile(roc_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
# 0.6591774 0.6624884 0.6663325 0.6729383 0.6795680 0.6861678 0.6917293 0.6952381 0.6978637 

preddf1_nocv <- preddf_nocv3_1
roc_cv_vector <- sapply(1:nrow(preddf1_nocv), rocfunc_nocv )
quantile(roc_cv_vector,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))

# 2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
# 0.6875045 0.6881967 0.6886717 0.6904881 0.6961690 0.6971715 0.6981740 0.6983172 0.6992720 







#   ###### remove entire sites at a time
# rownames(jags_dat$ReproStatus)
# rownames(obs.numeggs)
# sites <- gsub('[[:digit:]]+', '', rownames(obs.numeggs))
# sites <- gsub("\\.","",sites)
# 
# BUGSfilename <- writeJAGSmod()  # write JAGS model
# foldvec <- as.numeric(as.factor(sites))
# n.folds <- max(foldvec)
# cvdf_bysite <- RunCrossVal(foldvec,daymet = T)    # takes a while to run
# 
# rmse_cv_vector3 <- apply(cvdf_bysite,1,function(t) sqrt(mean(t)) )
# quantile(rmse_cv_vector3,c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))



############
# WAIC


library(loo)

cols <- grep("LogLik",names(mcmc.list))

names(mcmc.list)[cols]

length(cols)   # number of data points   

nrow(mcmc.list)    # number of mcmc samples

lik_df <- mcmc.list[,cols]

Allcovarswaic_25_5_MCL_Precip<-waic(as.matrix(lik_df))
Allcovarswaic_25_5_MCL_Precip

############
#Parameter stats
params <- c("b.p.reproduce","b.p.reproduce.l","b.numeggs","beta.cp","beta.MCLpr",
            "beta.MCLel","beta.st","beta.MTRpr","beta.reproduce")
cols <- 1

for(i in 1:length(params)){
  keep <- grep(params[i], names(mcmc.list))
  cols <- as.numeric(c(cols, keep))
}
cols <- cols[-1]

all.mean <- apply(mcmc.list[cols], 2, mean)
all.median <- apply(mcmc.list[cols], 2, median)


library(HDInterval)

all.hdi.95 <- apply(mcmc.list[cols], 2, function(x) hdi(x, 0.95))
all.hdi.95.df<-t(as.data.frame(all.hdi.95))
#####################################

mod1_results<-cbind(all.mean, all.median, all.hdi.95.df)

write.csv(mod1_results, file = "EDM1_params_lc_uc_allcovars_25_5_MCL_Precip_updated2.csv")

save.image(file=sprintf("EDM_RunModel_%s.RData",Sys.Date()))