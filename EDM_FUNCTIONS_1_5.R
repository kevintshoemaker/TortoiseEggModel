###################################################################################
####  Unscrambling the drivers of egg production in Agassiz's desert tortoise: ####
#########  Climate and individual attributes predict reproductive output  #########
#########  C.Mitchell, D.Friend, L.Phillips, E.Hunter, J.Lovich, M.Agha  ##########
########  S.Puffer, K.Cummings, P.Medica, T.Esque, K.Nussear, K.Shoemaker #########
###################################################################################


###################################
######### CUSTOM FUNCTIONS

#----------------------------
#reshape_egg_data()
#----------------------------
#DESCRIPTION
  #reshapes the egg data in the format we want it in for JAGS (rows - tortoise; columns - year)
#PARAMETERS
  #egg_data -> the egg data
  #tort_id_col -> the name of the column that contains the unique ID for each tortoise (i.e. TortSite)
  #year -> the name of the column that contains the year (i.e. Year)
  #val_col -> the name of the column that contains the data values
#RETURNS
  #a matrix; there is one row for each unique totoise. There is one
  #column for each year. The values are the data values in the val_col
  #column of the input data frame
#NOTE - the 'add_1996_NA_col' is a quick parameter I added - it's a bit of a hack, and is a hideous way
  #to accomplish what I'm trying to do (for the climate variables I wanted 1996 to actually have values, 
  #and the code I had before was setting it to NA... ). Doing it this way is hideous because it restricts
  #this function to this specific situation.. so in the future if I were to reuse this it'd be best to 
  #change this to make it better, but I don't think it's worth my time right now.
reshape_egg_data = function(egg_data, tort_id_col, year_col, val_col, add_1996_NA_col = TRUE){
  #first reshape the data
  egg_data_sub = egg_data[,c(tort_id_col, year_col, val_col)] #subset the data to only the columns we care about right now
  egg_data_sub = egg_data_sub[order(egg_data_sub[,year_col]),] #order the year column... this should make it so that after reshaping the columns are in the correct order
  dat_rshp1 <- reshape(data=egg_data_sub, idvar = tort_id_col, direction = "wide", timevar = year_col, v.names = val_col, sep=".") #reshape the data
  #head(dat_rshp1)
  
  #I want to get rid of the "TortSite" and instead have the names be row names
  rownames(dat_rshp1) = dat_rshp1[,tort_id_col] #set the rownames to be the same as 'TortSite'
  dat_rshp2 = dat_rshp1[,names(dat_rshp1) != tort_id_col] #get rid of the 'TortSite' column
  #head(dat_rshp2)
  
  #get rid of the prefix that reshape() adds to the column names
  dat_rshp2_names_split = strsplit(names(dat_rshp2), "[.]") #split the names based on the "." that is automatically added after the variable name
  dat_rshp3 = dat_rshp2
  names(dat_rshp3) = sapply(dat_rshp2_names_split, function(thing_i){return(thing_i[2])}) #retrive the year from the strings that we just split
  #head(dat_rshp3)
  
  #now that we've gotten the names how we want them, add a column called "1996" that just has NA's
  dat_rshp4 = dat_rshp3
  if(add_1996_NA_col){
    dat_rshp4$`1996` = NA
  }
  #head(dat_rshp4)
  
  #the columns aren't necessarily in the right order... re-order them
  dat_rshp5 = dat_rshp4[,order(names(dat_rshp4))]
  #head(dat_rshp5)
  
  return(as.matrix(dat_rshp5))
}


#----------------------------
#get_seasonal_prism_data()
#----------------------------
#DESCRIPTION
  #Makes use of the monthly PRISM data for each site (which I extracted and output in 'get_monthly_prism_data.R').
  #Aggregates the data in order to get values for seasons - the user can set a range of months and the data is   
  #aggregated for those months, using a user chosen function (i.e. mean(), sum(), etc.)
#PARAMETERS
  #monthly_prism_data -> A data frame containing monthly climate data for each site. Use the data output by 'get_monthly_prism_data.R' - 
    #the script outputs a CSV called "ppt_monthly_1995_2003.csv". This function expects a data frame with four columns:
    #'site', 'year', 'month', and 'val'
  #month_begin -> integer (1-12); the first month of the season
  #month_end -> integer (1-12); the last month of the season (this can be less than month_begin - the function will handle it)
  #agg_func -> the function to use to aggregate the data across a season (for example, 'sum' will sum the values
    #for each month, while 'mean' while take the average value across the months)
#RETURNS
  #a data frame with 7 columns:
    #season_id: the ID of the season. This is generated internally for the purpose of doing the aggregation - it is of little use afterwards
    #site: the name of the site
    #val: the value that results from using the aggregation function specified by 'agg_func'
    #start_year: the starting year of the season
    #start_month: the starting month of the season
    #end_year: the ending year of the season
    #end_month: the ending month of the season
get_seasonal_prism_data = function(monthly_prism_data, month_begin, month_end, agg_func = sum){
  prism = monthly_prism_data #I don't feel like typing that name out all the time, but I don't want to change the parameter name because it's nice and descriptive
  
  if(month_end < month_begin){
    month_end = month_end + 12
  }
  
  start_year = min(prism$year)
  end_year = max(prism$year)
  
  #get the range of months
  months = ((month_begin:month_end)-1)%%12 + 1
  
  #get a vector of months - one for each winter month
  months_vec = rep(months, length(start_year:end_year))
  #get a vector of years - one for each winter month
  years_vec = unlist(lapply(start_year:end_year, function(year_i){
    return(year_i + floor((month_begin:month_end-1)/12))
  }))
  #create IDs for each season so we know which season a month belongs to 
  season_id_vec = sort(rep(1:length(start_year:end_year), length(months)))
  season_df = data.frame(year = years_vec, month = months_vec, season_id = season_id_vec)
  head(season_df)
  
  prism_join = plyr::join(prism, season_df, by=c("year", "month"))
  #head(prism_join, 30)
  season_agg = aggregate(val ~ season_id + site, prism_join, agg_func)
  
  start_years = aggregate(year ~ season_id + site, prism_join, min)
  names(start_years) = c("season_id", "site", "start_year")
  
  end_years = aggregate(year ~ season_id + site, prism_join, max)
  names(end_years) = c("season_id", "site", "end_year")
  
  start_months = aggregate(month ~ season_id + site, prism_join, head, n=1)
  names(start_months) = c("season_id", "site", "start_month")
  
  end_months = aggregate(month ~ season_id + site, prism_join, tail, n=1)
  names(end_months) = c("season_id", "site", "end_month")
  
  #head(start_years)
  #head(end_years)
  site_vals = plyr::join_all(list(season_agg, start_years, start_months, end_years, end_months), by = c("season_id", "site"))
  site_vals_mod = site_vals[site_vals$start_month == month_begin & site_vals$end_month == ((month_end-1)%%12 + 1),] #get rid of incomplete seasons
  return(site_vals_mod)
}

#----------------------------
#format_data_for_JAGS()
#----------------------------
#DESCRIPTION
  #takes the tortoise egg data and puts it into the correct format for JAGS by utilizing the 
  #'reshape_egg_data()' function
#PARAMETERS
  #dat -> the egg data from the CSV
#RETURNS
  #a list where each element is a matrix or vector containing information needed by JAGS
#NOTE - I pulled this code out into a function in order to make the code in
  #"EDM_MAIN_SCRIPT" more streamlined and to abstract away some of the messiness
  #involved in formatting the data. The purpose of this function is just for the
  #user's convenience - this is NOT a reusable function, as it relies on data in
  #a data frame with specific column names and data types and also uses hard coded
  #values for the climate parameters
format_data_for_JAGS = function(eggs, prism, daymet){
  #define the MTR and precip windows, and the jdate parameters
  mtr_start_month = 3
  mtr_end_month = 4
  
  prcp_start_month = 3
  prcp_end_month = 15 #15 indicates the NEXT year's March (12 + 3)
  
  #for each site, I'll look for periods of time of length 'n_days' that don't go below 'min_temp'. I'll
  #keep track of the FIRST date of each of the periods of time. And then I'll return the earliest of these days 
  min_temp = 25 #the minimum temperature, in Celsius
  n_days = 5 #the minimum number of days
  
  #================================================================
  #Retrieve the data and initialize storage list
  #================================================================
  # 
  # eggs0 = read.csv(tort_dat_path)
  # keep_cols = c("Site", "Tort", "Year", "ReproStatus", "MCL", "TE", "TortSite")
  # eggs = eggs0[,keep_cols]
  # head(eggs, 20)
  
  tort_site = unique(eggs[,c("TortSite", "Site")])
  all_tort_year0 = expand.grid(TortSite = unique(eggs$TortSite), Year = 1996:max(eggs$Year))
  all_tort_year = plyr::join(all_tort_year0, tort_site, by="TortSite")
  dat_all_years = merge(all_tort_year, eggs[,-which(names(eggs) == "Site" | names(eggs) == "Tort")], by=c("TortSite", "Year"), all.x=TRUE)
  head(dat_all_years)
  
  #================================================================
  #get the data for precip (March [prev year] to March)
  #================================================================
  prcp_dat = prism[,c("site", "year", "month", "ppt")]
  names(prcp_dat)[names(prcp_dat) == "ppt"] = "val"
  head(prcp_dat)
  prcp_window = get_seasonal_prism_data(prcp_dat, 3, 15, agg_func = sum)
  #names(prcp_window)[names(prcp_window) == "val"] = "precip_window"
  head(prcp_window)
  
  eggs_precip_window = merge(all_tort_year, prcp_window[,c("site", "end_year", "val")], by.x = c("Site", "Year"), by.y = c("site", "end_year"), sort = FALSE)
  names(eggs_precip_window)[names(eggs_precip_window) == "val"] = "precip_window"
  head(eggs_precip_window)
  
  #================================================================
  #get the data for MTR (March-April)
  #================================================================
  mtr_dat = prism
  mtr_dat$val = prism$tmax - prism$tmin
  
  mtr_window = get_seasonal_prism_data(mtr_dat, mtr_start_month, mtr_end_month, agg_func = mean)
  eggs_mtr_window = merge(all_tort_year, mtr_window[,c("site", "start_year", "val")], by.x = c("Site", "Year"), by.y = c("site", "start_year"), sort = FALSE)
  names(eggs_mtr_window)[names(eggs_mtr_window) == "val"] = "mtr_window"
  head(eggs_mtr_window)
  
  
  #================================================================
  #reshape the DAYMET data for the temperature threshold
  #================================================================
  
  #--------------------------------------------
  #get the Julian date for the earliest day where the next n_days days do not go below min_temp
  
  #we want to get this date for each site and year
  unique_combs = unique(daymet[,c("Site", "Year")])
  jdate_temp_list = lapply(1:nrow(unique_combs), function(i){
    site_i = unique_combs$Site[i] #get the site and year (this is for convenience and readability)
    year_i = unique_combs$Year[i]
    
    dat_i = daymet[daymet$Year == year_i  & daymet$Site == site_i,] #get the DayMet data for this site and year
    rollmin = zoo::rollapply(dat_i$tmax, width = n_days, FUN = min, fill=NA, align = "left") #find the 'rolling minimum' - for each date, look at the next 'n_days' (in the future), and then take the minimum value of these temperature
    rollmin_bool = rollmin >= min_temp #if the value of the rolling minimum is above 'min_temp', that means that we know that for at least 'n_days' days (starting at the given date) the maximum temperature was not below 'min_temp'
    first_day = min(dat_i$Jdate[which(rollmin_bool)]) #use the Boolean to get the earliest of these days
    return(data.frame(Site = site_i, Year = year_i, daymet_jdate = first_day)) #return the results as a single-row data frame so that we can easily rbind the results into a single data frame.
  })
  #compile the data we just calculated
  temp_jdate = do.call(rbind,jdate_temp_list)
  head(temp_jdate)
  
  #now join the data for each site and year to the egg data so that we can get a value for each tortoise and year
  eggs_daymet_jdate = plyr::join(all_tort_year, temp_jdate, by = c("Site", "Year"))
  head(eggs_daymet_jdate)
  
  #====================================================
  #Combine all of the individual data frames
  #====================================================
  eggs_final = plyr::join_all(list(dat_all_years, eggs_precip_window, eggs_mtr_window, eggs_daymet_jdate), by=c("TortSite", "Year", "Site"))
  eggs_final = eggs_final[order(eggs_final$Site, eggs_final$Year, eggs_final$TortSite),]
  head(eggs_final,50)
  
  jags_dat = list() #initialize our 'jags_dat' list, which will store the data for each variable
  
  #loop over each of our "data" columns and reshape them into the format required by JAGS
  data_cols = c("MCL", "ReproStatus", "TE", "precip_window", "mtr_window", "daymet_jdate")
  jags_dat = lapply(data_cols, function(col_i){
    return(reshape_egg_data(eggs_final, tort_id_col = "TortSite", year_col = "Year", val_col=col_i, add_1996_NA_col = FALSE))
  })
  names(jags_dat) = data_cols
  jags_dat
  #====================================================
  #for each tortoise, we also need to get the first and last years that we have data for
  
  #get the max and min years and stick them in our 'jags_dat' list
  min_year = aggregate(Year ~ TortSite, eggs, min)
  max_year = aggregate(Year ~ TortSite, eggs, max)
  
  jags_dat$first_year = min_year$Year
  names(jags_dat$first_year) = min_year$TortSite
  jags_dat$last_year = max_year$Year
  names(jags_dat$last_year) = max_year$TortSite
  
  #====================================
  #check order of rows
  #====================================
  #we need to be absolutely sure that each of the matrices is in the same order
  
  #create a vector that will serve as the "reference order" - we'll order all the matrices to match this order
  tortSite_order = unique(eggs_final$TortSite[order(eggs_final$Site)])
  #put all the matrices and vectors in our list in the same order, based on a "reference order" vector we created above ('tortSite_order')
  jags_dat = lapply(jags_dat, function(thing_i){
    if(class(thing_i) == "matrix"){
      thing_ordered_i = thing_i[match(tortSite_order, rownames(thing_i)),]
    } else{
      thing_ordered_i = thing_i[match(tortSite_order, names(thing_i))]
    }
    return(thing_ordered_i)
  })
  
  name_check_list = lapply(jags_dat, function(thing_i){ #retrieve the names/rownames of the objects in the list
    if(class(thing_i) == "matrix"){
      return(rownames(thing_i))
    } else{
      return(names(thing_i))
    }
  })
  name_check = data.frame(do.call(cbind, name_check_list)) #create a dataframe where each column contains the names/rownames for a given element in the list
  name_check$order = tortSite_order #add a column for the TRUE order (the order we used to resort all of the matrices/vectors)
  head(name_check,20) #take a peek at it
  order_is_correct = apply(name_check, MARGIN = 1, function(row_i){ #loop over each row - check if all the elements in that row are the same (they should be) and return a boolean
    return(all(row_i == row_i["order"]))
  })
  
  if(!all(order_is_correct)){ #if it's TRUE, then all of our rows match up. If it's FALSE, something's wrong
    stop("The matrices in 'jags_dat' having varying row orders (they should be in the same order, and ordered by tortoise ID)")
  } 
  return(jags_dat)
  
}

writeJAGSmod <- function(){
  ###############
  # BUGS MODEL
  ###############
  
  
  BUGSfilename <- "Eggcellent_BUGS_25_5_MCL_Precip.txt"
  
  
  cat("
    
    model{
    
       	
    #############
    # LIKELIHOOD
    ############# 
    
        for(ind in 1:n.inds){                 # loop through each individual
                                              
            logit(p.reproduce[ind,first[ind]-1]) <- b.p.reproduce.l
            did.reproduce[ind,first[ind]-1] ~ dbern(p.reproduce[ind,first[ind]-1])
            for(yr in first[ind]:last[ind]){      
            ### loop through all years where there is data for each ind
            ### prob of reprod is a function of baseline prob and whether or not ind reproduced yr prior
    
                logit(p.reproduce[ind,yr]) <- b.p.reproduce.l 
                                            + beta.reproduce*(did.reproduce[ind,yr-1])
                                            + beta.cp.pr*c.precip[ind,yr]
                                            + beta.MCLpr*MCL[ind,yr]  
                                            + beta.MTRpr*MTR[ind,yr]   ### monthly temp range for PR
          
                                            
                did.reproduce[ind,yr] ~ dbern(p.reproduce[ind,yr])    ## datanode
                log(exp.numeggs[ind,yr]) <-  log(b.numeggs) 
                                    + beta.st*sp.temp[ind,yr]    ### spring temp
                                    + beta.MCLel*MCL[ind,yr]     ### MCL 
                                    + beta.cp*c.precip[ind,yr]   ### cumulative precip for EL
                                    
                
                exp.numeggs2[ind,yr] <-  did.reproduce[ind,yr]*exp.numeggs[ind,yr]                     
                obs.numeggs[ind,yr] ~ dpois(exp.numeggs2[ind,yr])     ## datanode
                obs.numeggs.sim[ind,yr] ~ dpois(exp.numeggs2[ind,yr])  ### simulate new data  
                                
                LogLik[ind,yr] <- log(dpois(obs.numeggs[ind,yr],exp.numeggs2[ind,yr]))   # add log likelihood computation for each observation to compute WAIC
                exp.numeggs3[ind,yr] <-  p.reproduce[ind,yr]*exp.numeggs[ind,yr]
            }
        } 
        
        ############
        # DEAL WITH NAS
        ############
        
         mcl0 ~ dunif(-2,2)    ### Had to change to match scaled predictors
         mcl0.p ~ dgamma(0.001,0.001)
         mcly.p ~ dgamma(0.001,0.001)
         
         for(ind in 1:n.inds){ 
           mmcl[ind] ~ dnorm(mcl0,mcl0.p)
           for(yr in first[ind]:last[ind]){
             MCL[ind,yr] ~ dnorm(mmcl[ind],mcly.p)
           }
         }
    
    #############
    # PRIORS
    #############
          
          b.p.reproduce ~ dbeta(1,1)    # baseline prob of reproducing 
          b.p.reproduce.l <- log(b.p.reproduce/(1-b.p.reproduce))
          b.numeggs ~   dunif(1,15)      # baseline num eggs
          beta.reproduce ~ dnorm(0,0.1)      # coefficient of prior year status
          beta.st ~ dnorm(0,0.1)             # coefficient of spring precip 
          beta.MCLpr  ~ dnorm(0,0.1)           # coefficient of MCL for PR
          beta.MCLel  ~ dnorm(0,0.1)           # coefficient of MCL for EL
          beta.MTRpr ~  dnorm(0,0.1)          # coeff for monthly temp range for PR
          beta.cp ~ dnorm(0,0.1)           # coeff for cummulative precip for EL
          beta.cp.pr ~ dnorm(0,0.1)
    #############
      # DERIVED QUANTITIES
      #############
      for(ind in 1:n.inds){  
        for(yr in first[ind]:last[ind]){  
          SE_obs[ind,yr] <- pow(obs.numeggs.full[ind,yr]-exp.numeggs3[ind,yr],2)      
          SE_sim[ind,yr] <- pow(obs.numeggs.sim[ind,yr]-exp.numeggs3[ind,yr],2)
        }
        
      }
      
      log(exp.numeggs.m) <-  
                                    log(b.numeggs) 
                                    + beta.st*st.m                   ### spring temp
                                    + beta.MCLel*MCL.m                     ### MCL 
                                    + beta.cp*cp.m
      logit(p.reproduce.m) <- 
                                    b.p.reproduce.l 
                                    + beta.reproduce*1
                                    + beta.cp.pr*cp.m
                                    + beta.MCLpr*MCL.m  
                                    + beta.MTRpr*MTR.m          ### monthly temp range for PR 
     
      for(st in 1:20){
        log(exp.numeggs.st[st]) <-  
                                    log(b.numeggs) 
                                    + beta.st*sts[st]                   ### spring temp
                                    + beta.MCLel*MCL.m                     ### MCL 
                                    + beta.cp*cp.m
        
        unc.numeggs.st[st] <- exp.numeggs.st[st] * p.reproduce.m
    
      }
      
      for(mcl in 1:20){
        log(exp.numeggs.mcl[mcl]) <-  
                                    log(b.numeggs) 
                                    + beta.st*st.m                   ### spring temp
                                    + beta.MCLel*mcls[mcl]                     ### MCL 
                                    + beta.cp*cp.m
        logit(p.reproduce.mcl[mcl]) <- 
                                    b.p.reproduce.l 
                                    + beta.reproduce*1
                                    + beta.cp.pr*cp.m
                                    + beta.MCLpr*mcls[mcl]  
                                    + beta.MTRpr*MTR.m   ### monthly temp range for PR
        unc.numeggs.mcl[mcl] <- exp.numeggs.mcl[mcl] * p.reproduce.mcl[mcl]
      }
      
      for(cp in 1:20){
        log(exp.numeggs.cp[cp]) <-  
                                    log(b.numeggs) 
                                    + beta.st*st.m                   ### spring temp
                                    + beta.MCLel*MCL.m                     ### MCL 
                                    + beta.cp*cps[cp]
        logit(p.reproduce.cp[cp]) <- 
                                    b.p.reproduce.l 
                                    + beta.reproduce*1
                                    + beta.cp.pr*cps[cp]
                                    + beta.MCLpr*MCL.m  
                                    + beta.MTRpr*MTR.m          ### monthly temp range for PR
                                    
        unc.numeggs.cp[cp] <- exp.numeggs.cp[cp] * p.reproduce.cp[cp]
      }
                          
      
      for(mtr in 1:20){
        logit(p.reproduce.mtr[mtr]) <- 
                                    b.p.reproduce.l 
                                    + beta.reproduce*1
                                    + beta.cp.pr*cp.m
                                    + beta.MCLpr*MCL.m  
                                    + beta.MTRpr*MTRs[mtr]   ### monthly temp range for PR
        unc.numeggs.mtr[mtr] <- exp.numeggs.m * p.reproduce.mtr[mtr]        
      }
      
      drs[1] <- 0
      drs[2] <- 1
      for(dr in 1:2){
        logit(p.reproduce.dr[dr]) <- 
                                    b.p.reproduce.l 
                                    + beta.reproduce*drs[dr]
                                    + beta.cp.pr*cp.m
                                    + beta.MCLpr*MCL.m  
                                    + beta.MTRpr*MTR.m   ### monthly temp range for PR
        unc.numeggs.dr[dr] <- exp.numeggs.m * p.reproduce.dr[dr]
      }

    }   ## end BUGS model
    
    ",file=BUGSfilename)
  return(BUGSfilename)
}

writeJAGSmod_siteeff <- function(){
  ###############
  # BUGS MODEL
  ###############
  
  
  BUGSfilename <- "Eggcellent_BUGS_25_5_MCL_Precip_siteeff.txt"
  
  
  cat("
    
    model{
    
       	
    #############
    # LIKELIHOOD
    ############# 
    
        for(ind in 1:n.inds){                 # loop through each individual
                                              
            logit(p.reproduce[ind,first[ind]-1]) <- b.p.reproduce.l
            did.reproduce[ind,first[ind]-1] ~ dbern(p.reproduce[ind,first[ind]-1])
            for(yr in first[ind]:last[ind]){      
            ### loop through all years where there is data for each ind
            ### prob of reprod is a function of baseline prob and whether or not ind reproduced yr prior
    
                logit(p.reproduce[ind,yr]) <- b.p.reproduce.l 
                                            + beta.reproduce*(did.reproduce[ind,yr-1])
                                            + beta.cp.pr*c.precip[ind,yr]
                                            + beta.MCLpr*MCL[ind,yr]  
                                            + beta.MTRpr*MTR[ind,yr]   ### monthly temp range for PR
                                            + siteeff1[sitenum[ind]]
          
                                            
                did.reproduce[ind,yr] ~ dbern(p.reproduce[ind,yr])    ## datanode
                log(exp.numeggs[ind,yr]) <-  log(b.numeggs) 
                                    + beta.st*sp.temp[ind,yr]    ### spring temp
                                    + beta.MCLel*MCL[ind,yr]     ### MCL 
                                    + beta.cp*c.precip[ind,yr]   ### cumulative precip for EL
                                    + siteeff2[sitenum[ind]]
                                    
                
                exp.numeggs2[ind,yr] <-  did.reproduce[ind,yr]*exp.numeggs[ind,yr]                     
                obs.numeggs[ind,yr] ~ dpois(exp.numeggs2[ind,yr])     ## datanode
                obs.numeggs.sim[ind,yr] ~ dpois(exp.numeggs2[ind,yr])  ### simulate new data  
                                
                LogLik[ind,yr] <- log(dpois(obs.numeggs[ind,yr],exp.numeggs2[ind,yr]))   # add log likelihood computation for each observation to compute WAIC
                exp.numeggs3[ind,yr] <-  p.reproduce[ind,yr]*exp.numeggs[ind,yr]
            }
        } 
        
        ############
        # DEAL WITH NAS
        ############
        
         mcl0 ~ dunif(-2,2)    ### Had to change to match scaled predictors
         mcl0.p ~ dgamma(0.001,0.001)
         mcly.p ~ dgamma(0.001,0.001)
         
         for(ind in 1:n.inds){ 
           mmcl[ind] ~ dnorm(mcl0,mcl0.p)
           for(yr in first[ind]:last[ind]){
             MCL[ind,yr] ~ dnorm(mmcl[ind],mcly.p)
           }
         }
    
    #############
    # PRIORS
    #############
          
          b.p.reproduce ~ dbeta(1,1)    # baseline prob of reproducing 
          b.p.reproduce.l <- log(b.p.reproduce/(1-b.p.reproduce))
          b.numeggs ~   dunif(1,15)      # baseline num eggs
          beta.reproduce ~ dnorm(0,0.1)      # coefficient of prior year status
          beta.st ~ dnorm(0,0.1)             # coefficient of spring precip 
          beta.MCLpr  ~ dnorm(0,0.1)           # coefficient of MCL for PR
          beta.MCLel  ~ dnorm(0,0.1)           # coefficient of MCL for EL
          beta.MTRpr ~  dnorm(0,0.1)          # coeff for monthly temp range for PR
          beta.cp ~ dnorm(0,0.1)           # coeff for cummulative precip for EL
          beta.cp.pr ~ dnorm(0,0.1)
          
          siteprec.rp ~ dgamma(0.001,0.001)       # site-level random effects
          siteprec.ep ~ dgamma(0.001,0.001)
          sitesd.rp <- pow((1/siteprec.rp),0.5)
          sitesd.ep <- pow((1/siteprec.ep),0.5)
          
          for(site in 1:nsites){
            siteeff1[site] ~ dnorm(0,siteprec.rp)
            siteeff2[site] ~ dnorm(0,siteprec.ep)
          }
          
    #############
      # DERIVED QUANTITIES
      #############
      for(ind in 1:n.inds){  
        for(yr in first[ind]:last[ind]){  
          SE_obs[ind,yr] <- pow(obs.numeggs.full[ind,yr]-exp.numeggs3[ind,yr],2)      
          SE_sim[ind,yr] <- pow(obs.numeggs.sim[ind,yr]-exp.numeggs3[ind,yr],2)
        }
        
      }
      
      log(exp.numeggs.m) <-  
                                    log(b.numeggs) 
                                    + beta.st*st.m                   ### spring temp
                                    + beta.MCLel*MCL.m                     ### MCL 
                                    + beta.cp*cp.m
      logit(p.reproduce.m) <- 
                                    b.p.reproduce.l 
                                    + beta.reproduce*1
                                    + beta.cp.pr*cp.m
                                    + beta.MCLpr*MCL.m  
                                    + beta.MTRpr*MTR.m          ### monthly temp range for PR 
     
      for(st in 1:20){
        log(exp.numeggs.st[st]) <-  
                                    log(b.numeggs) 
                                    + beta.st*sts[st]                   ### spring temp
                                    + beta.MCLel*MCL.m                     ### MCL 
                                    + beta.cp*cp.m
        
        unc.numeggs.st[st] <- exp.numeggs.st[st] * p.reproduce.m
    
      }
      
      for(mcl in 1:20){
        log(exp.numeggs.mcl[mcl]) <-  
                                    log(b.numeggs) 
                                    + beta.st*st.m                   ### spring temp
                                    + beta.MCLel*mcls[mcl]                     ### MCL 
                                    + beta.cp*cp.m
        logit(p.reproduce.mcl[mcl]) <- 
                                    b.p.reproduce.l 
                                    + beta.reproduce*1
                                    + beta.cp.pr*cp.m
                                    + beta.MCLpr*mcls[mcl]  
                                    + beta.MTRpr*MTR.m   ### monthly temp range for PR
        unc.numeggs.mcl[mcl] <- exp.numeggs.mcl[mcl] * p.reproduce.mcl[mcl]
      }
      
      for(cp in 1:20){
        log(exp.numeggs.cp[cp]) <-  
                                    log(b.numeggs) 
                                    + beta.st*st.m                   ### spring temp
                                    + beta.MCLel*MCL.m                     ### MCL 
                                    + beta.cp*cps[cp]
        logit(p.reproduce.cp[cp]) <- 
                                    b.p.reproduce.l 
                                    + beta.reproduce*1
                                    + beta.cp.pr*cps[cp]
                                    + beta.MCLpr*MCL.m  
                                    + beta.MTRpr*MTR.m          ### monthly temp range for PR
                                    
        unc.numeggs.cp[cp] <- exp.numeggs.cp[cp] * p.reproduce.cp[cp]
      }
                          
      
      for(mtr in 1:20){
        logit(p.reproduce.mtr[mtr]) <- 
                                    b.p.reproduce.l 
                                    + beta.reproduce*1
                                    + beta.cp.pr*cp.m
                                    + beta.MCLpr*MCL.m  
                                    + beta.MTRpr*MTRs[mtr]   ### monthly temp range for PR
        unc.numeggs.mtr[mtr] <- exp.numeggs.m * p.reproduce.mtr[mtr]        
      }
      
      drs[1] <- 0
      drs[2] <- 1
      for(dr in 1:2){
        logit(p.reproduce.dr[dr]) <- 
                                    b.p.reproduce.l 
                                    + beta.reproduce*drs[dr]
                                    + beta.cp.pr*cp.m
                                    + beta.MCLpr*MCL.m  
                                    + beta.MTRpr*MTR.m   ### monthly temp range for PR
        unc.numeggs.dr[dr] <- exp.numeggs.m * p.reproduce.dr[dr]
      }

    }   ## end BUGS model
    
    ",file=BUGSfilename)
  return(BUGSfilename)
}



writeJAGSmod_noclim <- function(){
  ###############
  # BUGS MODEL
  ###############
  
  
  BUGSfilename <- "Eggcellent_BUGS_25_5_MCL_Precip_noclim.txt"
  
  
  cat("
    
    model{
    
       	
    #############
    # LIKELIHOOD
    ############# 
    
        for(ind in 1:n.inds){                 # loop through each individual
                                              
            logit(p.reproduce[ind,first[ind]-1]) <- b.p.reproduce.l
            did.reproduce[ind,first[ind]-1] ~ dbern(p.reproduce[ind,first[ind]-1])
            for(yr in first[ind]:last[ind]){      
            ### loop through all years where there is data for each ind
            ### prob of reprod is a function of baseline prob and whether or not ind reproduced yr prior
    
                logit(p.reproduce[ind,yr]) <- b.p.reproduce.l 
                                            + beta.reproduce*(did.reproduce[ind,yr-1])
                                            #+ beta.cp.pr*c.precip[ind,yr]
                                            + beta.MCLpr*MCL[ind,yr]  
                                            #+ beta.MTRpr*MTR[ind,yr]   ### monthly temp range for PR
          
                                            
                did.reproduce[ind,yr] ~ dbern(p.reproduce[ind,yr])    ## datanode
                log(exp.numeggs[ind,yr]) <-  log(b.numeggs) 
                                    #+ beta.st*sp.temp[ind,yr]    ### spring temp
                                    + beta.MCLel*MCL[ind,yr]     ### MCL 
                                    #+ beta.cp*c.precip[ind,yr]   ### cumulative precip for EL
                                    
                
                exp.numeggs2[ind,yr] <-  did.reproduce[ind,yr]*exp.numeggs[ind,yr]                     
                obs.numeggs[ind,yr] ~ dpois(exp.numeggs2[ind,yr])     ## datanode
                obs.numeggs.sim[ind,yr] ~ dpois(exp.numeggs2[ind,yr])  ### simulate new data  
                
                exp.numeggs3[ind,yr] <-  p.reproduce[ind,yr]*exp.numeggs[ind,yr]                
                LogLik[ind,yr] <- log(dpois(obs.numeggs[ind,yr],exp.numeggs2[ind,yr]))   # add log likelihood computation for each observation to compute WAIC
            }
        } 
        
        ############
        # DEAL WITH NAS
        ############
        
         mcl0 ~ dunif(-2,2)    ### Had to change to match scaled predictors
         mcl0.p ~ dgamma(0.001,0.001)
         mcly.p ~ dgamma(0.001,0.001)
         
         for(ind in 1:n.inds){ 
           mmcl[ind] ~ dnorm(mcl0,mcl0.p)
           for(yr in first[ind]:last[ind]){
             MCL[ind,yr] ~ dnorm(mmcl[ind],mcly.p)
           }
         }
    
    #############
    # PRIORS
    #############
          
          b.p.reproduce ~ dbeta(1,1)    # baseline prob of reproducing 
          b.p.reproduce.l <- log(b.p.reproduce/(1-b.p.reproduce))
          b.numeggs ~   dunif(1,15)      # baseline num eggs   (bnumeggslog?)
          beta.reproduce ~ dnorm(0,0.1)      # coefficient of baseline pr???????
          #beta.st ~ dnorm(0,0.1)             # coefficient of spring precip 
          beta.MCLpr  ~ dnorm(0,0.1)           # coefficient of MCL for PR
          beta.MCLel  ~ dnorm(0,0.1)           # coefficient of MCL for EL
          #beta.MTRpr ~  dnorm(0,0.1)          # coeff for monthly temp range for PR
          #beta.cp ~ dnorm(0,0.1)           # coeff for cummulative precip for EL
          #beta.cp.pr ~ dnorm(0,0.1)
    #############
      # DERIVED QUANTITIES
      #############
      for(ind in 1:n.inds){  
        for(yr in first[ind]:last[ind]){  
          SE_obs[ind,yr] <- pow(obs.numeggs.full[ind,yr]-exp.numeggs3[ind,yr],2)      
          SE_sim[ind,yr] <- pow(obs.numeggs.sim[ind,yr]-exp.numeggs3[ind,yr],2)
        }
        
      }
      
      log(exp.numeggs.m) <-  
                                    log(b.numeggs) 
                                    #+ beta.st*st.m                   ### spring temp
                                    + beta.MCLel*MCL.m                     ### MCL 
                                    #+ beta.cp*cp.m
      logit(p.reproduce.m) <- 
                                    b.p.reproduce.l 
                                    + beta.reproduce*1
                                    #+ beta.cp.pr*cp.m
                                    + beta.MCLpr*MCL.m  
                                    #+ beta.MTRpr*MTR.m          ### monthly temp range for PR 
     
      # for(st in 1:20){
      #   log(exp.numeggs.st[st]) <-  
      #                               log(b.numeggs) 
      #                               + beta.st*sts[st]                   ### spring temp
      #                               + beta.MCLel*MCL.m                     ### MCL 
      #                               + beta.cp*cp.m
      #   
      #   unc.numeggs.st[st] <- exp.numeggs.st[st] * p.reproduce.m
      # 
      # }
      
      for(mcl in 1:20){
        log(exp.numeggs.mcl[mcl]) <-  
                                    log(b.numeggs) 
                                    #+ beta.st*st.m                   ### spring temp
                                    + beta.MCLel*mcls[mcl]                     ### MCL 
                                    #+ beta.cp*cp.m
        logit(p.reproduce.mcl[mcl]) <- 
                                    b.p.reproduce.l 
                                    + beta.reproduce*1
                                    #+ beta.cp.pr*cp.m
                                    + beta.MCLpr*mcls[mcl]  
                                    #+ beta.MTRpr*MTR.m   ### monthly temp range for PR
        unc.numeggs.mcl[mcl] <- exp.numeggs.mcl[mcl] * p.reproduce.mcl[mcl]
      }
      
      # for(cp in 1:20){
      #   log(exp.numeggs.cp[cp]) <-  
      #                               log(b.numeggs) 
      #                               + beta.st*st.m                   ### spring temp
      #                               + beta.MCLel*MCL.m                     ### MCL 
      #                               + beta.cp*cps[cp]
      #   logit(p.reproduce.cp[cp]) <- 
      #                               b.p.reproduce.l 
      #                               + beta.reproduce*1
      #                               + beta.cp.pr*cps[cp]
      #                               + beta.MCLpr*MCL.m  
      #                               + beta.MTRpr*MTR.m          ### monthly temp range for PR
      #                               
      #   unc.numeggs.cp[cp] <- exp.numeggs.cp[cp] * p.reproduce.cp[cp]
      # }
                          
      
      # for(mtr in 1:20){
      #   logit(p.reproduce.mtr[mtr]) <- 
      #                               b.p.reproduce.l 
      #                               + beta.reproduce*1
      #                               + beta.cp.pr*cp.m
      #                               + beta.MCLpr*MCL.m  
      #                               + beta.MTRpr*MTRs[mtr]   ### monthly temp range for PR
      #   unc.numeggs.mtr[mtr] <- exp.numeggs.m * p.reproduce.mtr[mtr]        
      # }
      
      drs[1] <- 0
      drs[2] <- 1
      for(dr in 1:2){
        logit(p.reproduce.dr[dr]) <- 
                                    b.p.reproduce.l 
                                    + beta.reproduce*drs[dr]
                                    #+ beta.cp.pr*cp.m
                                    + beta.MCLpr*MCL.m  
                                    #+ beta.MTRpr*MTR.m   ### monthly temp range for PR
        unc.numeggs.dr[dr] <- exp.numeggs.m * p.reproduce.dr[dr]
      }

    }   ## end BUGS model
    
    ",file=BUGSfilename)
  return(BUGSfilename)
}

writeJAGSmod_onlyclim <- function(){
  ###############
  # BUGS MODEL
  ###############
  
  
  BUGSfilename <- "Eggcellent_BUGS_25_5_MCL_Precip_onlyclim.txt"
  
  
  cat("
    
    model{
    
       	
    #############
    # LIKELIHOOD
    ############# 
    
        for(ind in 1:n.inds){                 # loop through each individual
                                              
            logit(p.reproduce[ind,first[ind]-1]) <- b.p.reproduce.l
            did.reproduce[ind,first[ind]-1] ~ dbern(p.reproduce[ind,first[ind]-1])
            for(yr in first[ind]:last[ind]){      
            ### loop through all years where there is data for each ind
            ### prob of reprod is a function of baseline prob and whether or not ind reproduced yr prior
    
                logit(p.reproduce[ind,yr]) <- b.p.reproduce.l 
                                            #+ beta.reproduce*(did.reproduce[ind,yr-1])
                                            + beta.cp.pr*c.precip[ind,yr]
                                            #+ beta.MCLpr*MCL[ind,yr]  
                                            + beta.MTRpr*MTR[ind,yr]   ### monthly temp range for PR
          
                                            
                did.reproduce[ind,yr] ~ dbern(p.reproduce[ind,yr])    ## datanode
                log(exp.numeggs[ind,yr]) <-  log(b.numeggs) 
                                    + beta.st*sp.temp[ind,yr]    ### spring temp
                                    #+ beta.MCLel*MCL[ind,yr]     ### MCL 
                                    + beta.cp*c.precip[ind,yr]   ### cumulative precip for EL
                                    
                
                exp.numeggs2[ind,yr] <-  did.reproduce[ind,yr]*exp.numeggs[ind,yr]                     
                obs.numeggs[ind,yr] ~ dpois(exp.numeggs2[ind,yr])     ## datanode
                obs.numeggs.sim[ind,yr] ~ dpois(exp.numeggs2[ind,yr])  ### simulate new data  
                
                exp.numeggs3[ind,yr] <-  p.reproduce[ind,yr]*exp.numeggs[ind,yr]                
                LogLik[ind,yr] <- log(dpois(obs.numeggs[ind,yr],exp.numeggs2[ind,yr]))   # add log likelihood computation for each observation to compute WAIC
            }
        } 
        
        ############
        # DEAL WITH NAS
        ############
        
         mcl0 ~ dunif(-2,2)    ### Had to change to match scaled predictors
         mcl0.p ~ dgamma(0.001,0.001)
         mcly.p ~ dgamma(0.001,0.001)
         
         for(ind in 1:n.inds){ 
           mmcl[ind] ~ dnorm(mcl0,mcl0.p)
           for(yr in first[ind]:last[ind]){
             MCL[ind,yr] ~ dnorm(mmcl[ind],mcly.p)
           }
         }
    
    #############
    # PRIORS
    #############
          
          b.p.reproduce ~ dbeta(1,1)    # baseline prob of reproducing 
          b.p.reproduce.l <- log(b.p.reproduce/(1-b.p.reproduce))
          b.numeggs ~   dunif(1,15)      # baseline num eggs   (bnumeggslog?)
          #beta.reproduce ~ dnorm(0,0.1)      # coefficient of baseline pr???????
          beta.st ~ dnorm(0,0.1)             # coefficient of spring precip 
          #beta.MCLpr  ~ dnorm(0,0.1)           # coefficient of MCL for PR
          #beta.MCLel  ~ dnorm(0,0.1)           # coefficient of MCL for EL
          beta.MTRpr ~  dnorm(0,0.1)          # coeff for monthly temp range for PR
          beta.cp ~ dnorm(0,0.1)           # coeff for cummulative precip for EL
          beta.cp.pr ~ dnorm(0,0.1)
    #############
      # DERIVED QUANTITIES
      #############
      for(ind in 1:n.inds){  
        for(yr in first[ind]:last[ind]){  
          SE_obs[ind,yr] <- pow(obs.numeggs.full[ind,yr]-exp.numeggs3[ind,yr],2)      
          SE_sim[ind,yr] <- pow(obs.numeggs.sim[ind,yr]-exp.numeggs3[ind,yr],2)
        }
        
      }
      
      log(exp.numeggs.m) <-  
                                    log(b.numeggs) 
                                    + beta.st*st.m                   ### spring temp
                                    #+ beta.MCLel*MCL.m                     ### MCL 
                                    + beta.cp*cp.m
      logit(p.reproduce.m) <- 
                                    b.p.reproduce.l 
                                    #+ beta.reproduce*1
                                    + beta.cp.pr*cp.m
                                    #+ beta.MCLpr*MCL.m  
                                    + beta.MTRpr*MTR.m          ### monthly temp range for PR 
     
      for(st in 1:20){
        log(exp.numeggs.st[st]) <-
                                    log(b.numeggs)
                                    + beta.st*sts[st]                   ### spring temp
                                    #+ beta.MCLel*MCL.m                     ### MCL
                                    + beta.cp*cp.m

        unc.numeggs.st[st] <- exp.numeggs.st[st] * p.reproduce.m

      }
      
      # for(mcl in 1:20){
      #   log(exp.numeggs.mcl[mcl]) <-  
      #                               log(b.numeggs) 
      #                               #+ beta.st*st.m                   ### spring temp
      #                               + beta.MCLel*mcls[mcl]                     ### MCL 
      #                               #+ beta.cp*cp.m
      #   logit(p.reproduce.mcl[mcl]) <- 
      #                               b.p.reproduce.l 
      #                               + beta.reproduce*1
      #                               #+ beta.cp.pr*cp.m
      #                               + beta.MCLpr*mcls[mcl]  
      #                               #+ beta.MTRpr*MTR.m   ### monthly temp range for PR
      #   unc.numeggs.mcl[mcl] <- exp.numeggs.mcl[mcl] * p.reproduce.mcl[mcl]
      # }
      
      for(cp in 1:20){
        log(exp.numeggs.cp[cp]) <-
                                    log(b.numeggs)
                                    + beta.st*st.m                   ### spring temp
                                    #+ beta.MCLel*MCL.m                     ### MCL
                                    + beta.cp*cps[cp]
        logit(p.reproduce.cp[cp]) <-
                                    b.p.reproduce.l
                                    #+ beta.reproduce*1
                                    + beta.cp.pr*cps[cp]
                                    #+ beta.MCLpr*MCL.m
                                    + beta.MTRpr*MTR.m          ### monthly temp range for PR

        unc.numeggs.cp[cp] <- exp.numeggs.cp[cp] * p.reproduce.cp[cp]
      }
                          
      
      for(mtr in 1:20){
        logit(p.reproduce.mtr[mtr]) <-
                                    b.p.reproduce.l
                                    #+ beta.reproduce*1
                                    + beta.cp.pr*cp.m
                                    #+ beta.MCLpr*MCL.m
                                    + beta.MTRpr*MTRs[mtr]   ### monthly temp range for PR
        unc.numeggs.mtr[mtr] <- exp.numeggs.m * p.reproduce.mtr[mtr]
      }
      
      # drs[1] <- 0
      # drs[2] <- 1
      # for(dr in 1:2){
      #   logit(p.reproduce.dr[dr]) <- 
      #                               b.p.reproduce.l 
      #                               + beta.reproduce*drs[dr]
      #                               #+ beta.cp.pr*cp.m
      #                               + beta.MCLpr*MCL.m  
      #                               #+ beta.MTRpr*MTR.m   ### monthly temp range for PR
      #   unc.numeggs.dr[dr] <- exp.numeggs.m * p.reproduce.dr[dr]
      # }

    }   ## end BUGS model
    
    ",file=BUGSfilename)
  return(BUGSfilename)
}



writeJAGSmod_nodaymet <- function(){
  ###############
  # BUGS MODEL
  ###############
  
  
  BUGSfilename <- "Eggcellent_BUGS_25_5_MCL_Precip_nodaymet.txt"
  
  
  cat("
    
    model{
    
       	
    #############
    # LIKELIHOOD
    ############# 
    
        for(ind in 1:n.inds){                 # loop through each individual
                                              
            logit(p.reproduce[ind,first[ind]-1]) <- b.p.reproduce.l
            did.reproduce[ind,first[ind]-1] ~ dbern(p.reproduce[ind,first[ind]-1])
            for(yr in first[ind]:last[ind]){      
            ### loop through all years where there is data for each ind
            ### prob of reprod is a function of baseline prob and whether or not ind reproduced yr prior
    
                logit(p.reproduce[ind,yr]) <- b.p.reproduce.l 
                                            + beta.reproduce*(did.reproduce[ind,yr-1])
                                            + beta.cp.pr*c.precip[ind,yr]
                                            + beta.MCLpr*MCL[ind,yr]  
                                            + beta.MTRpr*MTR[ind,yr]   ### monthly temp range for PR
          
                                            
                did.reproduce[ind,yr] ~ dbern(p.reproduce[ind,yr])    ## datanode
                log(exp.numeggs[ind,yr]) <-   
                                    log(b.numeggs) 
                                    #+ beta.st*sp.temp[ind,yr]    ### spring temp
                                    + beta.MCLel*MCL[ind,yr]    ### MCL 
                                    + beta.cp*c.precip[ind,yr] ### cumulative precip for EL
                                      
                exp.numeggs2[ind,yr] <-  did.reproduce[ind,yr]*exp.numeggs[ind,yr]
                obs.numeggs[ind,yr] ~ dpois(exp.numeggs2[ind,yr])     ## datanode
                obs.numeggs.sim[ind,yr] ~ dpois(exp.numeggs2[ind,yr])  ### simulate new data  
                
                exp.numeggs3[ind,yr] <-  p.reproduce[ind,yr]*exp.numeggs[ind,yr]                
                LogLik[ind,yr] <- log(dpois(obs.numeggs[ind,yr],exp.numeggs2[ind,yr]))   # add log likelihood computation for each observation to compute WAIC
            }
        } 
        
        ############
        # DEAL WITH NAS
        ############
        
         mcl0 ~ dunif(-2,2)    ### Had to change to match scaled predictors
         mcl0.p ~ dgamma(0.001,0.001)
         mcly.p ~ dgamma(0.001,0.001)
         
         for(ind in 1:n.inds){ 
           mmcl[ind] ~ dnorm(mcl0,mcl0.p)
           for(yr in first[ind]:last[ind]){
             MCL[ind,yr] ~ dnorm(mmcl[ind],mcly.p)
           }
         }
    
    #############
    # PRIORS
    #############
          
          b.p.reproduce ~ dbeta(1,1)    # baseline prob of reproducing 
          b.p.reproduce.l <- log(b.p.reproduce/(1-b.p.reproduce))
          b.numeggs ~   dunif(1,15)      # baseline num eggs   (bnumeggslog?)
          beta.reproduce ~ dnorm(0,0.1)      # coefficient of baseline pr???????
          #beta.st ~ dnorm(0,0.1)             # coefficient of spring precip 
          beta.MCLpr  ~ dnorm(0,0.1)           # coefficient of MCL for PR
          beta.MCLel  ~ dnorm(0,0.1)           # coefficient of MCL for EL
          beta.MTRpr ~  dnorm(0,0.1)          # coeff for monthly temp range for PR
          beta.cp ~ dnorm(0,0.1)           # coeff for cummulative precip for EL
          beta.cp.pr ~ dnorm(0,0.1)
    #############
      # DERIVED QUANTITIES
      #############
      for(ind in 1:n.inds){  
        for(yr in first[ind]:last[ind]){  
          SE_obs[ind,yr] <- pow(obs.numeggs.full[ind,yr]-exp.numeggs3[ind,yr],2)      
          SE_sim[ind,yr] <- pow(obs.numeggs.sim[ind,yr]-exp.numeggs3[ind,yr],2)
        }
        
      }
      
      log(exp.numeggs.m) <-  
                                    log(b.numeggs) 
                                    #+ beta.st*st.m                   ### spring temp
                                    + beta.MCLel*MCL.m                     ### MCL 
                                    + beta.cp*cp.m
      logit(p.reproduce.m) <- 
                                    b.p.reproduce.l 
                                    + beta.reproduce*1
                                    + beta.cp.pr*cp.m
                                    + beta.MCLpr*MCL.m  
                                    + beta.MTRpr*MTR.m          ### monthly temp range for PR 
     
      # 
      # for(st in 1:20){
      #   log(exp.numeggs.st[st]) <-  
      #                               log(b.numeggs) 
      #                               + beta.st*sts[st]                   ### spring temp
      #                               + beta.MCLel*MCL.m                     ### MCL 
      #                               + beta.cp*cp.m
      # 
      # }
      
      for(mcl in 1:20){
        log(exp.numeggs.mcl[mcl]) <-  
                                    log(b.numeggs) 
                                    #+ beta.st*st.m                   ### spring temp
                                    + beta.MCLel*mcls[mcl]                     ### MCL 
                                    + beta.cp*cp.m
      }                                
                          
      for(cp in 1:20){
        log(exp.numeggs.cp[cp]) <-  
                                    log(b.numeggs) 
                                    #+ beta.st*st.m                   ### spring temp
                                    + beta.MCLel*MCL.m                     ### MCL 
                                    + beta.cp*cps[cp]
        logit(p.reproduce.cp[cp]) <- 
                                    b.p.reproduce.l 
                                    + beta.reproduce*1
                                    + beta.cp.pr*cps[cp]
                                    + beta.MCLpr*MCL.m  
                                    + beta.MTRpr*MTR.m          ### monthly temp range for PR
                                    
        unc.numeggs.cp[cp] <- exp.numeggs.cp[cp] * p.reproduce.cp[cp]
      }
      
       for(mtr in 1:20){
        logit(p.reproduce.mtr[mtr]) <- 
                                    b.p.reproduce.l 
                                    + beta.reproduce*1
                                    + beta.cp.pr*cp.m
                                    + beta.MCLpr*MCL.m  
                                    + beta.MTRpr*MTRs[mtr]   ### monthly temp range for PR
        unc.numeggs.mtr[mtr] <- exp.numeggs.m * p.reproduce.mtr[mtr]        
      }
      
      drs[1] <- 0
      drs[2] <- 1
      for(dr in 1:2){
        logit(p.reproduce.dr[dr]) <- 
                                    b.p.reproduce.l 
                                    + beta.reproduce*drs[dr]
                                    + beta.cp.pr*cp.m
                                    + beta.MCLpr*MCL.m  
                                    + beta.MTRpr*MTR.m   ### monthly temp range for PR
        unc.numeggs.dr[dr] <- exp.numeggs.m * p.reproduce.dr[dr]
      }

    }   ## end BUGS model
    
    ",file=BUGSfilename)
  return(BUGSfilename)
}

#indices=numeric(0)
prepareJAGSdat <- function(indices=thisfold,daymet=T,clim=T,internal=T,site=F){   # "indices" are the indices of the data to be used for validation
  
  forfit <- setdiff(1:nrow(obs.numeggs),indices)
  obs.numeggs2 <- obs.numeggs
  did.reproduce2 <- did.reproduce
  obs.numeggs2[indices,] <- NA
  did.reproduce2[indices,] <- NA
  
  obs.numeggs3 <- obs.numeggs
  obs.numeggs3[is.na(obs.numeggs)] <- -100
   
  if(internal){
    sc.MCL <- (MCL-mean(MCL, na.rm = T))/sd(MCL, na.rm = T)
  } 
  
  if(clim){
    sc.MTR <- (MTR-mean(MTR, na.rm = T))/sd(MTR, na.rm = T)
    sc.cp <- (c.precip-mean(c.precip, na.rm = T))/sd(c.precip, na.rm = T) 
  }
  
  if(daymet){
    sc.sp.temp <- (sp.temp-mean(sp.temp, na.rm = T))/sd(sp.temp, na.rm = T) 
  }
  
  #######################################
  ##### calculate means of scaled variables
  #######################################
  
  if(internal){
    MCL.m <- mean(sc.MCL, na.rm=T) 
  }
  
  if(clim){
    MTR.m <-mean(sc.MTR, na.rm=T)
    cp.m <- mean(sc.cp, na.rm=T)
  }
  
  if(daymet){
    st.m <- mean(sc.sp.temp, na.rm=T)
  }
  
  ###############################
  # Generate values for PD Plots
  ###############################
  
  if(internal){
    mcls <- seq(from=min(sc.MCL, na.rm=T),to=max(sc.MCL, na.rm=T), length.out=20)
  }
  
  if(clim){
    cps <-seq(from=min(sc.cp, na.rm=T),to=max(sc.cp, na.rm=T), length.out=20)
    MTRs <-seq(from=min(sc.MTR, na.rm=T),to=max(sc.MTR, na.rm=T), length.out=20)
  }
  
  if(daymet){
    sts <- seq(from=min(sc.sp.temp), to=max(sc.sp.temp), length.out = 20)
  }
  
  out <- list()
  
  out$data.for.bugs <- list(
    n.inds = n.inds, ### total number of unqiue individuals 
    first = first,  ### a vector of years corresponding to the FIRST time each tort reproduced
    last = last,   ### a vector of years corresponding to the LAST time each tort reproduced 
    did.reproduce = did.reproduce2, ### matrix of 0 and 1 for each tort each year
    obs.numeggs = obs.numeggs2, ### matrix of total eggs for each tort each year
    obs.numeggs.full=obs.numeggs3
  )
  
  if(internal){
    out$data.for.bugs$MCL = sc.MCL ## scaled
    out$data.for.bugs$mcls = mcls ## seq of values for prediction plots 
    out$data.for.bugs$MCL.m = MCL.m ## mean of scaled values
  }
  
  if(clim){
    out$data.for.bugs$MTR = sc.MTR
    out$data.for.bugs$MTRs = MTRs
    out$data.for.bugs$MTR.m = MTR.m
    out$data.for.bugs$c.precip = sc.cp
    out$data.for.bugs$cps = cps
    out$data.for.bugs$cp.m = cp.m
  }
  
  if(daymet){
    out$data.for.bugs$sp.temp = sc.sp.temp  ## scaled spring temp
    out$data.for.bugs$sts = sts  ## seq of values for prediction plots
    out$data.for.bugs$st.m = st.m ## mean of scaled values
  } 
  
  if(site){
    out$data.for.bugs$sitenum = site.num
    out$data.for.bugs$nsites = max(site.num)
  }
  
  out$initz.bugs<-function(){       ### initial values for priors  (intialize coefficients between -.1 and .1, close to 0, for some variation in the starting points)
    inits <-
      list(
        b.p.reproduce = 0.5,                 # baseline prob of reproducing 
        b.numeggs = runif(1,3,4)             # baseline num eggs
      )
    
    if(internal){
      inits$beta.reproduce = runif(1,-0.01,0.01)              # coefficient on reproduction
      inits$beta.MCLpr  = runif(1,-0.01,0.01)           # coefficient on MCL 
      inits$beta.MCLel  = runif(1,-0.01,0.01)           # coefficient on MCL 
    }
    
    if(clim){
      inits$beta.MTRpr  = runif(1,-0.01,0.01)   # coefficient on MTR
      inits$beta.cp  = runif(1,-0.01,0.01)           # coefficient on cp
      inits$beta.cp.pr  = runif(1,-0.01,0.01)           # coefficient on cp
    }
    
    if(daymet) inits$beta.st = runif(1,-0.01,0.01)              # coefficient on spring temperature 
    return(inits)
  }
  
  out$toMonitor <- c(
    "b.p.reproduce",
    "b.p.reproduce.l",
    "b.numeggs",
    "SE_obs",
    "SE_sim",
    "p.reproduce",              # probability of reproducing
    "exp.numeggs",
    "exp.numeggs2",              # expected number of eggs produced
    "LogLik"
  )
  
  if(internal) out$toMonitor <- c(out$toMonitor,  "beta.reproduce",
                                  "beta.MCLpr",
                                  "beta.MCLel","exp.numeggs.mcl","p.reproduce.mcl","unc.numeggs.mcl",
                                  "p.reproduce.dr","unc.numeggs.dr")
  
  if(clim) out$toMonitor <- c(out$toMonitor,
                                "beta.MTRpr","beta.cp",  
                                "beta.cp.pr",
                                "exp.numeggs.cp","p.reproduce.cp","unc.numeggs.cp",
                                "p.reproduce.mtr","unc.numeggs.mtr"
                                )
  
  if(daymet) out$toMonitor <- c(out$toMonitor,"beta.st",
                                "exp.numeggs.st","unc.numeggs.st")
  
  if(site) out$toMonitor <- c(out$toMonitor,"siteeff1","siteeff2","sitesd.rp","sitesd.ep")
  
  return(out)
}

runJAGSmod <- function(fn,indat){
  mod2<-runjags::run.jags(
    model=fn,
    monitor=indat$toMonitor,
    data=indat$data.for.bugs,
    n.chains = 3,
    inits=indat$initz.bugs,
    burnin = 2000,
    sample=2500,
    adapt=1000,
    thin=10,
    method="parallel"
  )
  
  #mod.summary<-summary(mod2) 
  
  # Mod.mcmc <- mod2$mcmc
  # Mod.mcmc.list<- coda::mcmc.list(Mod.mcmc)
  return(mod2)    # Mod.mcmc.list
  
}


diagnoseJAGSconvergence <- function(Mod.mcmc.list,varstotest){
  
  #####################################
  ######## run diagnostics for convergence
  ####################################
  
  #heidel.diag(Mod.mcmc.list)
  out <- list()
  
  ### gelman rubin diagnostic for select variables
  out <- lapply(varstotest,function(t) coda::gelman.diag(Mod.mcmc.list[,t]) )
  names(out) <- varstotest
  
  
  
  ### visual assessment for select variables
  
  sapply(varstotest, function(t) plot(Mod.mcmc.list[,t],main=t)) 
  
  return(out)
  
}

computeJAGSpval <- function(mcmc.list){
  yr.dat<-array(NA, dim=c(n.inds,23)) ##Changed to 23 b/c of new data  ##create storage container to store ind,yr where there is data
  for(ind in 1:n.inds){  
    thistort <- uniqtort[ind]
    yr.dat.ftol <- (first[ind]:last[ind])
    reprostat <- as.matrix(did.reproduce[ind,])
    rownames(reprostat) <- seq(1,nrow(reprostat), by=1) #c(1,2,3,4,5,6,7)
    yr.dat.mx <- as.matrix(na.omit(reprostat))
    keep <- yr.dat.mx[,1] 
    keep2 <- as.numeric(names(keep))
    
    for(y in 1:length(keep2)){
      yr.dat[ind,y]<-keep2[y]
    }
  }
  
  obs.ndx<-"SE_obs[x,x]"  ### create storage container for index items
  sims.ndx<-"SE_obs[x,x]" ### create storage container for index items
  
  
  for(ind in 1:n.inds){
    
    yr.dat.nona<-na.omit(yr.dat[ind,])
    yr.dat2<-yr.dat.nona[1:length(yr.dat.nona)]
    if(is.na(yr.dat2[1])==F){
      
      keep.obs.ndx <- as.matrix(sprintf("SE_obs[%s,%s]",ind,yr.dat2)) 
      keep.sim.ndx <- as.matrix(sprintf("SE_sim[%s,%s]",ind,yr.dat2))
      
      obs.ndx <- rbind(obs.ndx, keep.obs.ndx)
      sims.ndx <- rbind(sims.ndx, keep.sim.ndx)
    }
  }
  obs.ndx<-obs.ndx[-1,]
  obs.ndx<-as.vector(obs.ndx)
  sims.ndx<-sims.ndx[-1,]
  sims.ndx<-as.vector(sims.ndx)
  
  obj.obs<-mcmc.list[,obs.ndx]
  obj.sim<-mcmc.list[,sims.ndx]
  
  RMSE_obs <- 1000 ##start storage container
  RMSE_sims <- 1000 ##start storage container
  for(i in 1:nrow(mcmc.list)){
    
    RMSE_obs.keep <- sqrt(mean(as.numeric(obj.obs[i,])))
    RMSE_obs <- rbind(RMSE_obs,RMSE_obs.keep)
    RMSE_sims.keep <- sqrt(mean(as.numeric(obj.sim[i,])))
    RMSE_sims <- rbind(RMSE_sims,RMSE_sims.keep)
  }
  
  RMSE_obs<-RMSE_obs[-1,] ## remove fake data 
  head(RMSE_obs)
  
  RMSE_sims<-RMSE_sims[-1,] ## remove fake data
  head(RMSE_sims)
  
  png("PosteriorPredictive.png", width=500, height=500)
  
  plot(RMSE_sims~RMSE_obs, xlab="RMSE - Observed Data", ylab="RMSE - Simulated Data", 
       cex.axis=1,cex.lab=1.3)
  abline(0,1,col="red",lwd=3)
  
  dev.off()
  
  pvalue <- length(which(RMSE_sims>RMSE_obs))/length(RMSE_sims)
  return(pvalue)
  
  # hist(RMSE_sims,freq=F)
  # hist(RMSE_obs,freq = F)
  
}

#indices <- c(11:20)
computeJAGS_CV <- function(ml=mcmc.list,dr=did.reproduce,fst=first,lst=last,ut=uniqtort,indices,stat="pred1"){   # NOTE: stat can be "SE" for squared error or pred for predictions
  n.ndx <- length(indices)
  yr.dat<-array(NA, dim=c(n.ndx,23)) ##Changed to 23 b/c of new data ## create storage container to store ind,yr where there is data
  counter <- 1
  ind = indices[2]
  for(ind in indices){  
    thistort <- ut[ind]
    yr.dat.ftol <- (fst[ind]:lst[ind])
    reprostat <- as.matrix(dr[ind,])
    rownames(reprostat) <- seq(1,nrow(reprostat), by=1)  #c(1,2,3,4,5,6,7)
    yr.dat.mx <- as.matrix(na.omit(reprostat))
    keep <- yr.dat.mx[,1] 
    keep2 <- as.numeric(names(keep))
    
    y=1
    for(y in 1:length(keep2)){
      yr.dat[counter,y]<-keep2[y]
    }
    counter=counter+1
  }
  rownames(yr.dat) <- indices
  
  if(stat=="SE") obs.ndx<-"SE_obs[x,x]"  ### create storage container for index items
  if(stat=="pred1") obs.ndx <- "p.reproduce[x,x]"
  if(stat=="pred2") obs.ndx <- "exp.numeggs[x,x]"
 
  counter=1 
  ind=indices[2]
  for(ind in indices){
    
    yr.dat.nona<-na.omit(yr.dat[counter,])
    yr.dat2<-yr.dat.nona[1:length(yr.dat.nona)]
    if(is.na(yr.dat2[1])==F){
      
      if(stat=="SE") keep.obs.ndx <- as.matrix(sprintf("SE_obs[%s,%s]",ind,yr.dat2))
      if(stat=="pred1") keep.obs.ndx <- as.matrix(sprintf("p.reproduce[%s,%s]",ind,yr.dat2))
      if(stat=="pred2") keep.obs.ndx <- as.matrix(sprintf("exp.numeggs[%s,%s]",ind,yr.dat2))
      obs.ndx <- rbind(obs.ndx, keep.obs.ndx)
    }
    counter=counter+1
  }
  
  obs.ndx<-obs.ndx[-1,]
  obs.ndx<-as.vector(obs.ndx)

  obj.obs<-ml[,obs.ndx]
  
  toret <- t(sapply(1:nrow(ml),function(t) as.numeric(obj.obs[t,]) ))
  colnames(toret) <- obs.ndx
  return(toret)

}

RunCrossVal <- function(fn=BUGSfilename,foldvec,daymet=T,clim=T,internal=T,CV=T,fullmod=mcmc.list,dr=did.reproduce,fst=first,lst=last,ut=uniqtort){
  n.folds = max(foldvec)
  f=1
  for(f in 1:n.folds){
    thisfold<-which(foldvec==f)   # vals to leave out!
    
    if(CV){
      datforjags.cv <- prepareJAGSdat(thisfold,daymet,clim,internal)    # function prepares data for running JAGS
      mod3 <- runJAGSmod(fn,datforjags.cv)   # run JAGS model
      Mod.mcmc <- mod3$mcmc
      Mod.mcmc.list.cv <- coda::mcmc.list(Mod.mcmc)
      mcmc.list.cv <- as.data.frame(runjags::combine.mcmc(Mod.mcmc.list.cv))
      
      sedf <- computeJAGS_CV(ml=mcmc.list.cv,dr=dr,fst=fst,lst=lst,ut=ut,indices=thisfold,stat="SE")
      preddf1 <- computeJAGS_CV(ml=mcmc.list.cv,dr=dr,fst=fst,lst=lst,ut=ut,indices=thisfold,stat="pred1")
      preddf2 <- computeJAGS_CV(ml=mcmc.list.cv,dr=dr,fst=fst,lst=lst,ut=ut,indices=thisfold,stat="pred2")
    }else{
      sedf <- computeJAGS_CV(ml=fullmod,dr=dr,fst=fst,lst=lst,ut=ut,indices=thisfold,stat="SE")
      preddf1 <- computeJAGS_CV(ml=fullmod,dr=dr,fst=fst,lst=lst,ut=ut,indices=thisfold,stat="pred1")
      preddf2 <- computeJAGS_CV(ml=fullmod,dr=dr,fst=fst,lst=lst,ut=ut,indices=thisfold,stat="pred2")
    }
    
    
    if(f==1){
      cvdf1 <- sedf 
      cvdf2 <- preddf1
      cvdf3 <- preddf2
    }else{
      cvdf1 <- cbind(cvdf1,sedf)
      cvdf2 <- cbind(cvdf2,preddf1)
      cvdf3 <- cbind(cvdf3,preddf2)
    }
  }
  
  toret <- list()
  toret$se <- cvdf1
  toret$pred1 <- cvdf2
  toret$pred2 <- cvdf3
  #str(cvdf)
  return(toret)
}


#num=1
rsquaredfunc <- function(num){
  SSresid <- sum((realobs-preddf3[num,])^2)
  SStot <- sum((realobs-meanobs)^2)
  1-(SSresid/SStot)
}

#num=1
rsquaredfunc_nocv <- function(num){
  SSresid <- sum((realobs-preddf3_nocv[num,])^2)
  SStot <- sum((realobs-meanobs)^2)
  1-(SSresid/SStot)
}

#num=1
rocfunc <- function(num){
  this <- preddf1[num,]
  pred <- ROCR::prediction(this,realdr)
  auc <- ROCR::performance(pred,"auc")
  auc@y.values[[1]]
}

rocfunc_nocv <- function(num){
  this <- preddf1_nocv[num,]
  pred <- ROCR::prediction(this,realdr)
  auc <- ROCR::performance(pred,"auc")
  auc@y.values[[1]]
}


###############
# END CUSTOM FUNCTIONS
###############





