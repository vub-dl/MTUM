# Balance correction of pretreatment variables (three treatment groups)
# df: data frame
# indicator_treatment: column name of the feature indicating the treatment given to each unit
# Treatment groups: treatment_1,treatment_2,control
# Pretreatment characteristics: features

ps_matching <- function(df,indicator_treatment,treatment_1,treatment_2,control,features){
   
  # First pairwise comparison: treatment_1 vs. control
  df.t1.ct <- subset(df,indicator_treatment  == treatment_1 | indicator_treatment == control)
  
  # Second pairwise comparison: treatment_2 vs. control
  df.t2.ct <- subset(df,indicator_treatment  == treatment_2 | indicator_treatment == control)
  
  # Propensity score (PS) formula
  formula <- as.formula(paste("indicator_treatment ~", paste(features, sep = "", collapse = "+")))
  
  # Assessment before applying matching treatment_1 vs. control
    df.t1.ct$indicator_treatment <- ifelse(df.t1.ct$indicator_treatment == treatment_1,1,0)
    # Chi-square
    prior.chi.t1.ct <-  RItools::xBalance(formula, data = df.t1.ct, report = c("chisquare.test"))
    print(prior.chi.t1.ct) 
    # SMD
    prior.smd.t1.ct <- tableone::CreateTableOne(vars = features, strata = "indicator_treatment", data = df.t1.ct) 
    print(prior.smd.t1.ct,smd = TRUE)
    addmargins(table(ExtractSmd(prior.smd.t1.ct)>0.1))
    # SD
    treated.t1 <- (df.t1.ct$indicator_treatment ==1)
    prior.std.diff.t1.ct <- apply(df.t1.ct[,features],2,function(x)100*(mean(x[treated.t1])-mean(x[!treated.t1]))/(sqrt(0.5*(var(x[treated.t1])+var(x[!treated.t1])))))
    abs(prior.std.diff.t1.ct)
  
  # Assessment before applying matching treatment_2 vs. control
    df.t2.ct$indicator_treatment <- ifelse(df.t2.ct$indicator_treatment == treatment_2,1,0) 
    # Chi-square
    prior.chi.t2.ct <-  RItools::xBalance(formula, data = df.t2.ct, report = c("chisquare.test"))
    print(prior.chi.t2.ct)
    # SMD
    prior.smd.t2.ct <- tableone::CreateTableOne(vars = features, strata = "indicator_treatment", data = df.t2.ct) 
    print(prior.smd.t2.ct,smd = TRUE)
    addmargins(table(ExtractSmd(prior.smd.t2.ct)>0.1)) 
    # SD
    treated.t2 <- (df.t2.ct$indicator_treatment ==1)
    prior.std.diff.t2.ct <- apply(df.t2.ct[,features],2,function(x)100*(mean(x[treated.t2])-mean(x[!treated.t2]))/(sqrt(0.5*(var(x[treated.t2])+var(x[!treated.t2])))))
    abs(prior.std.diff.t2.ct)
  
  # Applying matching for df.t1.ct (treatment_1 vs. control)
    set.seed(100)
    match.t1.ct <- MatchIt::matchit(formula, data = df.t1.ct, method = "optimal", ratio = 1)
    t1.ct.mtch <- MatchIt::match.data(match.t1.ct)
    summary(match.t1.ct)
    
    # Check balance after matching: df.t1.ct (treatment_1 vs. control)
    # Chi-square
    post.Chisquare.t1.ct <-  RItools::xBalance(formula, data = t1.ct.mtch, report = c("chisquare.test"))
    print(post.Chisquare.t1.ct)
    # SMD
    post.smd.t1.ct <- tableone::CreateTableOne(vars = features, strata = "indicator_treatment", data = t1.ct.mtch) 
    post.smd.t1.ct <- print(aft.smd.t1.ct,smd = TRUE)
 
    t1.ct.mtch$indicator_treatment <- ifelse(t1.ct.mtch$indicator_treatment == 1, treatment_1,control)
    
  # Matching df.t2.ct (treatment_2 vs. control)
    set.seed(100)
    match.t2.ct <- MatchIt::matchit(formula, data = df.t2.ct, method = "optimal", ratio = 1)
    t2.ct.mtch <- MatchIt::match.data(match.t2.ct)
    summary(match.t2.ct)
    
    # Check balance after matching: df.t2.ct (treatment_2 vs. control)
    # Chi-square
    post.Chisquare.t2.ct <-  RItools::xBalance(formula, data = t2.ct.mtch, report = c("chisquare.test"))
    print(post.Chisquare.t2.ct)  
    # SMD
    post.smd.t2.ct <- tableone::CreateTableOne(vars = features, strata = "indicator_treatment", data = t2.ct.mtch) 
    post.smd.t2.ct <- print(post.smd.t2.ct,smd = TRUE)
    addmargins(table(ExtractSmd(post.smd.t2.ct)>0.1)) 
    
    t2.ct.mtch$indicator_treatment <- ifelse(t2.ct.mtch$indicator_treatment == 1, treatment_2,control)
  
  # Final DF
    
    duprows <- rownames(t1.ct.mtch) %in% rownames(t2.ct.mtch)
    df <- rbind(t2.ct.mtch, t1.ct.mtch[!duprows,])
    
    summary <- list(dataframe=NULL,
                    ini.Chisquare.t1.ct=NULL,
                    ini.std.diff.t1.ct=NULL,
                    ini.Chisquare.t2.ct=NULL,
                    ini.std.diff.t2.ct=NULL,
                    aft.Chisquare.t1.ct =NULL,
                    aft.smd.t1.ct=NULL,
                    aft.Chisquare.t2.ct=NULL,
                    aft.smd.t2.ct=NULL)
    
  summary$dataframe <- df
  summary$Prior_Chi_T1 <- prior.Chisquare.t1.ct 
  summary$Prior_SD_diff_T1 <- prior.std.diff.t1.ct
  summary$Prior_Chi_T2 <- prior.Chisquare.t2.ct
  summary$Prior_SD_diff_T2 <- prior.std.diff.t2.ct
  summary$Post_Chi_T1 <- post.Chisquare.t1.ct
  summary$Post_SD_diff_T1 <- post.smd.t1.ct
  summary$Post_Chi_T2 <- post.Chisquare.t2.ct
  summary$Post_SD_diff_T2 <- post.smd.t2.ct
  
  return(summary)
}
