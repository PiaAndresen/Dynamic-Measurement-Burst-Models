# R Scripts Empirical Examples with Leuven Data

# Data and Packages ######
library(MplusAutomation)

options(scipen = 999)
setwd("~/Mplus/leuven")
detectCores()
Sys.setenv("OMP_THREAD_LIMIT" = 44)
stopCluster(cl)
# Load data

df_dep_res<-read.csv("leuven_dep_rescaledb10.csv")


df_dep_res<-df_dep_res[,c(2,4:7)]
colnames(df_dep_res)<-c("ID" ,"hour", "Y1","Y2","Y3")

df_stress<- read.csv("leuven_stress.csv")
df_stress<- df_stress[,c(2,4:7)]
colnames(df_stress)<-c("ID" ,"hour", "Y1","Y2","Y3")

df_rlx<- read.csv("leuven_rlx.csv")
df_rlx<- df_rlx[,c(2,4:7)]
colnames(df_rlx)<-c("ID" ,"hour", "Y1","Y2","Y3")

df_lone<- read.csv("leuven_lone.csv")
df_lone<- df_lone[,c(2,4:7)]
colnames(df_lone)<-c("ID" ,"hour", "Y1","Y2","Y3")

df_cheer<- read.csv("leuven_cheer.csv")
df_cheer<- df_cheer[,c(2,4:7)]
colnames(df_cheer)<-c("ID" ,"hour", "Y1","Y2","Y3")

df_anger<- read.csv("leuven_anger.csv")
df_anger<- df_anger[,c(2,4:7)]
colnames(df_anger)<-c("ID" ,"hour", "Y1","Y2","Y3")

df_conf_hap_res<- read.csv("leuven_conf_hap_rescaledb10.csv")
df_conf_hap_res<-df_conf_hap_res[,c(2,4:10)]
colnames(df_conf_hap_res)<-c("ID" ,"hour", "C1","C2","C3","H1","H2","H3")

install.packages("pastecs")
library(pastecs)

#depression
#df_dep_res[df_dep_res==999]<-NA
stat.desc(df_dep_res$Y1) # mean =12,8; SD =17,2
stat.desc(df_dep_res$Y2) # m= 11,4; sd = 16,2
stat.desc(df_dep_res$Y3) # m= 10,7; sd = 15,9

#confidence
df_conf_hap[df_conf_hap==999]<-NA
stat.desc(df_conf_hap$C1)# m= 59,6 ; SD = 21,1
stat.desc(df_conf_hap$C2) # m= 59,7; SD = 21,9
stat.desc(df_conf_hap$C3)# 59,4 ; sd= 22,0

#cheerfulness
df_cheer[df_cheer==999]<-NA
stat.desc(df_cheer$Y1)# m= 58,8 sd = 22,5
stat.desc(df_cheer$Y2)# m= 58,9 ; sd= 23,1
stat.desc(df_cheer$Y3)# m = 57,8 ; sd= 23,5

# anger
#df_anger[df_anger==999]<-NA
stat.desc(df_anger$Y1) # mean =12,0; SD =16,5
stat.desc(df_anger$Y2) # m= 10,7; sd = 15,9
stat.desc(df_anger$Y3) # m= 10,4; sd = 16,0

#stress
df_stress[df_stress==999]<-NA
stat.desc(df_stress$Y1) # mean =23,8,0; SD =23,6
stat.desc(df_stress$Y2) # m= 19,3; sd = 22,7
stat.desc(df_stress$Y3) # m= 19,5; sd = 23,2

# Rescale the Data
# to 0-1 scale

for (i in 1:length(df_conf_hap_res$ID)) {
  for (col in 3:8){
    if(df_conf_hap_res[i,col] != 999){
      df_conf_hap_res[i,col]<- df_conf_hap_res[i,col]/10
    }
  }
}
 
for (i in 1:length(df_stress$ID)) {
  for (col in 3:5){
    if(df_stress[i,col] != 999){
      df_stress[i,col]<- df_stress[i,col]/100
    }
    if(df_rlx[i,col] != 999){
      df_rlx[i,col]<- df_rlx[i,col]/100
    }
    if(df_lone[i,col] != 999){
      df_lone[i,col]<- df_lone[i,col]/100
    }
    if(df_cheer[i,col] != 999){
      df_cheer[i,col]<- df_cheer[i,col]/100
    }
    if(df_anger[i,col] != 999){
      df_anger[i,col]<- df_anger[i,col]/100
    }
    
  }
}




# Model 0: Variance Decomposition #####

### Depression


m0_depression= mplusObject(TITLE = "M0: Variance Decomposition for Depression",
                            rdata = df_dep_res,
                            usevariables = c("ID" ,"hour","Y1","Y2","Y3"),
                            VARIABLE = "
                            CLUSTER = ID;
                            USEVAR = Y1 Y2 Y3;
                            MISSING = ALL(999);",
                            ANALYSIS="
                              TYPE = TWOLEVEL;
                            ESTIMATOR = BAYES;
                            BITERATIONS = (1000);
                            CHAINS = 2;
                            PROCESSORS = 2;",
                            MODEL= "
                            
                            %WITHIN%
                            !Y1-Y3 WITH Y1-Y3@0;    ! by default these are zero
                            
                            ! defining measurement-occasion variance
                            Y1-Y3 (measurev1-measurev3); 
                            
                            %BETWEEN%
                            !Creating a random intercept for means of Y
                            Y BY Y1-Y3@1;
                            
                            !defining burst variance
                            Y1-Y3 (burstv1-burstv3);
                            
                            ! defining between person variance
                            Y (personv);", 	        
 MODELCONSTRAINT ="
  ! Compute the M-B-P proportions of variance:
NEW (measure1); measure1 = measurev1/(measurev1 + burstv1 + personv);
NEW (burst1); burst1 = burstv1/(measurev1 + burstv1 + personv);
NEW (person1); person1 = personv/(measurev1 + burstv1 + personv);
                            
NEW (measure2); measure2 = measurev2/(measurev2 + burstv2 + personv);
NEW (burst2); burst2 = burstv2/(measurev2 + burstv2 + personv);
NEW (person2); person2 = personv/(measurev2 + burstv2 + personv);
                            
NEW (measure3); measure3 = measurev3/(measurev3 + burstv3 + personv);
NEW (burst3); burst3 = burstv3/(measurev3 + burstv3 + personv);
NEW (person3); person3 = personv/(measurev3 + burstv3 + personv);",

OUTPUT= "TECH1 TECH8 STDYX FSCOMPARISON;",
                            
PLOT= "TYPE = PLOT3;"
 )

### Confidence:
m0_confidence= mplusObject(TITLE = "M0: Variance Decomposition for Confidence",
                            rdata = df_conf_hap_res,
                            usevariables = c("ID" ,"hour", "C1","C2","C3","H1","H2","H3"),
                            VARIABLE = "
                            CLUSTER = ID;
                            USEVAR = C1 C2 C3;
                            MISSING = ALL(999);",
                            ANALYSIS="
                              TYPE = TWOLEVEL;
                            ESTIMATOR = BAYES;
                            BITERATIONS = (10000);
                            PROCESSORS = 5;",
                            MODEL= "
                            
                            %WITHIN%
                            !C1-C3 WITH C1-C3@0;    ! by default these are zero
                            
                            ! defining measurement-occasion variance
                            C1-C3 (measurev1-measurev3); 
                            
                            %BETWEEN%
                            !Creating a random intercept for means of C
                            C BY C1-C3@1;
                            
                            !defining burst variance
                            C1-C3 (burstv1-burstv3);
                            
                            ! defining between person variance
                            C (personv);", 	        
                            
MODELCONSTRAINT="
! Compute the M-B-P proportions of variance:
NEW (measure1); measure1 = measurev1/(measurev1 + burstv1 + personv);
NEW (burst1); burst1 = burstv1/(measurev1 + burstv1 + personv);
NEW (person1); person1 = personv/(measurev1 + burstv1 + personv);
                            
NEW (measure2); measure2 = measurev2/(measurev2 + burstv2 + personv);
NEW (burst2); burst2 = burstv2/(measurev2 + burstv2 + personv);
NEW (person2); person2 = personv/(measurev2 + burstv2 + personv);
                            
NEW (measure3); measure3 = measurev3/(measurev3 + burstv3 + personv);
NEW (burst3); burst3 = burstv3/(measurev3 + burstv3 + personv);
NEW (person3); person3 = personv/(measurev3 + burstv3 + personv);",

OUTPUT= "TECH1 TECH8 STDYX FSCOMPARISON;",
                            
PLOT= "TYPE = PLOT3;"
)

## Anger

m0_anger= mplusObject(TITLE = "M0: Variance Decomposition for Anger",
                       rdata = df_anger,
                       usevariables = c("ID" ,"hour","Y1","Y2","Y3"),
                       VARIABLE = "
                            CLUSTER = ID;
                            USEVAR = Y1 Y2 Y3;
                            MISSING = ALL(999);",
                       ANALYSIS="
                              TYPE = TWOLEVEL;
                            ESTIMATOR = BAYES;
                            BITERATIONS = (10000);
                            PROCESSORS = 5;",
                       MODEL= "
                            
                            %WITHIN%
                            !Y1-Y3 WITH Y1-Y3@0;    ! by default these are zero
                            
                            ! defining measurement-occasion variance
                            Y1-Y3 (measurev1-measurev3); 
                            
                            %BETWEEN%
                            !Creating a random intercept for means of Y
                            Y BY Y1-Y3@1;
                            
                            !defining burst variance
                            Y1-Y3 (burstv1-burstv3);
                            
                            ! defining between person variance
                            Y (personv);", 	        
                       MODELCONSTRAINT ="
  ! Compute the M-B-P proportions of variance:
NEW (measure1); measure1 = measurev1/(measurev1 + burstv1 + personv);
NEW (burst1); burst1 = burstv1/(measurev1 + burstv1 + personv);
NEW (person1); person1 = personv/(measurev1 + burstv1 + personv);
                            
NEW (measure2); measure2 = measurev2/(measurev2 + burstv2 + personv);
NEW (burst2); burst2 = burstv2/(measurev2 + burstv2 + personv);
NEW (person2); person2 = personv/(measurev2 + burstv2 + personv);
                            
NEW (measure3); measure3 = measurev3/(measurev3 + burstv3 + personv);
NEW (burst3); burst3 = burstv3/(measurev3 + burstv3 + personv);
NEW (person3); person3 = personv/(measurev3 + burstv3 + personv);",
                       
                       OUTPUT= "TECH1 TECH8 STDYX FSCOMPARISON;",
                       
                       PLOT= "TYPE = PLOT3;"
)

## Stress

m0_stress= mplusObject(TITLE = "M0: Variance Decomposition for Stress",
                        rdata = df_stress,
                        usevariables = c("ID" ,"hour","Y1","Y2","Y3"),
                        VARIABLE = "
                            CLUSTER = ID;
                            USEVAR = Y1 Y2 Y3;
                            MISSING = ALL(999);",
                        ANALYSIS="
                              TYPE = TWOLEVEL;
                            ESTIMATOR = BAYES;
                            BITERATIONS = (10000);
                            PROCESSORS = 5;",
                        MODEL= "
                            
                            %WITHIN%
                            !Y1-Y3 WITH Y1-Y3@0;    ! by default these are zero
                            
                            ! defining measurement-occasion variance
                            Y1-Y3 (measurev1-measurev3); 
                            
                            %BETWEEN%
                            !Creating a random intercept for means of Y
                            Y BY Y1-Y3@1;
                            
                            !defining burst variance
                            Y1-Y3 (burstv1-burstv3);
                            
                            ! defining between person variance
                            Y (personv);", 	        
                        MODELCONSTRAINT ="
  ! Compute the M-B-P proportions of variance:
NEW (measure1); measure1 = measurev1/(measurev1 + burstv1 + personv);
NEW (burst1); burst1 = burstv1/(measurev1 + burstv1 + personv);
NEW (person1); person1 = personv/(measurev1 + burstv1 + personv);
                            
NEW (measure2); measure2 = measurev2/(measurev2 + burstv2 + personv);
NEW (burst2); burst2 = burstv2/(measurev2 + burstv2 + personv);
NEW (person2); person2 = personv/(measurev2 + burstv2 + personv);
                            
NEW (measure3); measure3 = measurev3/(measurev3 + burstv3 + personv);
NEW (burst3); burst3 = burstv3/(measurev3 + burstv3 + personv);
NEW (person3); person3 = personv/(measurev3 + burstv3 + personv);",
                        
                        OUTPUT= "TECH1 TECH8 STDYX FSCOMPARISON;",
                        
                        PLOT= "TYPE = PLOT3;"
)

## Cheerfulness
m0_cheer= mplusObject(TITLE = "M0: Variance Decomposition for Cheerfulness",
                       rdata = df_cheer,
                       usevariables = c("ID" ,"hour","Y1","Y2","Y3"),
                       VARIABLE = "
                            CLUSTER = ID;
                            USEVAR = Y1 Y2 Y3;
                            MISSING = ALL(999);",
                       ANALYSIS="
                              TYPE = TWOLEVEL;
                            ESTIMATOR = BAYES;
                            BITERATIONS = (10000);
                            PROCESSORS = 5;",
                       MODEL= "
                            
                            %WITHIN%
                            !Y1-Y3 WITH Y1-Y3@0;    ! by default these are zero
                            
                            ! defining measurement-occasion variance
                            Y1-Y3 (measurev1-measurev3); 
                            
                            %BETWEEN%
                            !Creating a random intercept for means of Y
                            Y BY Y1-Y3@1;
                            
                            !defining burst variance
                            Y1-Y3 (burstv1-burstv3);
                            
                            ! defining between person variance
                            Y (personv);", 	        
                       MODELCONSTRAINT ="
  ! Compute the M-B-P proportions of variance:
NEW (measure1); measure1 = measurev1/(measurev1 + burstv1 + personv);
NEW (burst1); burst1 = burstv1/(measurev1 + burstv1 + personv);
NEW (person1); person1 = personv/(measurev1 + burstv1 + personv);
                            
NEW (measure2); measure2 = measurev2/(measurev2 + burstv2 + personv);
NEW (burst2); burst2 = burstv2/(measurev2 + burstv2 + personv);
NEW (person2); person2 = personv/(measurev2 + burstv2 + personv);
                            
NEW (measure3); measure3 = measurev3/(measurev3 + burstv3 + personv);
NEW (burst3); burst3 = burstv3/(measurev3 + burstv3 + personv);
NEW (person3); person3 = personv/(measurev3 + burstv3 + personv);",
                       
                       OUTPUT= "TECH1 TECH8 STDYX FSCOMPARISON;",
                       
                       PLOT= "TYPE = PLOT3;"
)



## Running models

m0_depression<-MplusAutomation::mplusModeler(
  m0_depression,
  check = FALSE,
  modelout = "m0_depression.inp",
  hashfilename = FALSE,
  run = 1L)

m0_confidence<-MplusAutomation::mplusModeler(
  m0_confidence,
  check = FALSE,
  modelout = "m0_confidence.inp",
  hashfilename = FALSE,
  run = 1L)

m0_anger<-MplusAutomation::mplusModeler(
  m0_anger,
  check = FALSE,
  modelout = "m0_anger.inp",
  hashfilename = FALSE,
  run = 1L)

m0_stress<-MplusAutomation::mplusModeler(
  m0_stress,
  check = FALSE,
  modelout = "m0_stress.inp",
  hashfilename = FALSE,
  run = 1L)

m0_cheer<-MplusAutomation::mplusModeler(
  m0_cheer,
  check = FALSE,
  modelout = "m0_cheer.inp",
  hashfilename = FALSE,
  run = 1L)

# Model 1: Network Models #####

# Model 1a: Phi and Mu
##Depression
m1a_dep<- mplusObject(TITLE = "M1a: Network of Mean and Phi for Depression",
                      rdata = df_dep_res,
                      usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                      VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          BITERATIONS = (10000);
          CHAINS=2;
          PROCESSORS = 30;",
          MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          Y1-Y3 WITH Y1-Y3@0;
          %BETWEEN%
          Y1-Y3 phi1-phi3 WITH
        Y1-Y3 phi1-phi3;
          ", OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
          
          PLOT= "TYPE = PLOT3;
           FACTOR =All;")
          
##Confidence
m1a_confidence<- mplusObject(TITLE = "M1a: Network of Mean and Phi for Confidence",
                            rdata = df_conf_hap_res,
                            usevariables = c("ID" ,"hour", "C1","C2","C3","H1","H2","H3"),
                            VARIABLE = "
                            CLUSTER = ID;
                            USEVAR = C1 C2 C3;
                            LAGGED = C1(1) C2(1) C3(1);
                            MISSING = ALL(999);",
                            ANALYSIS="
                              TYPE = TWOLEVEL RANDOM;
                            ESTIMATOR = BAYES;
                            BITERATIONS = (10000);
                            PROCESSORS = 30;",
                            MODEL= "
                            %WITHIN%
                            phi1 | C1 ON C1&1;
                            phi2 | C2 ON C2&1;
                            phi3 | C3 ON C3&1;
                          
                            C1-C3 WITH C1-C3@0;
                            C1 WITH C2@0;
                            
                            %BETWEEN%
                            C1-C3 phi1-phi3 WITH
                            C1-C3 phi1-phi3;
                            ",
                            
                            OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                            
                            PLOT= "TYPE = PLOT3;
                            FACTOR =All;"
)

## Stress

m1a_stress<- mplusObject(TITLE = "M 1a: Network of Mean and Phi for Stress",
                      rdata = df_stress,
                      usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                      VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          BITERATIONS = (10000);
          CHAINS=2;
          PROCESSORS = 30;",
                      MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          Y1-Y3 WITH Y1-Y3@0;
          %BETWEEN%
          Y1-Y3 phi1-phi3 WITH
        Y1-Y3 phi1-phi3;
          ", OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                      
                      PLOT= "TYPE = PLOT3;
           FACTOR =All;")

## Anger

m1a_anger<- mplusObject(TITLE = "M1a: Network  of Mean and Phi for Anger",
                      rdata = df_anger,
                      usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                      VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          BITERATIONS = (10000);
          CHAINS=2;
          PROCESSORS = 30;",
                      MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          Y1-Y3 WITH Y1-Y3@0;
          %BETWEEN%
          Y1-Y3 phi1-phi3 WITH
          Y1-Y3 phi1-phi3;
          ", OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                      
                      PLOT= "TYPE = PLOT3;
           FACTOR =All;")
## Cheerfulness

m1a_cheer<- mplusObject(TITLE = "M1a: Network of Mean and Phi for Cheerfulness",
                      rdata = df_cheer,
                      usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                      VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          BITERATIONS = (10000);
          CHAINS=2;
          PROCESSORS = 30;",
                      MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          Y1-Y3 WITH Y1-Y3@0;
          %BETWEEN%
          Y1-Y3 phi1-phi3 WITH
        Y1-Y3 phi1-phi3;
          ", OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                      
                      PLOT= "TYPE = PLOT3;
           FACTOR =All;")
  # Model 1b: Phi Mu and RV
m1b_dep<- mplusObject(TITLE = "M1b: Network of Mean, Residuals and Phi for Depression",
                      rdata = df_dep_res,
                      usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                      VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          BITERATIONS = (10000);
          CHAINS=2;
          PROCESSORS = 30;",
                      MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;
          
          !defining random residual variances
          rv1 | Y1;
          rv2 | Y2;
          rv3 | Y3;

          %BETWEEN%
          Y1-Y3 phi1-phi3 rv1-rv3 WITH 
          Y1-Y3 phi1-phi3 rv1-rv3;
          ", OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                      
                      PLOT= "TYPE = PLOT3;
           FACTOR =All;")

##Confidence
m1b_confidence<- mplusObject(TITLE = "M1b: Network of Mean Residuals and Phi for Confidence",
                             rdata = df_conf_hap_res,
                             usevariables = c("ID" ,"hour", "C1","C2","C3","H1","H2","H3"),
                             VARIABLE = "
                            CLUSTER = ID;
                            USEVAR = C1 C2 C3;
                            LAGGED = C1(1) C2(1) C3(1);
                            MISSING = ALL(999);",
                             ANALYSIS="
                              TYPE = TWOLEVEL RANDOM;
                            ESTIMATOR = BAYES;
                            BITERATIONS = (10000);
                            PROCESSORS = 30;",
                             MODEL= "
                            %WITHIN%
                            phi1 | C1 ON C1&1;
                            phi2 | C2 ON C2&1;
                            phi3 | C3 ON C3&1;
                            
                            !defining random residual variances
                            rv1 | C1;
                            rv2 | C2;
                            rv3 | C3;
                            
                            %BETWEEN%
                            C1-C3 phi1-phi3 rv1-rv3 WITH 
                            C1-C3 phi1-phi3 rv1-rv3;
                            ",
                             
                             OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                             
                             PLOT= "TYPE = PLOT3;
                            FACTOR =All;")

## Stress
m1b_stress<- mplusObject(TITLE = "M1b: Network of Mean, Residuals and Phi for Stress",
                      rdata = df_stress,
                      usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                      VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          BITERATIONS = (10000);
          CHAINS=2;
          PROCESSORS = 30;",
                      MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;
          
          !defining random residual variances
          rv1 | Y1;
          rv2 | Y2;
          rv3 | Y3;

          %BETWEEN%
          Y1-Y3 phi1-phi3 rv1-rv3 WITH 
          Y1-Y3 phi1-phi3 rv1-rv3;
          ", OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                      
                      PLOT= "TYPE = PLOT3;
           FACTOR =All;")

## Anger
m1b_anger<- mplusObject(TITLE = "M1b: Network of Mean, Residuals and Phi for Anger",
                      rdata = df_anger,
                      usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                      VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          BITERATIONS = (10000);
          CHAINS=2;
          PROCESSORS = 30;",
                      MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;
          
          !defining random residual variances
          rv1 | Y1;
          rv2 | Y2;
          rv3 | Y3;

          %BETWEEN%
          Y1-Y3 phi1-phi3 rv1-rv3 WITH 
          Y1-Y3 phi1-phi3 rv1-rv3;
          ", OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                      
                      PLOT= "TYPE = PLOT3;
           FACTOR =All;")
## Cheerfulness
m1b_cheer<- mplusObject(TITLE = "M 1b: Network of Mean, Residuals and Phi for Cheerfulness",
                      rdata = df_dep_res,
                      usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                      VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          BITERATIONS = (10000);
          CHAINS=2;
          PROCESSORS = 30;",
                      MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;
          
          !defining random residual variances
          rv1 | Y1;
          rv2 | Y2;
          rv3 | Y3;

          %BETWEEN%
          Y1-Y3 phi1-phi3 rv1-rv3 WITH 
          Y1-Y3 phi1-phi3 rv1-rv3;
          ", OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                      
                      PLOT= "TYPE = PLOT3;
           FACTOR =All;")

## Run all M1 models
# depression 
m1a_dep<- MplusAutomation::mplusModeler(
  m1a_dep,
  check = FALSE,
  modelout = "m1a_dep.inp",
  hashfilename = FALSE,
  run = 1L)
m1b_dep<- MplusAutomation::mplusModeler(
  m1b_dep,
  check = FALSE,
  modelout = "m1b_dep.inp",
  hashfilename = FALSE,
  run = 1L)
#confidence
m1a_conf<- MplusAutomation::mplusModeler(
  m1a_confidence,
  check = FALSE,
  modelout = "m1a_conf.inp",
  hashfilename = FALSE,
  run = 1L)
m1a_conf_check<- MplusAutomation::mplusModeler(
  m1a_confidence,
  check = FALSE,
  modelout = "m1a_conf_check.inp",
  hashfilename = FALSE,
  run = 1L)
m1b_conf<- MplusAutomation::mplusModeler(
  m1b_confidence,
  check = FALSE,
  modelout = "m1b_conf.inp",
  hashfilename = FALSE,
  run = 1L)
# stress

m1a_stress<- MplusAutomation::mplusModeler(
  m1a_stress,
  check = FALSE,
  modelout = "m1a_stress.inp",
  hashfilename = FALSE,
  run = 1L)
m1b_stress<- MplusAutomation::mplusModeler(
  m1b_stress,
  check = FALSE,
  modelout = "m1b_stress.inp",
  hashfilename = FALSE,
  run = 1L)

# anger
m1a_anger<- MplusAutomation::mplusModeler(
  m1a_anger,
  check = FALSE,
  modelout = "m1a_anger.inp",
  hashfilename = FALSE,
  run = 1L)

m1b_anger<- MplusAutomation::mplusModeler(
  m1b_anger,
  check = FALSE,
  modelout = "m1b_anger.inp",
  hashfilename = FALSE,
  run = 1L)

# cheerfulness

m1a_cheer<- MplusAutomation::mplusModeler(
  m1a_cheer,
  check = FALSE,
  modelout = "m1a_cheer.inp",
  hashfilename = FALSE,
  run = 1L)
m1b_cheer<- MplusAutomation::mplusModeler(
  m1b_cheer,
  check = FALSE,
  modelout = "m1b_cheer.inp",
  hashfilename = FALSE,
  run = 1L)

# Model 2: RI of parameters #####
  # Model 2a: RI of Phi and Mu
m2a_dep<- mplusObject(TITLE = "M2a: Basic RI model of Mean and Phi for Depression",
                      rdata = df_dep_res,
                      usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                      VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          BITERATIONS = (10000);
          CHAINS=2;
          PROCESSORS = 30;",
                      MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          Y1-Y3 WITH Y1-Y3@0;
          %BETWEEN%
          !Defining Random Intercepts for Mean and Phi
          RI_phi BY phi1 phi2 phi3@1;
          RI_mu BY Y1 Y2 Y3@1;
          RI_phi WITH RI_mu;
          ", OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                      
                      PLOT= "TYPE = PLOT3;
           FACTOR =All;")
m2a2_dep<- mplusObject(TITLE = "M2a2: Basic RI model of Mean and Phi for Depression",
                      rdata = df_dep_res,
                      usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                      VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          BITERATIONS = (10000);
          CHAINS=2;
          PROCESSORS = 30;",
                      MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          Y1-Y3 WITH Y1-Y3@0;
          %BETWEEN%
          !Defining Random Intercepts for Mean and Phi
          RI_phi BY phi1 phi2 phi3@1;
          RI_mu BY Y1 Y2 Y3@1;
          RI_phi WITH RI_mu;
          
          Y1 WITH phi1;
          Y2 WITH phi2;
          Y3 WITH phi3;
          ", OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                      
                      PLOT= "TYPE = PLOT3;
           FACTOR =All;")

m2a_conf<- mplusObject(TITLE = "M2a: Basic RI model of Mean and Phi for Confidence",
                       rdata = df_conf_hap_res,
                       usevariables = c("ID" ,"hour", "C1","C2","C3", "H1","H2","H3"),
                       VARIABLE = "
                      !Note: in this model alone, Confidence is abbreviated as C
                         CLUSTER = ID;
USEVAR = C1 C2 
C3 ;

LAGGED = C1(1) C2(1) 
C3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          BITERATIONS = (10000);
          CHAINS=2;
          PROCESSORS = 30;",
                       MODEL ="%WITHIN%
          phi1 | C1 ON C1&1;
          phi2 | C2 ON C2&1;
          phi3 | C3 ON C3&1;

          C1-C3 WITH C1-C3@0;
          %BETWEEN%
          !Defining Random Intercepts for Mean and Phi
          RI_phi BY phi1 phi2 phi3@1;
          RI_mu BY C1 C2 C3@1;
          RI_phi WITH RI_mu;
          ", OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                       
                       PLOT= "TYPE = PLOT3;
           FACTOR =All;")
m2a_stress<- mplusObject(TITLE = "M2a: Basic RI model of Mean and Phi for Stress",
                         rdata = df_stress,
                         usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                         VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          BITERATIONS = (10000);
          CHAINS=2;
          PROCESSORS = 30;",
                         MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          Y1-Y3 WITH Y1-Y3@0;
          %BETWEEN%
          !Defining Random Intercepts for Mean and Phi
          RI_phi BY phi1 phi2 phi3@1;
          RI_mu BY Y1 Y2 Y3@1;
          RI_phi WITH RI_mu;
          ", OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                         
                         PLOT= "TYPE = PLOT3;
           FACTOR =All;")
m2a_anger<- mplusObject(TITLE = "M2a: Basic RI model of Mean and Phi for Anger",
                        rdata = df_anger,
                        usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                        VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          BITERATIONS = (10000);
          CHAINS=2;
          PROCESSORS = 30;",
                        MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          Y1-Y3 WITH Y1-Y3@0;
          %BETWEEN%
          !Defining Random Intercepts for Mean and Phi
          RI_phi BY phi1 phi2 phi3@1;
          RI_mu BY Y1 Y2 Y3@1;
          RI_phi WITH RI_mu;
          ", OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                        
                        PLOT= "TYPE = PLOT3;
           FACTOR =All;")

m2a_cheer<- mplusObject(TITLE = "M2a: Basic RI model of Mean and Phi for Cheerfulness",
                        rdata = df_cheer,
                        usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                        VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          BITERATIONS = (10000);
          CHAINS=2;
          PROCESSORS = 30;",
                        MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          Y1-Y3 WITH Y1-Y3@0;
          %BETWEEN%
          !Defining Random Intercepts for Mean and Phi
          RI_phi BY phi1 phi2 phi3@1;
          RI_mu BY Y1 Y2 Y3@1;
          RI_phi WITH RI_mu;
          ", OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                        
                        PLOT= "TYPE = PLOT3;
           FACTOR =All;")

  # Model 2b: RI of phi mu and rv
m2b_dep<- mplusObject(TITLE = "M2b: Basic RI model of Mean , RV and Phi for Depression",
                      rdata = df_dep_res,
                      usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                      VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          BITERATIONS = (10000);
          CHAINS=2;
          PROCESSORS = 30;",
                      MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          rv1 | Y1;
          rv2 | Y2;
          rv3 | Y3;
          
          %BETWEEN%
          !Defining Random Intercepts for Mean and Phi
          RI_phi BY phi1 phi2 phi3@1;
          RI_mu BY Y1 Y2 Y3@1;
          Ri_rv BY rv1 rv2 rv3@1;
          RI_phi WITH RI_mu RI_rv;
          ", OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                      
                      PLOT= "TYPE = PLOT3;
           FACTOR =All;")
m2b_conf<- mplusObject(TITLE = "M2b: Basic RI model of Mean RV and Phi for Confidence",
                       rdata = df_conf_hap_res,
                       usevariables = c("ID" ,"hour", "C1","C2","C3", "H1","H2","H3"),
                       VARIABLE = "
                      !Note: in this model alone, Confidence is abbreviated as C
                         CLUSTER = ID;
USEVAR = C1 C2 
C3 ;

LAGGED = C1(1) C2(1) 
C3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          BITERATIONS = (10000);
          CHAINS=2;
          PROCESSORS = 30;",
                       MODEL ="%WITHIN%
          phi1 | C1 ON C1&1;
          phi2 | C2 ON C2&1;
          phi3 | C3 ON C3&1;

          rv1 | C1;
          rv2 | C2;
          rv3 | C3;
          
          %BETWEEN%
          !Defining Random Intercepts for Mean and Phi
          RI_phi BY phi1 phi2 phi3@1;
          RI_mu BY C1 C2 C3@1;
          Ri_rv BY rv1 rv2 rv3@1;
          RI_phi WITH RI_mu RI_rv;
          ", OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                       
                       PLOT= "TYPE = PLOT3;
           FACTOR =All;")
m2b_stress<- mplusObject(TITLE = "M2b: Basic RI model of Mean  RV and Phi for Stress",
                         rdata = df_stress,
                         usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                         VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          BITERATIONS = (10000);
          CHAINS=2;
          PROCESSORS = 30;",
                         MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          rv1 | Y1;
          rv2 | Y2;
          rv3 | Y3;
          
          %BETWEEN%
          !Defining Random Intercepts for Mean and Phi
          RI_phi BY phi1 phi2 phi3@1;
          RI_mu BY Y1 Y2 Y3@1;
          Ri_rv BY rv1 rv2 rv3@1;
          RI_phi WITH RI_mu RI_rv;
          ", OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                         
                         PLOT= "TYPE = PLOT3;
           FACTOR =All;")
m2b_anger<- mplusObject(TITLE = "M2b: Basic RI model of Mean and Phi for Anger",
                        rdata = df_anger,
                        usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                        VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          BITERATIONS = (10000);
          CHAINS=2;
          PROCESSORS = 30;",
                        MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

         rv1 | Y1;
          rv2 | Y2;
          rv3 | Y3;
          
          %BETWEEN%
          !Defining Random Intercepts for Mean and Phi
          RI_phi BY phi1 phi2 phi3@1;
          RI_mu BY Y1 Y2 Y3@1;
          Ri_rv BY rv1 rv2 rv3@1;
          RI_phi WITH RI_mu RI_rv;
          ", OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                        
                        PLOT= "TYPE = PLOT3;
           FACTOR =All;")

m2b_cheer<- mplusObject(TITLE = "M2b: Basic RI model of Mean RV and Phi for Cheerfulness",
                        rdata = df_cheer,
                        usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                        VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          BITERATIONS = (10000);
          CHAINS=2;
          PROCESSORS = 30;",
                        MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          rv1 | Y1;
          rv2 | Y2;
          rv3 | Y3;
          
          %BETWEEN%
          !Defining Random Intercepts for Mean and Phi
          RI_phi BY phi1 phi2 phi3@1;
          RI_mu BY Y1 Y2 Y3@1;
          Ri_rv BY rv1 rv2 rv3@1;
          RI_phi WITH RI_mu RI_rv;
          ", OUTPUT= "TECH1 TECH3 TECH8 STDYX FSCOMPARISON;",
                        
                        PLOT= "TYPE = PLOT3;
           FACTOR =All;")


#Run all
m2a_dep<- MplusAutomation::mplusModeler(
  m2a_dep,
  check = FALSE,
  modelout = "m2a_dep.inp",
  hashfilename = FALSE,
  run = 1L)
m2b_dep<- MplusAutomation::mplusModeler(
  m2b_dep,
  check = FALSE,
  modelout = "m2b_dep.inp",
  hashfilename = FALSE,
  run = 1L)

m2a_conf<- MplusAutomation::mplusModeler(
  m2a_conf,
  check = FALSE,
  modelout = "m2a_conf.inp",
  hashfilename = FALSE,
  run = 1L)
m2b_conf<- MplusAutomation::mplusModeler(
  m2b_conf,
  check = FALSE,
  modelout = "m2b_conf.inp",
  hashfilename = FALSE,
  run = 1L)

m2a_stress<- MplusAutomation::mplusModeler(
  m2a_stress,
  check = FALSE,
  modelout = "m2a_stress.inp",
  hashfilename = FALSE,
  run = 1L)
m2b_stress<- MplusAutomation::mplusModeler(
  m2b_stress,
  check = FALSE,
  modelout = "m2b_stress.inp",
  hashfilename = FALSE,
  run = 1L)

m2a_anger<- MplusAutomation::mplusModeler(
  m2a_anger,
  check = FALSE,
  modelout = "m2a_anger.inp",
  hashfilename = FALSE,
  run = 1L)
m2b_anger<- MplusAutomation::mplusModeler(
  m2b_anger,
  check = FALSE,
  modelout = "m2b_anger.inp",
  hashfilename = FALSE,
  run = 1L)

m2a_cheer<- MplusAutomation::mplusModeler(
  m2a_cheer,
  check = FALSE,
  modelout = "m2a_cheer.inp",
  hashfilename = FALSE,
  run = 1L)
m2b_cheer<- MplusAutomation::mplusModeler(
  m2b_cheer,
  check = FALSE,
  modelout = "m2b_cheer.inp",
  hashfilename = FALSE,
  run = 1L)
# Model 3: RI-AR-Burst Model #####

## 3o model: CLPM

m3o_dep<-mplusObject(TITLE = "M3o Depression: CLPM with cov of residuals",
                     rdata = df_dep_res,
                     usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                     VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (8000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 30;",
                     MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          Y1 Y2 WITH Y3@0;
          Y1 WITH Y2@0;

          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR Y (MEANS) AND PHI
          !Y BY Y1-Y3@1;
          !phi BY phi1-phi3@1;
          !Y WITH phi;

          Y1 WITH phi1;
          Y2 WITH phi2;
          Y3 WITH phi3;

          ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS WITH AN AR(1)
          Y2 Y3 PON Y1 Y2 (a1);
          
          !phi1 WITH Y@0;
          !Y1 WITH phi@0;

          ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS OF PHI WITH AN AR(1) across bursts
          
          phi2 phi3 PON phi1 phi2 (a2);  
          
          Y2 Y3 PON phi1 phi2 (b1);
          phi2 phi3 PON Y1 Y2 (b2);
                     ",
                     OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                     PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)

m3o_dep<-MplusAutomation::mplusModeler(
  m3o_dep,
  check = FALSE,
  modelout = "m3o_dep.inp",
  hashfilename = FALSE,
  run = 1L)


## depression
m3a_dep<-mplusObject(TITLE = "M3a Depression",
            rdata = df_dep_res,
            usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
            VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 10;",
            MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          Y1 Y2 WITH Y3@0;
          Y1 WITH Y2@0;

          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR Y (MEANS) AND PHI
          Y BY Y1-Y3@1;
          phi BY phi1-phi3@1;
          Y WITH phi;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR Y
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCY1 BY Y1; Y1@0.001;
          WCY2 BY Y2; Y2@0.001;
          WCY3 BY Y3; Y3@0.001;
          
          WCY1 WITH Y@0;
          WCY1 WITH phi@0;

          ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS WITH AN AR(1)
          WCY2 WCY3 PON WCY1 WCY2 (a1);
          
          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR PHI
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCPHI1 BY phi1; phi1@0.001;
          WCPHI2 BY phi2; phi2@0.001;
          WCPHI3 BY phi3; phi3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH Y@0;
          
          WCPHI1 WITH WCY1@0;
          WCPHI3 WITH WCY3@0;

          ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS OF PHI WITH AN AR(1) across bursts
          
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2);  
                             ",
            OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
            PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)


m3a_dep<-MplusAutomation::mplusModeler(
  m5_dep_res_thin,
  check = FALSE,
  modelout = "m3a_dep.inp",
  hashfilename = FALSE,
  run = 1L)

## cheerful
m3a_cheer<-mplusObject(TITLE = "M3a Cheerful",
                     rdata = df_cheer,
                     usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                     VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 10;",
                     MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          Y1 Y2 WITH Y3@0;
          Y1 WITH Y2@0;

          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR Y (MEANS) AND PHI
          Y BY Y1-Y3@1;
          phi BY phi1-phi3@1;
          Y WITH phi;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR Y
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCY1 BY Y1; Y1@0.001;
          WCY2 BY Y2; Y2@0.001;
          WCY3 BY Y3; Y3@0.001;
          
          WCY1 WITH Y@0;
          WCY1 WITH phi@0;

          ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS WITH AN AR(1)
          WCY2 WCY3 PON WCY1 WCY2 (a1);
          
          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR PHI
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCPHI1 BY phi1; phi1@0.001;
          WCPHI2 BY phi2; phi2@0.001;
          WCPHI3 BY phi3; phi3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH Y@0;
          
          WCPHI1 WITH WCY1@0;
          WCPHI3 WITH WCY3@0;

          ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS OF PHI WITH AN AR(1) across bursts
          
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2);  
                             ",
                     OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                     PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)


m3a_cheer<-MplusAutomation::mplusModeler(
  m3a_cheer,
  check = FALSE,
  modelout = "m3a_cheer.inp",
  hashfilename = FALSE,
  run = 1L)

m3a_stress<-mplusObject(TITLE = "M3a Stress",
                     rdata = df_stress,
                     usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                     VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 10;",
                     MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          Y1 Y2 WITH Y3@0;
          Y1 WITH Y2@0;

          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR Y (MEANS) AND PHI
          Y BY Y1-Y3@1;
          phi BY phi1-phi3@1;
          Y WITH phi;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR Y
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCY1 BY Y1; Y1@0.001;
          WCY2 BY Y2; Y2@0.001;
          WCY3 BY Y3; Y3@0.001;
          
          WCY1 WITH Y@0;
          WCY1 WITH phi@0;

          ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS WITH AN AR(1)
          WCY2 WCY3 PON WCY1 WCY2 (a1);
          
          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR PHI
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCPHI1 BY phi1; phi1@0.001;
          WCPHI2 BY phi2; phi2@0.001;
          WCPHI3 BY phi3; phi3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH Y@0;
          
          WCPHI1 WITH WCY1@0;
          WCPHI3 WITH WCY3@0;

          ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS OF PHI WITH AN AR(1) across bursts
          
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2);  
                             ",
                     OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                     PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)


m3a_stress<-MplusAutomation::mplusModeler(
  m3a_stress,
  check = FALSE,
  modelout = "m3a_stress.inp",
  hashfilename = FALSE,
  run = 1L)

## Anger

m3a_anger<-mplusObject(TITLE = "M3a Anger",
                     rdata = df_anger,
                     usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                     VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 10;",
                     MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          Y1 Y2 WITH Y3@0;
          Y1 WITH Y2@0;

          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR Y (MEANS) AND PHI
          Y BY Y1-Y3@1;
          phi BY phi1-phi3@1;
          Y WITH phi;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR Y
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCY1 BY Y1; Y1@0.001;
          WCY2 BY Y2; Y2@0.001;
          WCY3 BY Y3; Y3@0.001;
          
          WCY1 WITH Y@0;
          WCY1 WITH phi@0;

          ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS WITH AN AR(1)
          WCY2 WCY3 PON WCY1 WCY2 (a1);
          
          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR PHI
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCPHI1 BY phi1; phi1@0.001;
          WCPHI2 BY phi2; phi2@0.001;
          WCPHI3 BY phi3; phi3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH Y@0;
          
          WCPHI1 WITH WCY1@0;
          WCPHI3 WITH WCY3@0;

          ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS OF PHI WITH AN AR(1) across bursts
          
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2);  
                             ",
                     OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                     PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)

library(tictoc)
tic()
m3a_stress<-MplusAutomation::mplusModeler(
  m3a_stress,
  check = FALSE,
  modelout = "m3a_stress.inp",
  hashfilename = FALSE,
  run = 1L)
toc()
tic()
m3a_anger<-MplusAutomation::mplusModeler(
  m3a_anger,
  check = FALSE,
  modelout = "m3a_anger.inp",
  hashfilename = FALSE,
  run = 1L)
toc()
tic()
m3a_cheer<-MplusAutomation::mplusModeler(
  m3a_cheer,
  check = FALSE,
  modelout = "m3a_cheer.inp",
  hashfilename = FALSE,
  run = 1L)
toc()
## Confidence
m3a_conf<- mplusObject(TITLE = "M3a Confidence",
                               rdata = df_conf_hap_res,
                               usevariables = c("ID" ,"hour", "C1","C2","C3","H1","H2","H3"),
                               VARIABLE = "
                         CLUSTER = ID;
USEVAR = C1 C2 
C3 ;

LAGGED = C1(1) C2(1) 
C3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 30;",
                               MODEL ="%WITHIN%
          phi1 | C1 ON C1&1;
          phi2 | C2 ON C2&1;
          phi3 | C3 ON C3&1;

          C1 C2 WITH C3@0;
          C1 WITH C2@0;

          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR C (MEANS) AND PHI
          C BY C1-C3@1;
          phi BY phi1-phi3@1;
          C WITH phi;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR C
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCC1 BY C1; C1@0.001;
          WCC2 BY C2; C2@0.001;
          WCC3 BY C3; C3@0.001;
          
          WCC1 WITH C@0;
          WCC1 WITH phi@0;

          ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS WITH AN AR(1)
          WCC2 WCC3 PON WCC1 WCC2 (b1);
          
          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR PHI
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCPHI1 BY phi1; phi1@0.001;
          WCPHI2 BY phi2; phi2@0.001;
          WCPHI3 BY phi3; phi3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH C@0;
          
          WCPHI1 WITH WCC1@0;
          WCPHI3 WITH WCC3@0;            
        ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS OF PHI WITH AN AR(1) across bursts
          
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2)",
                       OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                       PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
  
                     )

m3a_conf<-MplusAutomation::mplusModeler(
  m3a_conf,
  check = FALSE,
  modelout = "m3a_conf.inp",
  hashfilename = FALSE,
  run = 1L)
 
#3b confidence

####Something went wrong in convergence of model 3b for confidence

m3b_conf<- mplusObject(TITLE = "M3b: AR for Confidence icl RV",
                       rdata = df_conf_hap_res,
                       usevariables = c("ID" ,"hour", "C1","C2","C3","H1","H2","H3"),
                       VARIABLE = "
                         CLUSTER = ID;
USEVAR = C1 C2 
C3 ;

LAGGED = C1(1) C2(1) 
C3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 30;",
                       MODEL ="%WITHIN%
          phi1 | C1 ON C1&1;
          phi2 | C2 ON C2&1;
          phi3 | C3 ON C3&1;
          
          rv1 | C1;
          rv2 | C2;
          rv3 | C3;

          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR C (MEANS) AND PHI
          C BY C1-C3@1;
          RV BY rv1-rv3@1;
          phi BY phi1-phi3@1;
          C WITH phi RV;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR C
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCC1 BY C1; C1@0.001;
          WCC2 BY C2; C2@0.001;
          WCC3 BY C3; C3@0.001;
          
          WCC1 WITH C@0;
          WCC1 WITH phi@0;
          WCC1 WITH RV@0;

          
          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR PHI
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCPHI1 BY phi1; C1@0.001;
          WCPHI2 BY phi2; C2@0.001;
          WCPHI3 BY phi3; C3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH C@0;
          WCPHI1 WITH RV@0;
          
          WCPHI1 WITH WCC1@0;
          WCPHI3 WITH WCC3@0;
          
          !For Rv
          WCRV1 BY rv1; rv1@0.001;
          WCRV2 BY rv2; rv2@0.001;
          WCRV3 BY rv3; rv3@0.001;
          
          WCRV1 WITH phi@0;
          WCRV1 WITH C@0;
          WCRV1 WITH RV@0;
          
          !Removing mplus defaults 
          WCC1 WITH WCRV1@0;
          WCC1 WITH WCPHI1@0;
          
          WCC3 WITH WCRV3@0;
          WCC3 WITH WCPHI3@0;
          
          WCPHI1 WITH WCRV1@0;
          WCPHI3 WITH WCRV3@0;
          
          

          !Model autoregressive effects
          WCC2 WCC3 PON WCC1 WCC2 (a1);
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2);
          WCRV2 WCRV3 PON WCRV1 WCRV2 (a3);

                             ",
                       OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                       PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)
## Depression

m3b_dep<- mplusObject(TITLE = "M3b: AR for Derpession icl RV",
                      rdata = df_dep_res,
                      usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                      VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 30;",
                      MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;
          
          rv1 | Y1;
          rv2 | Y2;
          rv3 | Y3;

          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR Y (MEANS) AND PHI
          Y BY Y1-Y3@1;
          RV BY rv1-rv3@1;
          phi BY phi1-phi3@1;
          Y WITH phi RV;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR Y
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCY1 BY Y1; Y1@0.001;
          WCY2 BY Y2; Y2@0.001;
          WCY3 BY Y3; Y3@0.001;
          
          WCY1 WITH Y@0;
          WCY1 WITH phi@0;
          WCY1 WITH RV@0;

          
          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR PHI
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCPHI1 BY phi1; Y1@0.001;
          WCPHI2 BY phi2; Y2@0.001;
          WCPHI3 BY phi3; Y3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH Y@0;
          WCPHI1 WITH RV@0;
          
          WCPHI1 WITH WCY1@0;
          WCPHI3 WITH WCY3@0;
          
          !For Rv
          WCRV1 BY rv1; rv1@0.001;
          WCRV2 BY rv2; rv2@0.001;
          WCRV3 BY rv3; rv3@0.001;
          
          WCRV1 WITH phi@0;
          WCRV1 WITH Y@0;
          WCRV1 WITH RV@0;
          
          !Removing mplus defaults 
          WCY1 WITH WCRV1@0;
          WCY1 WITH WCPHI1@0;
          
          WCY3 WITH WCRV3@0;
          WCY3 WITH WCPHI3@0;
          
          WCPHI1 WITH WCRV1@0;
          WCPHI3 WITH WCRV3@0;
          
          

          !Model autoregressive effects
          WCY2 WCY3 PON WCY1 WCY2 (a1);
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2);
          WCRV2 WCRV3 PON WCRV1 WCRV2 (a3);

                             ",
                      OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                      PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)

m3b_stress<- mplusObject(TITLE = "M3b: AR for Stress icl RV",
                      rdata = df_stress,
                      usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                      VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 30;",
                      MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;
          
          rv1 | Y1;
          rv2 | Y2;
          rv3 | Y3;

          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR Y (MEANS) AND PHI
          Y BY Y1-Y3@1;
          RV BY rv1-rv3@1;
          phi BY phi1-phi3@1;
          Y WITH phi RV;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR Y
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCY1 BY Y1; Y1@0.001;
          WCY2 BY Y2; Y2@0.001;
          WCY3 BY Y3; Y3@0.001;
          
          WCY1 WITH Y@0;
          WCY1 WITH phi@0;
          WCY1 WITH RV@0;

          
          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR PHI
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCPHI1 BY phi1; Y1@0.001;
          WCPHI2 BY phi2; Y2@0.001;
          WCPHI3 BY phi3; Y3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH Y@0;
          WCPHI1 WITH RV@0;
          
          WCPHI1 WITH WCY1@0;
          WCPHI3 WITH WCY3@0;
          
          !For Rv
          WCRV1 BY rv1; rv1@0.001;
          WCRV2 BY rv2; rv2@0.001;
          WCRV3 BY rv3; rv3@0.001;
          
          WCRV1 WITH phi@0;
          WCRV1 WITH Y@0;
          WCRV1 WITH RV@0;
          
          !Removing mplus defaults 
          WCY1 WITH WCRV1@0;
          WCY1 WITH WCPHI1@0;
          
          WCY3 WITH WCRV3@0;
          WCY3 WITH WCPHI3@0;
          
          WCPHI1 WITH WCRV1@0;
          WCPHI3 WITH WCRV3@0;
          
          

          !Model autoregressive effects
          WCY2 WCY3 PON WCY1 WCY2 (a1);
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2);
          WCRV2 WCRV3 PON WCRV1 WCRV2 (a3);

                             ",
                      OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                      PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)

m3b_anger<- mplusObject(TITLE = "M3b: AR for Anger icl RV",
                      rdata = df_anger,
                      usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                      VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 30;",
                      MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;
          
          rv1 | Y1;
          rv2 | Y2;
          rv3 | Y3;

          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR Y (MEANS) AND PHI
          Y BY Y1-Y3@1;
          RV BY rv1-rv3@1;
          phi BY phi1-phi3@1;
          Y WITH phi RV;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR Y
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCY1 BY Y1; Y1@0.001;
          WCY2 BY Y2; Y2@0.001;
          WCY3 BY Y3; Y3@0.001;
          
          WCY1 WITH Y@0;
          WCY1 WITH phi@0;
          WCY1 WITH RV@0;

          
          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR PHI
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCPHI1 BY phi1; Y1@0.001;
          WCPHI2 BY phi2; Y2@0.001;
          WCPHI3 BY phi3; Y3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH Y@0;
          WCPHI1 WITH RV@0;
          
          WCPHI1 WITH WCY1@0;
          WCPHI3 WITH WCY3@0;
          
          !For Rv
          WCRV1 BY rv1; rv1@0.001;
          WCRV2 BY rv2; rv2@0.001;
          WCRV3 BY rv3; rv3@0.001;
          
          WCRV1 WITH phi@0;
          WCRV1 WITH Y@0;
          WCRV1 WITH RV@0;
          
          !Removing mplus defaults 
          WCY1 WITH WCRV1@0;
          WCY1 WITH WCPHI1@0;
          
          WCY3 WITH WCRV3@0;
          WCY3 WITH WCPHI3@0;
          
          WCPHI1 WITH WCRV1@0;
          WCPHI3 WITH WCRV3@0;
          
          

          !Model autoregressive effects
          WCY2 WCY3 PON WCY1 WCY2 (a1);
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2);
          WCRV2 WCRV3 PON WCRV1 WCRV2 (a3);

                             ",
                      OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                      PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)

m3b_cheer<- mplusObject(TITLE = "M3b: AR for Cheerfulness icl RV",
                      rdata = df_cheer,
                      usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                      VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 30;",
                      MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;
          
          rv1 | Y1;
          rv2 | Y2;
          rv3 | Y3;

          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR Y (MEANS) AND PHI
          Y BY Y1-Y3@1;
          RV BY rv1-rv3@1;
          phi BY phi1-phi3@1;
          Y WITH phi RV;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR Y
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCY1 BY Y1; Y1@0.001;
          WCY2 BY Y2; Y2@0.001;
          WCY3 BY Y3; Y3@0.001;
          
          WCY1 WITH Y@0;
          WCY1 WITH phi@0;
          WCY1 WITH RV@0;

          
          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR PHI
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCPHI1 BY phi1; Y1@0.001;
          WCPHI2 BY phi2; Y2@0.001;
          WCPHI3 BY phi3; Y3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH Y@0;
          WCPHI1 WITH RV@0;
          
          WCPHI1 WITH WCY1@0;
          WCPHI3 WITH WCY3@0;
          
          !For Rv
          WCRV1 BY rv1; rv1@0.001;
          WCRV2 BY rv2; rv2@0.001;
          WCRV3 BY rv3; rv3@0.001;
          
          WCRV1 WITH phi@0;
          WCRV1 WITH Y@0;
          WCRV1 WITH RV@0;
          
          !Removing mplus defaults 
          WCY1 WITH WCRV1@0;
          WCY1 WITH WCPHI1@0;
          
          WCY3 WITH WCRV3@0;
          WCY3 WITH WCPHI3@0;
          
          WCPHI1 WITH WCRV1@0;
          WCPHI3 WITH WCRV3@0;
          
          

          !Model autoregressive effects
          WCY2 WCY3 PON WCY1 WCY2 (a1);
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2);
          WCRV2 WCRV3 PON WCRV1 WCRV2 (a3);

                             ",
                      OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                      PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)

m3b_conf<-MplusAutomation::mplusModeler(
  m3b_conf,
  check = FALSE,
  modelout = "m3b_conf.inp",
  hashfilename = FALSE,
  run = 1L)



m3a_dep<-MplusAutomation::mplusModeler(
  m3a_dep,
  check = FALSE,
  modelout = "m3a_dep.inp",
  hashfilename = FALSE,
  run = 1L)

m3b_dep<-MplusAutomation::mplusModeler(
  m3b_dep,
  check = FALSE,
  modelout = "m3b_dep.inp",
  hashfilename = FALSE,
  run = 1L)

# Model 4: RI-Cross-lagged Burst Model #####
#correct section
#M4a
##depression
m4a_dep<- mplusObject(TITLE = "M 4a: Cross-lagged model for Depression",
                      rdata = df_dep_res,
                      usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                      VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 10;",
                      MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          Y1 Y2 WITH Y3@0;
          Y1 WITH Y2@0;

          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR Y (MEANS) AND PHI
          Y BY Y1-Y3@1;
          phi BY phi1-phi3@1;
          Y WITH phi;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR Y
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCY1 BY Y1; Y1@0.001;
          WCY2 BY Y2; Y2@0.001;
          WCY3 BY Y3; Y3@0.001;
          
          WCY1 WITH Y@0;
          WCY1 WITH phi@0;

    
          
          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR PHI
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCPHI1 BY phi1; phi1@0.001;
          WCPHI2 BY phi2; phi2@0.001;
          WCPHI3 BY phi3; phi3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH Y@0;
          
          WCPHI1 WITH WCY1@0;
          WCPHI3 WITH WCY3@0;


      ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS WITH AN AR(1)
          WCY2 WCY3 PON WCY1 WCY2 (a1);
          ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS OF PHI WITH AN AR(1) across bursts
          
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2); 
          
          !Cross-lagged effects
           WCPHI2 WCPHI3 PON WCY1 WCY2 (b1);
           WCY2 WCY3 PON WCPHI1 WCPHI2 (b2);
                             ",
                      OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                      PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)






## confidence
m4a_conf<- mplusObject(TITLE = "M4a: CL for Confidence",
                               rdata = df_conf_hap_res,
                       usevariables = c("ID" ,"hour", "C1","C2","C3","H1","H2","H3"),
                       VARIABLE = "
                         CLUSTER = ID;
USEVAR = C1 C2 
C3 ;

LAGGED = C1(1) C2(1) 
C3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 30;",
                               MODEL ="%WITHIN%
          phi1 | C1 ON C1&1;
          phi2 | C2 ON C2&1;
          phi3 | C3 ON C3&1;

          C1 C2 WITH C3@0;
          C1 WITH C2@0;

          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR C (MEANS) AND PHI
          C BY C1-C3@1;
          phi BY phi1-phi3@1;
          C WITH phi;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR C
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCC1 BY C1; C1@0.001;
          WCC2 BY C2; C2@0.001;
          WCC3 BY C3; C3@0.001;
          
          WCC1 WITH C@0;
          WCC1 WITH phi@0;

          
          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR PHI
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCPHI1 BY phi1; C1@0.001;
          WCPHI2 BY phi2; C2@0.001;
          WCPHI3 BY phi3; C3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH C@0;
          
          WCPHI1 WITH WCC1@0;
          WCPHI3 WITH WCC3@0;

          !AR effects
          ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS OF C
          WCC2 WCC3 PON WCC1 WCC2 (a1);
          
          ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS OF PHI 
          
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2); 
          
          !Cross-lagged effects
           WCPHI2 WCPHI3 PON WCC1 WCC2 (b1);
           WCC2 WCC3 PON WCPHI1 WCPHI2 (b2);
                             ",
                               OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                               PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)

m4a_stress<- mplusObject(TITLE = "M4a: Cross-lagged model for Stress",
                      rdata = df_stress,
                      usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                      VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 10;",
                      MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          Y1 Y2 WITH Y3@0;
          Y1 WITH Y2@0;

          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR Y (MEANS) AND PHI
          Y BY Y1-Y3@1;
          phi BY phi1-phi3@1;
          Y WITH phi;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR Y
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCY1 BY Y1; Y1@0.001;
          WCY2 BY Y2; Y2@0.001;
          WCY3 BY Y3; Y3@0.001;
          
          WCY1 WITH Y@0;
          WCY1 WITH phi@0;

    
          
          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR PHI
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCPHI1 BY phi1; phi1@0.001;
          WCPHI2 BY phi2; phi2@0.001;
          WCPHI3 BY phi3; phi3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH Y@0;
          
          WCPHI1 WITH WCY1@0;
          WCPHI3 WITH WCY3@0;


      ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS WITH AN AR(1)
          WCY2 WCY3 PON WCY1 WCY2 (a1);
          ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS OF PHI WITH AN AR(1) across bursts
          
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2); 
          
          !Cross-lagged effects
           WCPHI2 WCPHI3 PON WCY1 WCY2 (b1);
           WCY2 WCY3 PON WCPHI1 WCPHI2 (b2);
                             ",
                      OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                      PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)
m4a_anger<- mplusObject(TITLE = "M4a: Cross-lagged model for Anger",
                      rdata = df_anger,
                      usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                      VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 10;",
                      MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          Y1 Y2 WITH Y3@0;
          Y1 WITH Y2@0;

          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR Y (MEANS) AND PHI
          Y BY Y1-Y3@1;
          phi BY phi1-phi3@1;
          Y WITH phi;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR Y
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCY1 BY Y1; Y1@0.001;
          WCY2 BY Y2; Y2@0.001;
          WCY3 BY Y3; Y3@0.001;
          
          WCY1 WITH Y@0;
          WCY1 WITH phi@0;

    
          
          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR PHI
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCPHI1 BY phi1; phi1@0.001;
          WCPHI2 BY phi2; phi2@0.001;
          WCPHI3 BY phi3; phi3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH Y@0;
          
          WCPHI1 WITH WCY1@0;
          WCPHI3 WITH WCY3@0;


      ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS WITH AN AR(1)
          WCY2 WCY3 PON WCY1 WCY2 (a1);
          ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS OF PHI WITH AN AR(1) across bursts
          
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2); 
          
          !Cross-lagged effects
           WCPHI2 WCPHI3 PON WCY1 WCY2 (b1);
           WCY2 WCY3 PON WCPHI1 WCPHI2 (b2);
                             ",
                      OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                      PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)
m4a_cheer<- mplusObject(TITLE = "M4a: Cross-lagged model for Cheerfulness",
                      rdata = df_cheer,
                      usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                      VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 10;",
                      MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;

          Y1 Y2 WITH Y3@0;
          Y1 WITH Y2@0;

          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR Y (MEANS) AND PHI
          Y BY Y1-Y3@1;
          phi BY phi1-phi3@1;
          Y WITH phi;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR Y
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCY1 BY Y1; Y1@0.001;
          WCY2 BY Y2; Y2@0.001;
          WCY3 BY Y3; Y3@0.001;
          
          WCY1 WITH Y@0;
          WCY1 WITH phi@0;

    
          
          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR PHI
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCPHI1 BY phi1; phi1@0.001;
          WCPHI2 BY phi2; phi2@0.001;
          WCPHI3 BY phi3; phi3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH Y@0;
          
          WCPHI1 WITH WCY1@0;
          WCPHI3 WITH WCY3@0;


      ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS WITH AN AR(1)
          WCY2 WCY3 PON WCY1 WCY2 (a1);
          ! MODEL WITHIN-PERSON WAVE-SPECIFIC COMPONENTS OF PHI WITH AN AR(1) across bursts
          
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2); 
          
          !Cross-lagged effects
           WCPHI2 WCPHI3 PON WCY1 WCY2 (b1);
           WCY2 WCY3 PON WCPHI1 WCPHI2 (b2);
                             ",
                      OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                      PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)

m4a_dep<-MplusAutomation::mplusModeler(
  m4a_dep,
  check = FALSE,
  modelout = "m4a_dep.inp",
  hashfilename = FALSE,
  run = 1L)

m4a_conf<-MplusAutomation::mplusModeler(
  m4a_conf,
  check = FALSE,
  modelout = "m4a_conf.inp",
  hashfilename = FALSE,
  run = 1L)

m4a_stress<-MplusAutomation::mplusModeler(
  m4a_stress,
  check = FALSE,
  modelout = "m4a_stress.inp",
  hashfilename = FALSE,
  run = 1L)

m4a_anger<-MplusAutomation::mplusModeler(
  m4a_anger,
  check = FALSE,
  modelout = "m4a_anger.inp",
  hashfilename = FALSE,
  run = 1L)

m4a_cheer<-MplusAutomation::mplusModeler(
  m4a_cheer,
  check = FALSE,
  modelout = "m4a_cheer.inp",
  hashfilename = FALSE,
  run = 1L)

#4b

m4b_depression<- mplusObject(TITLE = "Model 4b Depression: Cross-lags including residual variance",
            rdata = df_dep_res,
            usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
            VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 30;",
            MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;
          
          rv1 | Y1;
          rv2 | Y2;
          rv3 | Y3;

          
          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR Y (MEANS), PHI & RV
          Y BY Y1-Y3@1;
          phi BY phi1-phi3@1;
          RV BY rv1-rv3@1;
          Y WITH phi RV;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR Y
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          
          !For Y
          WCY1 BY Y1; Y1@0.001;
          WCY2 BY Y2; Y2@0.001;
          WCY3 BY Y3; Y3@0.001;
          
          WCY1 WITH Y@0;
          WCY1 WITH phi@0;
          WCY1 WITH RV@0;
          
          !For Phi
          WCPHI1 BY phi1; phi1@0.001;
          WCPHI2 BY phi2; phi2@0.001;
          WCPHI3 BY phi3; phi3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH Y@0;
          WCPHI1 WITH RV@0;
          
          !For RV
          WCRV1 BY rv1; rv1@0.001;
          WCRV2 BY rv2; rv2@0.001;
          WCRV3 BY rv3; rv3@0.001;
          
          WCRV1 WITH phi@0;
          WCRV1 WITH Y@0;
          WCRV1 WITH RV@0;
          
  
          ! removing mplus defaults for covar of WC components
          WCY1 WITH WCRV1@0;
          WCY1 WITH WCPHI1@0;
          
          WCY3 WITH WCRV3@0;
          WCY3 WITH WCPHI3@0;
          
          WCPHI1 WITH WCRV1@0;
          WCPHI3 WITH WCRV3@0;
          

          ! MODEL Burst-Specific components across bursts using AR effects
          WCY2 WCY3 PON WCY1 WCY2 (a1);
          
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2);  
          
          WCRV2 WCRV3 PON WCRV1 WCRV2 (a3);
          
          !cross lagged effects
          WCY2 WCY3 PON WCPHI1 WCPHI2 (b1);
          WCY2 WCY3 PON WCRV1 WCRV2 (b2);
          
          WCPHI2 WCPHI3 PON WCY1 WCY2 (b3);
          WCPHI2 WCPHI3 PON WCRV1 WCRV2 (b4);
          
          WCRV2 WCRV3 PON WCY1 WCY2 (b5);
          WCRV2 WCRV3 PON WCPHI1 WCPHI2 (b6);
          
                       ",
            OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
            PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)


m4b_conf<- mplusObject(TITLE = "M4b: Cross-lags for Confidence icl RV",
                       rdata = df_conf_hap_res,
                       usevariables = c("ID" ,"hour", "C1","C2","C3","H1","H2","H3"),
                       VARIABLE = "
                         CLUSTER = ID;
USEVAR = C1 C2 
C3 ;

LAGGED = C1(1) C2(1) 
C3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 30;",
                       MODEL ="%WITHIN%
          phi1 | C1 ON C1&1;
          phi2 | C2 ON C2&1;
          phi3 | C3 ON C3&1;
          
          rv1 | C1;
          rv2 | C2;
          rv3 | C3;

          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR C (MEANS) AND PHI
          C BY C1-C3@1;
          RV BY rv1-rv3@1;
          phi BY phi1-phi3@1;
          C WITH phi RV;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR C
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCC1 BY C1; C1@0.001;
          WCC2 BY C2; C2@0.001;
          WCC3 BY C3; C3@0.001;
          
          WCC1 WITH C@0;
          WCC1 WITH phi@0;
          WCC1 WITH RV@0;

          
          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR PHI
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          WCPHI1 BY phi1; C1@0.001;
          WCPHI2 BY phi2; C2@0.001;
          WCPHI3 BY phi3; C3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH C@0;
          WCPHI1 WITH RV@0;
          
          WCPHI1 WITH WCC1@0;
          WCPHI3 WITH WCC3@0;
          
          !For Rv
          WCRV1 BY rv1; rv1@0.001;
          WCRV2 BY rv2; rv2@0.001;
          WCRV3 BY rv3; rv3@0.001;
          
          WCRV1 WITH phi@0;
          WCRV1 WITH C@0;
          WCRV1 WITH RV@0;
          
          !Removing mplus defaults 
          WCC1 WITH WCRV1@0;
          WCC1 WITH WCPHI1@0;
          
          WCC3 WITH WCRV3@0;
          WCC3 WITH WCPHI3@0;
          
          WCPHI1 WITH WCRV1@0;
          WCPHI3 WITH WCRV3@0;
          
          
          

          !Model autoregressive effects
          WCC2 WCC3 PON WCC1 WCC2 (a1);
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2);
          WCRV2 WCRV3 PON WCRV1 WCRV2 (a3);
   
          
          ! Model cross-lagged effects
          WCC2 WCC3 PON WCPHI1 WCPHI2 (b1);
          WCC2 WCC3 PON WCRV1 WCRV2 (b2);
          
          WCPHI2 WCPHI3 PON WCC1 WCC2 (b3);
          WCPHI2 WCPHI3 PON WCRV1 WCRV2 (b4);
          
          WCRV2 WCRV3 PON WCC1 WCC2 (b5);
          WCRV2 WCRV3 PON WCPHI1 WCPHI2 (b6);
                             ",
                       OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                       PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)

m4b_stress<- mplusObject(TITLE = "Model 4b Stress: Cross-lags including residual variance",
                             rdata = df_stress,
                             usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                             VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 30;",
                             MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;
          
          rv1 | Y1;
          rv2 | Y2;
          rv3 | Y3;

          
          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR Y (MEANS), PHI & RV
          Y BY Y1-Y3@1;
          phi BY phi1-phi3@1;
          RV BY rv1-rv3@1;
          Y WITH phi RV;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR Y
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          
          !For Y
          WCY1 BY Y1; Y1@0.001;
          WCY2 BY Y2; Y2@0.001;
          WCY3 BY Y3; Y3@0.001;
          
          WCY1 WITH Y@0;
          WCY1 WITH phi@0;
          WCY1 WITH RV@0;
          
          !For Phi
          WCPHI1 BY phi1; phi1@0.001;
          WCPHI2 BY phi2; phi2@0.001;
          WCPHI3 BY phi3; phi3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH Y@0;
          WCPHI1 WITH RV@0;
          
          !For RV
          WCRV1 BY rv1; rv1@0.001;
          WCRV2 BY rv2; rv2@0.001;
          WCRV3 BY rv3; rv3@0.001;
          
          WCRV1 WITH phi@0;
          WCRV1 WITH Y@0;
          WCRV1 WITH RV@0;
          
  
          ! removing mplus defaults for covar of WC components
          WCY1 WITH WCRV1@0;
          WCY1 WITH WCPHI1@0;
          
          WCY3 WITH WCRV3@0;
          WCY3 WITH WCPHI3@0;
          
          WCPHI1 WITH WCRV1@0;
          WCPHI3 WITH WCRV3@0;
          

          ! MODEL Burst-Specific components across bursts using AR effects
          WCY2 WCY3 PON WCY1 WCY2 (a1);
          
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2);  
          
          WCRV2 WCRV3 PON WCRV1 WCRV2 (a3);
          
          !cross lagged effects
          WCY2 WCY3 PON WCPHI1 WCPHI2 (b1);
          WCY2 WCY3 PON WCRV1 WCRV2 (b2);
          
          WCPHI2 WCPHI3 PON WCY1 WCY2 (b3);
          WCPHI2 WCPHI3 PON WCRV1 WCRV2 (b4);
          
          WCRV2 WCRV3 PON WCY1 WCY2 (b5);
          WCRV2 WCRV3 PON WCPHI1 WCPHI2 (b6);
          
                       ",
                             OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                             PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)
m4b_anger<- mplusObject(TITLE = "Model 4b Anger: Cross-lags including residual variance",
                             rdata = df_anger,
                             usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                             VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 30;",
                             MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;
          
          rv1 | Y1;
          rv2 | Y2;
          rv3 | Y3;

          
          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR Y (MEANS), PHI & RV
          Y BY Y1-Y3@1;
          phi BY phi1-phi3@1;
          RV BY rv1-rv3@1;
          Y WITH phi RV;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR Y
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          
          !For Y
          WCY1 BY Y1; Y1@0.001;
          WCY2 BY Y2; Y2@0.001;
          WCY3 BY Y3; Y3@0.001;
          
          WCY1 WITH Y@0;
          WCY1 WITH phi@0;
          WCY1 WITH RV@0;
          
          !For Phi
          WCPHI1 BY phi1; phi1@0.001;
          WCPHI2 BY phi2; phi2@0.001;
          WCPHI3 BY phi3; phi3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH Y@0;
          WCPHI1 WITH RV@0;
          
          !For RV
          WCRV1 BY rv1; rv1@0.001;
          WCRV2 BY rv2; rv2@0.001;
          WCRV3 BY rv3; rv3@0.001;
          
          WCRV1 WITH phi@0;
          WCRV1 WITH Y@0;
          WCRV1 WITH RV@0;
          
  
          ! removing mplus defaults for covar of WC components
          WCY1 WITH WCRV1@0;
          WCY1 WITH WCPHI1@0;
          
          WCY3 WITH WCRV3@0;
          WCY3 WITH WCPHI3@0;
          
          WCPHI1 WITH WCRV1@0;
          WCPHI3 WITH WCRV3@0;
          

          ! MODEL Burst-Specific components across bursts using AR effects
          WCY2 WCY3 PON WCY1 WCY2 (a1);
          
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2);  
          
          WCRV2 WCRV3 PON WCRV1 WCRV2 (a3);
          
          !cross lagged effects
          WCY2 WCY3 PON WCPHI1 WCPHI2 (b1);
          WCY2 WCY3 PON WCRV1 WCRV2 (b2);
          
          WCPHI2 WCPHI3 PON WCY1 WCY2 (b3);
          WCPHI2 WCPHI3 PON WCRV1 WCRV2 (b4);
          
          WCRV2 WCRV3 PON WCY1 WCY2 (b5);
          WCRV2 WCRV3 PON WCPHI1 WCPHI2 (b6);
          
                       ",
                             OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                             PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)

m4b_cheer<- mplusObject(TITLE = "Model 4b Cheerfulness: Cross-lags including residual variance",
                             rdata = df_cheer,
                             usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                             VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 30;",
                             MODEL ="%WITHIN%
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;
          
          rv1 | Y1;
          rv2 | Y2;
          rv3 | Y3;

          
          %BETWEEN%

          ! CREATE RANDOM INTERCEPTS FOR Y (MEANS), PHI & RV
          Y BY Y1-Y3@1;
          phi BY phi1-phi3@1;
          RV BY rv1-rv3@1;
          Y WITH phi RV;

          ! CREATE WITHIN-PERSON WAVE SPECIFIC COMPONENTS FOR Y
          ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO
          
          !For Y
          WCY1 BY Y1; Y1@0.001;
          WCY2 BY Y2; Y2@0.001;
          WCY3 BY Y3; Y3@0.001;
          
          WCY1 WITH Y@0;
          WCY1 WITH phi@0;
          WCY1 WITH RV@0;
          
          !For Phi
          WCPHI1 BY phi1; phi1@0.001;
          WCPHI2 BY phi2; phi2@0.001;
          WCPHI3 BY phi3; phi3@0.001;
          
          WCPHI1 WITH phi@0;
          WCPHI1 WITH Y@0;
          WCPHI1 WITH RV@0;
          
          !For RV
          WCRV1 BY rv1; rv1@0.001;
          WCRV2 BY rv2; rv2@0.001;
          WCRV3 BY rv3; rv3@0.001;
          
          WCRV1 WITH phi@0;
          WCRV1 WITH Y@0;
          WCRV1 WITH RV@0;
          
  
          ! removing mplus defaults for covar of WC components
          WCY1 WITH WCRV1@0;
          WCY1 WITH WCPHI1@0;
          
          WCY3 WITH WCRV3@0;
          WCY3 WITH WCPHI3@0;
          
          WCPHI1 WITH WCRV1@0;
          WCPHI3 WITH WCRV3@0;
          

          ! MODEL Burst-Specific components across bursts using AR effects
          WCY2 WCY3 PON WCY1 WCY2 (a1);
          
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2);  
          
          WCRV2 WCRV3 PON WCRV1 WCRV2 (a3);
          
          !cross lagged effects
          WCY2 WCY3 PON WCPHI1 WCPHI2 (b1);
          WCY2 WCY3 PON WCRV1 WCRV2 (b2);
          
          WCPHI2 WCPHI3 PON WCY1 WCY2 (b3);
          WCPHI2 WCPHI3 PON WCRV1 WCRV2 (b4);
          
          WCRV2 WCRV3 PON WCY1 WCY2 (b5);
          WCRV2 WCRV3 PON WCPHI1 WCPHI2 (b6);
          
                       ",
                             OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                             PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)

m4b_confidence<-MplusAutomation::mplusModeler(
  m4b_conf,
  check = FALSE,
  modelout = "m4b_confidence.inp",
  hashfilename = FALSE,
  run = 1L)
m4b_depression<-MplusAutomation::mplusModeler(
  m4b_depression,
  check = FALSE,
  modelout = "m4b_depression.inp",
  hashfilename = FALSE,
  run = 1L)

##### models 3b and 4b for stress, anger and cheer

m4b_stress<-MplusAutomation::mplusModeler(
  m4b_stress,
  check = FALSE,
  modelout = "m4b_stress.inp",
  hashfilename = FALSE,
  run = 1L)
m4b_anger<-MplusAutomation::mplusModeler(
  m4b_anger,
  check = FALSE,
  modelout = "m4b_anger.inp",
  hashfilename = FALSE,
  run = 1L)
m4b_cheer<-MplusAutomation::mplusModeler(
  m4b_cheer,
  check = FALSE,
  modelout = "m4b_cheer.inp",
  hashfilename = FALSE,
  run = 1L)

m3b_stress<-MplusAutomation::mplusModeler(
  m3b_stress,
  check = FALSE,
  modelout = "m3b_stress.inp",
  hashfilename = FALSE,
  run = 1L)
m3b_anger<-MplusAutomation::mplusModeler(
  m3b_anger,
  check = FALSE,
  modelout = "m3b_anger.inp",
  hashfilename = FALSE,
  run = 1L)
m3b_cheer<-MplusAutomation::mplusModeler(
  m3b_cheer,
  check = FALSE,
  modelout = "m3b_cheer.inp",
  hashfilename = FALSE,
  run = 1L)
# Model 4-2 with correlated residuals 

# For models 4a2 see Mplus desktop in and output!
# For models 4b2 anger and cheerfulness see Mplus desktop in and output!

m4b2_conf<- mplusObject(TITLE = "M4b2: Cross-lags for Confidence icl RV",
                       rdata = df_conf_hap_res,
                       usevariables = c("ID" ,"hour", "C1","C2","C3","H1","H2","H3"),
                       VARIABLE = "
                         CLUSTER = ID;
USEVAR = C1 C2 
C3 ;

LAGGED = C1(1) C2(1) 
C3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 30;",
                       MODEL ="%WITHIN%
        
        ! PER WAVE: SPECIFY AR(1) PROCESS WITH A RANDOM AUTOREGRESSION
          phi1 | C1 ON C1&1;
          phi2 | C2 ON C2&1;
          phi3 | C3 ON C3&1;
          
	    ! PER WAVE: SPECIFY RANDOM RESIDUAL VARIANCES (WILL BE LOG RESIDUAL VARIANCES AT BETWEEN LEVEL)
          rv1 | C1;
          rv2 | C2;
          rv3 | C3;

          
        %BETWEEN%

        ! CREATE RANDOM INTERCEPTS FOR C (MEANS), PHI & RV
          C BY C1-C3@1;
          phi BY phi1-phi3@1;
          RV BY rv1-rv3@1;

	    ! ESTIMATING THE COVARIANCES OF RIs
          C WITH phi RV;


        ! CREATE WITHIN-PERSON BURST-SPECIFIC COMPONENTS FOR C
        ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO (NO MEASUREMENT ERROR HERE)
          
        !FOR C
          WCC1 BY C1; C1@0.001;
          WCC2 BY C2; C2@0.001;
          WCC3 BY C3; C3@0.001;
       
        !SAME FOR Phi
          WCPHI1 BY phi1; phi1@0.001;
          WCPHI2 BY phi2; phi2@0.001;
          WCPHI3 BY phi3; phi3@0.001;
                  
        ! SAME FOR RV
          WCRV1 BY rv1; rv1@0.001;
          WCRV2 BY rv2; rv2@0.001;
          WCRV3 BY rv3; rv3@0.001;
   
        ! SET COVARIANCE OF WCs WITH RIs TO ZERO
          WCC1 WITH C@0;
          WCC1 WITH phi@0;
          WCC1 WITH RV@0;

          WCPHI1 WITH phi@0;
          WCPHI1 WITH C@0;
          WCPHI1 WITH RV@0;
          
          WCRV1 WITH phi@0;
          WCRV1 WITH C@0;
          WCRV1 WITH RV@0;
          

	    ! ESTIMATE COVARIANCES OF THE WITHIN COMPONENTS AT THE FIRST BURST 
          WCC1 WITH WCRV1;
          WCC1 WITH WCPHI1;
          WCPHI1 WITH WCRV1;
           
	    ! SPECIFY THE AUTOREGRESSIONS FOR THE WCs AND CONSTRAIN THESE TO BE INVARIANT OVER TIME
          WCC2 WCC3 PON WCC1 WCC2 (a1);
  
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2);  
          
          WCRV2 WCRV3 PON WCRV1 WCRV2 (a3);
          
        ! SPECIFY THE CROSS-LAGGED EFFECTS OF WCs AND CONSTRAIN THESE TO BE INVARIANT OVER WAVES
          WCC2 WCC3 PON WCPHI1 WCPHI2 (b1);
          WCC2 WCC3 PON WCRV1 WCRV2 (b2);
          
          WCPHI2 WCPHI3 PON WCC1 WCC2 (b3);
          WCPHI2 WCPHI3 PON WCRV1 WCRV2 (b4);
          
          WCRV2 WCRV3 PON WCC1 WCC2 (b5);
          WCRV2 WCRV3 PON WCPHI1 WCPHI2 (b6);

        ! ESTIMATE COVARIANCES OF THE RESIDUALS OF THE WITHIN COMPONENTS AT 2ND AND 3RD WAVE
        ! CONSTRAIN THESE TO BE INVARIANT OVER WAVES  
          WCC2 WITH WCRV2 (rc1);
	      WCC3 WITH WCRV3 (rc1);

          WCC2 WITH WCPHI2 (rc2);
  	      WCC3 WITH WCPHI3 (rc2);

          WCPHI2 WITH WCRV2 (rc3);
          WCPHI3 WITH WCRV3 (rc3);
          
        ! CONSTRAIN RESIDUAL VARIANCES OF THE WCs TO BE INVARIANT OVER WAVES
          WCC2 WCC3 (rv1);
          WCPHI2 WCPHI3 (rv2);
          WCRV2 WCRV3 (rv3);
                             ",
                       OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                       PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)

m4b2_stress<- mplusObject(TITLE = "Model 4b2 Stress: Cross-lags including residual variance",
                         rdata = df_stress,
                         usevariables = c("ID" ,"hour", "Y1","Y2","Y3"),
                         VARIABLE = "
                         CLUSTER = ID;
USEVAR = Y1 Y2 
Y3 ;

LAGGED = Y1(1) Y2(1) 
Y3(1);

TINTERVAL = hour(1);  
MISSING = ALL(999);", ANALYSIS = "
          TYPE = TWOLEVEL RANDOM;
          ESTIMATOR = BAYES;
          FBITERATIONS = (25000);
          THIN = 10;
          CHAINS=2;
          PROCESSORS = 30;",
                         MODEL ="%WITHIN%
        
        ! PER WAVE: SPECIFY AR(1) PROCESS WITH A RANDOM AUTOREGRESSION
          phi1 | Y1 ON Y1&1;
          phi2 | Y2 ON Y2&1;
          phi3 | Y3 ON Y3&1;
          
	    ! PER WAVE: SPECIFY RANDOM RESIDUAL VARIANCES (WILL BE LOG RESIDUAL VARIANCES AT BETWEEN LEVEL)
          rv1 | Y1;
          rv2 | Y2;
          rv3 | Y3;

          
        %BETWEEN%

        ! CREATE RANDOM INTERCEPTS FOR Y (MEANS), PHI & RV
          Y BY Y1-Y3@1;
          phi BY phi1-phi3@1;
          RV BY rv1-rv3@1;

	    ! ESTIMATING THE COVARIANCES OF RIs
          Y WITH phi RV;


        ! CREATE WITHIN-PERSON BURST-SPECIFIC COMPONENTS FOR Y
        ! SET THE RESIDUAL VARIANCES TO (ALMOST) ZERO (NO MEASUREMENT ERROR HERE)
          
        !FOR Y
          WCY1 BY Y1; Y1@0.001;
          WCY2 BY Y2; Y2@0.001;
          WCY3 BY Y3; Y3@0.001;
       
        !SAME FOR Phi
          WCPHI1 BY phi1; phi1@0.001;
          WCPHI2 BY phi2; phi2@0.001;
          WCPHI3 BY phi3; phi3@0.001;
                  
        ! SAME FOR RV
          WCRV1 BY rv1; rv1@0.001;
          WCRV2 BY rv2; rv2@0.001;
          WCRV3 BY rv3; rv3@0.001;
   
        ! SET COVARIANCE OF WCs WITH RIs TO ZERO
          WCY1 WITH Y@0;
          WCY1 WITH phi@0;
          WCY1 WITH RV@0;

          WCPHI1 WITH phi@0;
          WCPHI1 WITH Y@0;
          WCPHI1 WITH RV@0;
          
          WCRV1 WITH phi@0;
          WCRV1 WITH Y@0;
          WCRV1 WITH RV@0;
          

	    ! ESTIMATE COVARIANCES OF THE WITHIN COMPONENTS AT THE FIRST BURST 
          WCY1 WITH WCRV1;
          WCY1 WITH WCPHI1;
          WCPHI1 WITH WCRV1;
           
	    ! SPECIFY THE AUTOREGRESSIONS FOR THE WCs AND CONSTRAIN THESE TO BE INVARIANT OVER TIME
          WCY2 WCY3 PON WCY1 WCY2 (a1);
  
          WCPHI2 WCPHI3 PON WCPHI1 WCPHI2 (a2);  
          
          WCRV2 WCRV3 PON WCRV1 WCRV2 (a3);
          
        ! SPECIFY THE CROSS-LAGGED EFFECTS OF WCs AND CONSTRAIN THESE TO BE INVARIANT OVER WAVES
          WCY2 WCY3 PON WCPHI1 WCPHI2 (b1);
          WCY2 WCY3 PON WCRV1 WCRV2 (b2);
          
          WCPHI2 WCPHI3 PON WCY1 WCY2 (b3);
          WCPHI2 WCPHI3 PON WCRV1 WCRV2 (b4);
          
          WCRV2 WCRV3 PON WCY1 WCY2 (b5);
          WCRV2 WCRV3 PON WCPHI1 WCPHI2 (b6);

        ! ESTIMATE COVARIANCES OF THE RESIDUALS OF THE WITHIN COMPONENTS AT 2ND AND 3RD WAVE
        ! CONSTRAIN THESE TO BE INVARIANT OVER WAVES  
          WCY2 WITH WCRV2 (rc1);
	      WCY3 WITH WCRV3 (rc1);

          WCY2 WITH WCPHI2 (rc2);
  	      WCY3 WITH WCPHI3 (rc2);

          WCPHI2 WITH WCRV2 (rc3);
          WCPHI3 WITH WCRV3 (rc3);
          
        ! CONSTRAIN RESIDUAL VARIANCES OF THE WCs TO BE INVARIANT OVER WAVES
          WCY2 WCY3 (rv1);
          WCPHI2 WCPHI3 (rv2);
          WCRV2 WCRV3 (rv3);
          
                       ",
                         OUTPUT = "TECH1 TECH8 STDYX FSCOMPARISON;",
                         PLOT = "TYPE = PLOT3;
          FACTOR =ALL;"
)

m4b2_conf<-MplusAutomation::mplusModeler(
  m4b2_conf,
  check = FALSE,
  modelout = "m4b2_conf.inp",
  hashfilename = FALSE,
  run = 1L)

m4b2_stress<-MplusAutomation::mplusModeler(
  m4b2_stress,
  check = FALSE,
  modelout = "m4b2_stress.inp",
  hashfilename = FALSE,
  run = 1L)



