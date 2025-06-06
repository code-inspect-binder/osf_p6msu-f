################################################################################################################################################
# Joset A. Etzel, jetzel@wustl.edu. 
# updated 19 April 2019; adapted from "D:\svnFiles\HCP\byFamilyGroup\YaelSurface\fromHCP\WMbehavior_extract.R"
# WM HCP data analyses: extracting behavioral performance statistics from the WM task fMRI eprime output; HCP 1200-subjects release.
# code to create the files such as \output_behavioral\ei5289_WMbehaviorSummary_bothruns.txt

# The correspondence between "jo.ids" and HCP ids is restricted. Contact the authors (Jo Etzel) if access is needed.
# Note: Only Face and Place trials included; both 0-back and 2-back included.
# Note: this was run at Washington University in St. Louis, where access to the released HCP files is at a disk (/data/hcp-zfs/OpenAccess/); 
# the paths will need to be updated for other filesystems.
################################################################################################################################################
# create a file summarizing each person's WM task behavioral performance; store by "jo.id", not HCP id. 

rm(list=ls());

in.path <- "/scratch1/JoEtzel/fromHCP/inputs/";   # id key
out.path <- "/scratch1/JoEtzel/fromHCP/output_behavioral/";   # files written out to here
hcp.path <- "/data/hcp-zfs/OpenAccess/1200subject/";  # first part of path to hcp files; same for everyone
mid.path <- "/MNINonLinear/Results/";  # middle part of the hcp file path; same for everyone

id.key <- read.table(paste0(in.path, "subIDkey.txt"));  # restricted file giving the correspondence between hcp.ids and jo.ids 
sub.ids <- id.key$jo.ids;     # get all of the jo.ids version of the HCP ids.


# notes about the input files ... text-ized eprime output.
# lots and lots of columns ... here's the ones I think I need, and what it looks like they are
# $SessionSelectionList = 1:21, giving each row a number, and each task block a unique number
# $Procedure.Block. = TrialsPROC TRSyncPROC Cue2BackPROC Fix15secPROC, etc. The n-back trial rows are marked by TrialsPROC
# $TargetType = nonlure, lure, target
# $CorrectResponse = 2 3  which response button should be pushed
# $StimType = Body Face Place Tools
# $BlockType = 0-Back 2-Back
# $Stim.ACC = 1 0 = correct response (or not) on this trial
# $Stim.RESP = 2 3 or NA  which response button was pushed
# $Stim.RT = RT on this trial (looks like msec); NA trials have a zero

# this lists all the rows, but just the relevant columns
#tbl1[,c("SessionSelectionList", "Procedure.Block.", "TargetType", "CorrectResponse", "StimType", "BlockType", "Stim.ACC", "Stim.RESP", "Stim.RT")]

cat.names <- c("Face", "Place", "Body", "Tools");
load.names <- c("0-Back", "2-Back");
trial.names <- c("target", "lure", "nonlure");
run.ids <- c("tfMRI_WM_LR", "tfMRI_WM_RL");  


# that the input files are not perfect ... sometimes the person running the participant entered an extra character in the ID field,
# etc. The code in this get.data function tries to make sure that the input data is sensible, but special-case code is needed for for some people.
# wrong hcp.id in file: 81 (133982 instead of 133928 in both), 411 (790551 instead of 709551 in both), 207 (183043 instead of 183034 in both)
# wrong hcp.id in file: 670568 instead of 679568 in run 2.


# function to return the dprime & accuracy-related statistics. 
# Note: d' (d prime) calculated following the suggestions in Hautus1995.pdf, which reduces the bias from having unequal numbers of
# target and nontarget trials, as well as undefined stats from 0s in the contingency table. 
# definition of false alarm, etc from Pallier2002.pdf:
# hits: correct response to target trial  CR (correct rejection): correct response to non-target trial.
calc.dprime <- function(hit.ct, miss.ct, FA.ct, CR.ct) {
  dp <- NA;
  hit.rate <- (hit.ct + 0.5)/(hit.ct + miss.ct + 1);  # +0.5 for log-linear d' correction (Hautus1995)
  FA.rate <- (FA.ct + 0.5)/(FA.ct + CR.ct + 1);
  dp <- qnorm(hit.rate) - qnorm(FA.rate);   # calculate the d', with the log-linear rule correction
  
  return(dp);
}


get.data <- function(do.sub) {   # do.sub <- id.key$hcp.ids[3]
  # read in the eprime log table for each run. The numbers in the input file names are inconsistent, so list.files looks up the actual name.
  fname1 <- list.files(path=paste0(hcp.path, do.sub, mid.path, run.ids[1]), pattern="WM_run", full.names=TRUE); 
  fname2 <- list.files(path=paste0(hcp.path, do.sub, mid.path, run.ids[2]), pattern="WM_run", full.names=TRUE); 
  if (length(fname1) != 1) { stop(paste("missing:", fname1, "for", do.sub, run.ids[1])); }
  if (length(fname2) != 1) { stop(paste("missing:", fname2, "for", do.sub, run.ids[2])); }
  if (length(fname1) == 1 & length(fname2) == 1) {   # have both, so read in
    tbl1 <- read.delim(fname1, stringsAsFactors=FALSE);
    tbl2 <- read.delim(fname2, stringsAsFactors=FALSE);
    if (ncol(tbl1) != ncol(tbl2)) { stop("tbl1 & tbl2 column mismatch"); }
    tbl <- rbind(tbl1, tbl2);
    rm(tbl1, tbl2);   # clean up; delete the un-rbind-ed tables
  }
  
  # check the input files - right person, number of runs and blocks?
  # find the column that gives actual trial information. The naming varies a bit, so need to find the right spot.
  if (length(which(colnames(tbl) == "Procedure.Block.")) == 1) { use.col <- "Procedure.Block."; }
  if (length(which(colnames(tbl) == "Procedure[Block]")) == 1) { use.col <- "Procedure[Block]"; }
  if (is.na(use.col)) { stop("didn't fine the Procedure[Block]-ish columns"); }
  tbl <- subset(tbl, tbl[,use.col] == "TrialsPROC");    # just the trial rows
  
  # confirm expected blocks. should be 16 total, 8 from each run
  if (length(unique(tbl$SessionSelectionList)) != 16) { stop("length(block.ids) != 16"); }
  
  # confirm expected subject and runs
  tmp <- unique(tbl$HCPID);
  if (length(tmp) != 1) { stop("HCPID length(tmp) != 1"); }   # should always be only one HCPID in the combined input tbl
  if (do.sub != 965771) {   # don't check people with known errors in the IDs 
    if (as.numeric(strsplit(tmp, "_")[[1]][1]) != do.sub) { stop(paste("wrong HCPID in file:", do.sub)); }
  }
  if (length(unique(tbl$RunNumber)) != 2) { stop(paste("RunNumber length(tmp) != 2", do.sub)); }   # should be two RunNumbers, since rbind-ed the tables
  
  
  # make a blank output table to hold the derive stats, and fill it up
  out.tbl <- data.frame(array(NA, c(800, 5)));  # just make it big for now
  colnames(out.tbl) <- c("load.type", "cat.type", "trial.type", "stat.name", "stat.value");
  ctr <- 1;

  # proportion correct, all trials, NA trials as incorrect
  out.tbl$load.type[ctr] <- "all";  
  out.tbl$cat.type[ctr] <- "all";
  out.tbl$trial.type[ctr] <- "all";
  out.tbl$stat.name[ctr] <- "propcorrect";
  out.tbl$stat.value[ctr] <- mean(tbl$Stim.ACC);  # only have the trial rows in tbl, so can just average
  ctr <- ctr + 1;
  
  # proportion correct, all FACE and PLACE trials, NA trials as incorrect
  out.tbl$load.type[ctr] <- "all";  
  out.tbl$cat.type[ctr] <- "Face&Place";
  out.tbl$trial.type[ctr] <- "all";
  out.tbl$stat.name[ctr] <- "propcorrect";
  tmp <- tbl$Stim.ACC[which(tbl$StimType == "Face" | tbl$StimType == "Place")];
  out.tbl$stat.value[ctr] <- mean(tmp);  # only have the trial rows in tbl, so can just average
  tmp <- NA;   # blank, so won't accidentally carry forward
  ctr <- ctr + 1;
  
  # proportion correct, all FACE and PLACE trials by load, NA trials as incorrect
  for (j in 1:length(load.names)) { 
    out.tbl$load.type[ctr] <- load.names[j];  
    out.tbl$cat.type[ctr] <- "Face&Place";
    out.tbl$trial.type[ctr] <- "all";
    out.tbl$stat.name[ctr] <- "propcorrect";
    tmp <- tbl$Stim.ACC[which((tbl$StimType == "Face" | tbl$StimType == "Place") & tbl$BlockType == load.names[j])];
    out.tbl$stat.value[ctr] <- mean(tmp);  # only have the trial rows in tbl, so can just average
    tmp <- NA; 
    ctr <- ctr + 1;
  }
  
  # proportion correct, all FACE and PLACE trials by category, load, and type. NA trials as incorrect
  for (j in 1:length(load.names)) { 
    for (k in 1:length(trial.names)) {   
      out.tbl$load.type[ctr] <- load.names[j];  
      out.tbl$cat.type[ctr] <- "Face&Place";
      out.tbl$trial.type[ctr] <- trial.names[k];
      out.tbl$stat.name[ctr] <- "propcorrect";
      tmp <- tbl$Stim.ACC[which((tbl$StimType == "Face" | tbl$StimType == "Place") & tbl$BlockType == load.names[j] & tbl$TargetType == trial.names[k])];
      out.tbl$stat.value[ctr] <- mean(tmp);  # only have the trial rows in tbl, so can just average
      tmp <- NA; 
      ctr <- ctr + 1;
    }
    
    out.tbl$load.type[ctr] <- load.names[j];  
    out.tbl$cat.type[ctr] <- "Face&Place";
    out.tbl$trial.type[ctr] <- "nontarget";
    out.tbl$stat.name[ctr] <- "propcorrect";
    tmp <- tbl$Stim.ACC[which((tbl$StimType == "Face" | tbl$StimType == "Place") & tbl$BlockType == load.names[j] & tbl$TargetType != "target")];
    out.tbl$stat.value[ctr] <- mean(tmp);  # only have the trial rows in tbl, so can just average
    tmp <- NA; 
    ctr <- ctr + 1;
  }
  
  
  # proportion correct, trials by category, NA trials as incorrect
  for (i in 1:length(cat.names)) {  # i <- 1;
    out.tbl$load.type[ctr] <- "all";  
    out.tbl$cat.type[ctr] <- cat.names[i];
    out.tbl$trial.type[ctr] <- "all";
    out.tbl$stat.name[ctr] <- "propcorrect";
    tmp <- tbl$Stim.ACC[which(tbl$StimType == cat.names[i])];
    out.tbl$stat.value[ctr] <- mean(tmp);  # only have the trial rows in tbl, so can just average
    tmp <- NA; 
    ctr <- ctr + 1;
  }
  
  # proportion correct, trials by load, NA trials as incorrect
  for (i in 1:length(load.names)) {  # i <- 1;
    out.tbl$load.type[ctr] <- load.names[i];  
    out.tbl$cat.type[ctr] <- "all";
    out.tbl$trial.type[ctr] <- "all";
    out.tbl$stat.name[ctr] <- "propcorrect";
    tmp <- tbl$Stim.ACC[which(tbl$BlockType == load.names[i])];
    out.tbl$stat.value[ctr] <- mean(tmp);  # only have the trial rows in tbl, so can just average
    tmp <- NA; 
    ctr <- ctr + 1;
  }
  
  # proportion correct, trials by category and load, NA trials as incorrect
  for (i in 1:length(cat.names)) {  # i <- 1;
    for (j in 1:length(load.names)) { 
      out.tbl$load.type[ctr] <- load.names[j];  
      out.tbl$cat.type[ctr] <- cat.names[i];
      out.tbl$trial.type[ctr] <- "all";
      out.tbl$stat.name[ctr] <- "propcorrect";
      tmp <- tbl$Stim.ACC[which(tbl$StimType == cat.names[i] & tbl$BlockType == load.names[j])];
      out.tbl$stat.value[ctr] <- mean(tmp);  # only have the trial rows in tbl, so can just average
      tmp <- NA; 
      ctr <- ctr + 1;
    }
  }
  
  # proportion correct, trials by category, load, and type. NA trials as incorrect
  for (i in 1:length(cat.names)) {  # i <- 1;
    for (j in 1:length(load.names)) { 
      for (k in 1:length(trial.names)) {
        out.tbl$load.type[ctr] <- load.names[j];  
        out.tbl$cat.type[ctr] <- cat.names[i];
        out.tbl$trial.type[ctr] <- trial.names[k];
        out.tbl$stat.name[ctr] <- "propcorrect";
        tmp <- tbl$Stim.ACC[which(tbl$StimType == cat.names[i] & tbl$BlockType == load.names[j] & tbl$TargetType == trial.names[k])];
        out.tbl$stat.value[ctr] <- mean(tmp);  # only have the trial rows in tbl, so can just average
        tmp <- NA; 
        ctr <- ctr + 1;
      }
      
      out.tbl$load.type[ctr] <- load.names[j];  
      out.tbl$cat.type[ctr] <- cat.names[i];
      out.tbl$trial.type[ctr] <- "nontarget";
      out.tbl$stat.name[ctr] <- "propcorrect";
      tmp <- tbl$Stim.ACC[which(tbl$StimType == cat.names[i] & tbl$BlockType == load.names[j] & tbl$TargetType != "target")];
      out.tbl$stat.value[ctr] <- mean(tmp);  # only have the trial rows in tbl, so can just average
      tmp <- NA; 
      ctr <- ctr + 1;
    }
  }
  
  

  # d', all trials, NA trials as incorrect
  out.tbl$load.type[ctr] <- "all";  
  out.tbl$cat.type[ctr] <- "all";
  out.tbl$stat.name[ctr] <- "dprime";
  hit.ct <- length(which(tbl$TargetType == "target" & tbl$Stim.ACC == 1));
  CR.ct <- length(which(tbl$TargetType != "target" & tbl$Stim.ACC == 1));
  miss.ct <- length(which(tbl$TargetType == "target" & tbl$Stim.ACC == 0));
  FA.ct <- length(which(tbl$TargetType != "target" & tbl$Stim.ACC == 0));
  out.tbl$stat.value[ctr] <- calc.dprime(hit.ct, miss.ct, FA.ct, CR.ct);  # calculate the d', with the log-linear rule correction
  ctr <- ctr + 1;
  
  # d', all FACE and PLACE trials, NA trials as incorrect
  out.tbl$load.type[ctr] <- "all";  
  out.tbl$cat.type[ctr] <- "Face&Place";
  out.tbl$stat.name[ctr] <- "dprime";
  hit.ct <- length(which((tbl$StimType == "Face" | tbl$StimType == "Place") & tbl$TargetType == "target" & tbl$Stim.ACC == 1));
  CR.ct <- length(which((tbl$StimType == "Face" | tbl$StimType == "Place") & tbl$TargetType != "target" & tbl$Stim.ACC == 1));
  miss.ct <- length(which((tbl$StimType == "Face" | tbl$StimType == "Place") & tbl$TargetType == "target" & tbl$Stim.ACC == 0));
  FA.ct <- length(which((tbl$StimType == "Face" | tbl$StimType == "Place") & tbl$TargetType != "target" & tbl$Stim.ACC == 0));
  out.tbl$stat.value[ctr] <- calc.dprime(hit.ct, miss.ct, FA.ct, CR.ct);  # calculate the d', with the log-linear rule correction
  ctr <- ctr + 1;
  
  # d', FACE and PLACE trials by load, NA trials as incorrect
  for (j in 1:length(load.names)) { 
    out.tbl$load.type[ctr] <- load.names[j];  
    out.tbl$cat.type[ctr] <- "Face&Place";
    out.tbl$stat.name[ctr] <- "dprime";
    hit.ct <- length(which((tbl$StimType == "Face" | tbl$StimType == "Place") & tbl$BlockType == load.names[j] & tbl$TargetType == "target" & tbl$Stim.ACC == 1));
    CR.ct <- length(which((tbl$StimType == "Face" | tbl$StimType == "Place") & tbl$BlockType == load.names[j] & tbl$TargetType != "target" & tbl$Stim.ACC == 1));
    miss.ct <- length(which((tbl$StimType == "Face" | tbl$StimType == "Place") & tbl$BlockType == load.names[j] & tbl$TargetType == "target" & tbl$Stim.ACC == 0));
    FA.ct <- length(which((tbl$StimType == "Face" | tbl$StimType == "Place") & tbl$BlockType == load.names[j] & tbl$TargetType != "target" & tbl$Stim.ACC == 0));
    out.tbl$stat.value[ctr] <- calc.dprime(hit.ct, miss.ct, FA.ct, CR.ct);  # calculate the d', with the log-linear rule correction
    ctr <- ctr + 1;
  }
  
  # d', trials by load, NA trials as incorrect
  for (j in 1:length(load.names)) { 
    out.tbl$load.type[ctr] <- load.names[j];  
    out.tbl$cat.type[ctr] <- "all";
    out.tbl$stat.name[ctr] <- "dprime";
    hit.ct <- length(which(tbl$BlockType == load.names[j] & tbl$TargetType == "target" & tbl$Stim.ACC == 1));
    CR.ct <- length(which(tbl$BlockType == load.names[j] & tbl$TargetType != "target" & tbl$Stim.ACC == 1));
    miss.ct <- length(which(tbl$BlockType == load.names[j] & tbl$TargetType == "target" & tbl$Stim.ACC == 0));
    FA.ct <- length(which(tbl$BlockType == load.names[j] & tbl$TargetType != "target" & tbl$Stim.ACC == 0));
    out.tbl$stat.value[ctr] <- calc.dprime(hit.ct, miss.ct, FA.ct, CR.ct);  # calculate the d', with the log-linear rule correction
    ctr <- ctr + 1;
  }
  
  # d', trials by category, NA trials as incorrect
  for (i in 1:length(cat.names)) {  # i <- 1;
    out.tbl$load.type[ctr] <- "all";  
    out.tbl$cat.type[ctr] <- cat.names[i];
    out.tbl$stat.name[ctr] <- "dprime";
    hit.ct <- length(which(tbl$StimType == cat.names[i] & tbl$TargetType == "target" & tbl$Stim.ACC == 1));
    CR.ct <- length(which(tbl$StimType == cat.names[i] & tbl$TargetType != "target" & tbl$Stim.ACC == 1));
    miss.ct <- length(which(tbl$StimType == cat.names[i] & tbl$TargetType == "target" & tbl$Stim.ACC == 0));
    FA.ct <- length(which(tbl$StimType == cat.names[i] & tbl$TargetType != "target" & tbl$Stim.ACC == 0));
    out.tbl$stat.value[ctr] <- calc.dprime(hit.ct, miss.ct, FA.ct, CR.ct);  # calculate the d', with the log-linear rule correction
    ctr <- ctr + 1;
  }
  
  # d', trials by load and category, NA trials as incorrect
  for (i in 1:length(cat.names)) {  # i <- 1;
    for (j in 1:length(load.names)) { 
      out.tbl$load.type[ctr] <- load.names[j];  
      out.tbl$cat.type[ctr] <- cat.names[i];
      out.tbl$stat.name[ctr] <- "dprime";
      hit.ct <- length(which(tbl$BlockType == load.names[j] & tbl$StimType == cat.names[i] & tbl$TargetType == "target" & tbl$Stim.ACC == 1));
      CR.ct <- length(which(tbl$BlockType == load.names[j] & tbl$StimType == cat.names[i] & tbl$TargetType != "target" & tbl$Stim.ACC == 1));
      miss.ct <- length(which(tbl$BlockType == load.names[j] & tbl$StimType == cat.names[i] & tbl$TargetType == "target" & tbl$Stim.ACC == 0));
      FA.ct <- length(which(tbl$BlockType == load.names[j] & tbl$StimType == cat.names[i] & tbl$TargetType != "target" & tbl$Stim.ACC == 0));
      out.tbl$stat.value[ctr] <- calc.dprime(hit.ct, miss.ct, FA.ct, CR.ct);  # calculate the d', with the log-linear rule correction
      ctr <- ctr + 1;
    }
  }
  
  
  # median and mean RT, all correctly responded-to trials (omitting NA trials)
  out.tbl$load.type[ctr] <- "all";  
  out.tbl$cat.type[ctr] <- "all";
  out.tbl$trial.type[ctr] <- "all";
  out.tbl$stat.name[ctr] <- "medianRT";
  tmp <- tbl$Stim.RT[which(!is.na(tbl$Stim.RESP) & tbl$Stim.ACC == 1)]; 
  out.tbl$stat.value[ctr] <- median(tmp);
  ctr <- ctr + 1;
  
  out.tbl$load.type[ctr] <- "all";  
  out.tbl$cat.type[ctr] <- "all";
  out.tbl$trial.type[ctr] <- "all";
  out.tbl$stat.name[ctr] <- "meanRT";
  out.tbl$stat.value[ctr] <- mean(tmp);
  tmp <- NA; 
  ctr <- ctr + 1;
  
  
  # median and mean RT, all correctly responded-to FACE and PLACE trials
  out.tbl$load.type[ctr] <- "all";  
  out.tbl$cat.type[ctr] <- "Face&Place";
  out.tbl$trial.type[ctr] <- "all";
  out.tbl$stat.name[ctr] <- "medianRT";
  tmp <- tbl$Stim.RT[which((tbl$StimType == "Face" | tbl$StimType == "Place") & !is.na(tbl$Stim.RESP) & tbl$Stim.ACC == 1)];
  out.tbl$stat.value[ctr] <- median(tmp); 
  ctr <- ctr + 1;
  
  out.tbl$load.type[ctr] <- "all";  
  out.tbl$cat.type[ctr] <- "Face&Place";
  out.tbl$trial.type[ctr] <- "all";
  out.tbl$stat.name[ctr] <- "meanRT";
  out.tbl$stat.value[ctr] <- mean(tmp); 
  tmp <- NA; 
  ctr <- ctr + 1;
  
  
  # median and mean RT, all correctly responded-to FACE and PLACE trials by load
  for (j in 1:length(load.names)) { 
    out.tbl$load.type[ctr] <- load.names[j];  
    out.tbl$cat.type[ctr] <- "Face&Place";
    out.tbl$trial.type[ctr] <- "all";
    out.tbl$stat.name[ctr] <- "medianRT";
    tmp <- tbl$Stim.RT[which((tbl$StimType == "Face" | tbl$StimType == "Place") & tbl$BlockType == load.names[j] & !is.na(tbl$Stim.RESP) & tbl$Stim.ACC == 1)];
    out.tbl$stat.value[ctr] <- median(tmp);  
    ctr <- ctr + 1;
    
    out.tbl$load.type[ctr] <- load.names[j];  
    out.tbl$cat.type[ctr] <- "Face&Place";
    out.tbl$trial.type[ctr] <- "all";
    out.tbl$stat.name[ctr] <- "meanRT";
    out.tbl$stat.value[ctr] <- mean(tmp);  
    tmp <- NA; 
    
    ctr <- ctr + 1;
  }
  
  
  # median and mean RT, all correctly-responded to FACE and PLACE trials by category, load, and type.
  for (j in 1:length(load.names)) { 
    for (k in 1:length(trial.names)) {
      out.tbl$load.type[ctr] <- load.names[j];  
      out.tbl$cat.type[ctr] <- "Face&Place";
      out.tbl$trial.type[ctr] <- trial.names[k];
      out.tbl$stat.name[ctr] <- "medianRT";
      tmp <- tbl$Stim.RT[which((tbl$StimType == "Face" | tbl$StimType == "Place") & tbl$BlockType == load.names[j] & tbl$TargetType == trial.names[k] & !is.na(tbl$Stim.RESP) & tbl$Stim.ACC == 1)];
      out.tbl$stat.value[ctr] <- median(tmp);  
      ctr <- ctr + 1;
      
      out.tbl$load.type[ctr] <- load.names[j];  
      out.tbl$cat.type[ctr] <- "Face&Place";
      out.tbl$trial.type[ctr] <- trial.names[k];
      out.tbl$stat.name[ctr] <- "meanRT";
      out.tbl$stat.value[ctr] <- mean(tmp);  
      tmp <- NA; 
      ctr <- ctr + 1;
    }
    
    out.tbl$load.type[ctr] <- load.names[j];  
    out.tbl$cat.type[ctr] <- "Face&Place";
    out.tbl$trial.type[ctr] <- "nontarget";
    out.tbl$stat.name[ctr] <- "medianRT";
    tmp <- tbl$Stim.RT[which((tbl$StimType == "Face" | tbl$StimType == "Place") & tbl$BlockType == load.names[j] & tbl$TargetType != "target" & !is.na(tbl$Stim.RESP) & tbl$Stim.ACC == 1)];
    out.tbl$stat.value[ctr] <- median(tmp);  
    ctr <- ctr + 1;
    
    out.tbl$load.type[ctr] <- load.names[j];  
    out.tbl$cat.type[ctr] <- "Face&Place";
    out.tbl$trial.type[ctr] <- "nontarget";
    out.tbl$stat.name[ctr] <- "meanRT";
    out.tbl$stat.value[ctr] <- mean(tmp);  
    tmp <- NA; 
    ctr <- ctr + 1;
  }
  
  # median and mean RT, correct trials by category.
  for (i in 1:length(cat.names)) {  # i <- 1;
    out.tbl$load.type[ctr] <- "all";  
    out.tbl$cat.type[ctr] <- cat.names[i];
    out.tbl$trial.type[ctr] <- "all";
    out.tbl$stat.name[ctr] <- "medianRT";
    tmp <- tbl$Stim.RT[which(tbl$StimType == cat.names[i] & !is.na(tbl$Stim.RESP) & tbl$Stim.ACC == 1)];
    out.tbl$stat.value[ctr] <- median(tmp); 
    ctr <- ctr + 1;
    
    out.tbl$load.type[ctr] <- "all";  
    out.tbl$cat.type[ctr] <- cat.names[i];
    out.tbl$trial.type[ctr] <- "all";
    out.tbl$stat.name[ctr] <- "meanRT";
    out.tbl$stat.value[ctr] <- mean(tmp); 
    tmp <- NA; 
    ctr <- ctr + 1;
  }
  
  # median and mean RT, correct trials by load.
  for (i in 1:length(load.names)) {  # i <- 1;
    out.tbl$load.type[ctr] <- load.names[i];  
    out.tbl$cat.type[ctr] <- "all";
    out.tbl$trial.type[ctr] <- "all";
    out.tbl$stat.name[ctr] <- "medianRT";
    tmp <- tbl$Stim.RT[which(tbl$BlockType == load.names[i] & !is.na(tbl$Stim.RESP) & tbl$Stim.ACC == 1)];
    out.tbl$stat.value[ctr] <- median(tmp);
    ctr <- ctr + 1;
    
    out.tbl$load.type[ctr] <- load.names[i];  
    out.tbl$cat.type[ctr] <- "all";
    out.tbl$trial.type[ctr] <- "all";
    out.tbl$stat.name[ctr] <- "meanRT";
    out.tbl$stat.value[ctr] <- mean(tmp);
    tmp <- NA; 
    ctr <- ctr + 1;
  }
  
  # median and mean RT, correct trials by category and load.
  for (i in 1:length(cat.names)) {  # i <- 1;
    for (j in 1:length(load.names)) { 
      out.tbl$load.type[ctr] <- load.names[j];  
      out.tbl$cat.type[ctr] <- cat.names[i];
      out.tbl$trial.type[ctr] <- "all";
      out.tbl$stat.name[ctr] <- "medianRT";
      tmp <- tbl$Stim.RT[which(tbl$StimType == cat.names[i] & tbl$BlockType == load.names[j] & !is.na(tbl$Stim.RESP) & tbl$Stim.ACC == 1)];
      out.tbl$stat.value[ctr] <- median(tmp); 
      ctr <- ctr + 1;
      
      out.tbl$load.type[ctr] <- load.names[j];  
      out.tbl$cat.type[ctr] <- cat.names[i];
      out.tbl$trial.type[ctr] <- "all";
      out.tbl$stat.name[ctr] <- "meanRT";
      out.tbl$stat.value[ctr] <- mean(tmp); 
      tmp <- NA; 
      ctr <- ctr + 1;
    }
  }
  
  # proportion correct, trials by category, load, and type. NA trials as incorrect
  for (i in 1:length(cat.names)) {  # i <- 1;
    for (j in 1:length(load.names)) { 
      for (k in 1:length(trial.names)) {
        out.tbl$load.type[ctr] <- load.names[j];  
        out.tbl$cat.type[ctr] <- cat.names[i];
        out.tbl$trial.type[ctr] <- trial.names[k];
        out.tbl$stat.name[ctr] <- "medianRT";
        tmp <- tbl$Stim.RT[which(tbl$StimType == cat.names[i] & tbl$BlockType == load.names[j] & tbl$TargetType == trial.names[k] & !is.na(tbl$Stim.RESP))];
        out.tbl$stat.value[ctr] <- median(tmp);
        ctr <- ctr + 1;
        
        out.tbl$load.type[ctr] <- load.names[j];  
        out.tbl$cat.type[ctr] <- cat.names[i];
        out.tbl$trial.type[ctr] <- trial.names[k];
        out.tbl$stat.name[ctr] <- "meanRT";
        out.tbl$stat.value[ctr] <- mean(tmp);
        tmp <- NA; 
        ctr <- ctr + 1;
      }
      
      out.tbl$load.type[ctr] <- load.names[j];  
      out.tbl$cat.type[ctr] <- cat.names[i];
      out.tbl$trial.type[ctr] <- "nontarget";
      out.tbl$stat.name[ctr] <- "medianRT";
      tmp <- tbl$Stim.RT[which(tbl$StimType == cat.names[i] & tbl$BlockType == load.names[j] & tbl$TargetType != "target" & !is.na(tbl$Stim.RESP))];
      out.tbl$stat.value[ctr] <- median(tmp);
      ctr <- ctr + 1;
      
      out.tbl$load.type[ctr] <- load.names[j];  
      out.tbl$cat.type[ctr] <- cat.names[i];
      out.tbl$trial.type[ctr] <- "nontarget";
      out.tbl$stat.name[ctr] <- "meanRT";
      out.tbl$stat.value[ctr] <- mean(tmp);
      tmp <- NA; 
      ctr <- ctr + 1;
    }
  }
  
  # save it
  out.tbl <- out.tbl[1:(ctr-1),];    # take off any extra rows
  write.table(out.tbl, paste0(out.path, id.key$jo.id[which(id.key$hcp.id == do.sub)], "_WMbehaviorSummary_bothruns.txt"));
}


# and the loop to call the function
for (do.sub in sub.ids[1:length(sub.ids)]) {    # do.sub <- sub.ids[482];
  if (!file.exists(paste0(out.path, do.sub, "_WMbehaviorSummary.txt"))) { get.data(id.key$hcp.ids[which(id.key$jo.ids == do.sub)]); } 
}

####################################################################################################################################################################



