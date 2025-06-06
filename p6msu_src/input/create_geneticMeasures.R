################################################################################################################################################
# Joset A. Etzel, jetzel@wustl.edu. 
# updated 19 April 2019; adapted from "D:\svnFiles\HCP\byFamilyGroup\YaelSurface\fromHCP\extractGeneticMeasures.R"
# code to generate "geneticMeasures.txt": several measures that we expect to be genetically determined; HCP 1200-subjects release.

# The correspondence between "jo.ids" and HCP ids is restricted. Contact the authors (Jo Etzel) if access is needed.
# Note: this was run at Washington University in St. Louis, where access to the released HCP files is at a disk (/data/hcp-zfs/OpenAccess/); 
# the paths will need to be updated for other filesystems.
################################################################################################################################################

rm(list=ls());

path <- "/scratch1/JoEtzel/fromHCP/GeneticMeasures/"
res.tbl <- read.csv(paste0(path, "RESTRICTED_jaetzel_3_27_2017_11_28_8.csv"))
unr.tbl <- read.csv(paste0(path, "UNRESTRICTED_jaetzel_5_11_2017.csv"))
# which(unr.tbl$Subject != data.tbl$Subject)   # integer(0), so the rows match
source("/scratch1/JoEtzel/fromHCP/inputs/hcp_functions_GTunique.R");
id.key <- read.table("/scratch1/JoEtzel/fromHCP/inputs/subIDkey.txt", stringsAsFactors=FALSE);

# get the subject ID codes for the various subject groups
MZ.1s <- get.ids("MZ.1s.GTunique");   MZ.2s <- get.ids("MZ.2s.GTunique");  
DZ.1s <- get.ids("DZ.1s.GTunique");   DZ.2s <- get.ids("DZ.2s.GTunique");  
sib.1s <- get.ids("sib.1s.GTunique"); sib.2s <- get.ids("sib.2s.GTunique");
un.1s <- get.ids("unr.1s.GTunique");  un.2s <- get.ids("unr.2s.GTunique");
all.ids <- c(MZ.1s, MZ.2s, DZ.1s, DZ.2s, sib.1s, sib.2s, un.1s, un.2s);

gen.tbl <- data.frame(array(NA, c(length(all.ids), 8)));
colnames(gen.tbl) <- c("sub.id", "pmat.cr", "PicVocab", "ReadEng", "totGreyVol", "Height", "Weight", "BMI");
#cols.un.tbl <- c("PMAT24_A_CR", "PMAT24_A_RTCR", "PicVocab_AgeAdj");  # column names used by the HCP
# Penn Progressive Matrices: Number of Correct Responses	PMAT24_A_CR
#	NIH Toolbox Picture Vocabulary Test: Age-Adjusted Scale Score	PicVocab_AgeAdj
# NIH Toolbox Oral Reading Recognition Test: Age-Adjusted Scale Score:  ReadEng_AgeAdj
# FS_TotCort_GM_Vol: Total cortical gray matter volume
# Height, Weight, BMI are Height, Weight, BMI; from the restricted info file

for (i in 1:length(all.ids)) {    # i <- 1;
  hcp.id <- id.key$hcp.id[which(id.key$jo.id == all.ids[i])];
  un.ind <- which(unr.tbl$Subject == hcp.id);
  if (length(hcp.id) != 1 | length(un.ind) != 1) { stop("length(hcp.id) != 1"); }
  gen.tbl$sub.id[i] <- all.ids[i];
  gen.tbl$pmat.cr[i] <- unr.tbl$PMAT24_A_CR[un.ind];
  gen.tbl$PicVocab[i] <- unr.tbl$PicVocab_AgeAdj[un.ind];
  gen.tbl$ReadEng[i] <- unr.tbl$ReadEng_AgeAdj[un.ind];
  gen.tbl$totGreyVol[i] <- unr.tbl$FS_TotCort_GM_Vol[un.ind];
  
  res.ind <- which(res.tbl$Subject == hcp.id);
  if (length(res.ind) != 1) { stop("length(res.ind) != 1"); }
  gen.tbl$Height[i] <- res.tbl$Height[res.ind];
  gen.tbl$Weight[i] <- res.tbl$Weight[res.ind];
  gen.tbl$BMI[i] <- res.tbl$BMI[res.ind];
}
write.table(gen.tbl, "/scratch1/JoEtzel/fromHCP/GeneticMeasures/geneticMeasures.txt");

##################################################################################################################################################################

