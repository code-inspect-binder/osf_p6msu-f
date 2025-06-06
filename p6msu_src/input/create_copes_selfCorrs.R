################################################################################################################################################
# Joset A. Etzel, jetzel@wustl.edu. 
# updated 19 April 2019; code to create the files in \input\selfCorrelations\
# adapted from "D:\svnFiles\HCP\byFamilyGroup\YaelSurface\convertCifti.R" & "D:\svnFiles\HCP\byFamilyGroup\eachPerson\prep_eachPerson.R"

# The correspondence between "jo.ids" and HCP ids is restricted. Contact the authors (Jo Etzel) if access is needed.
# Note: this was run at Washington University in St. Louis, where access to the released HCP files is at a disk (/data/hcp-zfs/OpenAccess/); 
# the paths will need to be updated for other file systems.
################################################################################################################################################
################################################################################################################################################
# preparation: figure out which vertices needed to extract from the copes to have the Visual and FrontoParietal communties.

# text descriptions of each parcel are in Parcels.csv, which was converted from the Parcels.xlsx file released by Petersen lab to accompany the 
# Gordon atlas paper: https://sites.wustl.edu/petersenschlaggarlab/parcels-19cwpgu/. The conversion was opening Parcels.xlsx in excel, deleting
# the Surface area (mm2) and Centroid (MNI) columns, then saving as a csv. No reordering of ParcelIDs was done.
# Gordon, E.M., Laumann, T.O., Adeyemo, B., Huckins, J.F., Kelley, W.M., Petersen, S.E. (2016). Generation and evaluation of a cortical area 
# parcellation from resting-state correlations. Cereb Cortex. 2014 Oct 14. PMCID: PMC4677978

# see http://mvpa.blogspot.com/2017/05/extracting-values-from-cifti-files.html for general cifti-to-text info (which made Gordon_Parcels_LR.dtseries.txt)

rm(list=ls());

out.path <- "/scratch1/JoEtzel/";
g.key <- read.csv("/scratch2/JoEtzel/Parcels.csv", stringsAsFactors=FALSE);    # which parcels in each community; see note above. from Parcels.xlsx

# make a text version of the Gordon atlas - parcel assignment for each vertex
dt.fname <- paste0(out.path, "Parcels_LR.dtseries.txt");
if (!file.exists(dt.fname)) { 
  in.fname <- paste0(out.path, "Parcels_LR.dtseries.nii");     # part of the Gordon atlas download
  if (!file.exists(in.fname)) { stop(paste("missing:", in.fname)); }
  system(paste0("wb_command -cifti-convert -to-text ", in.fname, " ", dt.fname));   # call the  wb_command function
}
in.dt <- read.table(dt.fname)[,1];   # just one column in the file

# each community has a different number of component parcels, and vertices in each parcel.
# make a single file for each community listing *all* of its vertices.
for (do.comm in c("Visual", "FrontoParietal")) {   
  for (do.hem in c("L", "R")) {   # do.comm <- "Visual"; do.hem <- "L";
    need.parcels <- g.key$ParcelID[which(g.key$Community == do.comm & g.key$Hem == do.hem)];  # parcels for this community & hemisphere
    
    # vertices (cifti rows) for each of these parcels
    fout <- file(paste0(out.path, do.comm, "_", do.hem, "_ciftirows.txt"), 'wt');   # start a blank text file for writing
    for (pid in 1:length(need.parcels)) {   # pid <- 1;
      inds <- which(in.dt == need.parcels[pid]);   # rows of the cifti for parcel need.parcel[pid] only
      for (i in 1:length(inds)) { cat(inds[i], file=fout, sep="\n"); }    # write out a line with each row number
    }
    close(fout); unlink(fout);    # let go of the file
  }
}

################################################################################################################################################
################################################################################################################################################
# create text files for each person and community, with 4 rows: 0-back face, 0-back place, 2-back face, 2-back place (in that order).

rm(list=ls());  

in.path <- "/scratch1/JoEtzel/";   # _ciftirows.txt files, cope and id keys, etc.
out.path <- "/scratch1/JoEtzel/output/";   # files written out to here                       
hcp.path <- "/data/hcp-zfs/OpenAccess/1200subject/";  # first part of path to hcp files; same for everyone
mid.path1 <- "/MNINonLinear/Results/tfMRI_WM/tfMRI_WM_hp200_";  # middle part of the hcp file path; same for everyone
mid.path2 <- "_level2.feat/GrayordinatesStats/cope";    # more of the big path

comm.ids <- c("Visual", "FrontoParietal"); 
hem.ids <- c("L", "R");                    
s.lbl <- "s2";     # smoothing level label (as in the HCP-provided COPEs)

cope.key <- read.table(paste0(in.path, "wm_copeKey.txt"));    # lists which copes are which (adapted from Contrasts.txt in each person's /tfMRI_WM_hp200_s2_level2.feat/)
cope.ids <- c(which(cope.key$x == "0BK_FACE"), which(cope.key$x == "0BK_PLACE"), which(cope.key$x == "2BK_FACE"), which(cope.key$x == "2BK_PLACE"));

id.key <- read.table(paste0(in.path, "subIDkey_19June2017.txt"));    # RESTRICTED INFO!!!!  has two columns: hcp.ids and jo.ids, giving the correspondence
ids <- id.key$jo.ids;     

for (cid in 1:length(comm.ids)) {   
  for (hid in 1:length(hem.ids)) {   # cid <- 1; hid <- 1;
    # get the rows of the text-ized cifti for this community and hemisphere
    fname <- paste0(in.path, comm.ids[cid], "_", hem.ids[hid], "_ciftirows.txt");  
    if (!file.exists(fname)) { stop(paste("missing:", fname)); }
    need.rows <- read.table(fname)[,1];   # read in the file, just keeping the first column
    
    # loop through each person
    for (i in 1:length(ids)) {   # i <- 1;
      # find the hcp id for this person
      tmp <- which(id.key$jo.id == ids[i]); 
      if (length(tmp) != 1) { stop("length(tmp) != 1"); }   # should only one match - something wrong if not!
      this.hcp <- id.key$hcp.id[tmp];    
      
      out.fname <- paste0(out.path, ids[i], "_", comm.ids[cid], hem.ids[hid], "_", s.lbl, "_wmCOPEs.txt");   # output file to be written
      if (file.exists(out.fname) == FALSE) {    # don't want to make the same thing over if already there
        # convert each of the copes we need to text (same as gordon atlas was converted), writing into temporary files
        for (cpid in 1:length(cope.ids)) {  # cpid <- 1;
          in.fname <- paste0(hcp.path, this.hcp, mid.path1, s.lbl, mid.path2, cope.ids[cpid], ".feat/pe1.dtseries.nii");
          if (file.exists(in.fname)) { system(paste0("wb_command -cifti-convert -to-text ", in.fname, " ", out.path, "tmp", cpid, ".txt")); }
        }
        
        # get the needed vertices (rows) out of the temporary files, and write as a single new file
        out.tbl <- array(NA, c(length(cope.ids), length(need.rows)));   # blank table to hold the values we need to keep
        rownames(out.tbl) <- paste0("cope", cope.ids);
        colnames(out.tbl) <- paste0("v", 1:length(need.rows));
        for (cpid in 1:length(cope.ids)) {  # cpid <- 1;
          tmp <- read.table(paste0(out.path, "tmp", cpid, ".txt"))[,1];   # read in the temporary table for this cope
          out.tbl[cpid,] <- tmp[need.rows];    # save the needed verticies into the proper row for this cope
        }
        write.table(out.tbl, out.fname);   # save!
      }
    }
  }
}

################################################################################################################################################
################################################################################################################################################
# calculating the distance between each person's OWN examples - not to other people - "self correlations"; one output file per person.

rm(list=ls()); 

in.path <- "/scratch1/JoEtzel/";
out.path <- "/scratch1/JoEtzel/selfCorr_forRSA/";
cope.path <- "/scratch1/JoEtzel/output/";   # _wmCOPEs.txt files from previous code block

# get all the subject ID codes
source(paste0(in.path, "subPairings.R"));  # get.ids function
sub.ids <- c(get.ids("MZ.1s"), get.ids("MZ.2s"), get.ids("DZ.1s"), get.ids("DZ.2s"), 
             get.ids("SIB.1s"), get.ids("SIB.2s"), get.ids("UNR.1s"), get.ids("UNR.2s")); 

cope.ids <- c("0bkFace",  "0bkPlace", "2bkFace",  "2bkPlace");   # ordered as in input files; see previous code.
# these hard-code which copes we want to correlate: pair.ids1[1] with pair.ids2[1], etc. 
# this duplicates efforts: correlation of 0bkFace & 0bkPlace == correlation of 0bkPlace & 0bkFace since same person. But storing twice simplifies later code.
pair.ids1 <- c(rep(cope.ids[1],4), rep(cope.ids[2],4), rep(cope.ids[3],4), rep(cope.ids[4],4));   # for RSA-style; need mixed pairs
pair.ids2 <- rep(cope.ids, 4);    # rbind(pair.ids1, pair.ids2)

comm.ids <- c("Visual", "FrontoParietal");   # communities
hem.ids <- c("L", "R");   # hemispheres
s.lbl <- "s2";     # smoothing level label as in wmCOPEs.txt files

for (sid in 1:length(sub.ids)) {   # sid <- 1;     
  # calculate the distance between this person's examples and put into out.tbl; one output file per person (not community)
  out.tbl <- data.frame(array(NA, c(length(pair.ids1)*4, 5)));    # (each community on left and right)
  colnames(out.tbl) <- c("sub.id", "comm.id", "side", "pair.id", "distance"); 
  ctr <- 1;   # row counter to increment when filling up out.tbl
  # go through each community and hemisphere
  for (cid in 1:length(comm.ids)) {   
    for (hid in 1:length(hem.ids)) {   # cid <- 1; hid <- 1;
      # load in the data for this person, community, hemisphere, smoothing
      fname <- paste0(cope.path, sub.ids[sid], "_", comm.ids[cid], hem.ids[hid], "_", s.lbl, "_wmCOPEs.txt");
      if (!file.exists(fname)) { stop(paste("missing:", fname)); }
      in.tbl <- read.table(fname);
      
      # check in.tbl a bit to make sure it has sensible values
      if (nrow(in.tbl) != length(cope.ids)) { stop("mismatching row counts!"); }
      if (length(which(is.na(in.tbl))) > 0) { stop("missing data in in.tbl?"); }
      for (cpid in 1:length(cope.ids)) { if (sd(in.tbl[cpid,]) == 0) { stop("sd of a row == 0?");} }
      
      # now calculate each cope-pair correlation, storing into out.tbl
      for (pid in 1:length(pair.ids1)) {   # pid <- 1;
        out.tbl$side[ctr] <- hem.ids[hid];
        out.tbl$comm.id[ctr] <- comm.ids[cid] 
        out.tbl$sub.id[ctr] <- sub.ids[sid];
        out.tbl$pair.id[ctr] <- paste(pair.ids1[pid], pair.ids2[pid], sep=".");   # make a label showing which copes were correlated in this row of out.tbl
        data1 <- unlist(in.tbl[which(cope.ids == pair.ids1[pid]),]);    # row of in.tbl with values for the cope named in pair.ids1[pid]
        data2 <- unlist(in.tbl[which(cope.ids == pair.ids2[pid]),]);    # and pair.ids2;  unlist() gets the numbers into a vector (so cor works right)
        out.tbl$distance[ctr] <- cor(data1, data2, method="pearson");  # calculate the distance; store as raw correlation (not z transformed)
        ctr <- ctr + 1;   # increment the row counter 
      }
    }
  }
  write.table(out.tbl, paste0(out.path, sub.ids[sid], "_selfPairwiseDistanceRSA_pcor.txt"));
}

################################################################################################################################################
################################################################################################################################################

