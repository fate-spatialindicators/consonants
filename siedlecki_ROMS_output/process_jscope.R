library(googledrive)

# this should fire up browser window or prompt in the console
drive_auth()

# here grabbing annual data to play with smaller file size, other file sizes are huge
target = drive_ls(as_id("https://drive.google.com/drive/u/0/folders/10VmLS2h2TCCVxClBqjTShm8jCuBIPxQ7")) #annual
#target = drive_ls(as_id("https://drive.google.com/drive/u/0/folders/1hXEC_-AizPaE9dGJ2SMIwJpfQdToxy5H")) #seasonal
#target = drive_ls(as_id("https://drive.google.com/drive/u/0/folders/15PJPu3GmUSQeOfAOKF12PEtcgkwUrJSC")) #monthly
#target = drive_ls(as_id("https://drive.google.com/drive/u/0/folders/1IN2kPJwWXerl44vyXabg7jAE2JOFiiCM")) #daily

for(i in 1:nrow(target)) {
  # pull in each file as dribble
  drive_download(file=target[i,], 
                 path=paste0("siedlecki_ROMS_output/",target$name[i]), 
                 overwrite = TRUE)
}


# read matlab files
library(R.matlab)
dat_2018 <- readMat("siedlecki_ROMS_output/2018.mat")
str(dat_2018)

# output is list of matrices associated with $coords
head(dat_2018$coords[[1]])
