library(googledrive)

drive_auth()

target = drive_ls(as_id("https://drive.google.com/drive/u/1/folders/1lD5lrQN01yrmVe_PZa3zIYa8Hm9vQFeX"))

for(i in nrow(target)) {
  drive_download(file=target$name[1], 
    path=paste0("survey_data/",target$name[1]), 
    overwrite = TRUE)
}