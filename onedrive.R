library(Microsoft365R)
library(AzureAuth)


setwd("D:/test/onedrive")

# od <- get_personal_onedrive()
# token <- od$token
# saveRDS(token, "D:/test/onedrive/tokenOnedrive.RDS")

tokenRDS <- readRDS("D:/test/onedrive/tokenOnedrive.RDS")
od <- get_personal_onedrive(token = tokenRDS)
Buod <- get_business_onedrive()
# list files and folders
od$list_items()

# same as list_items()
od$list_files()
od$list_files("Documents")

# upload and download files
od$download_file("Hirasawa Ui.rar")
od$upload_file("D:/test/onedrive/Test.txt")

# create a folder
od$create_folder("Documents/newfolder")