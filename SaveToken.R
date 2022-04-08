library(rdrop2)

token <- drop_auth(new_user =T)
# setwd("")
path <- getwd()
saveRDS(token,file.path(path,"dropbox_Token.rds"))
