library(popcycle)

src <- Sys.getenv('INSTRUMENTDIR')
dest <- Sys.getenv('RAWDATADIR')
print(paste0(c("Copying from ", src, " to ", dest)))
file.transfer(dest, src)
