# To batch convert csv files

# first navigate to directory of xlsx files and set as working directory

library("rio")
xls <- dir(pattern = "xlsx")[2]
created <- mapply(convert, xls, gsub("xlsx", "csv", xls))
unlink(xls) # delete xlsx files


files.to.read = list.files(pattern="*.xlsx")

# Read each file and write it to csv
lapply(files.to.read[1], function(f) {
#  df = readxl::read_excel(f, na = "")
  df = readxl::read_excel(f, na = "")
  write.csv(df, gsub("xlsx", "csv", f), row.names=FALSE)
})


