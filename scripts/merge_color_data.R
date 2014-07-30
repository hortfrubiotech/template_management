##Merging field data with color data. Field data is stored in a list of dataframes (one plant line per df)
##If the data is not in a list but in a df, simply use merge: merge(df_1, df_2, by=c("shot", "tanda")
##Colorimeter data is stored in a dataframe containing all the "tandas" for one year.
##Both elements must contain two equal columns, 'shot' and 'tanda', which are the fields used for merging them. 
library(data.table)
library(WriteXLS)
library("yaml") # http://stackoverflow.com/questions/5272846/how-to-get-parameters-from-config-file-in-r-script
#Read the file names from an xml file
params<-yaml.load_file("../params/params_merge_color.xml")
#Define function to read data, since all sheet have the same columns, col.classes and col.names are defined here
read_data<-function(filename){
  field_col_class<-unlist(params[["field_colclass"]])
  field_col_names<-unlist(params[["field_colnames"]])
  read.table(file=filename, header = T, sep=",", 
             col.names = field_col_names, colClasses = field_col_class, blank.lines.skip=TRUE)
  
}

#loop over the list of filenames and read them all. Create a list of df containing 1 df per stock

color_cl<-c(params[["color_colclass"]])
color_col_n<-c(params[["color_colnames"]])
stock_file_ls<-params[["stock_files"]]

field_data<-lapply(stock_file_ls, read_data)

color_file<-params$files$color_file
color_data<-read.table(file=color_file, header=T, sep=",", col.names=color_col_n, 
                       colClasses = color_cl, blank.lines.skip=TRUE)

drops<-unlist(params[["removeCols"]]) #columns to be removed from the color df. Defined in params$merg file
color_data<-color_data[!names(color_data) %in% drops]#Remove columns defined in drops. http://stackoverflow.com/questions/4605206/drop-columns-r-data-frame


## Create a modified merge function to pass it through lapply, specify by which field you want to merge 
func<-function(x,y){
  df<-merge(x, y, by=unlist(params[["merg"]]))
  df[, c(-1, -2)]}#Remove disparo and tanda columns}
##Run lapply through the list: lapply(list_of_df, modified_merge_function, dataframe(color_data))
merged_ls<-lapply(field_data, func, y=color_data)

#Save output to a new .xls file

color_file<-params$files$out_file
WriteXLS("merged_ls", ExcelFileName="tedy.xls", perl = "perl") 