library(data.table)
library("yaml") # http://stackoverflow.com/questions/5272846/how-to-get-parameters-from-config-file-in-r-script
#Read the file names from an xml file
params<-yaml.load_file("params/params_merge_trans.xml")
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
func<-function(x,y, cols){
  df<-merge(x, y, by=cols)
  df[, c(-1, -2)]}#Remove disparo and tanda columns}
##Run lapply through the list: lapply(list_of_df, modified_merge_function, dataframe(color_data))
  merg_cols<-unlist(params[["merg"]])
  merged_ls<-lapply(field_data, func, y=color_data, cols=merg_cols)
  
  
bulk<-rbindlist(merged_ls)
  
  
  #Create fruit_shape column by dividing long into width
  bulk[["fruit_shape"]]<-bulk[["long"]] / bulk[["width"]]
  #Round to two decimal places in the shape column
  bulk[["fruit_shape"]]<-format(round(bulk$fruit_shape, 2))
  #Create phenotype uniquenames by concatening line names, dates and a unique number 
  bulk$stock.name<-paste0(bulk$line, "_", as.character(bulk$date))
  concat.split<-split(bulk$stock.name, as.factor(bulk$stock.name))
  s<-sapply(concat.split, function(x) paste0(x, "_", seq(along = x))) # funciona
  dfu<-data.frame(matrix(unlist(s))) #Create a df from the list
  bulk<-bulk[order(bulk$stock.name),] # Sort the bulk df to match the dfu 
  bulk[["phenotype_name"]]<-(dfu[[1]]) ## add the unique column in dfu to bulk df
  
  
  #Loop over the list of attr_columns (defined in params.xml). For each column, lapply create a df and append the values of the given column to a unique "value" column where all attributes will be stored.
  #The final object is a list of df, each one having the same structure, but with a different attribute in the "value" and "attribute" column.
  #It also creates the phenotype.uniquename and appends them to the pheno.sample column
  df.l<-lapply(params$attr_columns, function(i)
    data.frame(date = bulk$date, line = bulk$line, stock_sample = bulk$phenotype_name, 
               value = bulk[[i]], 
               attribute = rep(i), season= format(bulk$date, "%Y"),
               person = bulk$person, pheno_sample=paste0(bulk$phenotype_name, "_", i)))
  #Bind all the df in the df.l list by row
  f<-rbindlist(df.l)
  
  #The next function is used to search for the protocol name and will be used in the next lapply function
  #This fuction loop using sapply through the names of the protocol list(prot) provided in the params files.
  #it uses a character vector (at) and test if at = to any name of the protocol list, if so, it returns the value of the given element of the list which is stored in val
  #Then it tests if val is empty and return NA or return the value. 
  prot<-params$protocol #Get a list of protocols from params.xml file
  protocol<-function(at){ 
    val<-unlist(sapply(names(prot), function(x) if(at == x) prot[[x]])) #funciona, devuelve un vector con nmbre
    if (length(val) == 0){
      " "
    }else{
      val
    }
  }
  #Using the last function, lapply loops over the f$attribute column an pass each element to the protocol function 
  #which will determine if any attrubite has an associated protocol
  f[["protocol"]]<-unlist(lapply(f$attribute, protocol))
  
  
  #Add stock_type, stock_relationship and stock_description as new columns of f
  f[["stock.type_id"]]<-rep(params$cvterm$stock_type)
  f[["relationship"]]<-rep(params$cvterm$stock_rel)
  f[["description"]]<-rep(params$cvterm$descrip)
  f[["project"]]<-rep(params$cvterm$project)
  
  #The next function replace the _ in the attribute column to a ' ' , extracted from here:http://stackoverflow.com/questions/6954017/r-replace-characters-using-gsub-how-to-create-a-function
  gsub2 <- function(pattern, replacement, x, ...) {
    for(i in 1:length(pattern))
      x <- gsub(pattern[i], replacement[i], x, ...)
    x
  }
  from<-('_')
  to<-(' ')
  #Apply the function to each element of f$attr column
  f$attribute<-unlist(lapply(f$attribute, function(x) gsub2(pattern = from, replacement = to, x = x)))
  
  #Remove rows containing NA in "value" column: http://stackoverflow.com/questions/11254524/omit-rows-containing-specific-column-of-na
  completeFun <- function(data, desiredCols) {
    completeVec <- complete.cases(data[, desiredCols])
    return(data[completeVec, ])
  }
  f<-completeFun(f, "value")
  #Write f to the file specified in params.xml and don't write row names. 
  out_file<-params$files$out_file
  write.table(f, file=out_file, row.names = F, sep = "\t", quote = FALSE)