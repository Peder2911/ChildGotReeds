#' Merges Variables Into a Date Variable 
#'
#' This function lets you go through data, labelling it as either
#' 1 or 0. Useful for validating data. 
#' Enter numbers corresponding to entries displayed, unseparated.
#'
#' @param df Dataset to work with
#' @param col Column containing data to be displayed 
#' @param samplesize How many entries to display at once
#' @param reset Reset the process / all "seen" <- 0
#' @param idcol Values from idcol are displayed next to values from col.
#' @keywords validation 
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#' mtcars$names <- rownames(mtcars)
#' mtcars$brand <- lapply(strsplit(mtcars$names,' '),function(x)x[1]) %>% unlist()
#' valMt <- iaValidate(mtcars,col = 'names',samplesize = 5,idcol = 'brand') 

iaValidate <- function(df,col,samplesize = 5,reset = FALSE,idcol = 'id'){

   running <- TRUE
   df$interactive.uid <- seq(1,nrow(df)) 
   
   if(!'valid' %in% names(df)|reset){
     df$valid <- 1
   }
   
   if(!'seen' %in% names(df)|reset){
      df$seen <- 0
   }
  
   # 1st batch

   notseen <- df[df$seen != 1,] 
   subsample <- notseen[sample(nrow(notseen),size = samplesize),]
 
   subsample$subs.uid <- seq(1,nrow(subsample))
 
   writeLines(paste(strrep('#',37),'\n'))

   apply(subsample,1,function(row){
      identifier <- glue::glue_collapse(row[idcol],sep = ' - ')
      cat(paste(crayon::yellow$bold(row['subs.uid']),
                       ' - ',
                       crayon::blue(identifier),
                       ') - ',
                       row[col],
                       '\n\n',
                       sep = ''))
      })
    
   while(running){
      choice <- readline(prompt = 'choice > ')
    
      if(stringr::str_to_lower(choice) == 'q'){
         running = FALSE

      } else if (stringr::str_detect(choice,'[0-9]\\?')){ 

         # Inspect an entry, print other fields

         no <- stringr::str_extract(choice,'[0-9]')
         notsentence <- which(names(subsample) != col) 
         interestrow <- subsample[subsample$subs.uid == no,notsentence]
         if(nrow(interestrow) > 1) interestrow <- interestrow[1,]

         datesinrow <- lapply(interestrow,class) == 'Date'
         fmdates <- lapply(interestrow[datesinrow],format) %>% unlist()
         interestrowNames <- names(interestrow)
         interestrow <- as.character(interestrow)
         interestrow[datesinrow] <- fmdates

         i <- 1
         for(v in interestrow){
            cat(crayon::red(interestrowNames[i],interestrow[i]),sep = ': ')
            i <- i+1
            cat('|')
            }
         cat('\n')

      } else {

         # Make a choice, subset, print new options

         choice <- choice%>%
            stringr::str_split('')%>%
            unlist()%>%
            as.numeric()
      
         trash <- subsample[subsample$subs.uid %in% choice,'interactive.uid']
         subseen <- subsample$interactive.uid 
         
         df[df$interactive.uid %in% trash,'valid'] <- 0
         df[df$interactive.uid %in% subseen,'seen'] <- 1
         
         seenno <- sum(df$seen)
         seenpst <- ((seenno / nrow(df))*100)%>%
            round(digits = 2)
         
         cat(crayon::blue(paste('Seen ',seenno,' rows. (',seenpst,'%)',sep = '')),'\n')
         
         notseen <- df[df$seen != 1,] 

         if(nrow(notseen) > samplesize){
            subsample <- notseen[sample(nrow(notseen),size = samplesize),]
         } else if (nrow(notseen) > 0) {
            subsample <- notseen
         } else {
            running <- FALSE
         }
       
         if(running){
            subsample$subs.uid <- seq(1,nrow(subsample))
          
            writeLines(paste(strrep('#',37),'\n'))
            apply(subsample,1,function(row){
               identifier <- glue::glue_collapse(row[idcol],sep = ' - ')
               cat(paste(crayon::yellow$bold(row['subs.uid']),
                                ' - ',
                                crayon::blue(identifier),
                                ') - ',
                                row[col],
                                '\n\n',
                                sep = ''))
               })
         }
          
      }
   }
   df$interactive.uid <- NULL
   df
}
