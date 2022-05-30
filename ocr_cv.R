rm(list = ls())
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx16g"))
gc()
library(imager)
library(ggplot2)
library(dplyr)
library(pdftools)
library(magick)
library(png)
library(tabulizer)
library(openxlsx)
library(tesseract)
library(stringr)
library(sfsmisc)
library(stringdist)
library(SpatialPack)
library(gsubfn)
library(strex)
library(imagerExtra)


#---------------------------------------------#
#-CREATING A FUNCTION FOR EXCEPTION HANDLING--#
#---------------------------------------------#

show_condition = function(code) {
  tryCatch(code,
           error   = function(c) "error",
           warning = function(c) "warning",
           message = function(c) "message")
}


#------------------------------------------#
#-TO KEEP SAME LAYOUT UNIT FOR ALL IMAGES--#
#------------------------------------------#

par(mar=c(1,1,1,1))
  
input    = 'C:/Users/gshangar/Documents/ML Projects/OPR_Cash/'
output   = 'C:/Users/gshangar/Documents/ML Projects/OPR_Cash/output/'
subfiles = 'C:/Users/gshangar/Documents/ML Projects/OPR_Cash/subfiles/'
file     = ''
  
#-----------------------------------------------------#
#-INITIALIZE ENGLISH DICTIONARY FOR TESSERACT PACKAGE-#
#-----------------------------------------------------#
  
eng       = tesseract("eng")

#-------------------------------------#
#-DEFINE NO. OF PAGES IN THE DOCUMENT-#
#-------------------------------------#
  
pages     = pdf_info(paste0(input,file,".pdf"))$pages

#-------------------------------------#
#-SEPARATE PAGES INTO INDIVIDUAL DOCS-#
#-------------------------------------#
  
sf        = split_pdf(paste0(input,file,".pdf"),
            outdir = paste0(subfiles),password = NULL, copy = FALSE)
  
#----------------------------------------#
#-CREATE AN EMPTY DATASET FOR FINAL DATA-#
#----------------------------------------#
  
lb_data  = data.frame(lockbox_name  = character(),
                      lockbox_date  = character(),
                      check_number  = character(),
                      check_date    = character(),
                      payor_name    = character(),
                      service_month = character(),
                      check_amount  = character(),
                      total_amount  = character(),
                      verify        = character(),
                      stringsAsFactors = T)
  
#----------------------------------------------------------------#
#-CREATE AN EMPTY DATASET TO LIST PAGES WHERE CHECKS ARE PRESENT-#
#----------------------------------------------------------------#
  
main_seq  = data.frame(seq = character(),
                       page = numeric(),
                       stringsAsFactors = T)

#---------------------------------#
#--RUNNING THE LOOP FOR ALL PAGES-#
#---------------------------------#

for (i in 1:pages){
  if (pages >= 100 & (i>=10 & i<100)) {
    read_pdf   = image_read_pdf(paste0(subfiles,file,"0",i,".pdf")) 
    image_write(read_pdf,paste0(output,"temp_image.png"))
  }else if (pages >= 100 & i<10) {
    read_pdf   = image_read_pdf(paste0(subfiles,file,"00",i,".pdf")) 
    image_write(read_pdf,paste0(output,"temp_image.png"))
  }else if (pages >= 10 & i<10) {
     read_pdf   = image_read_pdf(paste0(subfiles,file,"0",i,".pdf")) 
     image_write(read_pdf,paste0(output,"temp_image.png"))
    }else{
      read_pdf   = image_read_pdf(paste0(subfiles,file,i,".pdf")) 
      image_write(read_pdf,paste0(output,"temp_image.png"))
    }
  
png_img   = image_read(paste0(output,"temp_image.png"))
    
#----------------------------------#
#--CONVERTING INTO GRAYSCALE IMAGE-#
#----------------------------------#
    
grey_img  = png_img %>% image_quantize(colorspace = 'gray')
  
#-----------------------------------------#
#--INCREASING THE BRIGHTNESS IN THE IMAGE-#
#-----------------------------------------#
    
black_img = grey_img %>% image_modulate(brightness = 200)

#----------------------#
#--SAVING AS PNG FILE--#
#----------------------#

image_write(black_img, path = paste0(output,"temp_image.png"), format = "png")

#--------------------#
#--READING PNG FILE--#
#--------------------#

im_lp         = load.image(paste0(output,"temp_image.png"))

#----------------------#
#--GETTING PIXEL DATA--#
#----------------------#

pixel_data    = as.data.frame(im_lp)
  
#-------------------------------------#
#--MAKING THE DARK PIXELS MORE DARKER-#
#-------------------------------------#
  
pixel_data$value = ifelse(pixel_data$value<1,(pixel_data$value*4)/5,pixel_data$value)

po_pixel      = pixel_data[pixel_data$x  >= 1900
                         & pixel_data$x  <= 3000,]
masked_image     = as.cimg(po_pixel)
#imager::save.image(masked_image1,paste0(output,"temp_image.png"))
# text       = tesseract::ocr(paste0(output,"temp_image.png"),engine = eng)
# data_list  = strsplit(text, "\n")
# 
# masked_image     = as.cimg(pixel_data)
# #masked_image = resize_doubleXY(masked_image)
  
#----------------------------------#
#--SAVING THE GREYSACLE IMAGE FILE-#
#----------------------------------#
  
imager::save.image(masked_image,paste0(output,"temp_image.png"))
  
#-----------------------------------------------------------------#
#--READING THE DATA FROM GREYSCALE IMAGE TO GET NUMBER OF CHEQUES-#
#-----------------------------------------------------------------#
  
text       = tesseract::ocr(paste0(output,"temp_image.png"),engine = eng)
    
data_list  = strsplit(text, "\n")

if (length(data_list[[1]])==0){
  next
}

for (count in 1:lengths(data_list)[[1]]){
  if (grepl(glob2rx("*Chk#*", FALSE, FALSE), data_list[[1]][count], fixed = FALSE) == T){
        seq        = data_list[[1]][count]
        page       = i
        final_seq  = data.frame(seq,page)
        main_seq   = rbind(main_seq,final_seq)
     }else if (grepl(glob2rx("*chk#*", FALSE, FALSE), data_list[[1]][count], fixed = FALSE) == T){
       seq        = data_list[[1]][count]
       page       = i
       final_seq  = data.frame(seq,page)
       main_seq   = rbind(main_seq,final_seq)
     }else if (grepl(glob2rx("*Ch*\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d*", FALSE, FALSE), data_list[[1]][count], fixed = FALSE) == T){
      seq        = data_list[[1]][count]
      page       = i
      final_seq  = data.frame(seq,page)
      main_seq   = rbind(main_seq,final_seq)
     }else if (grepl(glob2rx("*ch*\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d*", FALSE, FALSE), data_list[[1]][count], fixed = FALSE) == T){
      seq        = data_list[[1]][count]
      page       = i
      final_seq  = data.frame(seq,page)
      main_seq   = rbind(main_seq,final_seq)
    }
  }
}

#----------------------------------#
#--STORING NUMBER OF CHEQUE PAGES-#
#----------------------------------#

chk_pages = unique(main_seq$page)


#---------------------------------------------------#
#--RUNNING THE PROCESS THROUGH ALL THE CHEQUE PAGES-#
#---------------------------------------------------#

for (i in 1:length(chk_pages)){
  if (pages >= 100 & (chk_pages[i]>=10 & chk_pages[i]<100)) {
    read_pdf   = image_read_pdf(paste0(subfiles,file,"0",chk_pages[i],".pdf")) 
    image_write(read_pdf,paste0(output,"temp_image.png"))
  }else if (pages >= 100 & chk_pages[i]<10) {
    read_pdf   = image_read_pdf(paste0(subfiles,file,"00",chk_pages[i],".pdf")) 
    image_write(read_pdf,paste0(output,"temp_image.png"))
  }else if (pages >= 10 & chk_pages[i]<10) {
    read_pdf   = image_read_pdf(paste0(subfiles,file,"0",chk_pages[i],".pdf")) 
    image_write(read_pdf,paste0(output,"temp_image.png"))
  }else{
    read_pdf   = image_read_pdf(paste0(subfiles,file,chk_pages[i],".pdf")) 
    image_write(read_pdf,paste0(output,"temp_image.png"))
  }

png_img   = image_read(paste0(output,"temp_image.png"))
grey_img  = png_img %>% image_quantize(colorspace = 'gray')
black_img = grey_img %>% image_modulate(brightness = 200)
      
image_write(black_img, path = paste0(output,"temp_image.png"), format = "png")
  
text       = tesseract::ocr(paste0(output,"temp_image.png"), engine = eng)
  
data_list  = strsplit(text, "\n")

#----------------------------#
#--GETTING THE CHEQUE NUMBER-#
#----------------------------#

for (counter in 1:lengths(data_list)[[1]]){
    if (grepl(glob2rx("*Chk#*", FALSE, FALSE), data_list[[1]][counter], fixed = FALSE) == T){
        check_number = str_after_first(data_list[[1]][counter],"Chk#")
    }else if (grepl(glob2rx("*Ch*\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d*", FALSE, FALSE), data_list[[1]][counter], fixed = FALSE) == T){
        check_number = str_after_last(data_list[[1]][counter]," ")
    }else if (grepl(glob2rx("*ch*\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d*", FALSE, FALSE), data_list[[1]][counter], fixed = FALSE) == T){
          check_number = str_after_last(data_list[[1]][counter]," ")
    }else if (grepl(glob2rx("*chk#*", FALSE, FALSE), data_list[[1]][counter], fixed = FALSE) == T){
          check_number = str_after_first(data_list[[1]][counter],"chk#")
        }
    }
if (exists("check_number") == F){
  check_number = ''
}

#---------------------------#
#--GETTING THE LOCKBOX DATE-#
#---------------------------#

for (counter in 1:lengths(data_list)[[1]]){
    if (grepl(glob2rx("*Deposit Date*", FALSE, FALSE), data_list[[1]][counter], fixed = FALSE) == T){
       lockbox_date = strapplyc(data_list[[1]][counter],"\\d+/\\d+/\\d+",simplify = TRUE)
     }
       if (is.na(lockbox_date[[1]][1])!=T){
       break
     }
  }

if (exists("lockbox_date") == F){
  lockbox_date = ''
}


#------------------------------------------------------#
#--GETTING THE Y DIMENSION WHERE CHECQUE NUMBER EXISTS-#
#------------------------------------------------------#

text          = tesseract::ocr_data(paste0(output,"temp_image.png"), engine = eng)

for (obs in 1:dim(text)[1]){
    
    chk_y         = as.numeric(gsub("^.*,", "",text[text$word=='Chk#',c('bbox')]))  
    if (is.na(chk_y) == T){
    chk_y = ifelse((grepl(glob2rx("*Ch*", FALSE, FALSE), as.character(text[obs,c("word")]), fixed = FALSE) == T) &
                     (grepl(glob2rx("*\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d*", FALSE, FALSE), as.character(text[(obs+1),c("word")]), fixed = FALSE) == T),
                   as.numeric(gsub("^.*,", "",text[obs,c('bbox')])),NA)
    }
    if (is.na(chk_y) == T){
      chk_y = ifelse((grepl(glob2rx("*ch*", FALSE, FALSE), as.character(text[obs,c("word")]), fixed = FALSE) == T) &
                       (grepl(glob2rx("*\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d*", FALSE, FALSE), as.character(text[(obs+1),c("word")]), fixed = FALSE) == T),
                     as.numeric(gsub("^.*,", "",text[obs,c('bbox')])),NA)
    }
    if (is.na(chk_y)!=T){
      break
  }
}

#---------------------#
#--GETTING CHECK DATE-#
#---------------------#

im_lp         = load.image(paste0(output,"temp_image.png"))
pixel_data    = as.data.frame(im_lp)
po_pixel      = pixel_data[pixel_data$y >= (chk_y-473)
                         & pixel_data$y <= (chk_y+200)
                        & pixel_data$x  >= 0
                        & pixel_data$x  <= 1900,]

message = show_condition(as.cimg(po_pixel))

#-----------------------------------------------------------#
#--PUTTING CHECK DATE AS MISSING IF PAGE NOT CORRECTLY READ-#
#-----------------------------------------------------------#

if (message =='error'){
  check_date = ''
}

#-------------------------------------------------#
#--EXTRACTING CHECK DATE WITH PAGE CORRECTLY READ-#
#-------------------------------------------------#

if (message !='error'){
  masked_image  = as.cimg(po_pixel)
  masked_image = resize_tripleXY(masked_image)
  imager::save.image(masked_image,paste0(output,"chk_dt.png"))
  
  text       = tesseract::ocr(paste0(output,"chk_dt.png"), engine = eng)
  data_list  = strsplit(text, "\n")
  
  for (count in 1:lengths(data_list)[[1]]){
    check_date = strapplyc(data_list[[1]][count],"\\d+/\\d+/\\d+", simplify = TRUE)
    if (is.na(check_date[[1]][1])!=T){
      break
    }
    if (is.na(check_date[[1]][1])==T){
      check_date = strapplyc(data_list[[1]][count],"[a-zA-Z]{3,} \\d+, \\d+", simplify = TRUE)
      if (is.na(check_date[[1]][1])!=T){
        break
      }
    }
    if (is.na(check_date[[1]][1])==T){
      check_date = strapplyc(data_list[[1]][count],"\\d+-\\d+-\\d+", simplify = TRUE)
      if (is.na(check_date[[1]][1])!=T){
        break
      }
    }
  }
  
  if(is.na(check_date[[1]][1])){
    text       = tesseract::ocr(paste0(output,"temp_image.png"), engine = eng)
    data_list  = strsplit(text, "\n")
    #for (count in 1:lengths(data_list)[[1]]){
    for (count in 1:12){ 
      if (grepl(glob2rx("*Deposit Date*", FALSE, FALSE), data_list[[1]][count], fixed = FALSE) == T)
      {
        for (count_2 in (count+1):lengths(data_list)[[1]]){
          check_date = strapplyc(data_list[[1]][count_2],"\\d+/\\d+/\\d+",simplify = TRUE) 
          if (is.na(check_date[[1]][1])!=T){
            break
          }
          if (is.na(check_date[[1]][1])==T){
            check_date = strapplyc(data_list[[1]][count],"[a-zA-Z]{3,} \\d+, \\d+", simplify = TRUE)
            if (is.na(check_date[[1]][1])!=T){
              break
            }
          }
          if (is.na(check_date[[1]][1])==T){
            check_date = strapplyc(data_list[[1]][count],"\\d+-\\d+-\\d+", simplify = TRUE)
            if (is.na(check_date[[1]][1])!=T){
              break
            }
          }
        }
      }
    }
  }
}

#--------------------------#
#--EXTRACTING LOCKBOX NAME-#
#--------------------------#

if (i == 1){
   im_lp      = load.image(paste0(output,"temp_image.png"))
   pixel_data = as.data.frame(im_lp)
   po_pixel   = pixel_data[pixel_data$y >= 300
                            & pixel_data$y <= 800
                           & pixel_data$x  >= 100
                           & pixel_data$x  <= 1300,]
   masked_image = as.cimg(po_pixel)
   imager::save.image(masked_image,paste0(output,"lb_image.png"))
   text       = tesseract::ocr(paste0(output,"lb_image.png"), engine = eng)
   data_list  = strsplit(text, "\n")
      for (count in 1:lengths(data_list)[[1]]){
        if (grepl(glob2rx("*Name*", FALSE, FALSE), data_list[[1]][count], fixed = FALSE) == T)
          {
            lockbox_name = str_after_first(data_list[[1]][count],"Name:")
        }
      }
   im_lp1        = load.image(paste0(output,"temp_image.png"))
   pixel_data1    = as.data.frame(im_lp1)
   po_pixel1      = pixel_data1[pixel_data1$y >= 0
                              & pixel_data1$y <= 500
                              & pixel_data1$x  >= 1900
                              & pixel_data1$x  <= 3000,]
   masked_image1     = as.cimg(po_pixel1)
   imager::save.image(masked_image1,paste0(output,"temp_image1.png"))
   text1       = tesseract::ocr(paste0(output,"temp_image1.png"),engine = eng)
   data_list1  = strsplit(text1, "\n")
   for (count in 1:lengths(data_list1)[[1]]){
     if (grepl(glob2rx("*Amt*", FALSE, FALSE), data_list1[[1]][count], fixed = FALSE) == T)
     {
       total_amount = str_after_first(data_list1[[1]][count],"Amt")
     }
   }
}

if (exists("total_amount") == F){
  total_amount = ''
}


#------------------------------------------------------#
#--GETTING THE Y DIMENSION WHERE CHECQUE NUMBER EXISTS-#
#------------------------------------------------------#

text       = tesseract::ocr_data(paste0(output,"temp_image.png"), engine = eng)
  
for (obs in 1:dim(text)[1]){
        
  chk_y         = as.numeric(gsub("^.*,", "",text[text$word=='Chk#',c('bbox')]))  
    if (is.na(chk_y) == T){
      chk_y = ifelse((grepl(glob2rx("*Ch*", FALSE, FALSE), as.character(text[obs,c("word")]), fixed = FALSE) == T) &
                       (grepl(glob2rx("*\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d*", FALSE, FALSE), as.character(text[(obs+1),c("word")]), fixed = FALSE) == T),
                        as.numeric(gsub("^.*,", "",text[obs,c('bbox')])),NA)
    }
  if (is.na(chk_y) == T){
    chk_y = ifelse((grepl(glob2rx("*ch*", FALSE, FALSE), as.character(text[obs,c("word")]), fixed = FALSE) == T) &
                     (grepl(glob2rx("*\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d*", FALSE, FALSE), as.character(text[(obs+1),c("word")]), fixed = FALSE) == T),
                   as.numeric(gsub("^.*,", "",text[obs,c('bbox')])),NA)
  }
     if (is.na(chk_y)!=T){
          break
        }
      }
  
#--------------------------#
#--EXTRACTING CHECK AMOUNT-#
#--------------------------#

im_lp      = load.image(paste0(output,"temp_image.png"))
  
pixel_data = as.data.frame(im_lp)
      
amt_pixel  = pixel_data[pixel_data$y >= chk_y+50
                        & pixel_data$y <= chk_y+350
                        & pixel_data$x  >= 1900
                        & pixel_data$x  <= 2700,]
  
message = show_condition(as.cimg(amt_pixel))
  
  if (message =='error'){
    check_amount = '0'
  }
  
  if (message !='error'){
    masked_image = as.cimg(amt_pixel)
    imager::save.image(masked_image,paste0(output,"cbc_masked.png"))
    text       = tesseract::ocr(paste0(output,"cbc_masked.png"), engine = eng)
    data_list  = strsplit(text, "\n")
    
    for (counter in 1:lengths(data_list)[[1]]){
      if (grepl(glob2rx("*Amt*", FALSE, FALSE), data_list[[1]][counter], fixed = FALSE) == T)
      {
        check_amount = str_after_first(data_list[[1]][counter],"Amt")
        if (is.na(check_amount)!=T){
          break
        }
        
      }
    }
  }

#------------------------#
#--EXTRACTING PAYOR NAME-#
#------------------------#

po_pixel   = pixel_data[pixel_data$y >= chk_y-500
                        & pixel_data$y <= chk_y-100
                       & pixel_data$x  >= 0
                       & pixel_data$x  <= 900,]
message = show_condition(as.cimg(po_pixel))
  
if (message =='error'){
    payor_name = ''
  }
  
if (message !='error'){
    masked_image = as.cimg(po_pixel)
    
    imager::save.image(masked_image,paste0(output,"temp_image.png"))
    
    text       = tesseract::ocr(paste0(output,"temp_image.png"), engine = eng)
    data_list  = strsplit(text, "\n")
    payor       = read.csv(paste0(input,"payor_list.csv"))
    payor$Payor = trimws(payor$Payor,which = 'both')
    
    for (j in 1:lengths(data_list)){
      payor[,j+1] = stringsim(tolower(data_list[[1]][j]),tolower(payor$Payor))
    }
    
    message = show_condition(as.vector(apply(payor[,-c(1)],2, max)))
    
    if (message =='error'){
      a = as.vector(lapply(payor[-c(1)],max))
    }
    
    if (message !='error'){
      a = as.vector(apply(payor[,-c(1)],2, max))
    }
    
    name  = data_list[[1]][which.max(a)]
    
    payor = read.csv(paste0(input,"payor_list.csv"))
    payor$Payor = trimws(payor$Payor,which = 'both')
    payor$match = stringsim(tolower(name),tolower(payor$Payor))
    payor_check = payor[order(payor$match,decreasing = T),] 
    payor_check_d = payor_check[1:2,c('match')]
    diff = payor_check_d[1] - payor_check_d[2]
    if (payor_check$Payor[1] %in% c('Advocate Health Partners','Advocate Health Care','Advocate Health Centers')){
      if (diff >= 0.07){
        verify = 'No'
      }else{
        verify = 'Yes'
      }
    }else if (diff>=0.16){
      verify = 'No'
    }else{
      verify = 'Yes'
    }
    payor_name    = payor[which.max(payor$match),c("Payor")]
  }

  if (check_number == '' | check_date == '' | payor_name == '' | check_amount == '0'){
    verify = 'Yes'
  }
  service_month = ''
  interim_data  = data.frame(lockbox_name,
                             lockbox_date,
                             check_number,
                             check_date,
                             payor_name,
                             service_month,
                             check_amount,
                             total_amount,
                             verify)
      
      
  lb_data = rbind(lb_data,interim_data)
  check_number = ''
  check_date   = ''
  payor_name   = ''
  check_amount = '0'
  verify       = ''
}
#lb_data = unique(lb_data)
lb_data$check_amount = gsub(pattern = '[^0-9.]', replacement = '',lb_data$check_amount)
lb_data$total_amount = gsub(pattern = '[^0-9.]', replacement = '',lb_data$total_amount)
lb_data$check_number = trimws(lb_data$check_number,which = 'both')
lb_data$check_number = sub("^0+", "",lb_data$check_number)
sum(as.numeric(lb_data$check_amount))


  