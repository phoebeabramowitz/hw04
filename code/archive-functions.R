
library("stringr")
library("XML")
library("dplyr")
library('ggplot2')

#Q1.1
#tbl_html <-  readHTMLTable('http://cran.r-project.org/src/contrib/Archive/stringr')
#tbl_html

read_archive <- function(x){
  url <- str_c('http://cran.r-project.org/src/contrib/Archive/',x)
  readHTMLTable(url)$`NULL`
}
#Q1.2
#Mutate uses window functions
version_sizes <- function(x){
  sizes <- rep(0,length(x))
  for(i in 1:length(x)){
    if(str_detect(x[i],"K")){
      sizes[i] <- as.numeric(str_sub(x[i],end=-2))
    }
    else if(str_detect(x[i],"M")){
      sizes[i] <- as.numeric(str_sub(x[i],end=-2))*1000
    }
  }
  return(sizes)
}

clean_archive <- function(dat){
  dat <- na.omit(dat)
  dat <- dat[dat$Name !="Parent Directory",2:4]
  name_version <- str_split(dat$Name,"_")
  dat <- mutate(dat,name=name_version[[1]][1])
  dat <- mutate(dat,version=str_replace(name_version[[1]][2],'.tar.gz',""))
  dat <- mutate(dat, date=as.Date(str_sub(dat$`Last modified`, end=10)))
  dat <-mutate(dat,size=version_sizes(Size))
  dat[,4:7]
}


#Q1.3
plot_archive <- function(d){
  title <- d$name[1]
  ggplot()+
    geom_step(data=d, mapping=aes(x=date,y=size,group =1))+
    geom_point(data=d, mapping=aes(x=date,y=size))+
    ggtitle(paste0(title,":",' timeline of version sizes'))+
    ylab("Size(Kilobytes)")+
    theme_minimal()
}
plot_archive(clean_data)

