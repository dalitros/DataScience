library(DBI)
library(dplyr)
library(odbc)
library(Hmisc)
library(devtools)
library(ggplot2)
library(corrgram)
install.packages("dplyr")

############################################################################
#####   TABLE 1                                                         ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2016-03-09                                       ####
#####   Last Modified: 2018-12-19                                       ####
############################################################################

####################  FUNCTIONS  ###########################################
#### Usage:
####   x: character vector with the name of the variables
####   y: the name of the strata variable (optional)
####   rn: character vector with the text we want to replace the variable names
####   data: the dataset to be used
####   miss: include missing statistics: [0=none, 1=only for categorical variables, 2=for all variables]
####   excel: export the table to excel [0=no, 1=yes]
####   excel_file: the name of the excel file we want to save the table (optional)
####
###################

options(warn=-1)

Table1 <- function(x=NULL, y=NULL, rn=NULL, data=NULL, miss=3, catmiss=TRUE, formatted=TRUE, categorize=FALSE,
                   factorVars=NULL, maxcat=10, delzero=TRUE, decimals=1, messages=TRUE, excel=0, excel_file=NULL,
                   debug=FALSE) {
  ### define sub-functions
  Del <- NULL
  Pop <- NULL
  n <- NULL
  g1 <- function(var)c(Mean=mean(var,na.rm=TRUE), SD=stats::sd(var,na.rm=TRUE))
  g2 <- function(var)c(Median=stats::median(var,na.rm=TRUE), IQR=stats::quantile(var,c(0.25,0.75),na.rm=TRUE))
  msg <- NULL
  
  ### function for transforming variables to factors
  setFactors <- function(data=data, factorVars=factorVars, catmiss=catmiss, maxcat=maxcat) {
    if(is.null(factorVars)==TRUE) {
      aa <- sapply(sapply(data, unique), length)
      factorVars <- names(which(aa <= maxcat))
    }
    for (v in factorVars) {
      ct <- ifelse( ((is.null(factorVars)==FALSE & (v %in% factorVars)) | (is.null(factorVars)==TRUE & length(unique(data[[v]])) <= maxcat)),1,0)
      if (ct == 1) {
        data[[v]] <- factor(data[[v]])
        if(catmiss == TRUE & sum(is.na(data[[v]])==TRUE) > 0) {
          data[[v]] <- factor(data[[v]],levels=c(levels(data[[v]]),"Missing"))
          data[[v]][which(is.na(data[[v]])==TRUE)] <- "Missing"
        }
      }
    }
    return(data)
  }
  ### proceed to convert varibles to factors
  if (categorize == TRUE | is.null(factorVars)==FALSE ) {
    data <- setFactors(data, factorVars, catmiss, maxcat)
  }
  
  getSimpleTable  <- function(x=x, rn=rn, data=data, miss=miss, catmiss=catmiss,formatted=formatted,
                              categorize=categorize,maxcat=maxcat, delzero=delzero) {
    if (is.null(x)==TRUE) { x <- names(data)}
    if (is.null(rn)==TRUE) { rn <- x}
    ln <- length(x)
    pb <- utils::txtProgressBar(min=0,max=ln,style=3)
    msg <- NULL
    ### define the column names
    tableaaaa <- cbind(Del="Del",V1="Variables",V2="Categories",n="n","Population")
    tablebbbb <- cbind(Del="Del",V1="Variables",V2="Categories",n="n",val1="val1",val2="val2",val3="val3")
    tbl1 <- cbind(0,"Individuals","n",n=1, nrow(data))
    tbl2 <- cbind(0,"Individuals","n",n=1, nrow(data),NA,NA)
    tableaaaa <- rbind(tableaaaa,tbl1)
    tablebbbb <- rbind(tablebbbb,tbl2)
    q <- 1
    n <- 1
    ii <- 1
    for (v in x)
    {
      if (v %in% names(data)) {
        ### define if the actual variable has to be treated as numeric or factor
        ct <- ifelse(is.numeric(data[[v]])==TRUE & categorize==TRUE &
                       ((is.null(factorVars)==FALSE & (v %in% factorVars)) |
                          (is.null(factorVars)==TRUE & length(unique(data[[v]])) <= maxcat)),1,0)
        ### treat as numeric
        if (length(unique(data[v]))==0) {
          if (messages==TRUE) {
            msg <- c(msg, paste("The variable",v,"has no data... avoided"))
          }
        } else if (inherits(data[[v]], "Date")==TRUE) {
          if (messages==TRUE) {
            msg <- c(msg, paste("The variable",v,"is a date. Dates are not allowed in Table1... avoided"))
          }
        } else if (is.numeric(data[[v]])==TRUE & ct==0) {
          ## report mean and standard deviation
          t_n <- g1(data[[v]])
          tp <- paste(format(round(t_n[1],decimals),nsmall=1,big.mark=",")," (", format(round(t_n[2],decimals),nsmall=1,big.mark=","),")",sep="")
          tbl1 <- cbind(0,rn[q],"Mean (SD)",n=1, tp)
          tbl2 <- cbind(0,rn[q],"Mean (SD)",n=1,t_n[1],t_n[2],NA)
          tableaaaa <- rbind(tableaaaa,tbl1)
          tablebbbb <- rbind(tablebbbb,tbl2)
          ## report median and Interquartile ranges (25%,75%)
          t_n <- g2(data[[v]])
          tp <- paste(format(round(t_n[1],decimals),nsmall=1,big.mark=",")," (", format(round(t_n[2],decimals),nsmall=1,big.mark=","),"-", format(round(t_n[3],decimals),nsmall=1,big.mark=","), ")",sep="")
          tbl1 <- cbind(0,rn[q],"Median (IQR)",n=2, format(tp,big.mark=","))
          tbl2 <- cbind(0,rn[q],"Median (IQR)",n=2,t_n[1],t_n[2],t_n[3])
          tableaaaa <- rbind(tableaaaa,tbl1)
          tablebbbb <- rbind(tablebbbb,tbl2)
          ## report number and percent of missing
          if (miss >= 1) {
            #datams <- subset(data,is.na(data[[v]])==TRUE)
            datams <- data %>% filter(is.na(v)==TRUE)
            if (nrow(datams)>0) {
              data$cnt <- 1
              datams$cnt <- 1
              t_n <- table(data$cnt)
              t_m <- sum(datams$cnt)
              tp <- paste(format(t_m,big.mark=",")," (",format(round((t_m/t_n)*100,decimals),nsmall=1,big.mark=","),"%)",sep="")
              tbl1 <- cbind(0,rn[q],"Missing (%)",n=3, tp)
              tbl2 <- cbind(0,rn[q],"Missing (%)",n=3, t_m, (t_m/t_n)*100, NA)
            } else {
              tbl1 <- cbind(1,rn[q],"Missing (%)",n=3, " -- ")
              tbl2 <- cbind(1,rn[q],"Missing (%)",n=3, NA, NA, NA)
            }
            tableaaaa <- rbind(tableaaaa,tbl1)
            tablebbbb <- rbind(tablebbbb,tbl2)
          }
        } else {
          t_n <- table(data[[v]])
          ttotal <- sum(t_n)
          nm <- row.names(t_n)
          for (f in 1:length(nm)) {
            del1 <- ifelse(length(nm)==2 & (nm[f]=="No" | nm[f]=="no" | nm[f]==0 | nm[f]=="0" | nm[f]=="None" | nm[f]=="none"),1,0)
            tp <- t_n[f] / ttotal * 100
            pct <- paste(format(round(t_n[f],decimals),nsmall=0,big.mark=",")," (", format(round(tp,decimals),nsmall=1,big.mark=","), "%)",sep="")
            tbl1 <- cbind(del1,rn[q],nm[f],n=f, pct)             ########### delete rows 0/1 !!!!!!!!!
            tbl2 <- cbind(del1,rn[q],nm[f],n=f, t_n[f], tp, NA)  ########### delete rows 0/1 !!!!!!!!!
            tableaaaa <- rbind(tableaaaa,tbl1)
            tablebbbb <- rbind(tablebbbb,tbl2)
          }
          if (miss >= 2 & catmiss==FALSE ) {
            #datams <- subset(data,is.na(data[[v]])==TRUE)
            datams <- data %>% filter(is.na(v)==TRUE)
            if (nrow(datams)>0) {
              data$cnt <- 1
              datams$cnt <- 1
              t_n <- table(data$cnt)
              t_m <- sum(datams$cnt)
              tp <- paste(format(t_m,big.mark=",")," (",format(round((t_m/t_n)*100,decimals),nsmall=1,big.mark=","),"%)",sep="")
              tbl1 <- cbind(0,rn[q],"Missing (%)",n=f, tp)
              tbl2 <- cbind(0,rn[q],"Missing (%)",n=f, t_m, (t_m/t_n)*100, NA)
            } else {
              tbl1 <- cbind(1,rn[q],"Missing (%)",n=f, " -- ")
              tbl2 <- cbind(1,rn[q],"Missing (%)",n=f, NA, NA, NA)
            }
            tableaaaa <- rbind(tableaaaa,tbl1)
            tablebbbb <- rbind(tablebbbb,tbl2)
          }
        }
      } else {
        if (messages==TRUE) {
          msg <- c(msg, paste("The variable",v,"doesn't exists in the dataset... avoiding"))
        }
      }
      q <- q + 1
      if(debug==FALSE) {
        utils::setTxtProgressBar(pb,ii)
        ii <- ii + 1
      } else {
        message(v)
      }
    }
    if(formatted==TRUE) {
      return(tableaaaa)
    } else {
      return(tablebbbb)
    }
    close(pb)
  }
  
  pvals <- function(x=x,y=y,rn=rn,data=data,categorize=categorize,maxcat=maxcat) {
    ptab <- NULL
    if (is.null(y)==FALSE) {
      if (y %in% names(data)) {
        if (is.null(x)==TRUE) { x <- names(data)}
        if (is.null(rn)==TRUE | length(rn)<2) {rn <- x}
        q <- 1
        ptab <- cbind(V="Variables",pval="pval", n="n")
        
        ln <- length(x)
        ii <- 0
        pb <- utils::txtProgressBar(min=0,max=ln,style=3)
        
        for (v in x) {
          if (v %in% names(data)) {
            ct <- ifelse(is.numeric(data[[v]])==TRUE & categorize==TRUE & length(unique(data[[v]])) <= maxcat,1,0)
            if (is.numeric(data[[y]])==TRUE & categorize==TRUE & length(unique(data[[y]])) <= maxcat) {
              data[[y]] <- as.factor(data[[y]])
            } else if (is.numeric(data[[y]])==TRUE) {
              if (messages==TRUE) {
                msg <- c(msg, paste("The variable",y,"is not a factor. Please convert to factor or change the 'categorize' flag to TRUE."))
              }
              pval <- "Please rerun!!!"
            }
            if (is.numeric(data[[v]])==TRUE & length(unique(data[[v]])) > 1 & ct == 0) {
              ### first check for homoscedasticity
              tryCatch({
                if (stats::bartlett.test(data[[v]], data[[y]])[3] >= 0.05) {
                  pval <- suppressMessages(round(as.numeric(suppressMessages(car::Anova(stats::lm(data[[v]] ~ data[[y]])))[1, 4]), 3))
                } else {
                  pval <- suppressMessages(round(as.numeric(suppressMessages(car::Anova(stats::lm(data[[v]] ~ data[[y]]), white.adjust = TRUE))[1, 3]), 3))
                }
              }, warning = function(w) {
                suppressWarnings(w)
                #ww <- "suppress warnings..."
              }, error = function(e) {
                pval <- "---"
              })
            } else if (length(unique(data[[v]]))==1) {
              pval <- NA
            } else {
              if(length(unique(data[[v]])) < 15) {
                if (min(table(data[[v]],data[[y]])) > 5) {
                  pval <- round(as.numeric(stats::chisq.test(data[[v]],data[[y]])$p.val),3)
                } else {
                  if(min(table(data[[v]],data[[y]]))==0) {
                    #in cases where there are cells with zero, we use Fisher's exact test
                    tryCatch(
                      pval <- round(as.numeric(stats::fisher.test(data[[v]],data[[y]], workspace=1e9)$p.val),3),
                      error = function(e) {msg <- c(msg,paste0("Unable to calcualte the Fisher test for variables ",v," and ",y))})
                  } else {
                    pval <- round(as.numeric(stats::kruskal.test(data[[v]],data[[y]], workspace=1e9)$p.val),3)
                  }
                }
              } else {
                pval <- NA
              }
            }
            ptab <- rbind(ptab,cbind(V=rn[q],pval=pval,n=2))
          }
          if(debug==FALSE) {
            utils::setTxtProgressBar(pb,ii)
            ii <- ii + 1
          }
          q <- q + 1
        }
      }
    }
    return(ptab)
  }
  ####################### Begin analysis
  ##### check for x's witch have one unique values...get them out...
  vv <- NULL
  j <- 0
  jj <- NULL
  for(v in x) {
    if(length(unique(data[[v]])) < 2) {
      vv <- c(vv,v)
      j <- j + 1
      jj <- c(jj,j)
    }
  }
  warning(paste("The following variables have unique values and will not be included in the analysis:",vv))
  x <- setdiff(x, vv)
  if(is.null(rn)==FALSE & length(jj)>0) {
    rn <- rn[-jj]
  }
  
  ##### if y is null then make a simple table
  tabaaa1 <- getSimpleTable(x=x, rn=rn, data=data, miss=miss, catmiss=catmiss,formatted=formatted,categorize=categorize,maxcat=maxcat, delzero=delzero)
  tabaaa1 <- tibble::as_tibble(tabaaa1)
  ############################  CHANGE TO 5 !!!!!!!!!!!!!!
  if(length(tabaaa1) > 5) {
    names(tabaaa1) <- c("Del","V1","V2","n","Pop","pop2","pop3")
  } else {
    names(tabaaa1) <- c("Del","V1","V2","n","Pop")
  }
  ##### if y has two levels, then make a compound comparison
  if (is.null(y)==FALSE){
    if (y %in% names(data)) {
      if (is.factor(data[[y]])==FALSE) {
        if (length(levels(factor(data[[y]]))) > 8) {
          if (messages==TRUE) {
            message("The dependent variable has more than 8 levels, table too large!")
          }
        } else if(min(table(data[[y]]))==0) {
          message("The dependent variable has one or more levels with no individuals assigned!")
        } else {
          data[[y]] <- factor(data[[y]])
        }
      }
      if (length(levels(data[[y]])) >= 2) {
        for (lv in levels(data[[y]])) {
          #dtsub <- subset(data, data[[y]]==lv)
          dtsub <- data %>% filter(y==lv)
          tab <- getSimpleTable(x=x, rn=rn, data=dtsub, miss=miss, catmiss=catmiss, formatted=formatted,categorize=categorize,maxcat=maxcat, delzero=delzero)
          tab <- data.frame(tab)
          ############################  CHANGE TO 5 !!!!!!!!!!!!!!
          if(length(tab) > 5) {
            names(tab) <- c("Del","V1","V2","n",paste0(lv,"_1"),paste0(lv,"_2"),paste0(lv,"_3"))
          } else {
            names(tab) <- c("Del","V1","V2","n",lv)
          }
          ############################  CHANGE TO 5 !!!!!!!!!!!!!!
          tab[1,5] <- lv
          tabaaa1 <- suppressMessages(dplyr::left_join(tabaaa1, tab))
        }
        # what to do with dichotomous variables? We remove the "Zero" label...
        # clean unnecesary rows
        if (delzero == TRUE) {
          tabaaa1 <- dplyr::filter(tabaaa1,Del==0)
        }
        ### calculate the p-value
        ptab <- data.frame(pvals(x=x,y=y,rn=rn,data=data,categorize=categorize,maxcat=maxcat))
        names(ptab) <- c("V1","pval","n")
        tabaaa1 <- suppressMessages(dplyr::left_join(tabaaa1, ptab))
        tabaaa1 <- dplyr::filter(tabaaa1,Pop != " -- ") #%>%
      }
    }
  }
  
  tabaaa1 <- dplyr::select(tabaaa1,-n)
  tabaaa1 <- dplyr::select(tabaaa1,-Del)
  
  ##### Join the tables...
  #Sys.setenv(JAVA_HOME="")
  if (excel==1) {
    #wb <- xlsx::createWorkbook()
    #sheet1 <- xlsx::createSheet(wb, sheetName="Table 1")
    #xlsx::addDataFrame(tabaaa1,sheet1)
    #### save and close the workbook
    #xlsx::saveWorkbook(wb, excel_file)
    writexl::write_xlsx(tabaaa1,excel_file)
    return(tabaaa1)
  } else {
    return(tabaaa1)
  }
}

########################## END Table1 ###############

###################################################
####   Explore Data 
###################################################

exploreData <- function(data=data, y=NULL, rn=NULL, factorSize=10, dir=tempdir(), debug=FALSE, ...) {
  
  whatVarType <- function(var) {
    suppressWarnings(if (var=="integer" | var=="numeric") {
      return(1)
    } else if (var=="factor" | var=="character") {
      return(2)
    } else if (var=="Date" | "POSIXct" %in% var[[1]]) {
      return(3)
    } else {
      return(0)
    })
  }
  
  drawHistogram <- function(imgname=imgname, x=x) {
    d=stats::density(x, kernel = "gaussian",na.rm=TRUE)
    breakstar=(max(x,na.rm=TRUE) -min(x,na.rm=TRUE))/d$bw
    h=graphics::hist(x, breaks=breakstar)
    graphics::plot(h,main="",xlab=imgname)
    yfit<-seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),length=40)
    ffit<-stats::dnorm(yfit,mean=mean(x,na.rm=TRUE),sd=stats::sd(x,na.rm=TRUE))
    ffit <- ffit*diff(h$mids[1:2])*length(x)
    lines(yfit, ffit, col="blue", lwd=2)
  }
  
  drawFakeGraph <- function(imgname=imgname) {
    graphics::plot.window(xlim = c(0,0),ylim = c(0,0))
  }
  
  drawBars <- function(imgname=imgname, x=x) {
    graphics::plot(x)
  }
  
  drawGraphOne <- function(imgname=imgname, numVar=x, vartype=1) {
    if(vartype==1) {
      drawHistogram(imgname,numVar)
    } else if(vartype==2) {
      drawBars(imgname,numVar)
    } else {
      drawFakeGraph(imgname)
    }
  }
  
  getContinuousStats <- function(x) {
    N <- length(x)
    n <- length(x[which(is.na(x)==FALSE)])
    pct <- formatC(n/N * 100)
    nmiss <- length(x[which(is.na(x)==TRUE)])
    npct <- formatC(nmiss/N *100)
    ma <- mean(x, na.rm=TRUE)
    s <- stats::sd(x, na.rm=TRUE)
    me <- formatC(stats::median(x, na.rm=TRUE))
    q1 <- formatC(stats::quantile(x,1/4, na.rm=TRUE))
    q3 <- formatC(stats::quantile(x,3/4, na.rm=TRUE))
    mn <- formatC(min(x, na.rm=TRUE))
    mx <- formatC(max(x, na.rm=TRUE))
    html <- paste("<div class='Cell' style='align: top;'> <u>Data type</u>: Continuous <p> <u>Data length</u>: ",n ,"/", N, " (", pct, "%) <br> <u>Missing</u>: ",
                  nmiss, " (", npct, "%)<p> <u>Mean</u>: ", formatC(ma), "\t <u>StdDev</u>: ", formatC(s), "<br><u>Median</u>: ",me,
                  "\t <u>IQR</u>: ", q1, "-", q3, "<br><u>Min</u>: ", mn, "\t <u>Max</u>: ", mx, "</div>")
    return(html)
  }
  
  getCategortyStats <- function(x) {
    N <- length(x)
    n <- length(x[which(is.na(x)==FALSE)])
    pct <- formatC(n/N * 100)
    nmiss <- length(x[which(is.na(x)==TRUE)])
    npct <- formatC(nmiss/N *100)
    l <- levels(x)
    s <- summary(x)
    htm <- "<ul>"
    if (length(l) < 5) {
      for (lv in l) {
        htm <- paste(htm, "<li><u>", lv, "</u>: ", s[[lv]], "</li>")
      }
      htm <- paste(htm,"</ul>")
    }
    html <- paste("<div class='Cell'> <u>Data type</u>: Categorical Data <p> <u>Data length</u>: ",n, "/", N, " (", pct, "%) <br> <u>Missing</u>: ",
                  nmiss, " (", npct, "%) <p> <u>Number of levels</u>: ", length(l), "<br>", htm, "</div>")
    return(html)
  }
  
  getDatesStats <- function(x) {
    N <- length(x)
    n <- length(x[which(is.na(x)==FALSE)])
    pct <- formatC(n/N * 100)
    nmiss <- length(x[which(is.na(x)==TRUE)])
    npct <- formatC(nmiss/N *100)
    s <- summary(x)
    html <- paste("<div class='Cell'> <u>Data type</u>: Date <p> <u>Data length</u>: ",n, "/", N, " (", pct, "%) <br> <u>Missing</u>: ",
                  nmiss, " (", npct, "%) <p> <u>Min date</u>: ", min(x, na.rm=TRUE), "<br><u>Max date</u>:",max(x, na.rm=TRUE) , "</div>")
    return(html)
  }
  
  getStats <- function(numVar=x, vartype=1) {
    if(vartype==1) {
      html <- getContinuousStats(numVar)
    } else if(vartype==2) {
      html <- getCategortyStats(numVar)
    } else if (vartype==3) {
      html <- getDatesStats(numVar)
    } else {
      html <- "<div class='Cell'></div>"
    }
    return(html)
  }
  
  getOutliers <- function(x) {
    bp <- graphics::boxplot(x,plot=FALSE)
    return(bp$out)
  }
  
  getOutlierGraph <- function(x) {
    #  mod <- tryCatch({
    outl <- getOutliers(x)
    df <- data.frame(x=x, cl=1)
    if(length(outl)>0) {
      df$cl[which(df$x %in% outl)] <- 2
    }
    #pl <- stats::scatter.smooth(df$x,col=df$cl)
    pl <- tryCatch({
      stats::scatter.smooth(df$x,col=df$cl,xlab="index")
    }, warning = function(w) {
      suppressWarnings(w)
      #n <- "warning!"
    }, error = function(e) {
      n <- "error!"
    }, finally = {
      graphics::plot(df$x ~ row.names(df),col=df$cl,xlab="index")
    })
    ma <- mean(x, na.rm=TRUE)
    s <- stats::sd(x, na.rm=TRUE)
    graphics::abline(h=ma-(2*s), col="red", lty=2)
    graphics::abline(h=ma+(2*s), col="red", lty=2)
    #  }, error = function(e) {
    #    pl <- drawFakeGraph("none")
    #  })
    return(pl)
  }
  
  getScatterGraph <- function(df=data,x,y,dtype=1) {
    #  mod <- tryCatch({
    if(dtype==1) {
      pl <- ggplot2::ggplot(df) + ggplot2::geom_smooth(ggplot2::aes(x=data[[x]], y=data[[y]]), method="loess") + ggplot2::xlab(x) + ggplot2::ylab(y)
    } else {
      pl <- ggplot2::ggplot(df) + ggplot2::geom_boxplot(ggplot2::aes(y=data[[x]], color=data[[y]])) + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::labs(color=y)
    }
    return(pl)
  }
  
  getOutliersHtml <- function(imgname=imgname, x=x, srcdir=srcdir) {
    bp <- getOutliers(x)
    if (length(unique(bp)) > 10) {
      xtrm <- paste("There are ", length(unique(bp)), " outlier values")
    } else if (length(unique(bp)) == 0) {
      xtrm <- "No outlier values found"
    } else {
      xtrm <- paste(formatC(unique(bp)), collapse=', ' )
    }
    #imgsrc = paste(paste0(srcdir,"/fig/"),imgname, "_2.png",sep="")
    imgsrc = paste(paste0("fig/"),imgname, "_2.png",sep="")
    html <- paste0("<div class='Cell'><img class='origimg' src='",imgsrc,"' height='150' width='250'><br> <u>Outlier values</u>: <br> ", xtrm, "</div>")
    return(html)
  }
  ################## Prepare for the report ###################
  #report <- paste(mydir,"/report",sep="")
  
  ################## Check for values for rn ##################
  if(!is.null(rn)) {
    if(length(rn)!=ncol(data)) {
      message("the value of the 'rn' argument was avoided because it does not have the same number of columns of the dataframe")
      rn <- NULL
    }
    xname <- rn
    names(xname) <- names(data)
  } else {
    xname <- NULL
  }
  
  report <- dir
  if (!file.exists(report)) {
    dir.create(report)
  }
  fig <- paste(report,"/fig",sep="")
  if (!file.exists(fig)) {
    dir.create(fig)
  }
  srcdir <- report
  
  # determine which columns are integer
  int_col <- which(sapply(data, is.integer))
  int_col <- c(int_col,(which(sapply(data, is.numeric))))
  mi <- vector()
  # find only those integers with less than 10 unique values and convert to factor
  for (li in int_col) {
    if (length(unique(data[,li])) < factorSize) {
      mi <- c(mi,li)
      if (is.factor(data[,li]) == FALSE) {
        data[,li] <- factor(data[,li])
      }
    }
  }
  
  str_col <- which(sapply(data, is.character))
  mi <- vector()
  # find only those integers with less than 10 unique values and convert to factor
  for (li in str_col) {
    mi <- c(mi,li)
    data[,li] <- factor(data[,li])
  }
  
  # create the html report page
  myhtml <- paste(report,"/report.html",sep="")
  cat("<!DOCTYPE html>
      <html>
      <head>
      <title>Data Visualization</title>
      <meta http-equiv='Content-Type' content='text/html; charset=UTF-8' />
      <link rel='stylesheet' href='http://code.jquery.com/mobile/1.4.5/jquery.mobile-1.4.5.min.css'>

      <script src='http://code.jquery.com/jquery-1.10.2.js'></script>
      <script>
      $(document).ready(function(){
      $('.onetoone').hide();
      });

      $(function() {
      $('.origimg').click(function(e) {
      $('#popup_img').attr('src',$(this).attr('src'));
      $('#myContainer').hide();
      var pos = $(document).scrollTop();
      $('#myContainer').css({'top':pos+20,'left':250, 'position':'absolute', 'border':'1px solid black', 'padding':'0px'});
      $('#myContainer').show();
      });
      $('#myContainer').click(function(e) {
      $('#myContainer').hide();
      });

      $('#myform2').submit(function(e) {
      e.preventDefault();
      });

      $('#onetoone').on('click',function() {
      console.log('onetone button - 1');
      $('#onetoone').hide();
      $('#aslist').show();
      // To show only individual rows:
      $('.Row').hide();
      $('.onetoone').show();
      // then we iterate
      var i = $('.Row').length;
      // Then we iterate
      var nxt = $('#idx').val();
      if (nxt < i & nxt >0) {
      $('.Row').hide();
      $('.Row').eq(0).show();
      $('.Row').eq(nxt).show();
      } else {
      $('#idx').val(1)
      }
      console.log('onetone button - 2');
      });

      $('#aslist').on('click',function() {
      console.log('aslist button - 1');
      $('#onetoone').show();
      $('#aslist').hide();
      $('.onetoone').hide();
      $('.Row').show();
      console.log('aslist button - 2');
      });

      $('#less').on('click',function(){
      console.log('less button - 1');
      var i = $('.Row').length;
      var nxt = parseInt($('#idx').val(),10) - 1;
      if (nxt < i & nxt >0) {
      $('#idx').val(nxt)
      $('.Row').hide();
      $('.Row').eq(0).show();
      $('.Row').eq(nxt).show();
      } else {
      $('#idx').val(1)
      }
      console.log('less button - 2');
      });

      $('#more').on('click',function(){
      console.log('more button - 1');
      var i = $('.Row').length;
      var nxt = parseInt($('#idx').val(),10) + 1;
      if (nxt < i & nxt >0) {
      $('#idx').val(nxt)
      $('.Row').hide();
      $('.Row').eq(0).show();
      $('.Row').eq(nxt).show();
      } else {
      $('#idx').val(i)
      }
      console.log('more button - 2');
      });

      $('#idx').on('change', function(){
      console.log('idx changed - 1');
      var i = $('.Row').length;
      var nxt = $('#idx').val();
      if (nxt < i & nxt >0) {
      $('#idx').val(nxt)
      $('.Row').hide();
      $('.Row').eq(0).show();
      $('.Row').eq(nxt).show();
      } else {
      $('#idx').val(i)
      }
      console.log('idx changed - 2');
      });
      });

      </script>

      <style type='text/css'>
      .Table
      {
      display: table;
      }
      .Title
      {
      display: table-caption;
      text-align: center;
      font-weight: bold;
      font-size: larger;
      background-color:#4C6F50;
      color: #fff;
      }
      .Row
      {
      display: table-row;
      }
      .Row:nth-child(even) {
        background-color: #56882433;
      }
      .Cell
      {
      display: table-cell;
      border: solid;
      border-width: thin;
      padding-left: 5px;
      padding-right: 5px;
      vertical-align: top;
      font-family: Arial, Helvetica, sans-serif;
      font-size: 14px;
      }
      </style>

      </head>

      <body>
      <div id='pageone' data-role='main' class='ui-content'>
      ", file = myhtml, sep='\n',append=FALSE)
  
  html <- paste("<p><p><h1> Data Visualization & Exploration </h1>
                <form>
                <input type='button' id='onetoone' value='Show as Cards'>
                <input type='button' id='aslist' class='onetoone' value='Show as List'>
                </form>
                <p>
                ")
  cat(html, file = myhtml, sep='\n', append=TRUE)
  # begin table
  alt1 <- ifelse(is.null(y)== TRUE, "", "<div class='Cell Title'> Dependent <br> Variable <br> Distribution </div>")
  html <- paste("<p><p>
                <div class='Table'>
                <div class='Row'>
                <div class='Cell Title'> Variable </div>
                <div class='Cell Title'> Distribution </div>
                <div class='Cell Title'> Descriptive <br> Statistics</div>
                <div class='Cell Title'> Outliers </div>"
                , alt1,
                "</div>")
  cat(html, file = myhtml, sep='\n', append=TRUE)
  
  #### determinate the type of each variable...
  data_types <- sapply(sapply(data, class), whatVarType)
  ln <- length(data)
  ii <- 0
  pb <- utils::txtProgressBar(min=0,max=ln,style=3)
  for(x in names(data)) {
    
    ## check if the value has at least more than one unique value...
    if(length(unique(data[[x]])) < 2) {
      message(paste("The variable",x,"has less than two unique values, so will not be included"))
    } else {
      
      if(debug==TRUE) {
        message(x)
      } else {
        pb <- utils::txtProgressBar(min=0,max=ln,style=3)
      }
      
      html <- paste("<div class='Row'><div class='Cell'><b>",x,"</b><p>",xname[x],"</p></div>")
      
      cat(html, file = myhtml, sep='\n', append=TRUE)
      #### initialize the first graph
      imgname = paste(fig,"/",x, "_1.png",sep="")
      #imgsrc = paste(paste0(srcdir,"/fig/"),x, "_1.png",sep="")
      imgsrc = paste("fig/",x, "_1.png",sep="")
      ### send the data with the type to generate the correct graph..
      grDevices::png(imgname)
      drawGraphOne(x, data[[x]], data_types[x])
      grDevices::dev.off()
      html <- paste0("<div class='Cell'><img class='origimg'  src='",imgsrc,"' height='150' width='150'><br></div>")
      cat(html, file = myhtml, sep='\n', append=TRUE)
      
      # second, show the statistics
      html <- getStats(data[[x]],data_types[x])
      cat(html, file = myhtml, sep='\n', append=TRUE)
      
      # third, determine the outliers
      imgname = paste(fig,"/",x, "_2.png",sep="")
      if(data_types[x]==1) {
        grDevices::png(imgname)
        getOutlierGraph(data[[x]])
        grDevices::dev.off()
        html <- getOutliersHtml(x,data[[x]],srcdir)
      } else {
        html <- "<div class='Cell'></div>"
      }
      cat(html, file = myhtml, sep='\n', append=TRUE)
      
      # fourth, if y is assigned, make a corresponding plot
      if(is.null(y)==FALSE) {
        imgname = paste(fig,"/",x, "_3.png",sep="")
        #imgsrc = paste(paste0(srcdir,"/fig/"),x, "_3.png",sep="")
        imgsrc = paste("fig/",x, "_3.png",sep="")
        grDevices::png(imgname)
        ### scatter.smooth(data[[x]] ~ data[[y]])
        #suppressWarnings(getScatterGraph(data,x,y,data_types[y]))
        plot(getScatterGraph(data,x,y,data_types[y]))
        grDevices::dev.off()
        html <- paste0("<div class='Cell'><img class='origimg' src='",imgsrc,"' height='150' width='150'><br></div>")
        cat(html, file = myhtml, sep='\n', append=TRUE)
      }
      html <- paste("</div>")
      cat(html, file = myhtml, sep='\n', append=TRUE)
      
      if(debug==FALSE) {
        utils::setTxtProgressBar(pb,ii)
        ii <- ii + 1
      }
    }
  }
  html <- paste("</div>")
  cat(html, file = myhtml, sep='\n', append=TRUE)
  # end table
  html <- paste("</div>
                <div data-role='popup' id='myContainer' style='display: none;'>
                <img id='popup_img' src='' />
                </div>
                </div>
                </div>
                </div>
                <p>
                <div class='onetoone'>
                <form id='myform2'>
                <span> <input type='button' id='less' value=' << '> </span>
                <span> <input id='idx' name='idx' value='1'></input></span>
                <span> <input type='button' id='more' value=' >> '> </span>
                </form>
                </div>
                <p>
                </body></html>
                ")
  cat(html, file = myhtml, sep='\n', append=TRUE)
  ## call the default browser or the one which is open (if any)
  browseURL(myhtml)
}

###################### END exploreData ###############


############################################################################
#####   GET THE MISSINGNESS OF A DATASET                                ####
#####   Author: Tomas Karpati M.D.                                      ####
#####   Creation date: 2017-05-07                                       ####
############################################################################
### search for the number & % of missinf
### then count the number of rows with complete data
getMissingness <- function(data, getRows=FALSE) {
  #utils::globalVariables(c("desc","na_count","na_cnt","rn","pred","dc"))
  desc <- na_count <- na_cnt <- rn <- pred <- dc <- NULL
  l <- nrow(data)
  vn <- names(data)
  ### copy the dataset and replace the NAs by 1 else 0
  nadf <- data
  cnt <- NULL
  miss <- function(x) return(sum(is.na(x) ))
  for(n in vn) {
    nadf[[n]] <- ifelse(is.na(nadf[[n]])==TRUE,1,0)
    cnt <- rbind(cnt, data.frame(n,sum(nadf[[n]])))
  }
  names(cnt) <- c("var","na_count")
  cnt$rate <- round((cnt$na_count / nrow(nadf))*100,1)
  ### now sum by column
  nadf$na_cnt <- 0
  nadf$na_cnt <- rowSums(nadf)
  ### order descending the count of mossings and leave only those with missings
  cnt <- cnt %>%
    dplyr::arrange(desc(na_count)) %>%
    dplyr::filter(na_count>0)
  #totmiss <- nadf %>% dplyr::filter(na_cnt==0) %>% dplyr::tally()
  totmiss <- nadf %>% dplyr::filter(na_cnt==0) %>% dplyr::summarise(n=n())
  idx <- NULL
  msg <- (paste("This dataset has ", as.character(totmiss), " (",as.character(round(totmiss/nrow(data)*100,1)),"%)" ," complete rows. Original data has ",nrow(data)," rows.",sep=""))
  ### check id needs to return the row indexes
  if(getRows==TRUE & totmiss != 0) {
    nadf$rn <- seq_len(nrow(data))
    idx <- nadf %>% dplyr::filter(na_cnt==0) %>% dplyr::select(rn)
  }
  message(list(head(cnt,n=10), msg))
  return(list(missingness=cnt, message=msg, rows=idx$rn))
}


####################### READ FILE:
movie_ff <- read.csv("~/DataScience/data/FF_Shiri.csv")

####################### function for most frequent value:
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

####################### Fix the type of the columns:
movie_ff$runtime <- as.numeric(movie_ff$runtime)
movie_ff$revenue <- as.numeric(movie_ff$revenue)
movie_ff$actors_cnt <- as.numeric(movie_ff$actors_cnt)
movie_ff$producers_cnt <- as.numeric(movie_ff$producers_cnt)
movie_ff$actor1_prev_revenue <- as.numeric(movie_ff$actor1_prev_revenue)
movie_ff$depart_Camera <- as.numeric(movie_ff$depart_Camera)
movie_ff$depart_Directing <- as.numeric(movie_ff$depart_Directing)
movie_ff$depart_Production <- as.numeric(movie_ff$depart_Production)
movie_ff$depart_Writing <- as.numeric(movie_ff$depart_Writing)
movie_ff$depart_Crew_female <- as.numeric(movie_ff$depart_Crew_female)
movie_ff$depart_Editing_female <- as.numeric(movie_ff$depart_Editing_female)
movie_ff$depart_Sound_female <- as.numeric(movie_ff$depart_Sound_female)
movie_ff$original_language <- factor(movie_ff$original_language)
movie_ff$depart_Writing <- factor(movie_ff$depart_Writing)

####################### EDA ANALYSIS:
class(movie_ff)
str(movie_ff)

####################### USE MECHKAR FOR EXPLORE_DATA:
exploreData(movie_ff)

####################### Analyze revenue:
summary(movie_ff$revenue, na.rm=TRUE)
table(movie_ff$revenue)
sum(is.na(movie_ff$revenue))
sd(movie_ff$revenue, na.rm = TRUE)
nrow(table(movie_ff$revenue))
max(table(movie_ff$revenue))
calculate_mode(movie_ff$revenue)
hist(movie_ff$revenue, breaks = 100, na.rm = TRUE)
boxplot(movie_ff$revenue, na.rm = TRUE)
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$revenue, probs=c(.25, .75), na.rm = TRUE)
iqr<-IQR(movie_ff$revenue, na.rm=TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$revenue, plot=FALSE)$out
#### check distribution without outliers
movie_ff$revenue2 <- movie_ff$revenue
movie_ff$revenue2[movie_ff$revenue2 < lower | movie_ff$revenue2 > higher] <- NA
summary(movie_ff$revenue2)
hist(movie_ff$revenue2, breaks = 400, na.rm = TRUE)
boxplot(movie_ff$revenue2, na.rm = TRUE)

####################### Analyze original_language:
summary(movie_ff$original_language)
sort(table(movie_ff$original_language))
sum(is.na(movie_ff$original_language))
nrow(table(movie_ff$original_language))
max(table(movie_ff$original_language))
barplot(sort(table(movie_ff$original_language), decreasing = TRUE), col="Blue")
barplot(movie_ff$revenue~movie_ff$original_language)

####################### Analyze popularity_group:
summary(movie_ff$popularity_group)
sum(is.na(movie_ff$popularity_group))
nrow(table(movie_ff$popularity_group))
max(table(movie_ff$popularity_group))
calculate_mode(movie_ff$popularity_group)
boxplot(movie_ff$popularity_group)
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$popularity_group, probs=c(.25, .75), na.rm = TRUE)
iqr<-IQR(movie_ff$popularity_group, na.rm=TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$popularity_group, plot=FALSE)$out
#### check distribution without outliers
movie_ff$popularity_group2 <- movie_ff$popularity_group
movie_ff$popularity_group2[movie_ff$popularity_group2 < lower | movie_ff$popularity_group2 > higher] <- NA
summary(movie_ff$popularity_group2)
barplot(table(movie_ff$popularity_group2), col="Green")
boxplot(movie_ff$popularity_group, na.rm = TRUE, ylim=c(0,25))
boxplot(movie_ff$popularity_group2, na.rm = TRUE, ylim=c(0,25))
#### check correlation with Revenue
cor.test(movie_ff$popularity_group, movie_ff$revenue, method = "spearman")
cor.test(movie_ff$popularity_group2, movie_ff$revenue, method = "spearman")
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=popularity_group, y=revenue))
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=popularity_group2, y=revenue))

####################### Analyze runtime:
summary(movie_ff$runtime)
sum(is.na(movie_ff$runtime))
nrow(table(movie_ff$runtime))
max(table(movie_ff$runtime))
calculate_mode(movie_ff$runtime)
boxplot(movie_ff$runtime)
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$runtime, probs=c(.25, .75), na.rm = TRUE)
iqr<-IQR(movie_ff$runtime, na.rm=TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$runtime, plot=FALSE)$out
#### check distribution without outliers
movie_ff$runtime2 <- movie_ff$runtime
movie_ff$runtime2[movie_ff$runtime2 < lower | movie_ff$runtime2 > higher] <- NA
summary(movie_ff$runtime2)
barplot(table(movie_ff$runtime2), col="Green")
boxplot(movie_ff$runtime2, na.rm = TRUE)
#### check correlation with Revenue
cor.test(movie_ff$runtime, movie_ff$revenue, method = "spearman")
cor.test(movie_ff$runtime2, movie_ff$revenue, method = "spearman")
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=runtime, y=revenue))
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=runtime2, y=revenue))
#### exclude zeros:
movie_ff$runtime3 <- movie_ff$runtime
movie_ff$runtime3[movie_ff$runtime3 ==0] <- NA
summary(movie_ff$runtime3)
sum(is.na(movie_ff$runtime3))
nrow(table(movie_ff$runtime3))
max(table(movie_ff$runtime3))
calculate_mode(movie_ff$runtime3)
boxplot(movie_ff$runtime3)
cor.test(movie_ff$runtime, movie_ff$revenue, method = "spearman")
cor.test(movie_ff$runtime3, movie_ff$revenue, method = "spearman")
#### exclude zeros and outliers:
outliers <- boxplot(movie_ff$runtime3, plot=FALSE)$out
movie_ff$runtime4 <- movie_ff$runtime3
movie_ff$runtime4[movie_ff$runtime4 < lower | movie_ff$runtime4 > higher] <- NA
summary(movie_ff$runtime4)
barplot(table(movie_ff$runtime4), col="Green")
boxplot(movie_ff$runtime4, na.rm = TRUE)
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=runtime, y=revenue))
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=runtime4, y=revenue))
cor.test(movie_ff$runtime, movie_ff$revenue, method = "spearman")
cor.test(movie_ff$runtime4, movie_ff$revenue, method = "spearman")

####################### Analyze sw_web_presence:
summary(movie_ff$sw_web_presence)
table(movie_ff$sw_web_presence)
sum(is.na(movie_ff$sw_web_presence))
plot(movie_ff$revenue ~ movie_ff$sw_web_presence)
cor.test(movie_ff$sw_web_presence, movie_ff$revenue, method = "spearman")


####################### Analyze keyword_cnt:
summary(movie_ff$keyword_cnt)
sum(is.na(movie_ff$keyword_cnt))
nrow(table(movie_ff$keyword_cnt))
max(table(movie_ff$keyword_cnt))
calculate_mode(movie_ff$keyword_cnt)
boxplot(movie_ff$keyword_cnt)
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$keyword_cnt, probs=c(.25, .75), na.rm = TRUE)
iqr<-IQR(movie_ff$keyword_cnt, na.rm=TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$keyword_cnt, plot=FALSE)$out
#### check distribution without outliers
movie_ff$keyword_cnt2 <- movie_ff$keyword_cnt
movie_ff$keyword_cnt2[movie_ff$keyword_cnt2 < lower | movie_ff$keyword_cnt2 > higher] <- NA
summary(movie_ff$keyword_cnt2)
barplot(table(movie_ff$keyword_cnt2), col="Green")
boxplot(movie_ff$keyword_cnt2, na.rm = TRUE)
#### check correlation with Revenue
cor.test(movie_ff$keyword_cnt, movie_ff$revenue, method = "spearman")
cor.test(movie_ff$keyword_cnt2, movie_ff$revenue, method = "spearman")
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=keyword_cnt, y=revenue))
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=keyword_cnt2, y=revenue))

####################### Analyze high_release_month:
summary(movie_ff$high_release_month)
table(movie_ff$high_release_month)
sum(is.na(movie_ff$high_release_month))

####################### Analyze sw_collection:
summary(movie_ff$sw_collection)
table(movie_ff$sw_collection)
sum(is.na(movie_ff$sw_collection))
##correct the category:
movie_ff$sw_collection2 <- ifelse(movie_ff$sw_collection==1 | movie_ff$sw_collection==2,1,0)
summary(movie_ff$sw_collection2)
table(movie_ff$sw_collection2)

####################### Analyze lang_US:
summary(movie_ff$lang_US)
table(movie_ff$lang_US)
sum(is.na(movie_ff$lang_US))
boxplot(movie_ff$revenue ~ movie_ff$lang_US)

####################### Analyze lang_ES:
summary(movie_ff$lang_ES)
table(movie_ff$lang_ES)
sum(is.na(movie_ff$lang_ES))

####################### Analyze actors_cnt:
summary(movie_ff$actors_cnt)
sum(is.na(movie_ff$actors_cnt))
nrow(table(movie_ff$actors_cnt))
max(table(movie_ff$actors_cnt))
calculate_mode(movie_ff$actors_cnt)
boxplot(movie_ff$actors_cnt)
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$actors_cnt, probs=c(.25, .75), na.rm = TRUE)
iqr<-IQR(movie_ff$actors_cnt, na.rm=TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$actors_cnt, plot=FALSE)$out
#### check distribution without outliers
movie_ff$actors_cnt2 <- movie_ff$actors_cnt
movie_ff$actors_cnt2[movie_ff$actors_cnt2 < lower | movie_ff$actors_cnt2 > higher] <- NA
summary(movie_ff$actors_cnt2)
barplot(table(movie_ff$actors_cnt2), col="Green")
boxplot(movie_ff$actors_cnt2, na.rm = TRUE)
#### check correlation with Revenue
cor.test(movie_ff$actors_cnt, movie_ff$revenue, method = "spearman")
cor.test(movie_ff$actors_cnt2, movie_ff$revenue, method = "spearman")
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=actors_cnt, y=revenue))
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=actors_cnt2, y=revenue))

####################### Analyze producers_cnt:
summary(movie_ff$producers_cnt)
sum(is.na(movie_ff$producers_cnt))
nrow(table(movie_ff$producers_cnt))
max(table(movie_ff$producers_cnt))
calculate_mode(movie_ff$producers_cnt)
boxplot(movie_ff$producers_cnt)
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$producers_cnt, probs=c(.25, .75), na.rm = TRUE)
iqr<-IQR(movie_ff$producers_cnt, na.rm=TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$producers_cnt, plot=FALSE)$out
#### check distribution without outliers
movie_ff$producers_cnt2 <- movie_ff$producers_cnt
movie_ff$producers_cnt2[movie_ff$producers_cnt2 < lower | movie_ff$producers_cnt2 > higher] <- NA
summary(movie_ff$producers_cnt2)
barplot(table(movie_ff$producers_cnt2), col="Green")
boxplot(movie_ff$producers_cnt2, na.rm = TRUE)
#### check correlation with Revenue
cor.test(movie_ff$producers_cnt, movie_ff$revenue, method = "spearman")
cor.test(movie_ff$producers_cnt2, movie_ff$revenue, method = "spearman")
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=producers_cnt, y=revenue))
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=producers_cnt2, y=revenue))

####################### Analyze actor0_movies_5y_cnt:
summary(movie_ff$actor0_movies_5y_cnt)
sum(is.na(movie_ff$actor0_movies_5y_cnt))
nrow(table(movie_ff$actor0_movies_5y_cnt))
max(table(movie_ff$actor0_movies_5y_cnt))
calculate_mode(movie_ff$actor0_movies_5y_cnt)
boxplot(movie_ff$actor0_movies_5y_cnt)
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$actor0_movies_5y_cnt, probs=c(.25, .75), na.rm = TRUE)
iqr<-IQR(movie_ff$actor0_movies_5y_cnt, na.rm=TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$actor0_movies_5y_cnt, plot=FALSE)$out
#### check distribution without outliers
movie_ff$actor0_movies_5y_cnt2 <- movie_ff$actor0_movies_5y_cnt
movie_ff$actor0_movies_5y_cnt2[movie_ff$actor0_movies_5y_cnt2 < lower | movie_ff$actor0_movies_5y_cnt2 > higher] <- NA
summary(movie_ff$actor0_movies_5y_cnt2)
barplot(table(movie_ff$actor0_movies_5y_cnt2), col="Green")
boxplot(movie_ff$actor0_movies_5y_cnt2, na.rm = TRUE)
#### check correlation with Revenue
cor.test(movie_ff$actor0_movies_5y_cnt, movie_ff$revenue, method = "spearman")
cor.test(movie_ff$actor0_movies_5y_cnt2, movie_ff$revenue, method = "spearman")
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=actor0_movies_5y_cnt, y=revenue))
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=actor0_movies_5y_cnt2, y=revenue))

####################### Analyze actor2_movies_cnt:
summary(movie_ff$actor2_movies_cnt)
sum(is.na(movie_ff$actor2_movies_cnt))
nrow(table(movie_ff$actor2_movies_cnt))
max(table(movie_ff$actor2_movies_cnt))
calculate_mode(movie_ff$actor2_movies_cnt)
boxplot(movie_ff$actor2_movies_cnt)
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$actor2_movies_cnt, probs=c(.25, .75), na.rm = TRUE)
iqr<-IQR(movie_ff$actor2_movies_cnt, na.rm=TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$actor2_movies_cnt, plot=FALSE)$out
#### check distribution without outliers
movie_ff$actor2_movies_cnt2 <- movie_ff$actor2_movies_cnt
movie_ff$actor2_movies_cnt2[movie_ff$actor2_movies_cnt2 < lower | movie_ff$actor2_movies_cnt2 > higher] <- NA
summary(movie_ff$actor2_movies_cnt2)
barplot(table(movie_ff$actor2_movies_cnt2), col="Green")
boxplot(movie_ff$actor2_movies_cnt2, na.rm = TRUE)
#### check correlation with Revenue
cor.test(movie_ff$actor2_movies_cnt, movie_ff$revenue, method = "spearman")
cor.test(movie_ff$actor2_movies_cnt2, movie_ff$revenue, method = "spearman")
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=actor2_movies_cnt, y=revenue))
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=actor2_movies_cnt2, y=revenue))


####################### Analyze sw_female_actor1:
summary(movie_ff$sw_female_actor1)
table(movie_ff$sw_female_actor1)
sum(is.na(movie_ff$sw_female_actor1))

####################### Analyze sw_male_actor1:
summary(movie_ff$sw_male_actor1)
table(movie_ff$sw_male_actor1)
sum(is.na(movie_ff$sw_male_actor1))

####################### Analyze actor1_prev_revenue:
summary(movie_ff$actor1_prev_revenue)
sum(is.na(movie_ff$actor1_prev_revenue))
nrow(table(movie_ff$actor1_prev_revenue))
max(table(movie_ff$actor1_prev_revenue))
calculate_mode(movie_ff$actor1_prev_revenue)
boxplot(movie_ff$actor1_prev_revenue)
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$actor1_prev_revenue, probs=c(.25, .75), na.rm = TRUE)
iqr<-IQR(movie_ff$actor1_prev_revenue, na.rm=TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$actor1_prev_revenue, plot=FALSE)$out
#### check distribution without outliers
movie_ff$actor1_prev_revenue2 <- movie_ff$actor1_prev_revenue
movie_ff$actor1_prev_revenue2[movie_ff$actor1_prev_revenue2 < lower | movie_ff$actor1_prev_revenue2 > higher] <- NA
summary(movie_ff$actor1_prev_revenue2)
barplot(table(movie_ff$actor1_prev_revenue2), col="Green")
boxplot(movie_ff$actor1_prev_revenue2, na.rm = TRUE)
#### check correlation with Revenue
cor.test(movie_ff$actor1_prev_revenue, movie_ff$revenue, method = "spearman")
cor.test(movie_ff$actor1_prev_revenue2, movie_ff$revenue, method = "spearman")
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=actor1_prev_revenue, y=revenue))
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=actor1_prev_revenue2, y=revenue))

####################### Analyze director_movies_5y_cnt:
summary(movie_ff$director_movies_5y_cnt)
sum(is.na(movie_ff$director_movies_5y_cnt))
nrow(table(movie_ff$director_movies_5y_cnt))
max(table(movie_ff$director_movies_5y_cnt))
calculate_mode(movie_ff$director_movies_5y_cnt)
boxplot(movie_ff$director_movies_5y_cnt)
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$director_movies_5y_cnt, probs=c(.25, .75), na.rm = TRUE)
iqr<-IQR(movie_ff$director_movies_5y_cnt, na.rm=TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$director_movies_5y_cnt, plot=FALSE)$out
#### check distribution without outliers
movie_ff$director_movies_5y_cnt2 <- movie_ff$director_movies_5y_cnt
movie_ff$director_movies_5y_cnt2[movie_ff$director_movies_5y_cnt2 < lower | movie_ff$director_movies_5y_cnt2 > higher] <- NA
summary(movie_ff$director_movies_5y_cnt2)
barplot(table(movie_ff$director_movies_5y_cnt2), col="Green")
boxplot(movie_ff$director_movies_5y_cnt2, na.rm = TRUE)
#### check correlation with Revenue
cor.test(movie_ff$director_movies_5y_cnt, movie_ff$revenue, method = "spearman")
cor.test(movie_ff$director_movies_5y_cnt2, movie_ff$revenue, method = "spearman")
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=director_movies_5y_cnt, y=revenue))
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=director_movies_5y_cnt2, y=revenue))

####################### Analyze genre_animation:
summary(movie_ff$genre_animation)
table(movie_ff$genre_animation)
sum(is.na(movie_ff$genre_animation))

####################### Analyze genre_action:
summary(movie_ff$genre_action)
table(movie_ff$genre_action)
sum(is.na(movie_ff$genre_action))

####################### Analyze genre_western:
summary(movie_ff$genre_western)
table(movie_ff$genre_western)
sum(is.na(movie_ff$genre_western))

####################### Analyze genre_documentary:
summary(movie_ff$genre_documentary)
table(movie_ff$genre_documentary)
sum(is.na(movie_ff$genre_documentary))

####################### Analyze genre_music:
summary(movie_ff$genre_music)
table(movie_ff$genre_music)
sum(is.na(movie_ff$genre_music))

####################### Analyze genre_war:
summary(movie_ff$genre_war)
table(movie_ff$genre_war)
sum(is.na(movie_ff$genre_war))

####################### Analyze depart_Camera:
summary(movie_ff$depart_Camera)
sum(is.na(movie_ff$depart_Camera))
nrow(table(movie_ff$depart_Camera))
max(table(movie_ff$depart_Camera))
calculate_mode(movie_ff$depart_Camera)
boxplot(movie_ff$depart_Camera)
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$depart_Camera, probs=c(.25, .75), na.rm = TRUE)
iqr<-IQR(movie_ff$depart_Camera, na.rm=TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$depart_Camera, plot=FALSE)$out
#### check distribution without outliers
movie_ff$depart_Camera2 <- movie_ff$depart_Camera
movie_ff$depart_Camera2[movie_ff$depart_Camera2 < lower | movie_ff$depart_Camera2 > higher] <- NA
summary(movie_ff$depart_Camera2)
barplot(table(movie_ff$depart_Camera2), col="Green")
boxplot(movie_ff$depart_Camera2, na.rm = TRUE)
#### check correlation with Revenue
cor.test(movie_ff$depart_Camera, movie_ff$revenue, method = "spearman")
cor.test(movie_ff$depart_Camera2, movie_ff$revenue, method = "spearman")
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=depart_Camera, y=revenue))
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=depart_Camera2, y=revenue))

####################### Analyze depart_Directing:
summary(movie_ff$depart_Directing)
sum(is.na(movie_ff$depart_Directing))
nrow(table(movie_ff$depart_Directing))
max(table(movie_ff$depart_Directing))
calculate_mode(movie_ff$depart_Directing)
boxplot(movie_ff$depart_Directing)
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$depart_Directing, probs=c(.25, .75), na.rm = TRUE)
iqr<-IQR(movie_ff$depart_Directing, na.rm=TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$depart_Directing, plot=FALSE)$out
#### check distribution without outliers
movie_ff$depart_Directing2 <- movie_ff$depart_Directing
movie_ff$depart_Directing2[movie_ff$depart_Directing2 < lower | movie_ff$depart_Directing2 > higher] <- NA
summary(movie_ff$depart_Directing2)
barplot(table(movie_ff$depart_Directing2), col="Green")
boxplot(movie_ff$depart_Directing2, na.rm = TRUE)
#### check correlation with Revenue
cor.test(movie_ff$depart_Directing, movie_ff$revenue, method = "spearman")
cor.test(movie_ff$depart_Directing2, movie_ff$revenue, method = "spearman")
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=depart_Directing, y=revenue))
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=depart_Directing2, y=revenue))

####################### Analyze depart_Production:
summary(movie_ff$depart_Production)
sum(is.na(movie_ff$depart_Production))
nrow(table(movie_ff$depart_Production))
max(table(movie_ff$depart_Production))
calculate_mode(movie_ff$depart_Production)
boxplot(movie_ff$depart_Production)
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$depart_Production, probs=c(.25, .75), na.rm = TRUE)
iqr<-IQR(movie_ff$depart_Production, na.rm=TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$depart_Production, plot=FALSE)$out
#### check distribution without outliers
movie_ff$depart_Production2 <- movie_ff$depart_Production
movie_ff$depart_Production2[movie_ff$depart_Production2 < lower | movie_ff$depart_Production2 > higher] <- NA
summary(movie_ff$depart_Production2)
barplot(table(movie_ff$depart_Production2), col="Green")
boxplot(movie_ff$depart_Production2, na.rm = TRUE)
#### check correlation with Revenue
cor.test(movie_ff$depart_Production, movie_ff$revenue, method = "spearman")
cor.test(movie_ff$depart_Production2, movie_ff$revenue, method = "spearman")
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=depart_Production, y=revenue))
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=depart_Production2, y=revenue))

####################### Analyze depart_Writing:
summary(movie_ff$depart_Writing)
sum(is.na(movie_ff$depart_Writing))
nrow(table(movie_ff$depart_Writing))
max(table(movie_ff$depart_Writing))
calculate_mode(movie_ff$depart_Writing)
boxplot(movie_ff$depart_Writing)
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$depart_Writing, probs=c(.25, .75), na.rm = TRUE)
iqr<-IQR(movie_ff$depart_Writing, na.rm=TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$depart_Writing, plot=FALSE)$out
#### check distribution without outliers
movie_ff$depart_Writing2 <- movie_ff$depart_Writing
movie_ff$depart_Writing2[movie_ff$depart_Writing2 < lower | movie_ff$depart_Writing2 > higher] <- NA
summary(movie_ff$depart_Writing2)
barplot(table(movie_ff$depart_Writing2), col="Green")
boxplot(movie_ff$depart_Writing2, na.rm = TRUE)
#### check correlation with Revenue
cor.test(movie_ff$depart_Writing, movie_ff$revenue, method = "spearman")
cor.test(movie_ff$depart_Writing2, movie_ff$revenue, method = "spearman")
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=depart_Writing, y=revenue))
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=depart_Writing2, y=revenue))

####################### Analyze depart_Crew_female:
summary(movie_ff$depart_Crew_female)
sum(is.na(movie_ff$depart_Crew_female))
nrow(table(movie_ff$depart_Crew_female))
max(table(movie_ff$depart_Crew_female))
calculate_mode(movie_ff$depart_Crew_female)
boxplot(movie_ff$depart_Crew_female)
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$depart_Crew_female, probs=c(.25, .75), na.rm = TRUE)
iqr<-IQR(movie_ff$depart_Crew_female, na.rm=TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$depart_Crew_female, plot=FALSE)$out
#### check distribution without outliers
movie_ff$depart_Crew_female2 <- movie_ff$depart_Crew_female
movie_ff$depart_Crew_female2[movie_ff$depart_Crew_female2 < lower | movie_ff$depart_Crew_female2 > higher] <- NA
summary(movie_ff$depart_Crew_female2)
barplot(table(movie_ff$depart_Crew_female2), col="Green")
boxplot(movie_ff$depart_Crew_female2, na.rm = TRUE)
#### check correlation with Revenue
cor.test(movie_ff$depart_Crew_female, movie_ff$revenue, method = "spearman")
cor.test(movie_ff$depart_Crew_female2, movie_ff$revenue, method = "spearman")
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=depart_Crew_female, y=revenue))
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=depart_Crew_female2, y=revenue))

####################### Analyze depart_Editing_female:
summary(movie_ff$depart_Editing_female)
sum(is.na(movie_ff$depart_Editing_female))
nrow(table(movie_ff$depart_Editing_female))
max(table(movie_ff$depart_Editing_female))
calculate_mode(movie_ff$depart_Editing_female)
boxplot(movie_ff$depart_Editing_female)
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$depart_Editing_female, probs=c(.25, .75), na.rm = TRUE)
iqr<-IQR(movie_ff$depart_Editing_female, na.rm=TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$depart_Editing_female, plot=FALSE)$out
#### check distribution without outliers
movie_ff$depart_Editing_female2 <- movie_ff$depart_Editing_female
movie_ff$depart_Editing_female2[movie_ff$depart_Editing_female2 < lower | movie_ff$depart_Editing_female2 > higher] <- NA
summary(movie_ff$depart_Editing_female2)
barplot(table(movie_ff$depart_Editing_female2), col="Green")
boxplot(movie_ff$depart_Editing_female2, na.rm = TRUE)
#### check correlation with Revenue
cor.test(movie_ff$depart_Editing_female, movie_ff$revenue, method = "spearman")
cor.test(movie_ff$depart_Editing_female2, movie_ff$revenue, method = "spearman")
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=depart_Editing_female, y=revenue))
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=depart_Editing_female2, y=revenue))


####################### Analyze depart_Sound_female:
summary(movie_ff$depart_Sound_female)
sum(is.na(movie_ff$depart_Sound_female))
nrow(table(movie_ff$depart_Sound_female))
max(table(movie_ff$depart_Sound_female))
calculate_mode(movie_ff$depart_Sound_female)
boxplot(movie_ff$depart_Sound_female)
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$depart_Sound_female, probs=c(.25, .75), na.rm = TRUE)
iqr<-IQR(movie_ff$depart_Sound_female, na.rm=TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$depart_Sound_female, plot=FALSE)$out
#### check distribution without outliers
movie_ff$depart_Sound_female2 <- movie_ff$depart_Sound_female
movie_ff$depart_Sound_female2[movie_ff$depart_Sound_female2 < lower | movie_ff$depart_Sound_female2 > higher] <- NA
summary(movie_ff$depart_Sound_female2)
barplot(table(movie_ff$depart_Sound_female2), col="Green")
boxplot(movie_ff$depart_Sound_female2, na.rm = TRUE)
#### check correlation with Revenue
cor.test(movie_ff$depart_Sound_female, movie_ff$revenue, method = "spearman")
cor.test(movie_ff$depart_Sound_female2, movie_ff$revenue, method = "spearman")
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=depart_Sound_female, y=revenue))
ggplot(data = movie_ff)+ geom_smooth(mapping = aes(x=depart_Sound_female2, y=revenue))

####################### outlierMatrix:
outlierMatrix <- function(data,threshold=1.5) {
  vn <- names(data)
  outdata <- data.frame(row1=1:nrow(data))
  for(v in vn) {
    if(is.numeric(data[[v]])) {
      outlow <- quantile(data[[v]],probs = 0.25,na.rm = T) 
      outhigh <- quantile(data[[v]],probs = 0.75, na.rm = T)
      irq_level <- (outhigh - outlow) * threshold
      outlow <- outlow - irq_level
      outhigh <- outhigh +  irq_level
      mv <- ifelse(data[[v]] < outlow | data[[v]] > outhigh, 1, 0)
      outdata[v] <- mv
    } else {
      mv <- rep(0,nrow(data))
    }
  }
  outdata$row1 <- NULL
  return(outdata)
}

####################### missingMatrix:
missingMatrix <- function(data) {
  vn <- names(data)
  missdata <- data.frame(row1=1:nrow(data))
  for(v in vn) {
    mv <- ifelse(is.na(data[[v]]),1,0)
    missdata[v] <- mv
  }
  missdata$row1 <- NULL
  return(missdata)
}

####################### Missing analysis:

m<-missingMatrix(movie_ff)
head(m)
heatmap(as.matrix(m))
movies_na <- missingMatrix(movie_ff)
head(movies_na)
movies <- cbind(movie_ff,movies_na)
head(movies_na)
ggplot(movies_na, aes(x=revenue, group=factor(producers_cnt_na), color=factor(producers_cnt_na))) + geom_density()

# Correlation matrix - need to decide which parameters:
rcorr(as.matrix(movie_ff$budget, movie_ff$popularity_group, movie_ff$runtime, movie_ff$revenue),type = "spearman")
# t.test - need to decide which parameters:
t.test(movie_ff$sw_has_poster, movie_ff$sw_tagline, paired = FALSE)


### check if relevant:
pairs(movie_ff[,c(3,5,6,8,10)])

getMissingness(movie_ff)
m<-missingMatrix(movie_ff)
heatmap(as.matrix(m))

