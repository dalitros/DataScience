library(DBI)
library(dplyr)
library(odbc)
library(Hmisc)
library(devtools)
library(ggplot2)
library(corrgram)
install.packages("dplyr")
##install_github("karpatit/mechkar")

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
######################################################################
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


con <- dbConnect(odbc::odbc(), 
                 Driver = "SQL Server", 
                 Server = "localhost\\SQLEXPRESS", 
                 Database = "BOXOFFICE", 
                 Trusted_Connection = "True")
conn=con

####################### READ FILE:
movie_ff <- read.csv("C:/Users/dalit/DataScience/project/BoxOffice/movie_ff.csv")
movie_ff
movie_ff_F <- read.csv("C:/Users/dalit/DataScience/project/BoxOffice/movie_ff_fixed.csv")
movie_ff_F
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

movie_ff$budget <- as.numeric(movie_ff$budget)
movie_ff$release_month <- factor(movie_ff$release_month)
movie_ff$sw_lang_en <- factor(movie_ff$sw_lang_en)
movie_ff$seasonality <- factor(movie_ff$seasonality)
movie_ff$countries_cnt <- as.numeric(movie_ff$countries_cnt)
movie_ff$depart_Art <- as.numeric(movie_ff$depart_Art)
movie_ff$depart_Custom_Mkup <- as.numeric(movie_ff$depart_Custom_Mkup)
movie_ff$depart_Lighting <- as.numeric(movie_ff$depart_Lighting)
movie_ff$depart_Visual_Effects <- as.numeric(movie_ff$depart_Visual_Effects)
movie_ff$depart_Camera_female <- as.numeric(movie_ff$depart_Camera_female)
movie_ff$depart_Directing_female <- as.numeric(movie_ff$depart_Directing_female)
movie_ff$depart_Production_female <- as.numeric(movie_ff$depart_Production_female)
movie_ff$depart_Writing_female <- as.numeric(movie_ff$depart_Writing_female)
####################### EDA ANALYSIS:
class(movie_ff)
str(movie_ff)

####################### USE MECHKAR FOR EXPLORE_DATA:
exploreData(movie_ff)

####################### Analyze revenue:
summary(movie_ff$revenue)
table(movie_ff$revenue)
sum(is.na(movie_ff$revenue))
sd(movie_ff$revenue, na.rm = TRUE)
nrow(table(movie_ff$revenue))
max(table(movie_ff$revenue))
calculate_mode(movie_ff$revenue)
hist(movie_ff$revenue, breaks = 400, na.rm = TRUE)
hist(log(movie_ff$revenue), breaks = 400, na.rm = TRUE)
boxplot(movie_ff$revenue, na.rm = TRUE)
boxplot(log(movie_ff$revenue), na.rm = TRUE)

#### calculate the IQR and outliers:
Q <- quantile(movie_ff$revenue, probs=c(.25, .75), na.rm = TRUE)
iqr<-IQR(movie_ff$revenue, na.rm = TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$revenue, plot=FALSE)$out
#### check distribution without outliers
movie_ff$revenue2 <- movie_ff$revenue
movie_ff$revenue2[movie_ff$revenue2 < lower | movie_ff$revenue2 > higher] <- NA
summary(movie_ff$revenue2)
hist(movie_ff$revenue2, breaks = 400, na.rm = TRUE)
hist(log(movie_ff$revenue2), breaks = 400, na.rm = TRUE)
boxplot(movie_ff$revenue2, na.rm = TRUE)

####################### Analyze budget:
summary(movie_ff$budget)
table(movie_ff$budget)
sum(is.na(movie_ff$budget))
sd(movie_ff$budget, na.rm = TRUE)
nrow(table(movie_ff$budget))
max(table(movie_ff$budget))
calculate_mode(movie_ff$budget)
hist(movie_ff$budget, breaks = 50, xlim=c(0,3e08),  ylim=c(0,4000), na.rm = TRUE)
boxplot(movie_ff$budget, na.rm = TRUE)
plot(movie_ff$budget)
ggplot(data=movie_ff)+
  geom_smooth(mapping=
                aes(x=movie_ff$budget,
                    y=revenue))
outliers <- boxplot(movie_ff$budget, plot=FALSE)$out
outliers


###correlation function
plot(movie_ff$budget ~ movie_ff$revenue, xlim=c(0,1.5e09),  ylim=c(0,5e08))
mod1 <- lm(movie_ff$budget ~ movie_ff$revenue)
pred1<-predict(mod1)
abline(reg=mod1, col="red")
summary(mod1)
cor.test(movie_ff$budget,movie_ff$revenue,method="spearman")

##log transformation in order to see the lower values
hist(log(movie_ff$budget), breaks = 50, na.rm = TRUE)

##relationship between budget and original_language
plot(movie_ff$budget ~ movie_ff$original_language)

#### calculate the IQR and outliers:
Q <- quantile(movie_ff$budget, probs=c(.25, .75), na.rm = TRUE)
iqr<-quantile(movie_ff$budget, probs= .75, na.rm = TRUE) - quantile(movie_ff$budget3, probs= .25, na.rm = TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$budget, plot=FALSE)$out
outliers

#### check distribution without outliers
movie_ff$budget2 <- movie_ff$budget
movie_ff$budget2[movie_ff$budget2 < lower | movie_ff$budget2 > higher] <- NA
summary(movie_ff$budget2)
hist(movie_ff$budget2, breaks = 50, xlim=c(0,3e08),  ylim=c(0,4000), na.rm = TRUE)
hist(log(movie_ff$budget2), breaks = 50, xlim=c(0,20), na.rm = TRUE)
boxplot(movie_ff$budget2,  ylim=c(0,3e08),na.rm = TRUE)
ggplot(data=movie_ff)+
  geom_smooth(mapping=
                aes(x=movie_ff$budget2,
                    y=revenue))

###correlation function after NA removal 
plot(movie_ff$budget2 ~ movie_ff$revenue, xlim=c(0,1.5e09),  ylim=c(0,5e08))
mod12 <- lm(movie_ff$budget2 ~ movie_ff$revenue)
pred2<-predict(mod2)
abline(reg=mod2, col="red")
summary(mod2)
cor.test(movie_ff$budget2,movie_ff$revenue,method="spearman")

#### chanage 0 to NA
movie_ff$budget3 <- movie_ff$budget
movie_ff$budget3[movie_ff$budget3==0] <- NA
movie_ff$budget3

summary(movie_ff$budget3)
hist(movie_ff$budget3, breaks = 50, na.rm = TRUE)
boxplot(movie_ff$budget3, na.rm = TRUE)
hist(log(movie_ff$budget3), breaks = 50, na.rm = TRUE)
plot(movie_ff$budget3)
ggplot(data=movie_ff)+
  geom_smooth(mapping=
                aes(x=movie_ff$budget3,
                    y=revenue))

plot(log(movie_ff$budget3) ~ movie_ff$original_language, na.rm = TRUE)
plot(movie_ff$budget3 ~ movie_ff$revenue)
mod3 <- lm(movie_ff$budget3 ~ movie_ff$revenue)
pred3<-predict(mod3)
abline(reg=mod3, col="red")
summary(mod3)
cor.test(movie_ff$budget3,movie_ff$revenue,method="spearman")


#### calculate the IQR and outliers:
Q <- quantile(movie_ff$budget3, probs=c(.25, .75), na.rm = TRUE)
iqr<-quantile(movie_ff$budget3, probs= .75, na.rm = TRUE) - quantile(movie_ff$budget3, probs= .25, na.rm = TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$budget3, plot=FALSE)$out
outliers

#### check distribution without outliers
movie_ff$budget4 <- movie_ff$budget3
movie_ff$budget4[movie_ff$budget4 < lower | movie_ff$budget4 > higher] <- NA
summary(movie_ff$budget4)
hist(movie_ff$budget4, breaks = 50, na.rm = TRUE)
hist(log(movie_ff$budget4), breaks = 50, na.rm = TRUE)
boxplot(movie_ff$budget4, na.rm = TRUE)
ggplot(data=movie_ff)+
  geom_smooth(mapping=
                aes(x=movie_ff$budget4,
                    y=revenue))

plot(movie_ff$budget4 ~ movie_ff$revenue)
mod4 <- lm(movie_ff$budget4 ~ movie_ff$revenue)
pred4<-predict(mod4)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$budget4,movie_ff$revenue,method="spearman")



####################### Analyze sw_lang_en:
summary(movie_ff$sw_lang_en)
table(movie_ff$sw_lang_en)
sum(is.na(movie_ff$sw_lang_en))
plot(movie_ff$revenue ~ movie_ff$sw_lang_en, na.rm = TRUE)
boxplot(movie_ff$revenue ~ movie_ff$sw_lang_en)
chisq.test(movie_ff$sw_lang_en,movie_ff$revenue)


####################### Analyze sw_tagline:
summary(movie_ff$sw_tagline)
table(movie_ff$sw_tagline)
sum(is.na(movie_ff$sw_tagline))
boxplot(movie_ff$revenue ~ movie_ff$sw_tagline)
chisq.test(movie_ff$sw_lang_en,movie_ff$sw_tagline)


####################### Analyze release_month:
summary(movie_ff$release_month)
sort(table(movie_ff$release_month))
sum(is.na(movie_ff$release_month))
nrow(table(movie_ff$release_month))
max(table(movie_ff$release_month))
boxplot(table(movie_ff$release_month))
t <- table(movie_ff$release_month)
class(t)
summary(t)
t
barplot(sort(table(movie_ff$release_month), decreasing = TRUE), xlab="Release month", ylab="Number of released movies")
boxplot(movie_ff$revenue ~ movie_ff$release_month, xlab="Release month", ylab="revenue")

chisq.test(movie_ff$sw_lang_en,movie_ff$release_month)
kruskal.test(x = movie_ff$revenue, g = as.factor(movie_ff$release_month))
mod_ff1 <- glm(revenue ~ release_month, data=movie_ff, family = "binomial")

####################### Analyze seasonality:
summary(movie_ff$seasonality)
sort(table(movie_ff$seasonality))
sum(is.na(movie_ff$seasonality))
nrow(table(movie_ff$seasonality))
max(table(movie_ff$seasonality))
boxplot(table(movie_ff$seasonality))
t <- table(movie_ff$seasonality)
class(t)
summary(t)
t
barplot(sort(table(movie_ff$seasonality), decreasing = TRUE), xlab="seasonality", ylab="Number of released movies",las=2)
boxplot(movie_ff$revenue ~ movie_ff$seasonality, xlab="seasonality", ylab="revenue", ylim=c(0,1e9),cex.axis=1, las=2)
abline(h=h)

chisq.test(movie_ff$sw_lang_en,movie_ff$seasonality)
table(movie_ff$seasonality)
summary(movie_ff$seasonality)



####################### Analyze countries_cnt:
summary(movie_ff$countries_cnt)
table(movie_ff$countries_cnt)
sum(is.na(movie_ff$countries_cnt))
sd(movie_ff$countries_cnt, na.rm = TRUE)
nrow(table(movie_ff$countries_cnt))
max(table(movie_ff$countries_cnt))
calculate_mode(movie_ff$countries_cnt)
hist(movie_ff$countries_cnt, breaks = 400, na.rm = TRUE)
boxplot(movie_ff$countries_cnt, na.rm = TRUE)

plot(movie_ff$revenue~movie_ff$countries_cnt)
cor.test(movie_ff$countries_cnt,movie_ff$revenue,method="spearman")

#### calculate the IQR and outliers:
Q <- quantile(movie_ff$countries_cnt, probs=c(.25, .75), na.rm = TRUE)
iqr<-quantile(movie_ff$countries_cnt, probs= .75, na.rm = TRUE) - quantile(movie_ff$countries_cnt, probs= .25, na.rm = TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$countries_cnt, plot=FALSE)$out
outliers

#### check distribution without outliers
movie_ff$countries_cnt2 <- movie_ff$countries_cnt
movie_ff$countries_cnt2[movie_ff$countries_cnt2 < lower | movie_ff$countries_cnt2 > higher] <- NA
summary(movie_ff$countries_cnt2)
hist(movie_ff$countries_cnt2, breaks = 400, na.rm = TRUE)
hist(log(movie_ff$countries_cnt2), breaks = 400, na.rm = TRUE)
boxplot(movie_ff$countries_cnt2,na.rm = TRUE)


###correlation function after NA removal 
plot(movie_ff$countries_cnt2 ~ movie_ff$revenue)
mod12 <- lm(movie_ff$countries_cnt2 ~ movie_ff$revenue)
pred2<-predict(mod2)
abline(reg=mod2, col="red")
summary(mod2)
cor.test(movie_ff$countries_cnt2,movie_ff$revenue,method="spearman")

####################### Analyze lang_RU:
summary(movie_ff$lang_RU)
table(movie_ff$lang_RU)
sum(is.na(movie_ff$lang_RU))
barplot(table(movie_ff$lang_RU),
    main="lang_RU")
boxplot(movie_ff$revenue ~ movie_ff$lang_RU)
chisq.test(movie_ff$lang_RU,movie_ff$revenue)


####################### Analyze actor0_movies_cnt:
movie_ff$actor0_movies_cnt <- as.numeric(movie_ff$actor0_movies_cnt )
summary(movie_ff$actor0_movies_cnt )
table(plot(movie_ff$actor0_movies_cnt3, na.rm=TRUE))
sd(movie_ff$actor0_movies_cnt3)
hist(movie_ff$actor0_movies_cnt, breaks=200, ylim=c(0,2000),cex.axis=1.5, na.rm = TRUE)
hist(log(movie_ff$actor0_movies_cnt), breaks=200, na.rm=TRUE)
boxplot(movie_ff$actor0_movies_cnt, ylim=c(0,70),cex.axis=1.5, na.rm = TRUE)
plot(movie_ff_F$actor0_movies_cnt3, na.rm=TRUE)
###correlation function 
plot(movie_ff$revenue ~ movie_ff$actor0_movies_cnt)
mod1 <- lm(movie_ff$actor0_movies_cnt ~ movie_ff$revenue)
pred1<-predict(mod1F)
abline(reg=mod1, col="red")
summary(mod1)
cor.test(movie_ff$actor0_movies_cnt,movie_ff$revenue,method="spearman")
####remove the missing items in rows 3037-3673:
movie_ff$actor0_movies_cnt3 <- movie_ff$actor0_movies_cnt
movie_ff$actor0_movies_cnt3 <- ifelse((movie_ff$ï..movie_id > 3037 & movie_ff$ï..movie_id < 3673), NA, movie_ff$actor0_movies_cnt3)
#####analyze after 0 (3037-3673) become NA:
movie_ff$actor0_movies_cnt3 <- as.numeric(movie_ff$actor0_movies_cnt3 )
summary(movie_ff$actor0_movies_cnt3 )
table(plot(movie_ff$actor0_movies_cnt3, na.rm=TRUE))
sum(is.na(movie_ff$actor0_movies_cnt3))
nrow(table(movie_ff$actor0_movies_cnt3))
max(table(movie_ff$actor0_movies_cnt3))
calculate_mode(movie_ff$actor0_movies_cnt3)
sd(movie_ff$actor0_movies_cnt3)
hist(movie_ff$actor0_movies_cnt3, breaks=200, ylim=c(0,2000),cex.axis=1.5, na.rm = TRUE)
hist(log(movie_ff$actor0_movies_cnt3), breaks=200, na.rm=TRUE)
boxplot(movie_ff$actor0_movies_cnt3,  ylim=c(0,70),cex.axis=1.5, na.rm = TRUE)
plot(movie_ff$actor0_movies_cnt3, na.rm=TRUE)
####outlier after 0 (3037-3673) become NA :
outliers <- boxplot(movie_ff$actor0_movies_cnt3, plot=FALSE)$out
###correlation function after 0 (3037-3673) become NA
plot(log(movie_ff$revenue)~log(movie_ff$actor0_movies_cnt3))
plot(movie_ff$revenue ~ movie_ff$actor0_movies_cnt3)
mod1 <- lm(movie_ff$actor0_movies_cnt3 ~ movie_ff$revenue)
pred1<-predict(mod1)
abline(reg=mod1, col="red")
summary(mod1)
cor.test(movie_ff$actor0_movies_cnt,movie_ff$revenue,method="spearman")
#### calculate the IQR and outliers  after 0 (3037-3673) become NAs:
Q <- quantile(movie_ff$actor0_movies_cnt3, probs=c(.25, .75), na.rm = TRUE)
iqr<-quantile(movie_ff$actor0_movies_cnt3, probs= .75, na.rm = TRUE) - quantile(movie_ff$actor0_movies_cnt3, probs= .25, na.rm = TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$actor0_movies_cnt3, plot=FALSE)$out
#### check distribution without outliers
movie_ff$actor0_movies_cnt4 <- movie_ff$actor0_movies_cnt3
movie_ff$actor0_movies_cnt4[movie_ff$actor0_movies_cnt4 < lower | movie_ff$actor0_movies_cnt4> higher] <- NA
summary(movie_ff$actor0_movies_cnt4)
sd(movie_ff$actor0_movies_cnt4, na.rm=TRUE)
hist(movie_ff$actor0_movies_cnt4, breaks = 50,xlim=c(0,60), ylim=c(0,2000),cex.axis=1.5, na.rm = TRUE)
boxplot(movie_ff$actor0_movies_cnt4, ylim=c(0,70),cex.axis=1.5, na.rm = TRUE)
plot( movie_ff$revenue~movie_ff$actor0_movies_cnt4, xlim=c(0,60), ylim=c(0,1.5e9))
mod4 <- lm(movie_ff$actor0_movies_cnt4 ~ movie_ff$revenue)
pred4<-predict(mod4)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$actor0_movies_cnt4,movie_ff$revenue,method="spearman")
####Compare missings:
movie_ff2 <- movie_ff
movie_ff2$actor0_movies_cnt3 <- ifelse(is.na(movie_ff2$actor0_movies_cnt3)==TRUE,0,1)
table(movie_ff2$actor0_movies_cnt3)
mod1 <- glm(actor0_movies_cnt3 ~ .-actor0_movies_cnt3, data= movie_ff2, family="binomial")
summary(mod1)
mod1 <- glm(actor0_movies_cnt3 ~ revenue + budget + original_language + depart_Production_female + actor0_prev_revenue, data= movie_ff2, family="binomial")
summary(mod1)

### Added a category to include missing:
movie_ff$actor0_movies_cnt5 <- as.numeric(movie_ff$actor0_movies_cnt3)
##movie_ff$actor0_movies_cnt5 <- rep(NA, length(movie_ff$actor0_movies_cnt5))
movie_ff$actor0_movies_cnt5[which(movie_ff$actor0_movies_cnt3 >= Q[2])] <- "High"
movie_ff$actor0_movies_cnt5[which(movie_ff$actor0_movies_cnt3 <= Q[1])] <- "Low"
movie_ff$actor0_movies_cnt5[which(movie_ff$actor0_movies_cnt3 > Q[1] & movie_ff$actor0_movies_cnt3 < Q[2])] <- "Medium"
movie_ff$actor0_movies_cnt5[is.na(movie_ff$actor0_movies_cnt3) == TRUE] <- "Missing"
table(movie_ff$actor0_movies_cnt5)
describe(movie_ff$actor0_movies_cnt5)
#repeat analysis with new, categorical, column:
movie_ff$actor0_movies_cnt5 <- factor(movie_ff$actor0_movies_cnt5)
boxplot(movie_ff$revenue ~ movie_ff$actor0_movies_cnt5, col='Blue', main = "actor0_movies_cnt5 vs. revenue", cex.axis=1.5)
chisq.test(movie_ff$actor0_movies_cnt5,movie_ff$revenue)


####################### Analyze actor1_movies_5y_cnt:
movie_ff$actor1_movies_5y_cnt <- as.numeric(movie_ff$actor1_movies_5y_cnt )
summary(movie_ff$actor1_movies_5y_cnt)
table(movie_ff$actor1_movies_5y_cnt)
sum(is.na(movie_ff$actor1_movies_5y_cnt))
nrow(table(movie_ff$actor1_movies_5y_cnt))
max(table(movie_ff$actor1_movies_5y_cnt))
calculate_mode(movie_ff$actor1_movies_5y_cnt)
sd(movie_ff$actor1_movies_5y_cnt)
hist(movie_ff$actor1_movies_5y_cnt, breaks=50)
boxplot(movie_ff$actor1_movies_5y_cnt)
###correlation function 
plot(movie_ff$revenue ~ movie_ff$actor1_movies_5y_cnt)
mod1 <- lm(movie_ff$actor1_movies_5y_cnt ~ movie_ff$revenue)
pred1<-predict(mod1F)
abline(reg=mod1, col="red")
summary(mod1)
cor.test(movie_ff$actor1_movies_5y_cnt,movie_ff$revenue,method="spearman")
####remove the missing items in rows 3037-3673:
movie_ff$actor1_movies_5y_cnt <- movie_ff$actor1_movies_5y_cnt
movie_ff$actor1_movies_5y_cnt <- ifelse((movie_ff$ï..movie_id > 3037 & movie_ff$ï..movie_id < 3673), NA, movie_ff$actor1_movies_5y_cnt3)
#####analyze after 0 (3037-3673) become NA:
####remove the missing items in rows 3037-3673:
movie_ff$actor1_movies_5y_cnt3 <- as.numeric(movie_ff$actor1_movies_5y_cnt3 )
summary(movie_ff$actor1_movies_5y_cnt3 )
table(plot(movie_ff$actor1_movies_5y_cnt3, na.rm=TRUE))
sum(is.na(movie_ff$actor1_movies_5y_cnt3))
nrow(table(movie_ff$actor1_movies_5y_cnt3))
max(table(movie_ff$actor1_movies_5y_cnt3))
calculate_mode(movie_ff$actor1_movies_5y_cnt3)
sd(movie_ff$actor1_movies_5y_cnt3, na.rm=TRUE)
hist(movie_ff$actor1_movies_5y_cnt3, breaks=50, ylim=c(0,2500),cex.axis=1.5, na.rm = TRUE)
hist(log(movie_ff$actor1_movies_5y_cnt3), breaks=200, na.rm=TRUE)
boxplot(movie_ff$actor1_movies_5y_cnt3,  ylim=c(0,20),cex.axis=1.5, na.rm = TRUE)
plot(movie_ff$actor1_movies_5y_cnt3, na.rm=TRUE)
####outlier after 0 (3037-3673) become NA :
outliers <- boxplot(movie_ff$actor1_movies_5y_cnt3, plot=FALSE)$out
outliers
###correlation function after 0 (3037-3673) become NA
plot(log(movie_ff$revenue)~log(movie_ff$actor1_movies_5y_cnt3))
plot(movie_ff$revenue ~ movie_ff$actor1_movies_5y_cnt3)
mod1 <- lm(movie_ff$actor1_movies_5y_cnt3 ~ movie_ff$revenue)
pred1<-predict(mod1)
abline(reg=mod1, col="red")
summary(mod1)
cor.test(movie_ff$actor1_movies_5y_cnt,movie_ff$revenue,method="spearman")
#### calculate the IQR and outliers  after 0 (3037-3673) become NAs:
Q <- quantile(movie_ff$actor1_movies_5y_cnt3, probs=c(.25, .75), na.rm = TRUE)
iqr<-quantile(movie_ff$actor1_movies_5y_cnt3, probs= .75, na.rm = TRUE) - quantile(movie_ff$actor1_movies_5y_cnt3, probs= .25, na.rm = TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$actor1_movies_5y_cnt3, plot=FALSE)$out
outliers
#### check distribution without outliers
movie_ff$actor1_movies_5y_cnt4 <- movie_ff$actor1_movies_5y_cnt3
movie_ff$actor1_movies_5y_cnt4[movie_ff$actor1_movies_5y_cnt4 < lower | movie_ff$actor1_movies_5y_cnt4> higher] <- NA
summary(movie_ff$actor1_movies_5y_cnt4)
sd(movie_ff$actor1_movies_5y_cnt4, na.rm=TRUE)
hist(movie_ff$actor1_movies_5y_cnt4, breaks=50, ylim=c(0,2500),cex.axis=1.5, na.rm = TRUE)
boxplot(movie_ff$actor1_movies_5y_cnt4, ylim=c(0,20),cex.axis=1.5, na.rm = TRUE)
plot( movie_ff$revenue~movie_ff$actor1_movies_5y_cnt4, xlim=c(0,17), ylim=c(0,1.5e9))
mod4 <- lm(movie_ff$actor1_movies_5y_cnt4 ~ movie_ff$revenue)
pred4<-predict(mod4)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$actor1_movies_5y_cnt4,movie_ff$revenue,method="spearman")
####Compare missings:
movie_ff2 <- movie_ff
movie_ff2$actor1_movies_5y_cnt3 <- ifelse(is.na(movie_ff2$actor1_movies_5y_cnt3)==TRUE,0,1)
table(movie_ff2$actor0_movies_cnt3)
mod1 <- glm(actor0_movies_cnt3 ~ .-actor0_movies_cnt3, data= movie_ff2, family="binomial")
summary(mod1)
mod1 <- glm(actor1_movies_5y_cnt3~ actor1_movies_5y_cnt3+revenue + budget + original_language + depart_Production_female + actor0_prev_revenue, data= movie_ff2, family="binomial")
summary(mod1)

### Added a category to include missing:
movie_ff$actor1_movies_5y_cnt5 <- as.numeric(movie_ff$actor1_movies_5y_cnt3)
##movie_ff$actor0_movies_cnt5 <- rep(NA, length(movie_ff$actor0_movies_cnt5))
movie_ff$actor1_movies_5y_cnt5[which(movie_ff$actor1_movies_5y_cnt3 >= Q[2])] <- "High"
movie_ff$actor1_movies_5y_cnt5[which(movie_ff$actor1_movies_5y_cnt3 <= Q[1])] <- "Low"
movie_ff$actor1_movies_5y_cnt5[which(movie_ff$actor1_movies_5y_cnt3 > Q[1] & movie_ff$actor1_movies_5y_cnt3 < Q[2])] <- "Medium"
movie_ff$actor1_movies_5y_cnt5[is.na(movie_ff$actor1_movies_5y_cnt3) == TRUE] <- "Missing"
table(movie_ff$actor1_movies_5y_cnt5)
describe(movie_ff$actor1_movies_5y_cnt5)
#repeat analysis with new, categorical, column:
movie_ff$actor1_movies_5y_cnt5 <- factor(movie_ff$actor1_movies_5y_cnt5)
boxplot(movie_ff$revenue ~ movie_ff$actor1_movies_5y_cnt5, col='Blue', main = "actor1_movies_5y_cnt5 vs. revenue", cex.axis=1.5)
chisq.test(movie_ff$actor1_movies_5y_cnt5,movie_ff$revenue)


####################### Analyze sw_female_actor0:
movie_ff$sw_female_actor0 <- factor(movie_ff$sw_female_actor0)
summary(movie_ff$sw_female_actor0)
table(movie_ff$sw_female_actor0)
sum(is.na(movie_ff$sw_female_actor0))
barplot(table(movie_ff$sw_female_actor0),
        main="sw_female_actor0")
boxplot(movie_ff$revenue ~ movie_ff$sw_female_actor0, cex.axis=1.5)
chisq.test(movie_ff$sw_female_actor0,movie_ff$revenue)

###replace null to 1 and 0
movie_ff2 <- movie_ff
# movie_ff2$sw_female_actor0<- as.numeric(movie_ff2$sw_female_actor0)
movie_ff2$sw_female_actor0
movie_ff2$sw_female_actor0 <- movie_ff2$sw_female_actor0 %>% replace(.=="NULL", NA)
movie_ff2$sw_female_actor0<- ifelse(is.na(movie_ff2$sw_female_actor0)==TRUE,0,1)
# movie_ff2$sw_female_actor0[which(movie_ff2$sw_female_actor0 == 0)]  <- "1"
##mod1 <- glm(sw_female_actor0 ~ .-sw_female_actor0, data= movie_ff2, family="binomial")
mod1 <- glm(sw_female_actor0 ~ actor1_movies_5y_cnt3+revenue + budget + original_language + depart_Production_female + actor0_prev_revenue, data= movie_ff, family="binomial")
summary(mod1)


movie_ff2$sw_female_actor0<- ifelse(is.na(movie_ff2$sw_female_actor0)==TRUE,0,1)
movie_ff2$sw_female_actor0[which(movie_ff2$sw_female_actor0 == na)]  <- "0"
####Compare missings:
movie_ff$sw_female_actor0<- as.numeric(movie_ff$sw_female_actor0)
movie_ff2 <- movie_ff
movie_ff$sw_female_actor0_5 <- as.numeric(movie_ff$sw_female_actor0)
movie_ff$sw_female_actor0_5<- ifelse(is.na(movie_ff$sw_female_actor0_5)==TRUE,0,1)
movie_ff$sw_female_actor0_5[which(movie_ff$sw_female_actor0_5 == 1 & movie_ff$sw_female_actor0_5 == 0)] <- "1"
movie_ff$sw_female_actor0_5[which(movie_ff$sw_female_actor0_5 == NA)] <- "0"
table(movie_ff2$sw_female_actor0)
describe(movie_ff$sw_female_actor0)
#repeat analysis with new, categorical, column:
movie_ff$actor1_sw_female_actor0<- factor(movie_ff$actor1_sw_female_actor0)
boxplot(movie_ff$revenue ~ movie_ff$sw_female_actor0, col='Blue', main = "actor1_movies_5y_cnt5 vs. revenue", cex.axis=1.5)
chisq.test(movie_ff$sw_female_actor0,movie_ff$revenue)


####################### Analyze sw_male_actor0:
summary(movie_ff$sw_male_actor0)
table(movie_ff$sw_male_actor0)
sum(is.na(movie_ff$sw_male_actor0))
barplot(table(movie_ff$sw_male_actor0),
        main="sw_male_actor0")
boxplot(movie_ff$revenue ~ movie_ff$sw_male_actor0, cex.axis=1.5)
chisq.test(movie_ff$sw_male_actor0,movie_ff$revenue)
KV <- kruskal.test(x = movie_ff$revenue, g = as.factor(movie_ff$sw_male_actor0))
KV
###mod_ff1 <- glm(sw_male_actor0~revenue, data=movie_ff, family = "binomial")
chiSquare(movie_ff$sw_male_actor0 ~ movie_ff$revenue)


####################### Analyze actor0_prev_revenue:
summary(movie_ff$actor0_prev_revenue)
table(movie_ff$actor0_prev_revenue)
sum(is.na(movie_ff$actor0_prev_revenue))
nrow(table(movie_ff$actor0_prev_revenue))
max(table(movie_ff$actor0_prev_revenue))
calculate_mode(movie_ff$actor0_prev_revenue)
sd(movie_ff$actor0_prev_revenue, na.rm = TRUE)
hist(movie_ff$actor0_prev_revenue, breaks=100, ylim=c(0,700),cex.axis=1.5, na.rm = TRUE)
boxplot(movie_ff$actor0_prev_revenue)
plot(movie_ff$actor0_prev_revenue)
plot( movie_ff$revenue~movie_ff$actor0_prev_revenue)
mod4 <- lm(movie_ff$actor0_prev_revenue ~ movie_ff$revenue)
pred4<-predict(mod4)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$actor0_prev_revenue,movie_ff$revenue,method="spearman")
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$actor0_prev_revenue, probs=c(.25, .75), na.rm = TRUE)
iqr<-quantile(movie_ff$actor0_prev_revenue, probs= .75, na.rm = TRUE) - quantile(movie_ff$actor0_prev_revenue, probs= .25, na.rm = TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$actor0_prev_revenue, plot=FALSE)$out
outliers
#### check distribution without outliers
movie_ff$actor0_prev_revenue <- as.numeric(movie_ff$actor0_prev_revenue)
movie_ff$actor0_prev_revenue2 <- movie_ff$actor0_prev_revenue
movie_ff$actor0_prev_revenue2[movie_ff$actor0_prev_revenue2 < lower | movie_ff$actor0_prev_revenue2> higher] <- NA
summary(movie_ff$actor0_prev_revenue2)
sd(movie_ff$actor0_prev_revenue2, na.rm=TRUE)
hist(movie_ff$actor0_prev_revenue2, breaks=100, ylim=c(0,700),xlim=c(0,1.5e9),cex.axis=1.5, na.rm = TRUE)
boxplot(movie_ff$actor0_prev_revenue2, ylim=c(0,1.5e9),cex.axis=1.5, na.rm = TRUE)
plot( movie_ff$revenue~movie_ff$depart_Production_female)
mod4 <- lm(movie_ff$actor0_prev_revenue2 ~ movie_ff$revenue)
pred4<-predict(mod4)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$actor0_prev_revenue2,movie_ff$revenue,method="spearman")
####Compare missings:
movie_ff2 <- movie_ff
movie_ff2$actor0_prev_revenue <- ifelse(is.na(movie_ff2$actor0_prev_revenue)==TRUE,0,1)
table(movie_ff2$actor0_prev_revenue)
mod1 <- glm(actor0_movies_cnt3 ~ .-actor0_movies_cnt3, data= movie_ff2, family="binomial")
summary(mod1)
mod1 <- glm(actor0_prev_revenue~ revenue + budget + original_language + depart_Production_female + actor0_prev_revenue, data= movie_ff2, family="binomial")
summary(mod1)
### Added a category to include missing:
movie_ff$actor0_prev_revenue5 <- as.numeric(movie_ff$actor0_prev_revenue)
##movie_ff$actor0_movies_cnt5 <- rep(NA, length(movie_ff$actor0_movies_cnt5))
movie_ff$actor0_prev_revenue5[which(movie_ff$actor0_prev_revenue >= Q[2])] <- "High"
movie_ff$actor0_prev_revenue5[which(movie_ff$actor0_prev_revenue <= Q[1])] <- "Low"
movie_ff$actor0_prev_revenue5[which(movie_ff$actor0_prev_revenue > Q[1] & movie_ff$actor0_prev_revenue < Q[2])] <- "Medium"
movie_ff$actor0_prev_revenue5[is.na(movie_ff$actor0_prev_revenue) == TRUE] <- "Missing"
table(movie_ff$actor0_prev_revenue5)
describe(movie_ff$actor0_prev_revenue5)
#repeat analysis with new, categorical, column:
movie_ff$actor0_prev_revenue5 <- factor(movie_ff$actor0_prev_revenue5)
boxplot(movie_ff$revenue ~ movie_ff$actor0_prev_revenue5, col='Blue', main = "actor1_movies_5y_cnt5 vs. revenue", cex.axis=1.5)
chisq.test(movie_ff$actor0_prev_revenue5,movie_ff$revenue)



####################### Analyze director_movies_cnt:
summary(movie_ff$director_movies_cnt)
table(movie_ff$director_movies_cnt)
sum(is.na(movie_ff$director_movies_cnt))
nrow(table(movie_ff$director_movies_cnt))
max(table(movie_ff$director_movies_cnt))
calculate_mode(movie_ff$director_movies_cnt)
sd(movie_ff$director_movies_cnt)
hist(movie_ff$director_movies_cnt, breaks=100, ylim=c(0,3500),cex.axis=1.5, na.rm = TRUE)
boxplot(movie_ff$director_movies_cnt)

plot( movie_ff$revenue~movie_ff$director_movies_cnt)
mod4 <- lm(movie_ff$director_movies_cnt ~ movie_ff$revenue)
pred4<-predict(mod4)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$director_movies_cnt,movie_ff$revenue,method="spearman")


#### calculate the IQR and outliers:
Q <- quantile(movie_ff$director_movies_cnt, probs=c(.25, .75), na.rm = TRUE)
iqr<-quantile(movie_ff$director_movies_cnt, probs= .75, na.rm = TRUE) - quantile(movie_ff$director_movies_cnt, probs= .25, na.rm = TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$director_movies_cnt, plot=FALSE)$out
outliers

#### check distribution without outliers
movie_ff$director_movies_cnt2 <- movie_ff$director_movies_cnt
movie_ff$director_movies_cnt2[movie_ff$director_movies_cnt2 < lower | movie_ff$director_movies_cnt2> higher] <- NA
summary(movie_ff$director_movies_cnt2)
sd(movie_ff$director_movies_cnt2, na.rm=TRUE)
hist(movie_ff$director_movies_cnt2, breaks=100, ylim=c(0,3500),xlim=c(0,30),cex.axis=1.5, na.rm = TRUE)
boxplot(movie_ff$director_movies_cnt2, ylim=c(0,30),cex.axis=1.5, na.rm = TRUE)


plot( movie_ff$revenue~movie_ff$director_movies_cnt2,ylim=c(0,1.5e9),xlim=c(0,30),cex.axis=1.5, na.rm = TRUE)
mod4 <- lm(movie_ff$director_movies_cnt2 ~ movie_ff$revenue)
pred4<-predict(mod4)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$director_movies_cnt2,movie_ff$revenue,method="spearman")

####################### Analyze genre_fantasy:
movie_ff$genre_fantasy <- factor(movie_ff$genre_fantasy)
summary(movie_ff$genre_fantasy)
table(movie_ff$genre_fantasy)
sum(is.na(movie_ff$genre_fantasy))
barplot(table(movie_ff$genre_fantasy),
        main="sw_female_actor0")
boxplot(movie_ff$revenue ~ movie_ff$genre_fantasy, cex.axis=1.5)
chisq.test(movie_ff$genre_fantasy,movie_ff$revenue)

####################### Analyze genre_horror:
movie_ff$genre_horror <- factor(movie_ff$genre_horror)
summary(movie_ff$genre_horror)
table(movie_ff$genre_horror)
sum(is.na(movie_ff$genre_horror))
barplot(table(movie_ff$genre_horror),
        main="sw_female_actor0")
boxplot(movie_ff$revenue ~ movie_ff$genre_horror, cex.axis=1.5)
chisq.test(movie_ff$genre_horror,movie_ff$revenue)

####################### Analyze genre_history:
movie_ff$genre_history <- factor(movie_ff$genre_history)
summary(movie_ff$genre_history)
table(movie_ff$genre_history)
sum(is.na(movie_ff$genre_history))
barplot(table(movie_ff$genre_history),
        main="sw_female_actor0")
boxplot(movie_ff$revenue ~ movie_ff$genre_history, cex.axis=1.5)
chisq.test(movie_ff$genre_history,movie_ff$revenue)

####################### Analyze genre_crime:
movie_ff$genre_crime <- factor(movie_ff$genre_crime)
summary(movie_ff$genre_crime)
table(movie_ff$genre_crime)
sum(is.na(movie_ff$genre_crime))
barplot(table(movie_ff$genre_crime),
        main="sw_female_actor0")
boxplot(movie_ff$revenue ~ movie_ff$genre_crime, cex.axis=1.5)
chisq.test(movie_ff$genre_crime,movie_ff$revenue)

####################### Analyze genre_mystery:
movie_ff$genre_mystery <- factor(movie_ff$genre_mystery)
summary(movie_ff$genre_mystery)
table(movie_ff$genre_mystery)
sum(is.na(movie_ff$genre_mystery))
barplot(table(movie_ff$genre_mystery),
        main="sw_female_actor0")
boxplot(movie_ff$revenue ~ movie_ff$genre_mystery, cex.axis=1.5)
chisq.test(movie_ff$genre_mystery,movie_ff$revenue)

####################### Analyze genre_family:
movie_ff$genre_family <- factor(movie_ff$genre_family)
summary(movie_ff$genre_family)
table(movie_ff$genre_family)
sum(is.na(movie_ff$genre_family))
barplot(table(movie_ff$genre_family),
        main="sw_female_actor0")
boxplot(movie_ff$revenue ~ movie_ff$genre_family, cex.axis=1.5)
chisq.test(movie_ff$genre_family,movie_ff$revenue)

####################### Analyze depart_Art:
movie_ff$depart_Art <- as.numeric(movie_ff$depart_Art)
summary(movie_ff$depart_Art)
table(movie_ff$depart_Art)
sum(is.na(movie_ff$depart_Art))
nrow(table(movie_ff$depart_Art))
max(table(movie_ff$depart_Art))
calculate_mode(movie_ff$depart_Art)
sd(movie_ff$depart_Art, na.rm=TRUE)
hist(movie_ff$depart_Art, breaks=50, ylim=c(0,4000),xlim=c(0,35),cex.axis=1.5, na.rm = TRUE)
boxplot(movie_ff$depart_Art)
plot(movie_ff$depart_Art)
outliers <- boxplot(movie_ff$depart_Art, plot=FALSE)$out
outliers
plot( movie_ff$revenue~movie_ff$depart_Art)
mod4 <- lm(movie_ff$depart_Art ~ movie_ff$revenue)
pred4<-predict(mod4)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$depart_Art,movie_ff$revenue,method="spearman")
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$depart_Art, probs=c(.25, .75), na.rm = TRUE)
iqr<-quantile(movie_ff$depart_Art, probs= .75, na.rm = TRUE) - quantile(movie_ff$depart_Art, probs= .25, na.rm = TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$depart_Art, plot=FALSE)$out
outliers
#### check distribution without outliers
movie_ff$depart_Art2 <- movie_ff$depart_Art
movie_ff$depart_Art2[movie_ff$depart_Art2 < lower | movie_ff$depart_Art2> higher] <- NA
summary(movie_ff$depart_Art2)
sd(movie_ff$depart_Art2, na.rm=TRUE)
hist(movie_ff$depart_Art2, breaks=50, ylim=c(0,4000),xlim=c(0,35),cex.axis=1.5, na.rm = TRUE)
boxplot(movie_ff$depart_Art2, ylim=c(0,35),cex.axis=1.5, na.rm = TRUE)
plot( movie_ff$revenue~movie_ff$depart_Art2,ylim=c(0,1.5e9),xlim=c(0,35),cex.axis=1.5, na.rm = TRUE)
mod4 <- lm(movie_ff$depart_Art2 ~ movie_ff$revenue)
pred4<-predict(mod4)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$depart_Art2,movie_ff$revenue,method="spearman")

####Compare missings:
movie_ff2 <- movie_ff
NV<-movie_ff$depart_Art <- ifelse(is.na(movie_ff$depart_Art)==TRUE,0,1)
table(NV)
mod1 <- glm(NV ~ .-NV, data= movie_ff, family="binomial")
summary(mod1)
mod1 <- glm(NV~ +high_release_month + depart_Visual_Effects_female+actor2_prev_revenue +depart_Custom_Mkup_female+ depart_Production_female+ depart_Lighting  +ï..movie_id + revenue + budget + original_language + popularity + runtime + release_year+ release_month+ sw_collection+ countries_cnt+sw_lang_en+ actor0_movies_cnt+ depart_Production_female + actor0_prev_revenue +genre_fantasy+genre_thriller+ depart_Custom_Mkup, data= movie_ff, family="binomial")
summary(mod1)
##mod1 <- glm(NV ~ .-sw_lang_en- actor0_movies_cnt5-sw_female_actor0 - sw_female_actor0-actor0_prev_revenue5 - depart_Art2 -  actor1_movies_5y_cnt5- original_language - release_month - release_date- release_year- release_day-seasonality, data= movie_ff, family="binomial")
##summary(mod1)
##l <- sapply(movie_ff, function(x) is.factor(x))
##l

#actors_cnt_full has rows where actors_cnt is not missing
depart_Art_full <- subset(movie_ff,is.na(depart_Art)==FALSE)
#actors_cnt_na has rows where actors_cnt is missing
depart_Art_na <- subset(movie_ff,is.na(depart_Art)==TRUE)
exploreData(actors_cnt_full)
exploreData(actors_cnt_na)
t.test(depart_Art_full$revenue, depart_Art_na$revenue, paired = FALSE)
t.test(depart_Art_full$revenue, movie_ff$revenue, paired = FALSE)
t.test(depart_Art_na$revenue, movie_ff$revenue, paired = FALSE)
boxplot(movie_ff$revenue, na.rm = TRUE, ylim=c(0,1800000000), col="blue")
boxplot(depart_Art_full$revenue, na.rm = TRUE, ylim=c(0,1800000000), col="green")
boxplot(depart_Art_na$revenue, na.rm = TRUE, ylim=c(0,1800000000), col="red")


####################### depart_Custom_Mkup:
movie_ff$depart_Custom_Mkup <- as.numeric(movie_ff$depart_Custom_Mkup)
summary(movie_ff$depart_Custom_Mkup)
table(movie_ff$depart_Custom_Mkup)
sum(is.na(movie_ff$depart_Custom_Mkup))
nrow(table(movie_ff$depart_Custom_Mkup))
max(table(movie_ff$depart_Custom_Mkup))
calculate_mode(movie_ff$depart_Custom_Mkup)
sd(movie_ff$depart_Custom_Mkup, na.rm=TRUE)
hist(movie_ff$depart_Custom_Mkup, breaks=50)
boxplot(movie_ff$depart_Custom_Mkup)
plot(movie_ff$depart_Custom_Mkup)
outliers <- boxplot(movie_ff$depart_Custom_Mkup, plot=FALSE)$out
outliers

plot( movie_ff$revenue~movie_ff$depart_Custom_Mkup)
mod4 <- lm(movie_ff$depart_Custom_Mkup ~ movie_ff$revenue)
pred4<-predict(mod4)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$depart_Custom_Mkup,movie_ff$revenue,method="spearman")


#### calculate the IQR and outliers:
Q <- quantile(movie_ff$depart_Custom_Mkup, probs=c(.25, .75), na.rm = TRUE)
iqr<-quantile(movie_ff$depart_Custom_Mkup, probs= .75, na.rm = TRUE) - quantile(movie_ff$depart_Custom_Mkup, probs= .25, na.rm = TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$depart_Custom_Mkup, plot=FALSE)$out
outliers

#### check distribution without outliers
movie_ff$depart_Custom_Mkup2 <- movie_ff$depart_Custom_Mkup
movie_ff$depart_Custom_Mkup2[movie_ff$depart_Custom_Mkup2 < lower | movie_ff$depart_Custom_Mkup2> higher] <- NA
summary(movie_ff$depart_Custom_Mkup2)
sd(movie_ff$depart_Custom_Mkup2, na.rm=TRUE)
hist(movie_ff$depart_Custom_Mkup2, breaks=50, ylim=c(0,3500),xlim=c(0,35),cex.axis=1.5, na.rm = TRUE)
boxplot(movie_ff$depart_Custom_Mkup2, ylim=c(0,30),cex.axis=1.5, na.rm = TRUE)


plot( movie_ff$revenue~movie_ff$depart_Custom_Mkup2,ylim=c(0,1.5e9),xlim=c(0,30),cex.axis=1.5, na.rm = TRUE)
mod4 <- lm(movie_ff$depart_Custom_Mkup2 ~ movie_ff$revenue)
pred4<-predict(mod4)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$depart_Custom_Mkup2,movie_ff$revenue,method="spearman")


####################### depart_Lighting:
movie_ff$depart_depart_Lighting <- as.numeric(movie_ff$depart_Lighting)
summary(movie_ff$depart_Lighting)
table(movie_ff$depart_Lighting)
sum(is.na(movie_ff$depart_Lighting))
nrow(table(movie_ff$depart_Lighting))
max(table(movie_ff$depart_Lighting))
calculate_mode(movie_ff$depart_Lighting)
sd(movie_ff$depart_Lighting, na.rm = TRUE)
hist(movie_ff$depart_Lighting, breaks=50,cex.axis=1.5)
boxplot(movie_ff$depart_Lighting,cex.axis=1.5)
plot(movie_ff$depart_Lighting)
outliers <- boxplot(movie_ff$depart_Lighting, plot=FALSE)$out
outliers
P <-plot( movie_ff$revenue~movie_ff$depart_Lighting)
mod4 <- lm(movie_ff$depart_Lighting ~ movie_ff$revenue)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$depart_Lighting,movie_ff$revenue,method="spearman")
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$depart_Lighting, probs=c(.25, .75), na.rm = TRUE)
iqr<-quantile(movie_ff$depart_Lighting, probs= .75, na.rm = TRUE) - quantile(movie_ff$depart_Lighting, probs= .25, na.rm = TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$depart_Lighting, plot=FALSE)$out
outliers
#### check distribution without outliers
movie_ff$depart_Lighting2 <- movie_ff$depart_Lighting
movie_ff$depart_Lighting2[movie_ff$depart_Lighting2 < lower | movie_ff$depart_Lighting2> higher] <- NA
summary(movie_ff$depart_Lighting2)
sd(movie_ff$depart_Lighting2, na.rm=TRUE)
hist(movie_ff$depart_Lighting2, breaks=50, ylim=c(0,6000),xlim=c(0,35),cex.axis=1.5, na.rm = TRUE)
boxplot(movie_ff$depart_Lighting2, ylim=c(0,30),cex.axis=1.5, na.rm = TRUE)
plot( movie_ff$revenue~movie_ff$depart_Lighting2,ylim=c(0,1.5e9),xlim=c(0,30),cex.axis=1.5, na.rm = TRUE)
mod4 <- lm(movie_ff$depart_Lighting2 ~ movie_ff$revenue)
pred4<-predict(mod4)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$depart_Lighting2,movie_ff$revenue,method="spearman")


####################### depart_Visual_Effects:
movie_ff$depart_Visual_Effects <- as.numeric(movie_ff$depart_Visual_Effects)
summary(movie_ff$depart_Visual_Effects)
table(movie_ff$depart_Visual_Effects)
sum(is.na(movie_ff$depart_Visual_Effects))
nrow(table(movie_ff$depart_Visual_Effects))
max(table(movie_ff$depart_Visual_Effects))
calculate_mode(movie_ff$depart_Visual_Effects)
sd(movie_ff$depart_Visual_Effects, na.rm=TRUE)
hist(movie_ff$depart_Visual_Effects, breaks=50)
boxplot(movie_ff$depart_Visual_Effects)
plot(movie_ff$depart_Visual_Effects)
outliers <- boxplot(movie_ff$depart_Visual_Effects, plot=FALSE)$out
outliers
plot( movie_ff$revenue~movie_ff$depart_Visual_Effects)
mod4 <- lm(movie_ff$depart_Visual_Effects ~ movie_ff$revenue)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$depart_Visual_Effects,movie_ff$revenue,method="spearman")
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$depart_Visual_Effects, probs=c(.25, .75), na.rm = TRUE)
iqr<-quantile(movie_ff$depart_Visual_Effects, probs= .75, na.rm = TRUE) - quantile(movie_ff$depart_Visual_Effects, probs= .25, na.rm = TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$depart_Visual_Effects, plot=FALSE)$out
outliers
#### check distribution without outliers
movie_ff$depart_Visual_Effects2 <- movie_ff$depart_Visual_Effects
movie_ff$depart_Visual_Effects2[movie_ff$depart_Visual_Effects2 < lower | movie_ff$depart_Visual_Effects2> higher] <- NA
summary(movie_ff$depart_Visual_Effects2)
sd(movie_ff$depart_Visual_Effects2, na.rm=TRUE)
hist(movie_ff$depart_Visual_Effects2, breaks=50, ylim=c(0,6000),xlim=c(0,35),cex.axis=1.5, na.rm = TRUE)
boxplot(movie_ff$depart_Visual_Effects2, ylim=c(0,30),cex.axis=1.5, na.rm = TRUE)
plot( movie_ff$revenue~movie_ff$depart_Visual_Effects2,ylim=c(0,1.5e9),xlim=c(0,30),cex.axis=1.5, na.rm = TRUE)
mod4 <- lm(movie_ff$depart_Visual_Effects2 ~ movie_ff$revenue)
pred4<-predict(mod4)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$depart_Visual_Effects2,movie_ff$revenue,method="spearman")
count(movie_ff,depart_Visual_Effects, "0")

####################### depart_Camera_female:
summary(movie_ff$depart_Camera_female)
table(movie_ff$depart_Camera_female)
sum(is.na(movie_ff$depart_Camera_female))
nrow(table(movie_ff$depart_Camera_female))
max(table(movie_ff$depart_Camera_female))
calculate_mode(movie_ff$depart_Camera_female)
sd(movie_ff$depart_Camera_female)
hist(movie_ff$depart_Camera_female, breaks=50)
boxplot(movie_ff$depart_Camera_female)
plot(movie_ff$depart_Camera_female)
outliers <- boxplot(movie_ff$depart_Camera_female, plot=FALSE)$out
outliers

####################### depart_Directing_female:
summary(movie_ff$depart_Directing_female)
table(movie_ff$depart_Directing_female)
sum(is.na(movie_ff$depart_Directing_female))
nrow(table(movie_ff$depart_Directing_female))
max(table(movie_ff$depart_Directing_female))
calculate_mode(movie_ff$depart_Directing_female)
sd(movie_ff$depart_Directing_female, na.rm=TRUE)
hist(movie_ff$depart_Directing_female, breaks=50)
boxplot(movie_ff$depart_Directing_female)
plot(movie_ff$depart_Directing_female)
outliers <- boxplot(movie_ff$depart_Directing_female, plot=FALSE)$out
outliers
plot( movie_ff$revenue~movie_ff$depart_Directing_female)
mod4 <- lm(movie_ff$depart_Directing_female ~ movie_ff$revenue)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$depart_Directing_female,movie_ff$revenue,method="spearman")
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$depart_Directing_female, probs=c(.25, .75), na.rm = TRUE)
iqr<-quantile(movie_ff$depart_Directing_female, probs= .75, na.rm = TRUE) - quantile(movie_ff$depart_Directing_female, probs= .25, na.rm = TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$depart_Directing_female, plot=FALSE)$out
outliers
#### check distribution without outliers
movie_ff$depart_Directing_female2 <- movie_ff$depart_Directing_female
movie_ff$depart_Directing_female2[movie_ff$depart_Directing_female2 < lower | movie_ff$depart_Directing_female2> higher] <- NA
summary(movie_ff$depart_Directing_female2)
sd(movie_ff$depart_Directing_female2, na.rm=TRUE)
hist(movie_ff$depart_Directing_female2, breaks=50, ylim=c(0,6000),xlim=c(0,35),cex.axis=1.5, na.rm = TRUE)
boxplot(movie_ff$depart_Directing_female2, ylim=c(0,30),cex.axis=1.5, na.rm = TRUE)
plot( movie_ff$revenue~movie_ff$depart_Directing_female2,ylim=c(0,1.5e9),xlim=c(0,30),cex.axis=1.5, na.rm = TRUE)
mod4 <- lm(movie_ff$depart_Directing_female2 ~ movie_ff$revenue)
pred4<-predict(mod4)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$depart_Directing_female2,movie_ff$revenue,method="spearman")


####################### depart_Production_female:
summary(movie_ff$depart_Production_female)
table(movie_ff$depart_Production_female)
sum(is.na(movie_ff$depart_Production_female))
nrow(table(movie_ff$depart_Production_female))
max(table(movie_ff$depart_Production_female))
calculate_mode(movie_ff$depart_Production_female)
sd(movie_ff$depart_Production_female, na.rm = TRUE)
hist(movie_ff$depart_Production_female, breaks=50)
boxplot(movie_ff$depart_Production_female)
plot(movie_ff$depart_Production_female)
outliers <- boxplot(movie_ff$depart_Production_female, plot=FALSE)$out
outliers
plot( movie_ff$revenue~movie_ff$depart_Production_female)
mod4 <- lm(movie_ff$depart_Production_female ~ movie_ff$revenue)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$depart_Production_female,movie_ff$revenue,method="spearman")
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$depart_Production_female, probs=c(.25, .75), na.rm = TRUE)
iqr<-quantile(movie_ff$depart_Production_female, probs= .75, na.rm = TRUE) - quantile(movie_ff$depart_Production_female, probs= .25, na.rm = TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$depart_Production_female, plot=FALSE)$out
outliers
#### check distribution without outliers
movie_ff$depart_Production_female2 <- movie_ff$depart_Production_female
movie_ff$depart_Production_female2[movie_ff$depart_Production_female2 < lower | movie_ff$depart_Production_female2> higher] <- NA
summary(movie_ff$depart_Production_female2)
sd(movie_ff$depart_Production_female2, na.rm=TRUE)
hist(movie_ff$depart_Production_female2, breaks=50, ylim=c(0,4000),xlim=c(0,12),cex.axis=1.5, na.rm = TRUE)
boxplot(movie_ff$depart_Production_female2, ylim=c(0,30),cex.axis=1.5, na.rm = TRUE)
plot( movie_ff$revenue~movie_ff$depart_Production_female2,ylim=c(0,1.5e9),xlim=c(0,30),cex.axis=1.5, na.rm = TRUE)
mod4 <- lm(movie_ff$depart_Production_female2 ~ movie_ff$revenue)
pred4<-predict(mod4)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$depart_Production_female2,movie_ff$revenue,method="spearman")
####Compare missings:
NV<-movie_ff$depart_Production_female <- ifelse(is.na(movie_ff$depart_Production_female)==TRUE,0,1)
table(NV)
##mod1 <- glm(NV ~ .-NV, data= movie_ff, family="binomial")
##summary(mod1)
mod1 <- glm(NV~ +high_release_month + depart_Visual_Effects_female+actor2_prev_revenue +depart_Custom_Mkup_female+ depart_Production_female+ depart_Lighting  +ï..movie_id + revenue + budget + original_language + popularity + runtime + release_year+ release_month+ sw_collection+ countries_cnt+sw_lang_en+ actor0_movies_cnt+ depart_Production_female + actor0_prev_revenue +genre_fantasy+genre_thriller+ depart_Custom_Mkup, data= movie_ff, family="binomial")
summary(mod1)

count(movie_ff, movie_ff$depart_Production_female, "0")

####################### depart_Writing_female:
summary(movie_ff$depart_Writing_female)
table(movie_ff$depart_Writing_female)
sum(is.na(movie_ff$depart_Writing_female))
nrow(table(movie_ff$depart_Writing_female))
max(table(movie_ff$depart_Writing_female))
calculate_mode(movie_ff$depart_Writing_female)
sd(movie_ff$depart_Writing_female)
hist(movie_ff$depart_Writing_female, breaks=50)
boxplot(movie_ff$depart_Writing_female)
plot(movie_ff$depart_Writing_female)
outliers <- boxplot(movie_ff$depart_Writing_female, plot=FALSE)$out
outliers

movie_ff2 <- movie_ff
movie_ff2$sws <- ifelse(is.na(movie_ff2$sws)==TRUE, 0,1)
movie_ff2$ps <- ifelse(is.na(movie_ff2$ps)==TRUE, 0,1)
movie_ff2$mls <- ifelse(is.na(movie_ff2$mls)==TRUE, 0,1)

count(movie_ff, depart_Writing_female, "0") 
animal2

##the percentage of missing 
mm <- getMissingness(movie_ff, getRows = T)
head(mm$rows)
sum(mm$rows)
head(missingMatrix(movie_ff))

