closeAllConnections()
rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table,
               plyr,
               dplyr,
               Amelia,
               Rtsne,
               Boruta,
               h2o)

setwd("C:/Users/satyakama.paul/Desktop/tt/test data/train")


raw_acc_70 <- read.csv("raw_account_70_new.csv",
                       sep = ",",
                       header = T,
                       na.strings = c(""),
                       stringsAsFactors = F)

dim(raw_acc_70)


raw_data_70 <- read.csv("raw_data_70_new.csv",
                        sep = ",",
                        header = T,
                        na.strings = c(""),
                        stringsAsFactors = F)
dim(raw_data_70)

raw_enquiry_70 <- read.csv("raw_enquiry_70_new.csv",
                           sep = ",",
                           header = T,
                           na.strings = c(""),
                           stringsAsFactors = F)

dim(raw_enquiry_70)

#Finding variables common in the 3 datasets
common_vars <- print(intersect(intersect(colnames(raw_acc_70),
                                         colnames(raw_data_70)),
                               colnames(raw_enquiry_70)))


#Joining raw_acc_70 and raw_data_70

data_join1 <- left_join(raw_acc_70,
                        raw_data_70,
                        by = common_vars)

dim(data_join1)

#Joining data_join1 and raw_data_70

data_join2 <- left_join(data_join1,
                        raw_enquiry_70,
                        by = common_vars)

dim(data_join2)

#Removing duplicate rows, if any

data_remove.iden.rows <- data_join2 %>% distinct(customer_no, 
                                                 dt_opened, 
                                                 .keep_all = TRUE)

dim(data_remove.iden.rows)

#Removing certain duplicate variables
#The duplicate variables are:
# [1] upload_dt.x, upload_dt.y


na_sum.upload_dt.x <- print(sum(is.na(data_remove.iden.rows$upload_dt.x)))

na_sum.upload_dt.y <- print(sum(is.na(data_remove.iden.rows$upload_dt.y)))

# Dropping variable upload_dt.y that has more NA's than upload_dt.x

data_drop.dup.vars <- subset(data_remove.iden.rows,
                             select = -c(
                                  if(na_sum.upload_dt.x >= na_sum.upload_dt.y){
                                       upload_dt.x
                                  }
                                  
                                  else{upload_dt.y}
                             ))

#Working with dates

#Changing appropriate variables to Date

date_vars.col.position <- c(1,3,6,7,8,9,15,16,22,24,43,75,76, 103)

data_drop.dup.vars[,date_vars.col.position] <- as.Date(sapply(data_drop.dup.vars[, date_vars.col.position], 
                                                              as.Date, 
                                                              format="%d-%b-%y"), 
                                                       origin="1970-01-01")

#Note the dates have now been changed to numbers

#Removing unimportatn variables manually

data_rem.unimp.vars <- subset(data_drop.dup.vars,
                              select = -c(customer_no,
                                          paymenthistory1,
                                          paymenthistory2,
                                          feature_20,
                                          feature_22,
                                          feature_24,
                                          feature_45,
                                          feature_47,
                                          feature_63,
                                          feature_70,
                                          feature_75,
                                          feature_77)) 
#feature_63 and 70 and 75 is some incomplete dates


#Removing features with large number of NA values

# missmap(data_rem.unimp.vars,
#         main = "Missmap on data_rem.unimp.vars",
#         x.cex = 0.5)


#find the number of nonmissing values each variable

data_rem.unimp.vars.nonNA <- sapply(data_rem.unimp.vars,
                                    function(x) sum(!is.na(x)))

print(sort((data_rem.unimp.vars.nonNA/nrow(data_rem.unimp.vars))*100, 
           decreasing = T))

#varlist = variables that satisfy a specific pre criteria
varlist = NULL
for (i in 1:length(data_rem.unimp.vars.nonNA)){
     
     if (data_rem.unimp.vars.nonNA[i] > (0.6*nrow(data_rem.unimp.vars)))
     {varlist[i] = names(data_rem.unimp.vars.nonNA[i])}
     else {varlist[i] = "not.adequete.datapoints.variable"}
     
}

print(varlist)


colnames(data_rem.unimp.vars) <- c(varlist)

print(colnames(data_rem.unimp.vars))

#data_reduced = reduced dataset (reduced by the criteria of nonNA)

data_reduced <- data_rem.unimp.vars[, !duplicated(colnames(data_rem.unimp.vars))]

#removing the specific variable "not.adequete.datapoints.variable" from data_reduced

data_reduced <- subset(data_reduced,
                       select = -c(not.adequete.datapoints.variable))
dim(data_reduced)

# missmap(data_reduced,
#         main = "Missing values of data_reduced",
#         x.cex = 0.5)

#Complete cases of data_reduced
#data_reduced.c = reduced dataset that is Complete
data_reduced.c <- data_reduced[complete.cases(data_reduced),]
dim(data_reduced.c)

write.table(data_reduced.c,
            "data_reduced_c.csv",
            sep = ",",
            na = "NA",
            col.names = T,
            row.names = F)


#Removing certain variables that have the same value

data_reduced.c <- subset(data_reduced.c,
                         select = -c(feature_5,
                                     feature_6,
                                     feature_54))

#Changing certain variables into factors

factor.vars <- c(
     "acct_type",
     "owner_indic",
     "feature_1",
     "feature_4",
     "feature_11",
     "feature_12",
     "feature_15",
     "feature_16",
     "feature_19",
     "feature_23",
     "feature_25",
     "feature_27",
     "feature_28",
     "feature_32",
     "feature_33",
     "feature_34",
     "feature_43",
     "feature_46",
     "feature_50",
     "feature_55",
     "feature_58",
     "feature_59",
     "feature_60",
     "feature_62",
     "feature_67",
     "feature_68",
     "feature_72",
     "feature_76",
     "feature_78",
     "feature_79",
     "Bad_label",
     "enq_purpose")



data_reduced.c[factor.vars] <- lapply(data_reduced.c[factor.vars], as.factor)

str(data_reduced.c)


#View(data_reduced.c)

#To view all factor variables and their levels - objective is to reduce levels of some that
#contain large no. of levels

rapply(data_reduced.c, class = "factor", f = levels, how = "list")


# Working with features that have a large no. of levels

data_reduced.c$feature_15 <- casefold(data_reduced.c$feature_15,
                                      upper = T)

data_reduced.c$feature_15 <- substr(data_reduced.c$feature_15,
                                    start = 1,
                                    stop = 2)

length(unique(data_reduced.c$feature_15))

#Too many levels - hence dropping
data_reduced.c <- subset(data_reduced.c,
                         select = -c(feature_15))


data_reduced.c$feature_16 <- casefold(data_reduced.c$feature_16,
                                      upper = T)

data_reduced.c$feature_16 <- substr(data_reduced.c$feature_16,
                                    start = 1,
                                    stop = 2)

length(unique(data_reduced.c$feature_16))

#Too many levels - hence dropping
data_reduced.c <- subset(data_reduced.c,
                         select = -c(feature_16,
                                     feature_62,
                                     feature_43))

train_final <- data_reduced.c

#======================================================
#Training data preprocessing ends here
#======================================================

#======================================================
#Extracting test data
#======================================================

raw_acc_30 <- read.csv("raw_account_30_new.csv",
                       sep = ",",
                       header = T,
                       na.strings = c(""),
                       stringsAsFactors = F)

dim(raw_acc_30)


raw_data_30 <- read.csv("raw_data_30_new.csv",
                        sep = ",",
                        header = T,
                        na.strings = c(""),
                        stringsAsFactors = F)
dim(raw_data_30)

raw_enquiry_30 <- read.csv("raw_enquiry_30_new.csv",
                           sep = ",",
                           header = T,
                           na.strings = c(""),
                           stringsAsFactors = F)

dim(raw_enquiry_30)

#Finding variables common in the 3 datasets
common_vars30 <- print(intersect(intersect(colnames(raw_acc_30),
                                           colnames(raw_data_30)),
                                 colnames(raw_enquiry_30)))


#Joining raw_acc_70 and raw_data_70

data_join11 <- left_join(raw_acc_30,
                         raw_data_30,
                         by = common_vars30)

dim(data_join11)

#Joining data_join1 and raw_data_70

data_join22 <- left_join(data_join11,
                         raw_enquiry_30,
                         by = common_vars)

dim(data_join22)

#Removing duplicate rows, if any

test_data_remove.iden.rows <- data_join22 %>% distinct(customer_no, 
                                                       dt_opened, 
                                                       .keep_all = TRUE)

dim(test_data_remove.iden.rows)

#Keeping only those variables in test that exist in train
test_final <- subset(test_data_remove.iden.rows,
                     select = colnames(train_final))

test_final <- test_final[complete.cases(test_final),]

dim(test_final)

#Changing test variables in as categorical or numeric varaibles as same as in train

factor_vars.in.training <- names(Filter(is.factor, train_final))

test_final[factor_vars.in.training] <- lapply(test_final[factor_vars.in.training], 
                                              factor)
#Correct uptill now

date_vars.col.position.test <- c(1,2,5:7,10:12,14,21,59 )

test_final[,date_vars.col.position.test] <- as.Date(sapply(test_final[, 
                                                                      date_vars.col.position.test], 
                                                           as.Date, 
                                                           format="%d-%b-%y"), 
                                                    origin="1970-01-01")



#======================================================
#Modeling
#======================================================


print(require(h2o))

h2o.init(ip = "localhost",
         nthreads = 3,
         max_mem_size = "3g")

h2o.removeAll()


d <- as.h2o(train_final)
splits = h2o.splitFrame(d,
                        0.8,
                        c("train","test"),
                        seed=100)

train = splits[[1]]
dim(train)

test = splits[[2]]
dim(test)

levels(test_final$feature_27) <- levels(train_final$feature_27)
levels(test_final$feature_28) <- levels(train_final$feature_28)
levels(test_final$feature_76) <- levels(train_final$feature_76)
levels(test_final$enq_purpose) <- levels(train_final$enq_purpose)

d2 <- as.h2o(test_final)


# Deeplearning hyperparamters

activation_opt <- c("Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout")
l1_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1)
l2_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1)
hyper_params <- list(activation = activation_opt,
                     l1 = l1_opt,
                     l2 = l2_opt)
search_criteria <- list(strategy = "RandomDiscrete", 
                        max_models = 50)


dl_grid <- h2o.grid("deeplearning", 
                    x = c(1:57,59:61),
                    y = 58,
                    training_frame = train,
                    balance_classes = T,
                    grid_id = "dl_grid",
                    validation_frame = test,
                    seed = 1,
                    hidden = c(25,25, 25),
                    hyper_params = hyper_params,
                    search_criteria = search_criteria)

dl_gridperf <- h2o.getGrid(grid_id = "dl_grid", 
                           sort_by = "auc", 
                           decreasing = TRUE)
print(dl_gridperf)


l21_opt <- seq(0.09,0.11, 0.001)
l22_opt <- seq(9e-04, 0.0011, 1e-04)
hyper_params2 <- list(activation = activation_opt,
                     l1 = l21_opt,
                     l2 = l22_opt)


dl_grid2 <- h2o.grid("deeplearning", 
                      x = c(1:57,59:61),
                      y = 58,
                      training_frame = train,
                      balance_classes = T,
                      grid_id = "dl_grid2",
                      validation_frame = test,
                      seed = 1,
                      hidden = c(25,25, 25),
                      hyper_params = hyper_params2,
                      search_criteria = search_criteria)

dl_gridperf2 <- h2o.getGrid(grid_id = "dl_grid2", 
                           sort_by = "auc", 
                           decreasing = TRUE)
print(dl_gridperf2)


best_dl_model_id <- dl_gridperf2@model_ids[[1]]
best_dl <- h2o.getModel(best_dl_model_id)


best_dl_perf <- h2o.performance(model = best_dl, 
                                newdata = test)
h2o.auc(best_dl_perf)  


h2o.auc(best_dl) 

h2o.confusionMatrix(best_dl, d2)
