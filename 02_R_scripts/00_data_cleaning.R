# load the HSLS open dataset
load("/Users/panpeter/Desktop/PhD_Learning/HSLS-09_Project/public_dataset/hsls_17_student_pets_sr_v1_0.rdata")

# subset the data
df <- hsls_17_student_pets_sr_v1_0
hist(df$X1SES)
dim(df)
df_01 <- df[which(df$X1SES >=-2), c("STU_ID","X1SES","X1MTHEFF","X1TXMTSCOR","X3TGPAMAT")]
df_02 <- df_01[which(df$X1MTHEFF >=-3),]
df_03 <- df_02[which(df$X1TXMTSCOR >0),]
df_04 <- df_03[which(df_03$X3TGPAMAT>0),]
hsls_sub <- df_04

write.csv(hsls_sub,"~/Desktop/PhD_Learning/DeCarlo's Meeting/simdata/01_data/02_processed_data/hsls_sub.csv",
          row.names = F)