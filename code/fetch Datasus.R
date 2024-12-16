#install.packages("remotes")
remotes::install_github("rfsaldanha/microdatasus")




library(microdatasus)


### Info about the variables in the files ("Convenções SIH RD"): https://github.com/rfsaldanha/microdatasus/wiki/Conven%C3%A7%C3%B5es-SIH-RD


### Distrito Federal
dados.df <- fetch_datasus(year_start = 2012, year_end = 2021, month_start= 1, month_end=12, uf = "DF", information_system = "SIH-RD", 
                          vars=c("UF_ZI", "ANO_CMPT", "MES_CMPT", "ESPEC", "MUNIC_RES", "NASC", "SEXO", "QT_DIARIAS", "DT_INTER", "DT_SAIDA", "DIAG_PRINC", "DIAG_SECUN",
                                 "COBRANCA", "MUNIC_MOV", "COD_IDADE", "IDADE", "DIAS_PERM", "MORTE", "CAR_INT", "CBOR", "CID_ASSO", "CID_MORTE", "COMPLEX", "RACA_COR", "ETNIA"))
dados.dfproc <- process_sih(dados.df, municipality_data = TRUE)
saveRDS(dados.df, file='dadosdf.rds')

### Pernambuco
dados.pe <- fetch_datasus(year_start = 2012, year_end = 2021, month_start= 1, month_end=12, uf = "PE", information_system = "SIH-RD", 
                          vars=c("UF_ZI", "ANO_CMPT", "MES_CMPT", "ESPEC", "MUNIC_RES", "NASC", "SEXO", "QT_DIARIAS", "DT_INTER", "DT_SAIDA", "DIAG_PRINC", "DIAG_SECUN",
                                 "COBRANCA", "MUNIC_MOV", "COD_IDADE", "IDADE", "DIAS_PERM", "MORTE", "CAR_INT", "CBOR", "CID_ASSO", "CID_MORTE", "COMPLEX", "RACA_COR", "ETNIA"))
dados.peproc <- process_sih(dados.pe, municipality_data = TRUE)
saveRDS(dados.pe, file='dadospe.rds')


### São Paulo
dados.sp <- fetch_datasus(year_start = 2012, year_end = 2021, month_start= 1, month_end=12, uf = "SP", information_system = "SIH-RD", 
                          vars=c("UF_ZI", "ANO_CMPT", "MES_CMPT", "ESPEC", "MUNIC_RES", "NASC", "SEXO", "QT_DIARIAS", "DT_INTER", "DT_SAIDA", "DIAG_PRINC", "DIAG_SECUN",
                                 "COBRANCA", "MUNIC_MOV", "COD_IDADE", "IDADE", "DIAS_PERM", "MORTE", "CAR_INT", "CBOR", "CID_ASSO", "CID_MORTE", "COMPLEX", "RACA_COR", "ETNIA"))
dados.spproc <- process_sih(dados.sp, municipality_data = TRUE)
saveRDS(dados.sp, file='dadossp.rds')

### Rio Grande do Sul
dados.rs <- fetch_datasus(year_start = 2012, year_end = 2021, month_start= 1, month_end=12, uf = "RS", information_system = "SIH-RD", 
                          vars=c("UF_ZI", "ANO_CMPT", "MES_CMPT", "ESPEC", "MUNIC_RES", "NASC", "SEXO", "QT_DIARIAS", "DT_INTER", "DT_SAIDA", "DIAG_PRINC", "DIAG_SECUN",
                                 "COBRANCA", "MUNIC_MOV", "COD_IDADE", "IDADE", "DIAS_PERM", "MORTE", "CAR_INT", "CBOR", "CID_ASSO", "CID_MORTE", "COMPLEX", "RACA_COR", "ETNIA"))
dados.rsproc <- process_sih(dados.rs, municipality_data = TRUE)
saveRDS(dados.rs, file='dadosrs.rds')



#### Extra states 

### Amazonas
dados.am <- fetch_datasus(year_start = 2012, year_end = 2021, month_start= 1, month_end=12, uf = "AM", information_system = "SIH-RD", 
                          vars=c("UF_ZI", "ANO_CMPT", "MES_CMPT", "ESPEC", "MUNIC_RES", "NASC", "SEXO", "QT_DIARIAS", "DT_INTER", "DT_SAIDA", "DIAG_PRINC", "DIAG_SECUN",
                                 "COBRANCA", "MUNIC_MOV", "COD_IDADE", "IDADE", "DIAS_PERM", "MORTE", "CAR_INT", "CBOR", "CID_ASSO", "CID_MORTE", "COMPLEX", "RACA_COR", "ETNIA"))
dados.amproc <- process_sih(dados.am, municipality_data = TRUE)
saveRDS(dados.am, file='dadosam.rds')

### Santa Catarina
dados.sc <- fetch_datasus(year_start = 2012, year_end = 2021, month_start= 1, month_end=12, uf = "SC", information_system = "SIH-RD", 
                          vars=c("UF_ZI", "ANO_CMPT", "MES_CMPT", "ESPEC", "MUNIC_RES", "NASC", "SEXO", "QT_DIARIAS", "DT_INTER", "DT_SAIDA", "DIAG_PRINC", "DIAG_SECUN",
                                 "COBRANCA", "MUNIC_MOV", "COD_IDADE", "IDADE", "DIAS_PERM", "MORTE", "CAR_INT", "CBOR", "CID_ASSO", "CID_MORTE", "COMPLEX", "RACA_COR", "ETNIA"))
dados.scproc <- process_sih(dados.sc, municipality_data = TRUE)
saveRDS(dados.sc, file='dadossc.rds')

### Bahia
dados.ba <- fetch_datasus(year_start = 2012, year_end = 2021, month_start= 1, month_end=12, uf = "BA", information_system = "SIH-RD", 
                          vars=c("UF_ZI", "ANO_CMPT", "MES_CMPT", "ESPEC", "MUNIC_RES", "NASC", "SEXO", "QT_DIARIAS", "DT_INTER", "DT_SAIDA", "DIAG_PRINC", "DIAG_SECUN",
                                 "COBRANCA", "MUNIC_MOV", "COD_IDADE", "IDADE", "DIAS_PERM", "MORTE", "CAR_INT", "CBOR", "CID_ASSO", "CID_MORTE", "COMPLEX", "RACA_COR", "ETNIA"))
dados.baproc <- process_sih(dados.ba, municipality_data = TRUE)
saveRDS(dados.ba, file='dadosba.rds')


### Rio de Janeiro:

dados.rj <- fetch_datasus(year_start = 2012, year_end = 2021, month_start= 1, month_end=12, uf = "RJ", information_system = "SIH-RD", 
                          vars=c("UF_ZI", "ANO_CMPT", "MES_CMPT", "ESPEC", "MUNIC_RES", "NASC", "SEXO", "QT_DIARIAS", "DT_INTER", "DT_SAIDA", "DIAG_PRINC", "DIAG_SECUN",
                                 "COBRANCA", "MUNIC_MOV", "COD_IDADE", "IDADE", "DIAS_PERM", "MORTE", "CAR_INT", "CBOR", "CID_ASSO", "CID_MORTE", "COMPLEX", "RACA_COR", "ETNIA"))
dados.rjproc <- process_sih(dados.rj, municipality_data = TRUE)
saveRDS(dados.rj, file='dadosrj.rds')
