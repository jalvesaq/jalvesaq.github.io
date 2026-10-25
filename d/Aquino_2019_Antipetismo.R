# Código para replicar resultados publicados no capítulo:
#
# AQUINO, Jakson Alves de.
# Conservadorismo e ressentimento: duas fontes do antipetismo.
# In: SILVA, Emanuel Freitas da; FROTA, Francisco Horácio da Silva;
# SILVA, Maria Andrea Luz da (Orgs.).
# Atores políticos e dinâmicas eleitorais.
# Fortaleza: Edmeta, 2019, p. 232-273.


###################################
####         INICIO            ####

library("foreign")
library("descr")
library("jtools")
options(OutDec = ",")

FormatP <- function(x)
{
    if(x < 0.0005 && x > -0.0005)
        return("0,000")
    return(sub("\\.", ",", sprintf("%0.3f", x)))
}

SubOmissos <- function(x)
{
    o <- is.na(x)
    dados <- x
    dados[o] <- sample(x[!o], sum(o), replace = TRUE)
    dados
}

SubNomes <- function(x)
{
    x <- sub("renda.fam", "Renda familiar", x)
    x <- sub("religiaoCatólica", "Católica", x)
    x <- sub("religiaoEvangélica", "Evangélica", x)
    x <- sub("religiosoSim", "Religiosa", x)
    x <- sub("religioso", "Religiosa", x)
    x <- sub("igualitarismo", "Igualitarismo", x)
    x <- sub("estatismo", "Estatismo", x)
    x <- sub("contraBF", "Contra Bolsa Família", x)
    x <- sub("contraaborto", "Contra aborto", x)
    x <- sub("homofobia", "Homofobia", x)
    x <- sub("recebeBFSim", "Beneficiário do PBF", x)
    x <- sub("escolaEnsino superior", "Ensino superior", x)
    x <- sub("escolaAté ensino médio", "Ensino médio", x)
    x <- sub("escolaAté 9º ano", "Até 9º ano", x)
    x <- sub("corParda", "Parda", x)
    x <- sub("corAmarela", "Amarela", x)
    x <- sub("corOutra", "Outra", x)
    x <- sub("corNegra", "Negra", x)
    x <- sub("corIndígena", "Indígena", x)
    x <- sub("sexFemale", "Sexo feminino", x)
    x <- sub("escola.n", "Escolaridade", x)
    x <- sub("branca", "Cor branca", x)
    x
}


###########################################################################
####  Figura 1. Comparação do PIB per capita: Brasil x América Latina  ####

# Link para os dados: https://www.imf.org/external/pubs/ft/weo/2018/01/weodata/download.aspx
w <- read.delim("WEOApr2018all.xls", fileEncoding = "latin1",
                stringsAsFactors = FALSE)

p <- c("Argentina", "Bolivia", "Chile", "Colombia", "Costa Rica",
        "Dominican Republic", "Ecuador", "El Salvador", "Guatemala", "Guyana",
        "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama",
        "Paraguay", "Peru", "Puerto Rico", "Uruguay", "Venezuela")

gdp <- "NGDPDPC"

m <- w[w$Country %in% p & w$WEO.Subject.Code == gdp, c(4, 11:47)]
rn <- m$Country
m$Country <- NULL

Num <- function(x)
{
    x <- gsub(",", "", x)
    x <- as.numeric(x)
    x
}
m <- as.matrix(as.data.frame(lapply(m, Num)))
rownames(m) <- rn
colnames(m) <- sub("X", "", colnames(m))
al <- apply(m, 2, mean, na.rm = TRUE)

br <- w[w$Country == "Brazil" & w$WEO.Subject.Code == gdp, 11:47]
br <- Num(br)

par(mar = c(4, 4, 2, 0)+0.1, cex = 0.8)
plot(NULL, xlim = c(1981, 2017), ylim = c(0, max(c(al, br))),
     xlab = "Ano", ylab = "PIB per capita")

rect(1995, 0, 2003, max(c(al, br)), col = "#EEEEEE", border = NA)
rect(2011, 0, 2016, max(c(al, br)), col = "#EEEEEE", border = NA)

lines(1981:2017, al, lwd = 2, lty = 1)
lines(1981:2017, br, lwd = 2, lty = 2)

text(1999, 1000, "FHC")
text(2007, 1000, "Lula")
text(2013.5, 1000, "Dilma")

legend("topleft", lty = c(2, 1), bty = "n", legend = c("Brasil", "América Latina"))


###################################################################
####  Figura 2. Preferência partidária no Brasil -- 2010-2017  ####
# 2006:
#    SEP2. Qual o partido que melhor representa a maneira como o(a) Sr(a) pensa?
#    SEP6. Zero significa que o(a) Sr(a) NÃO gosta do partido e dez que o(a) Sr(a) gosta muito. 

# 2008: Não tem perguntas sobre preferência partidária.

# 2010
# VB11. Com qual partido o(a) sr./sra. simpatiza?
b10 <- read.dta("7948266051039660950Brazil_LAPOP_AmericasBarometer 2010 data set  approved v4.dta")
PT <- sum(b10$vb11 == "PT", na.rm = TRUE) / nrow(b10)
PMDB <- sum(b10$vb11 == "PMDB", na.rm = TRUE) / nrow(b10)
PSDB <- sum(b10$vb11 == "PSDB", na.rm = TRUE) / nrow(b10)
d <- mean(as.Date(b10$data, format = "%Y-%m-%d"), na.rm = TRUE)

# 2012
# VB11. Com qual partido o(a) sr./sra. simpatiza?
b11 <- read.dta("54861031Brazil LAPOP AmericasBarometer 2012 Rev1_W.dta")
PT <- c(PT, sum(grepl("1501", b11$vb11)) / nrow(b11))
PMDB <- c(PMDB, sum(grepl("1502", b11$vb11)) / nrow(b11))
PSDB <- c(PSDB, sum(grepl("1503", b11$vb11)) / nrow(b11))
d <- c(d, as.Date("2012-03-31", format = "%Y-%m-%d"))

# 2014
# VB11. Com qual partido o(a) sr./sra. simpatiza?
b14 <- read.spss("863896541Brazil LAPOP AmericasBarometer 2014 Espanol v3.0_W.sav")
PT <- c(PT, sum(b14$vb11 == "PT", na.rm = TRUE) / length(b14$vb11))
PMDB <- c(PMDB, sum(b14$vb11 == "PMDB", na.rm = TRUE) / length(b14$vb11))
PSDB <- c(PSDB, sum(b14$vb11 == "PSDB", na.rm = TRUE) / length(b14$vb11))
d <- c(d, mean(as.Date(as.numeric(as.character(b14$fecha)) / 86400, origin = "1582-10-14")))

# 2016
b16 <- read.dta("780314464Brazil LAPOP AmericasBarometer 2017 V1.0_W.dta")

# VB11. Com qual partido o(a) sr./sra. simpatiza?
levels(b16$vb11) <- iconv(levels(b16$vb11), "latin1", "utf-8")
PT <- 100 * c(PT, sum(b16$vb11 == "PT (Partido dos Trabalhadores)",
                na.rm = TRUE) / nrow(b16))
PMDB <- 100 * c(PMDB, sum(b16$vb11 == "PMDB (Partido do Movimento Democrático Brasileiro)",
                    na.rm = TRUE) / nrow(b16))
PSDB <- 100 * c(PSDB, sum(b16$vb11 == "PSDB (Partido da Social Democracia Brasileira)",
                    na.rm = TRUE) / nrow(b16))
d <- c(d, mean(b16$fecha))


par(mar = c(4, 4, 0.2, 1.2)+0.1, cex = 0.8)
plot(NULL, xlim = c(min(d), max(d)), ylim = c(0, 100), xaxt = "n",
     xlab  = "Data média da aplicação do questionário",
     ylab = "Preferência partidária")
axis(1, d, as.character(d))
lines(d, PT, lty = 1)
lines(d, PMDB, lty = 2)
lines(d, PSDB, lty = 3)
legend("topright", lty = c(1, 2, 3), legend = c("PT", "PMDB", "PSDB"), bty = "n")


#############################################
####  Criação da variável "Antipetista"  ####

b16$antipetista <- b16$bragrup4 == 1
b16$antipetista[is.na(b16$antipetista)] <- FALSE
p.ap <- round(100 * sum(b16$antipetista) / nrow(b16))
b16$antipetista.f <- factor(b16$antipetista,
                            levels = c(FALSE, TRUE),
                            labels = c("Não", "Sim"))


######################################################################
####  Figura 3. Proporção de antipetistas segundo a escolaridade  ####

b16$escola <- cut(b16$ed,
                  c(-1, 4, 8, 11, 18),
                  c("Até 5º ano", "Até 9º ano", "Até ensino médio", "Ensino superior"))
b16$escola.n <- as.numeric(b16$escola)
m <- summary(glm(antipetista ~ escola.n, data = b16, family = binomial()))
pesc <- FormatP(m$coefficients[2, 4])
par(mar = c(2, 4, 2, 1)+0.1, cex = 0.8)
tmp <- with(b16, crosstab(antipetista.f, escola, xlab = "Nível de escolaridade",
                          ylab = "Antipetista", inv.y = TRUE))


########################################################################
####  Figura 4. Proporção de antipetistas segundo a renda familiar  ####

b16$renda.fam <- as.numeric(b16$q10new) - 1
b16$renda.fam.f <- cut(b16$renda.fam, c(-1, 4, 12, 20),
                       labels = c("Baixa", "Média", "Alta"))
m <- summary(glm(antipetista ~ renda.fam, data = b16, family = binomial))
prenda <- FormatP(m$coefficients[2, 4])


par(mar = c(2, 2, 0, 0)+0.1, cex = 0.8)
tmp <- crosstab(b16$antipetista.f, factor(b16$renda.fam), inv.y = TRUE,
                xlab = "Nível da renda familiar", ylab = "Antipetista")


#############################################################
####  Figura 5. Proporção de antipetistas segundo a cor  ####

b16$cor <- factor(b16$etid)
levels(b16$cor) <- sub("White", "Branca", levels(b16$cor))
levels(b16$cor) <- sub("Indigenous", "Indígena", levels(b16$cor))
levels(b16$cor) <- sub("Black", "Negra", levels(b16$cor))
levels(b16$cor) <- sub("Mulatto", "Parda", levels(b16$cor))
levels(b16$cor) <- sub("Other", "Outra", levels(b16$cor))
levels(b16$cor) <- sub("Asian", "Amarela", levels(b16$cor))
m <- summary(glm(antipetista ~ etid, data = b16, family = binomial()))
pcorparda <- FormatP(m$coefficients["etidMulatto", 4])
b16$branca <- as.numeric(b16$cor == "Branca")
b16$branca[is.na(b16$branca)] <- 0
m <- summary(glm(antipetista ~ branca, data = b16, family = binomial()))
pbranca <- FormatP(m$coefficients[2, 4])


par(mar = c(2, 2, 0, 0)+0.1, cex = 0.8)
tmp <- crosstab(b16$antipetista.f, b16$cor, inv.y = TRUE,
                xlab = "Cor", ylab = "Antipetista")


##############################################################
####  Figura 6. Proporção de antipetistas segundo o sexo  ####

b16$sexo <- as.numeric(b16$sex == "Female")
b16$sexo <- factor(b16$sexo, levels = c(0, 1), labels = c("Masculino", "Feminino"))
m <- summary(glm(antipetista ~ sexo, data = b16, family = binomial))
psexo <- FormatP(m$coefficients[2, 4])

par(mar = c(2, 2, 0, 0)+0.1, cex = 0.8)
tmp <- crosstab(b16$antipetista.f, b16$sexo, inv.y = TRUE,
                xlab = "Sexo", ylab = "Antipetista")


######################################################################
####  Figura 7. Proporção de antipetistas segundo a faixa etária  ####

b16$fxetaria <- cut(b16$q2, c(min(b16$q2)-1, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, max(b16$q2)))
m <- summary(glm(antipetista ~ q2, data = b16, family = binomial()))
pidade <- FormatP(m$coefficients[2, 4])

par(mar = c(2, 2, 0, 0)+0.1, cex = 0.8)
tmp <- with(b16, crosstab(antipetista.f, fxetaria, inv.y = TRUE,
                          xlab = "Faixa etária", ylab = "Antipetista"))


##################################################################
####  Figura 8. Proporção de antipetistas segundo a religião  ####

# Recodificação da religião
b16$q3c[is.na(b16$q3c)] <- "Other"
b16$q3c <- factor(b16$q3c)
b16$religiao <- 1
b16$religiao[b16$q3c == "Catholic"] <- 2
b16$religiao[b16$q3c == "Evangelical and Protestant"] <- 3
b16$religiao <- factor(b16$religiao,
                       levels = c(1, 2, 3),
                       labels = c("Outra", "Católica", "Evangélica"))
m <- summary(glm(antipetista ~ religiao, data = b16, family = binomial()))
pcatolica <- FormatP(m$coefficients["religiaoCatólica", 4])

par(mar = c(2, 2, 0, 0)+0.1, cex = 0.8)
tmp <- crosstab(b16$antipetista.f, b16$religiao, inv.y = TRUE,
                xlab = "Religião", ylab = "Antipetista")


###############################################
####  Figura 9. Proporção de antipetistas  ####
####   segundo o índice de religiosidade   ####

q5a <- factor(b16$q5a)
q5b <- factor(b16$q5b)
q5a[is.na(q5a)] <- "Never or almost never"
q5b[is.na(q5b)] <- "Not Important at All"
b16$religiosidd <- 9 - (as.numeric(q5a) + as.numeric(q5b))
b16$religioso <- b16$religiosidd > 2
b16$religioso <- factor(b16$religioso, levels = c(FALSE, TRUE), labels = c("Não", "Sim"))
par(mar = c(2, 2, 0, 0)+0.1, cex = 0.8)
tmp <- crosstab(b16$antipetista.f, b16$religiosidd, inv.y = TRUE,
                xlab = "Religiosidade", ylab = "Antipetista")

m <- summary(glm(antipetista ~ religioso,
                 data = b16[b16$religiao == "Católica", ], family = binomial()))
pcatrelig <- FormatP(m$coefficients[2, 4])
m <- summary(glm(antipetista ~ religioso,
                 data = b16[b16$religiao == "Evangélica", ], family = binomial()))
pevarelig <- FormatP(m$coefficients[2, 4])


########################################################################
####  Figura 10. Proporção de antipetistas segundo a religiosidade  ####

par(mfrow = c(1, 2), mar = c(2, 2, 2, 2)+0.1, cex = 0.8)
tmp <- with(b16[b16$religiao == "Católica", ],
            crosstab(antipetista.f, religioso, inv.y = TRUE,
                     ylab = "Religioso", xlab = "Antipetista"))
title(main = "Católicos")
par(mar = c(2, 2, 2, 0)+0.1, cex = 0.8)
tmp <- with(b16[b16$religiao == "Evangélica", ],
            crosstab(antipetista.f, religioso, inv.y = TRUE,
                     ylab = "Religioso", xlab = "Antipetista"))
title(main = "Evangélicos")


########################################################
####  Figura 11. Proporção de antipetistas segundo  ####
####        a percepção de mobilidade social        ####

c1 <- 5 - as.numeric(b16$brasoclass2)
c2 <- 5 - as.numeric(b16$mov1)
c1 <- SubOmissos(c1)
c2 <- SubOmissos(c2)
b16$mobldd <- c2 - c1
m <- summary(glm(antipetista ~ mobldd, data = b16, family = binomial()))
pmobldd <- FormatP(m$coefficients[2, 4])
rm(c1, c2)

par(mfrow = c(1, 1), mar = c(2, 2, 2, 0)+0.1, cex = 0.8)
tmp <- crosstab(b16$antipetista.f, b16$mobldd, inv.y = TRUE,
                ylab = "Antipetista", xlab = "Percepção de mobilidade social")


################################################################
####  Figura 12. Proporção de antipetistas segundo o nível  ####
####          de apoio a políticas igualitárias             ####

b16$igualitarismo <- SubOmissos(b16$ros4)
m <- summary(glm(antipetista ~ igualitarismo, data = b16, family = binomial()))
pigualit <- FormatP(m$coefficients[2, 4])

par(mar = c(2, 2, 2, 0)+0.1, cex = 0.8)
tmp <- with(b16,
            crosstab(antipetista.f, ros4, inv.y = TRUE,
                     ylab = "Antipetista", xlab = "Estado deve reduzir desigualdade"))


#############################################################################
####  Figura 13. Proporção de antipetistas segundo o nível de estatismo  ####

b16$estatismo <- SubOmissos(b16$ros1)
m <- summary(glm(antipetista ~ estatismo, data = b16, family = binomial()))
pesttt <- FormatP(m$coefficients[2, 4])

par(mar = c(2, 2, 2, 0)+0.1, cex = 0.8)
tmp <- with(b16,
            crosstab(antipetista.f, ros1, inv.y = TRUE,
                     ylab = "Antipetista", xlab = "Estatismo"))


########################################################
####  Figura 14. Proporção de antipetistas segundo  ####
####          o nível de oposição ao aborto         ####

# Índice de oposição ao aborto
# W14A. E agora, pensando em outros assuntos. O(a) sr./sra acredita que se
# justifica a interrupção da gravidez, ou seja, um aborto, quando a saúde da
# mãe está em perigo?
# BRAGRUP1. Pessoas que defendem a legalização do aborto
b16$contraaborto <- (b16$w14a == "No, it is not justified") + (b16$bragrup1 == 1)
b16$contraaborto[is.na(b16$contraaborto)] <- 1
m <- summary(glm(antipetista ~ contraaborto, data = b16, family = binomial()))
paborto <- FormatP(m$coefficients[2, 4])

par(mar = c(2, 4, 2, 1)+0.1, cex = 0.8)
tmp <- with(b16, crosstab(antipetista.f, contraaborto, inv.y = TRUE,
                          xlab = "Contrário ao aborto", ylab = "Antipetista"))


##############################################################################
####  Figura 15. Proporção de antipetistas segundo o índice de homofobia  ####

# Índice de homofobia
# D5. homossexuais possam candidatar-se para cargos públicos
# D6. homossexuais tenham direito de se casar
d5 <- SubOmissos(b16$d5)
d6 <- SubOmissos(b16$d6)
b16$homofobia <- 20 - (d5 + d6)
rm(d5, d6)

m <- summary(glm(antipetista ~ homofobia, data = b16, family = binomial()))
phomofobia <- FormatP(m$coefficients[2, 4])

par(mar = c(2, 4, 2, 1)+0.1, cex = 0.8)
tmp <- crosstab(b16$antipetista.f, b16$homofobia, inv.y = TRUE,
                xlab = "Índice de homofobia", ylab = "Antipetista")


######################################################
####  Figura 16. Proporção de antipetistas entre  ####
####   beneficiários e não beneficiários do PBF   ####

# CCT1B. Agora falando especificamente sobre o Programa Bolsa Família, o(a) sr./sra. ou alguém em sua casa é beneficiário deste programa?
b16$recebeBF <- b16$cct1b == "Yes"
b16$recebeBF[is.na(b16$recebeBF)] <- FALSE
b16$recebeBF <- factor(b16$recebeBF, levels = c(FALSE, TRUE), labels = c("Não", "Sim"))
m <- summary(glm(antipetista ~ recebeBF, data = b16, family = binomial()))
prcbBF <- FormatP(m$coefficients[2, 4])

# CCT4BRA. O(A) sr./sra. pensa que o governo deve ampliar a quantidade de
# famílias que recebem o Bolsa Família, manter como está, diminuir, ou acabar
# com o Programa Bolsa Família?
b16$contraBF <- factor(b16$cct4bra)
b16$contraBF[is.na(b16$contraBF)] <- "Keep it as it is"
b16$contraBF <- as.numeric(b16$contraBF) - 1

bf <- factor(b16$cct4bra)
levels(bf) <- sub("Enlarge it", "Ampliar", levels(bf))
levels(bf) <- sub("Keep it as it is", "Manter", levels(bf))
levels(bf) <- sub("Decrease it", "Reduzir", levels(bf))
levels(bf) <- sub("End the program", "Extinguir", levels(bf))

par(mar = c(2, 4, 2, 1)+0.1, cex = 0.8)
tmp <- with(b16, crosstab(antipetista.f, recebeBF, inv.x = TRUE, inv.y = TRUE,
                          ylab = "Antipetista", xlab = "Beneficiário do PBF"))


################################################
####  Figura 17. Proporção de antipetistas  ####
####   segundo a atitude em relação ao PBF  ####

m <- summary(glm(antipetista ~ contraBF, data = b16, family = binomial()))
pcbf <- FormatP(m$coefficients[2, 4])

par(mar = c(2, 4, 2, 1)+0.1, cex = 0.8)
tmp <- with(b16, crosstab(antipetista.f, bf, inv.x = TRUE, inv.y = TRUE,
                          xlab = "Atitude em relação PBF", ylab = "Antipetista"))


######################################################
####  Figura 18. Análise de regressão logística:  ####
####     antipetismo como variável dependente     ####

b <- b16[, c("antipetista", "q2", "escola.n", "renda.fam", "cor", "religiao", "religioso", "igualitarismo", "estatismo", "recebeBF", "contraBF", "contraaborto", "homofobia")]
b$escola.n <- b$escola.n / max(b$escola.n, na.rm = TRUE)
b$renda.fam <- b$renda.fam / max(b$renda.fam, na.rm = TRUE)
b$igualitarismo <- b$igualitarismo / max(b$igualitarismo, na.rm = TRUE)
b$estatismo <- b$estatismo / max(b$estatismo, na.rm = TRUE)
b$contraBF <- b$contraBF / max(b$contraBF, na.rm = TRUE)
b$contraaborto <- b$contraaborto / max(b$contraaborto, na.rm = TRUE)
b$homofobia <- b$homofobia / max(b$homofobia, na.rm = TRUE)
b$Católica <- as.numeric(b$religiao == "Católica")
b$Evangélica <- as.numeric(b$religiao == "Evangélica")
b$religioso <- as.numeric(b$religioso == "Sim")
b$Idade <- b$q2
m <- glm(antipetista ~ Idade + renda.fam + Católica + Evangélica + Católica:religioso + Evangélica:religioso + estatismo + recebeBF + contraBF + contraaborto + homofobia, data = b, family = binomial(link = "logit"))
p <- plot_coefs(m, colors = "black")
p$labels$x <- "Estimativa"
levels(p$data$term) <- SubNomes(levels(p$data$term))
p


#########################################################
####    Figura 19. Análise de regressão logística:   ####
####  ser contrário ao PBF como variável dependente  ####

m <- lm(contraBF ~ sex + branca + renda.fam + escola.n + recebeBF,
        data = b16)
p <- plot_coefs(m, colors = "black")
p$labels$x <- "Estimativa"
levels(p$data$term) <- SubNomes(levels(p$data$term))
p


############################################################
####     Figura 20. Análise de regressão logística:     ####
####  ser contrário ao aborto como variável dependente  ####

m <- lm(contraaborto ~ sex + religiao + religioso + renda.fam + escola.n,
        data = b16)
p <- plot_coefs(m, colors = "black")
p$labels$x <- "Estimativa"
levels(p$data$term) <- SubNomes(levels(p$data$term))
p


######################################################
####  Figura 21. Análise de regressão logística:  ####
####    ser religioso como variável dependente    ####

b16$religioso.n <- as.numeric(b16$religioso == "Sim")
m <- glm(religioso ~ sex + renda.fam + escola.n, data = b16, family=binomial)
p <- plot_coefs(m, colors = "black")
p$labels$x <- "Estimativa"
levels(p$data$term) <- SubNomes(levels(p$data$term))
p

