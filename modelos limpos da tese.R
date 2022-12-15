
#0. Carregar pacotes ==========================================================
library(rio)
library(janitor)   # para tabela de frequencia
library(Hmisc)     # para calcular peso amostral
library(pollster)  # para tabelas com peso
library(arm)       # para funcaoo 'invlogit'
library(readxl)    # para dados es xlsl
library(dplyr)     # para limpeza do banco
library(sjPlot)    # para tabela
library(stargazer) # para tabela
library(lme4)
library(coefplot)  # para plotar regressao
library(ggplot2)
library(cowplot)
library(scales)
library(forcats)
library(tidyr)
library(gridExtra)
library(coefplot)
library(haven)
library(readr)
# 1 abrindo o banco ==========================================================

dados = import('Dados_TESE_limpo.xlsx')


# salvando o banco original ==================================================

q = dados

# nome das variaveis

names(q)         

# tamano do banco de dados

dim(q)            

# transforma tudo em minusculo

tolower(names(q)) 

dados1 = q %>% 
  setNames(tolower(names(q))) %>% 
  dplyr::rename( 
    etnia  = 'p3',
    idade = 'p1',
    escolaridade = 'p5', 
    filiado = 'p10',
    estado = 'p11',
    renda = 'p52',
    partidos  = 'p17',
    temp_fil  = 'p20',
    horas = 'p21',
    inf_part = 'p29',
    inf_pais = 'p30',
    inf_voto = 'p31',
    mot_cand = 'motivo_candidato',
    mot_asso = 'motivo_associacao',
    mot_amiz = 'motivo_amizade',
    mot_carg = 'motivo_cargopart',
    mot_cesp = 'motivo_cespf',
    mot_convic = 'motivo_convicpol',
    mot_conviv = 'motivo_conviv',
    mot_empr = 'motivo_emprego',
    mot_opos = 'motivo_oposicao',
    mot_reli = 'motivo_religiao',
    mot_trad = 'motivo_tradfam',
    Part_tol = 'p47',
    Part_rej = 'rej_pt',
    atvc_reuniao = "atvc_reuniao",
    atvc_comicio = "atvc_comicio",
    atvc_convencao = "atvc_convencao",
    atvc_eldir = "atvc_eldir",
    atvc_panfl = "atvc_panfl",
    atvc_adm = "atvc_adm",
    atvc_recrt = "atvc_recrt",
    atvc_del = "atvc_del",
    atvc_cand = "atvc_cand",
    atvc_internet = "atvc_internet",
    atvc_rua = "atvc_rua",
    pos_ideol = 'p49',
    pos_ideol_part = 'p50') %>% 
  
  mutate (
    etnia = ifelse(etnia == 3,1,0),
    idade = as.numeric(idade),
    idade_recod = 2020 - idade,
    escolaridade = as.numeric(escolaridade),
    escolaridade = ifelse(escolaridade == 8,1,0),
    renda = case_when(renda < 4 ~ 1,
                            renda  == 4 ~ 2,
                            renda > 4 ~ 3),
    estado = as.numeric(estado),
    temp_fil = as.numeric(temp_fil),
    temp_fil = 2020 - temp_fil,
    horas = horas,
    horas = ifelse(horas > 3, 1,0),
    partidos = as.factor(partidos),
    inf_part = ifelse(inf_part > 5, 1,0),
    inf_pais = ifelse(inf_pais > 5, 1,0),
    inf_voto = ifelse(inf_voto > 5, 1,0),
    mot_cand = ifelse(mot_cand == 'Muito importante', 1,0),
    mot_asso = ifelse(mot_asso == 'Muito importante', 1,0),
    mot_amiz = ifelse(mot_amiz == 'Muito importante', 1,0),
    mot_carg = ifelse(mot_carg == 'Muito importante', 1,0),
    mot_cesp = ifelse(mot_cesp == 'Muito importante', 1,0),
    mot_convic = ifelse(mot_convic == 'Muito importante', 1,0),
    mot_conviv = ifelse(mot_conviv == 'Muito importante', 1,0),
    mot_emp = ifelse(mot_empr == 'Muito importante', 1,0),
    mot_opos = ifelse(mot_opos == 'Muito imporante', 1,0),
    mot_reli = ifelse(mot_reli == 'Muito importante', 1,0),
    mot_trad = ifelse(mot_trad == 'Muito importante', 1,0),
    pos_ideol = as.numeric(pos_ideol)) %>%  
  
  dplyr::select( etnia, partidos, idade_recod, 
                 escolaridade, renda, temp_fil, horas,
                 horas, inf_part, inf_pais, inf_voto, 
                 mot_cand, mot_asso, mot_amiz, mot_carg,
                 mot_cesp,mot_convic, mot_conviv, mot_emp,
                 mot_opos, mot_reli, mot_trad, pos_ideol,
                 pos_ideol_part, Part_rej, Part_tol, atvc_reuniao, atvc_comicio,
                 atvc_convencao, atvc_eldir, atvc_panfl, atvc_adm, atvc_recrt,
                 atvc_del, atvc_cand, atvc_internet, atvc_rua,Part_rej, estado)


dados <- df

dados1[is.na(dados1)] <- 0

dados1$Part_rej <- as.factor(dados1$Part_rej)


dados1$Part_rej <- relevel(dados1$Part_rej, ref = "0")


# Limpeza das variáveis de ativismo ==========================================

reuniao = as.numeric(dados1$atvc_reuniao)
comicio = as.numeric(dados1$atvc_comicio)
convencao = as.numeric(dados1$atvc_convencao)
eldir = as.numeric(dados1$atvc_eldir)
panfl = as.numeric(dados1$atvc_panfl)
adm = as.numeric(dados1$atvc_adm)
recrt = as.numeric(dados1$atvc_recrt)
del = as.numeric(dados1$atvc_del)
cand = as.numeric(dados1$atvc_cand)
internet = as.numeric(dados1$atvc_internet)
rua = as.numeric(dados1$atvc_rua)

df = data.frame(convencao, eldir, panfl,adm, recrt,
                del, cand, internet, rua)

cor(df)

# Análise multivariada =========================================================

# civico =======================================================================

CivicModel <- glm(horas ~ inf_voto + mot_trad + mot_amiz + mot_asso + idade_recod +
                    escolaridade + renda + etnia + temp_fil, data = dados1, 
                    family = binomial(link = logit))

summary(modelocivico)


# rational_choice =============================================================

RationalChoice <- glm(horas ~ inf_voto + mot_convic +  mot_emp  + mot_cand +
                        idade_recod  + mot_carg + inf_voto + inf_part +
                      escolaridade + renda + etnia + temp_fil, data = dados1, 
                    family = binomial(link = logit))
summary(RationalChoice)

# GIM =========================================================================

GIM <- glm(horas ~ idade_recod + mot_convic + mot_cand + mot_asso + mot_trad +
                 mot_amiz + mot_conviv + mot_carg + inf_part + 
                 inf_pais + mot_cesp + mot_emp + Part_rej + estado +
                 inf_voto + escolaridade + renda + etnia + temp_fil, data = dados1,
                 family = binomial(link = logit))


stargazer(GIM, title = 'Modelo dos incentivos gerais', align = T, type = 'html', out = 'modelo.html')


tab_model(GIM,RationalChoice,CivicModel, show.ci = F, show.se = T, collapse.se = T, p.style = "stars")


plot(modelo1, which = 5)
summary(stdres(modelo1))
vif(GIM)
cor(modelo1)


library(sjPlot)

multiplot(CivicModel, RationalChoice, GIM, 
          title = 'Gráfico 11. Preditores da participação de alta intensidade', 
          legend.position = 'right',
          intercept = F)+
          theme_bw()

dflist <- list(buildModelCI(CivicModel), buildModelCI(RationalChoice), buildModelCI(GIM))
dflist

modelfunc <- function(model, outerCI=2, innerCI=1, intercept=TRUE, numeric=FALSE, 
                      sort=c("natural", "magnitude", "alphabetical"),
                      decreasing=TRUE, name=NULL, ...)
{
  modelCI <- model
  sort <- match.arg(sort)    
  ordering <- switch(sort,
                     natural=order(1:nrow(modelCI), decreasing=decreasing),   
                     magnitude=order(modelCI$Value, decreasing=decreasing),    
                     alphabetical=order(modelCI$Coefficient, decreasing=decreasing),    
                     order(1:nrow(modelCI))     
  ) 
  modelCI <- modelCI[ordering, ]
  modelCI$Coefficient <- factor(modelCI$Coefficient, levels=modelCI$Coefficient)
  
  return(modelCI)
}

mymultiplot <- function (..., title = "Coefficient Plot", xlab = "Value", ylab = "Coefficient", 
                         innerCI = 1, outerCI = 0, lwdInner = 1, lwdOuter = 0, pointSize = 3, 
                         dodgeHeight = 1, color = "blue", shape = 16, linetype = 1, 
                         cex = 0.8, textAngle = 0, numberAngle = 90, zeroColor = "grey", 
                         zeroLWD = 1, zeroType = 2, single = FALSE, scales = "fixed", 
                         ncol = length(unique(modelCI$Model)), sort = c("natural", 
                                                                        "normal", "magnitude", "size", "alphabetical"), decreasing = TRUE, 
                         names = NULL, numeric = FALSE, fillColor = "grey", alpha = 1/2, 
                         horizontal = FALSE, factors = NULL, only = NULL, shorten = TRUE, 
                         intercept = TRUE, interceptName = "(Intercept)", coefficients = NULL, 
                         predictors = NULL, strict = FALSE, newNames = NULL, plot = TRUE, 
                         drop = FALSE, by = c("Coefficient", "Model"), plot.shapes = FALSE, 
                         plot.linetypes = FALSE, legend.position = "right", secret.weapon = FALSE) 
{
  if (tryCatch(is.list(...), error = function(e) FALSE)) {
    theDots <- list(...)[[1]]
    if (is.null(names(theDots))) {
      names(theDots) <- sprintf("Model%s", 1:length(theDots))
    }
  }
  else {
    theDots <- list(...)
  }
  theArgs <- unlist(structure(as.list(match.call()[-1]), class = "uneval"))
  if (is.null(names(theArgs))) {
    theNames <- theArgs
  }
  else {
    theNames <- theArgs[names(theArgs) == ""]
  }
  if (is.null(names(theDots))) {
    names(theDots) <- theNames
  }
  sort <- match.arg(sort)
  by <- match.arg(by)
  legend.position <- match.arg(legend.position)
  if (secret.weapon) {
    by <- "Model"
    horizontal <- TRUE
  }
  if (by == "Model" & length(coefficients) != 1) {
    stop("If plotting the model along the axis then exactly one coefficient must be specified for plotting")
  }
  
  modelCI <- plyr:::ldply(theDots, modelfunc, outerCI = outerCI, 
                          innerCI = innerCI, intercept = intercept, numeric = numeric, 
                          sort = sort, decreasing = decreasing, factors = factors, 
                          shorten = shorten, coefficients = coefficients, predictors = predictors, 
                          strict = strict, newNames = newNames)
  modelCI$Model <- modelCI$.id
  modelCI$.id <- NULL
  if (!is.null(names)) {
    names(names) <- theNames
    modelCI$Model <- names[modelCI$Model]
  }
  if (drop) {
    notNA <- daply(modelCI, .variables = "Model", function(x) {
      !all(is.na(x$Coef))
    })
    modelCI <- modelCI[modelCI$Model %in% names(which(notNA == 
                                                        TRUE)), ]
  }
  if (!plot) {
    return(modelCI)
  }
  p <- coefplot:::buildPlotting.default(modelCI = modelCI, title = title, 
                                        xlab = xlab, ylab = ylab, lwdInner = lwdInner, lwdOuter = lwdOuter, 
                                        pointSize = pointSize, dodgeHeight = dodgeHeight, color = color, 
                                        shape = shape, linetype = linetype, cex = cex, textAngle = textAngle, 
                                        numberAngle = numberAngle, zeroColor = zeroColor, zeroLWD = zeroLWD, 
                                        outerCI = outerCI, innerCI = innerCI, zeroType = zeroType, 
                                        numeric = numeric, fillColor = fillColor, alpha = alpha, 
                                        multi = TRUE, value = "Value", coefficient = by, horizontal = horizontal, 
                                        facet = FALSE, scales = "fixed")
  theColorScale <- list(Coefficient = scale_colour_discrete("Model"), 
                        Model = scale_color_manual(values = rep(color, length(unique(modelCI$Model))), 
                                                   guide = FALSE))
  theShapeScale <- list(NoShapes = scale_shape_manual(values = rep(shape, 
                                                                   length(unique(modelCI$Model))), guide = FALSE), Shapes = scale_shape_manual(values = 1:length(unique(modelCI$Model))))
  theLinetypeScale <- list(NoShapes = scale_linetype_manual(values = rep(linetype, 
                                                                         length(unique(modelCI$Model))), guide = FALSE), Shapes = scale_linetype_manual(values = 1:length(unique(modelCI$Model))))
  p + theColorScale[[by]] + theShapeScale[[plot.shapes + 1]] + 
    theLinetypeScale[[plot.linetypes + 1]] + theme(legend.position = legend.position) + 
    if (!single) 
      facet_wrap(~Model, scales = scales, ncol = ncol)
}  

mymultiplot(dflist) + theme_bw()
