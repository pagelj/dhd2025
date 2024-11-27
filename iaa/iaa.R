library(tidyverse)
library(irr)
library(knitr)
library(kableExtra)

handmade = c("Georg Bendemann ist ein junger Kaufmann.",
            "Georg Bendemann sitzt in seinem Privatzimmer.",
            "Georg Bendemann sitzt in einem der niedrigen, leichtgebauten Häuser.",
            "Georg Bendemann sah aus dem Fenster.",
            "Georg Bendemann hatte gerade einen Brief an einen sich im Ausland befindlichen Jugendfreund beendet.",
            "Ein junger Kaufmann sitzt in seinem Privatzimmer.",
            "Ein junger Kaufmann sitzt in einem der niedrigen, leichtgebauten Häuser.",
            "Ein junger Kaufmann sah aus dem Fenster.",
            "Ein junger Kaufmann hatte gerade einen Brief an einen sich im Ausland befindlichen Jugendfreund beendet.",
            "Der Jugendfreund arbeitete sich in der Fremde ab.",
            "Georg Bendemann ist ein junger Kaufmann und arbeitet sich nicht in der Fremde nutzlos ab.",
            "Georg Bendemann ist ein junger Kaufmann und und betreibt kein Geschäft in Petersburg.",
            "Georg Bendemann ist ein junger Kaufmann und hat keine rechte Verbindung mit der dortigen Kolonie der Landsleute.",
            "Georg Bendemann ist ein junger Kaufmann und hatte einen Brief verschlossen.",
            "Georg Bendemann ist ein junger Kaufmann und es ist Sonntagvormittag.",
            "Wenn Georg Bendemann im ersten Stock sitzt, dann sitzt ein junger Kaufmann im ersten Stock.",
            "Wenn Georg Bendemann im ersten Stock sitzt, dann hat ein junger Kaufmann keine rechte Verbindung mit der dortigen Kolonie.", 
            "Wenn Georg Bendemann im ersten Stock sitzt, dann hat ein junger Kaufmann fast keinen gesellschaftlichen Verkehr .", 
            "Wenn Georg Bendemann im ersten Stock sitzt, dann hat ein junger Kaufmann eine rechte Verbindung mit der dortigen Kolonie.", 
            "Wenn Georg Bendemann im ersten Stock sitzt, dann hatte ein junger Kaufmann gerade einen Brief an einen sich im Ausland befindlichen Jugendfreund beendet.", 
            "Wenn der Freund in Petersburg ist, dann ist der Freund in Rußland.",
            "Wenn der Freund in Petersburg ist, dann ist ein junger Kaufmann in Rußland.",
            "Wenn der Freund in Petersburg ist, dann richtete sich ein junger Kaufmann für ein endgültiges Junggesellentum ein.",
            "Wenn der Freund in Petersburg ist, dann arbeitet ein junger Kaufmann sich in der Fremde nutzlos ab.",
            "Wenn der Freund in Petersburg ist, dann sitzt Georg Bendemann in seinem Privatzimmer.")

junktor <- read.csv("junktor.csv", sep = ",")

anno1 <- read.csv("kafka_saetze_annotation1.tsv", sep = "\t")
anno1 <- within(anno1, rm("X.dropdown"))
anno1$id <- 1:nrow(anno1)
anno1$Wahrheitswert <- ifelse("FALSCH" == anno1$Wahrheitswert, "FALSE", "TRUE")
anno1$intrinsisch.extrinsisch <- ifelse("INTR." == anno1$intrinsisch.extrinsisch, "intrinsisch", "extrinsisch")
anno1$Ich.weiss.es.nicht <- ifelse("x" == anno1$Ich.weiss.es.nicht, "X", "")
anno1$manuell <- ifelse(anno1$Sätze %in% handmade, "manual", "automatic")
anno1$Junktor <- junktor$Junktor

anno2 <- read.csv("kafka_saetze_annotation2.tsv", sep = "\t")
anno2$id <- 1:nrow(anno2)
anno2$Ich.weiss.es.nicht[is.na(anno2$Ich.weiss.es.nicht)] <- ""
anno2$manuell <- ifelse(anno2$Sätze %in% handmade, "manual", "automatic")
anno2$Junktor <- junktor$Junktor

anno3 <- read.csv("kafka_saetze_annotation3.tsv", sep = "\t")
anno3 <- within(anno3, rm("X.dropdown"))
anno3$id <- 1:nrow(anno3)
anno3$Wahrheitswert[anno3$Wahrheitswert == ""] <- "FALSE"
anno3$intrinsisch.extrinsisch[anno3$intrinsisch.extrinsisch == ""] <- "intrinsisch"
anno3$manuell <- ifelse(anno3$Sätze %in% handmade, "manual", "automatic")
anno3$Junktor <- junktor$Junktor

create_iaa_table <- function(iaa_object, subsection) {
  data.frame(subsection = subsection, fleiss.kappa = iaa_object$value, p.value = iaa_object$p.value, raters = iaa_object$raters, subjects = iaa_object$subjects)
}

iaa_processing <- function(df) {
  iaa.alle <- create_iaa_table(kappam.fleiss(df[,c("anno1", "anno2", "anno3")], detail = TRUE), "Alle")
  iaa.automatic <- create_iaa_table(kappam.fleiss(df[df$manuell=="automatic",c("anno1", "anno2", "anno3")], detail = TRUE), "nur automatisch")
  iaa.manual <- create_iaa_table(kappam.fleiss(df[df$manuell=="manual",c("anno1", "anno2", "anno3")], detail = TRUE), "nur manuell")
  iaa.proposition <- create_iaa_table(kappam.fleiss(df[df$Junktor=="Proposition",c("anno1", "anno2", "anno3")], detail = TRUE), "nur ohne Junktoren")
  iaa.allejunktoren <- create_iaa_table(kappam.fleiss(df[df$Junktor!="Proposition",c("anno1", "anno2", "anno3")], detail = TRUE), "nur mit Junktoren")
  iaa.negation <- create_iaa_table(kappam.fleiss(df[df$Junktor=="Negation",c("anno1", "anno2", "anno3")], detail = TRUE), "nur Negationen")
  iaa.implikation <- create_iaa_table(kappam.fleiss(df[df$Junktor=="Implikation",c("anno1", "anno2", "anno3")], detail = TRUE), "nur Implikationen")
  iaa.konjunktion <- create_iaa_table(kappam.fleiss(df[df$Junktor=="Konjunktion",c("anno1", "anno2", "anno3")], detail = TRUE), "nur Konjunktionen")
  iaa.disjunktion <- create_iaa_table(kappam.fleiss(df[df$Junktor=="Disjunktion",c("anno1", "anno2", "anno3")], detail = TRUE), "nur Disjunktionen")
  iaa <- rbind(iaa.alle, iaa.automatic, iaa.manual, iaa.proposition, iaa.allejunktoren, iaa.negation, iaa.implikation, iaa.konjunktion, iaa.disjunktion)
  iaa
}

df_wahrheitswert <- as.data.frame(cbind(anno1$Wahrheitswert, anno2$Wahrheitswert, anno3$Wahrheitswert, anno1$manuell, junktor$Junktor))
colnames(df_wahrheitswert) <- c("anno1", "anno2", "anno3", "manuell", "Junktor")
df_wahrheitswert$id <- 1:nrow(df_wahrheitswert)
iaa_wahrheitswerte <- iaa_processing(df_wahrheitswert)

df_exintr <- as.data.frame(cbind(anno1$intrinsisch.extrinsisch, anno2$intrinsisch.extrinsisch, anno3$intrinsisch.extrinsisch, 
                                 anno1$manuell, junktor$Junktor))
colnames(df_exintr) <- c("anno1", "anno2", "anno3", "manuell", "Junktor")
iaa_exintr <- iaa_processing(df_exintr)

save_kable(x = kable(x = iaa_wahrheitswerte, format = "html"), file = "iaa_results_wahrheitswerte.html")
save_kable(x = kable(x = iaa_exintr, format = "html"), file = "iaa_results_exintr.html")

write.csv(x = df_wahrheitswert, file = "same_annotations.csv", row.names = FALSE)
