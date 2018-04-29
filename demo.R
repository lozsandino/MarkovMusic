library(MarkovMusic)

# Lectura de los datos de entrenamiento.
Cm <- readMusicXML(paste0(system.file(package = "MarkovMusic"), 
                          "/TrainingBlues.xml"))
Cm <- Cm$song[, "note"] # Se preserva solo la información del tono.
Cm <- c(Cm, rev(Cm)) # Melodía en escala menor de blues en C.
Fm <- Cm + 5 # Melodía en escala menor de blues en F.
Gm <- Fm + 2

# Conversión de la melodía en notación MIDI a notación anglosajona.
Cm <- fromMidiToNote(Cm)
Fm <- fromMidiToNote(Fm)
Gm <- fromMidiToNote(Gm)

# Espacio estado común para las tres secuencias.
state.space <- unique(unlist(c(Cm, Fm, Gm)))

# Matrices de transición de los procesos.
M0Cm <- priorVec(Cm, state.space = state.space) # Prob. inicial.
M1Cm <- transMat(Cm, state.space = state.space) # Orden 1.
M2Cm <- transMat(Cm, order = 2, state.space = state.space) # Orden 2.
M2Fm <- transMat(Fm, order = 2, state.space = state.space)
M2Gm <- transMat(Gm, order = 2, state.space = state.space)

# Cadenas de Markov no restringidas.
M <- c(list(M0Cm), list(M1Cm), rep(list(M2Cm), 46), # compases 1-4.
       rep(list(M2Fm), 24), # 24 notas, compases 5 y 6.
       rep(list(M2Cm), 24), # 24 notas, compases 7 y 8.
       rep(list(M2Gm), 12), # 12 notas, compás 9.
       rep(list(M2Fm), 12),
       rep(list(M2Cm), 24))

# Definición de las restricciones.
constraints <- list(list(1, "C"),   # La primera nota debe ser C.
                    list(49, "F"),  # La nota 49 debe ser F.
                    list(73, "C"),
                    list(97, "G"),
                    list(109, "F"),
                    list(121, "C"),
                    list(144, "C"))

# Acro-consistencia.
Z <- arcConsistency(M, constraints)

# Generación de la melodía.
set.seed(69)
melody <- genMelody(Z, state.space)
writeMusicXML(melody, file = "MarkovMelody2_69.xml", 
              beat = 12, beat.type = 8)
