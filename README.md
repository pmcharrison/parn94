# HarmonyParncutt

The analysis currently uses a 1/n roll-off in the dB domain, which seems unrealistic. We should change this to a 1/n roll-off in the amplitude domain. In this context, Parncutt's 1/n roll-off for the audibility template seems wrong,because amplitude and audibility have a non-linear relationship. I think it would be best to empirically estimate a new template by repeatedly sampling from the model at different pitches.
