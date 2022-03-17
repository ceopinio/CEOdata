# Hack to avoid NOTES in R CMD check
# Hadley does not seem to like it: http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
if (base::getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "ANY", "BOP_NUM", "Data", "DIA", "MES", "REO",
    ".",
    #"Data alta CEO", 
    "sid", "id", "position", "created_at", "created_meta",
    "updated_at", "updated_meta", "meta",
    "Metodologia enquesta", "Metode de recollida de dades",
    "Ambit territorial", 
    "Dia inici treball de camp", "Dia final treball de camp",
    "Any d'entrada al REO", "Data d'alta al REO", "Mostra estudis quantitatius",
    "Enllac matriu de dades",
    "Cost",
    "Variable", "Original.Variable",
    "is_haven_labelled"  # from CEOdata
  ))
}
