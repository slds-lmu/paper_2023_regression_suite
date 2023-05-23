* Die Skripte sauber schreiben
* Das Skript zur Abschätzung der Laufzeit muss angepasst werden (weil sich die job ids geändert haben muss 
  mindestens das endsWith zu startsWith geändert werden)

* Resubmit the gam jobs (mlr3misc etc. was missing from packages and therefore %nin% was not found) need to update packages before (already changed it in experiments.R but need to manually change the registry and then save the registry which does not work while jobs are running)
* 