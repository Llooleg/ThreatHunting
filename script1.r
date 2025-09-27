file.remove(list.files(pattern = ".swirl", recursive = TRUE, all.files = TRUE))
swirl::reset()
swirl::swirl()