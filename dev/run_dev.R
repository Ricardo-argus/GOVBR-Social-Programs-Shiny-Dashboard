# Set options here
options(golem.app.prod = FALSE) # Falso para desenvolvimento, TRUE para produção

# Desanexa e recarrega o pacote
golem::detach_all_attached()
golem::document_and_reload()

