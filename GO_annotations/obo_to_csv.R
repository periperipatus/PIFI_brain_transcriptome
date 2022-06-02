library(ontologyIndex)
file<- "go.obo"
ontology<- get_ontology(file)

ontology <- get_ontology(file, extract_tags="everything")

ontology_out<- as.data.frame(ontology$id)
colnames(ontology_out)<- "id"
ontology_out$name <- ontology$name
namespace<- ontology$namespace
namespace<- do.call("rbind", namespace)
ontology_out$namespace<- namespace[,1]

write.csv(ontology_out, file="ontology_obo_out.csv", row.names=FALSE)