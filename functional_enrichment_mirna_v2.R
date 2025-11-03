# Principal author: Borja Gómez Cabañes

# Updated by Carla Perpiñá Clérigues

### En este script tenemos como objetivo reducir el número de miRNA significativos.
### Para ello vamos a limitar el número de targets que tiene cada miRNA aumentando
### su nivel de confianza. De esta forma, seleccionaremos como targets válidos
### solo aquellos que aparezcan en más de una base de datos con la relación
### miRNA - gen. A partir del resultado de esta combinación vamos a intentar 
### tanto un ORA como un GSEA.

#### PREPARACION DE LOS DATOS ####
pacman::p_load(multiMiR, mdgsa, readxl, org.Hs.eg.db, GO.db, reactome.db, KEGGREST, KEGG.db, dplyr)
setwd("/home/cperpina/RNA/RAMIREZLAU_03/mature_version/")


# Conectamos con la base de datos de GO.db
con <- GO.db::GO_dbconn()
go_term <- tbl(con, "go_term")
go_db = as.data.frame(go_term)

# Cargamos ambas bases de datos. 
# El fichero Homo_sapiens_TarBase-v9.tsv me lo descargo antes de https://dianalab.e-ce.uth.gr/tarbasev9/downloads
tarbase_database = read.table(file="provetes/target/Homo_sapiens_TarBase-v9.tsv", header = TRUE, sep = "\t") # 4724537

# Quitamos los symbol y ID vacios
tarbase_database <- tarbase_database[!is.na(tarbase_database$gene_name),] # 4674235
# tarbase_database = tarbase_database[!tarbase_database$gene_name == "",] #no hay cadenas vacías

tarbase_database <- tarbase_database[!is.na(tarbase_database$mirna_name),] # 4674235
# tarbase_database = tarbase_database[!tarbase_database$mirna_name == "",]

tarbase_database <- tarbase_database[!is.na(tarbase_database$species),] # 4674235
# tarbase_database = tarbase_database[!tarbase_database$species == "",]

# creamos una columna uniendo micro-gen
tarbase_database$combi = paste0(tarbase_database$mirna_name,"_",tarbase_database$gene_name)

# Descargar hsa_MTI.xlsx antes de https://mirtarbase.cuhk.edu.cn/~miRTarBase/miRTarBase_2025/php/index.php nota: si da error escribirles, te envían ellos el fichero
mirtarbase_database = read_excel("provetes/target/hsa_MTI.xlsx") # 502652

# creamos una columna uniendo micro-gen
mirtarbase_database$combi = paste0(mirtarbase_database$miRNA,"_",mirtarbase_database$`Target Gene`)

# unicas combinaciones en ambos ficheros
tarbase_micro = unique(tarbase_database$mirna_name)
mirtarbase_micro = unique(mirtarbase_database$miRNA)

# Variables maestras
combinar = FALSE # TRUE solo me quedaria con lo único entre las dos databases
analisis = "GSEA" #"GSEA" #"ORA" 
universo = TRUE # si utilizo ORA debería ser TRUE pero en GSEA me da igual
estudio = "sex" #"PM_MM" "Ulcerated" "PM "sex" "female" "male"
ontologia = "BP"
corte_logfc = 0 # Solo para ORA. En GSEA deberiamos cortar el LOR
database = "hs" #biomart #hs 



# Cargamos los miRNA analizados ###
# añades tus rutas a los resultados que queremos analizar
# load("~cperpina/RNA/RAMIREZLAU_03/mature_version/results/df_resFemale.RData")
# load("~cperpina/RNA/RAMIREZLAU_03/mature_version/results/df_resMale.RData")
load("~cperpina/RNA/RAMIREZLAU_03/mature_version/results/df_resSex.RData")
# load("~cperpina/RNA/RAMIREZLAU_03/mature_version/results/df_resCase.RData")
# load("/clinicfs/userhomes/cperpina/mixOmics/AUD/mature/graficos.RData")



# Extraemos los miRNA a analizar  ###
# mirna <- df_resFemale$mirna
# mirna <- df_resMale$mirna
mirna <- df_resSex$mirna
# mirna <- df_resCase$mirna


# Especificamso el df results a analizar (usamos el df_res para trabajar)

# df_res <- df_resFemale
# df_res <- df_resMale
df_res <- df_resSex
# df_res <- df_resCase

# df_res <- df_resComp3

# Decidimos como queremos procesar los datos, podemos quedarnos con los miRNA
# donde el match miRNA - target coincida en ambas bases de datos o podemos
# quedarnos con los match combinados de ambas bases de datos. En la teoria
# lo lógico sería la segunda opción, pues en todos los casos estamos cogiendo
# los match validados experimentalmente, sin embargo, dado que un miRNA puede
# regular muchisimos genes, esto puede hacer que parte de nuestros resultados
# no se de por relevancia biológica, sino por puro azar.

# aquí empezamos a utilizar las variables maestras
if (combinar == TRUE){
  # # SI QUEREMOS COMBINAR SOLO LO COMUN
  union = merge(tarbase_database, mirtarbase_database, by = "combi") # COMBINAMOS SOLO LO COMUN
  union = union[!union$target_symbol == "",] # Quitamos los pocos ensembl vacios
  union = union[union$mature_mirna_id %in% mirna,] # Nos quedamos solo con la informacion de los miRNA incluidos en el metaanálisis
  mirna.union = unique(union$combi)
} else {
  # SI QUEREMOS COMBINAR TODO
  a = tarbase_database[,c(2,4,23)]
  colnames(a) = c("mature_mirna_id", "target_symbol", "combi")
  b = mirtarbase_database[,c(2,4,10)]
  colnames(b) = c("mature_mirna_id", "target_symbol", "combi")
  union = rbind(a,b) # COMBINAMOS TODO
  union = union[!union$target_symbol == "",] # Quitamos los pocos ensembl vacios
  union = union[union$mature_mirna_id %in% mirna,] # Nos quedamos solo con la informacion de los miRNA incluidos en el metaanálisis
  mirna.union = unique(union$combi)
}

# x = unique(union$target_symbol)
# patron <- "\\b\\w*P(?=\\d)"
# y = x[grep(patron, x, perl=TRUE, value=FALSE)]

# Podemos ver el número de relaciones miRNA - gene que tenemos en nuestros datos
length(unique(mirna.union))
unique.mirna.union <- unique(union)


if (analisis == "ORA"){
  
  ### Una vez tenemos los datos donde relacionamos todos nuestros miRNA con aquellos
  ### target que aparezcan en ambas bases de datos pasamos a la preparación de la
  ### estructura para realizar el mdgsea
  
  ### ORA ###
  
  # Universo de genes = Targets de todos los miRNA del MA.
  universe = unique(union$target_symbol)          
  
  # Seleccionamos todos los genes NO problematicos que puede haber incluidos la funcion 'get_multimir'
  universe = universe[grep("^[a-zA-Z0-9.-]+$",universe)]
  
  # Escogemos los miRNA que consideremos UP o DOWN
  select.mirna = mat %>% filter(logFC > corte_logfc, p.adjust.fdr < 0.05) %>% pull(ID)
  
  # Seleccionamos los genes sobre los que actuan dichos miRNA
  select.genes = unique(union[union$mature_mirna_id %in% select.mirna, "target_symbol"])
  
  
  # Extraemos para nuestro universo de genes los términos GO a los que pertenece cada gen
  go_genes <- AnnotationDbi::select(org.Hs.eg.db, keys=universe,
                                    columns=c("SYMBOL", "GOALL"), 
                                    keytype="SYMBOL")
  
  # Combinamos estos datos con la base de datos de GO cargada al inicio
  go_term = merge(go_db, go_genes, by.x = "go_id", by.y = "GOALL")
  go_term_ont = go_term[go_term$ontology == ontologia,]
  
  # Formateamos los datos para poder hacer el enriquecimiento funcional
  gentogo = split(go_term_ont$go_id, go_term_ont$SYMBOL)
  gotogen = split( go_term_ont$SYMBOL, go_term_ont$go_id)
  
  if (universo == TRUE){
    x = clusterProfiler::enricher(
      gene = select.genes,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universe,
      minGSSize = 15,
      maxGSSize = 500,
      qvalueCutoff = 0.2,
      TERM2GENE = go_term_ont[,c("go_id", "SYMBOL")],
      TERM2NAME = go_term_ont[,c("go_id", "definition")])
    
    up_bp = x@result
    
  }else{
    ego <- enrichGO(gene = select.genes,
                    keyType = "SYMBOL",
                    OrgDb = org.Hs.eg.db,
                    ont = "BP",
                    minGSSize = 15,
                    maxGSSize = 500,
                    pAdjustMethod = "BH",
                    pvalueCutoff  = 0.05,
                    qvalueCutoff  = 0.05,
                    readable      = FALSE)
    up_bp = ego@result
  }
  
}else{
  
  ### GSEA ###
  
  # Extraemos los estadísticos utilizados por mdgsa para el ranking
  pvalue <- as.numeric(df_res$padj)
  names(pvalue) = df_res$mirna 
  statistic <- as.numeric(df_res$log2FoldChange)
  names(statistic) = df_res$mirna
  
  # generating a new index using pvalue and statistic from miRNA differential expression
  rindex0 <- rindexT <- rindex <- list ()
  rindex0[[estudio]] <- pval2index(pval = pvalue, sign = statistic) # Creamos un ranking para los miRNA
  rindex0[[2]] <- NULL
  rindexT[[2]] <- NULL
  
  # Generamos una funcion que extraiga para cada miRNA sus genes correspondientes
  tomate = function(mirna){
    a = unique(subset(union, union$mature_mirna_id == mirna, select = target_symbol, drop = TRUE))
    return(a)
  }
  
  # Generamos una lista donde para cada miRNA tenemos sus genes asociados
  tomatito <- lapply(names(rindex0[[estudio]]), tomate)
  names(tomatito) = names(rindex0[[estudio]])
  
  # Pasamos al punto clave del análisis, pasar el ranking de miRNA a ranking de genes
  rindexT[[estudio]] <- transferIndex(index = rindex0[[estudio]], # Ranking de los miRNA
                                      targets = tomatito, # Asociación entre miRNA y targets
                                      method = "average") # Decidir si usamos sum (mas terminos) o average (menos terminos)
  
  # Normalizamos la distribución de los datos
  rindex[[estudio]] <- indexTransform(index = rindexT[[estudio]], 
                                      method = "normalize")
  
  
  # Conservamos los genes ranked 
  genes = unlist(names(rindex[[estudio]]))
  
  # Seleccionamos todos los genes NO problematicos que puede haber incluidos la funcion 'get_multimir'
  error.genes = rindex[[estudio]][grep("^[a-zA-Z0-9.-]+$",genes)] 
  
  # Detectamos los SÍ problemáticos
  diferencia = setdiff(genes, names(error.genes))
  
  # Los eliminamos de la lista
  for (i in diferencia){
    rindex[[estudio]] = rindex[[estudio]][-(which(names(rindex[[estudio]]) == i))]
  }
  
  
  #### ENRIQUECIMIENTO FUNCIONAL ####
  
  ## CARGA DE BASES DE DATOS ##
  
  ## OPCION A: BIOMART ##
  
  if (database == "biomart"){
    library(biomaRt)
    # Pasamos a cargar BIOMART
    mart <- useMart(biomart = "ensembl", dataset = "hsapiens_gene_ensembl") # Conectamos con biomart
    
    go_terms <- getBM(attributes = c("ensembl_gene_id","hgnc_symbol","go_id", "name_1006", "namespace_1003"),
                      filter = 'hgnc_symbol',
                      values =  names(rindex[[estudio]]), 
                      mart = mart)
    
    go_terms <- go_terms[!(go_terms$go_id == ""),  ]
    
    # Hay algunos terminos que fallan, vamos a cogerlos
    malitos <- go_terms[which(go_terms$namespace_1003 == ""), 3]
    
    library(dplyr)
    con <- GO.db::GO_dbconn()
    go_term <- tbl(con, "go_term")
    buenitos = as.data.frame(go_term)
    
    arregladitos = buenitos[buenitos$go_id %in% malitos,c(2,3,4)]
    arregladitos$ontology = gsub("MF", "molecular_function", arregladitos$ontology)
    arregladitos$ontology = gsub("BP", "biological_process", arregladitos$ontology)
    arregladitos$ontology = gsub("CC", "cellular_component", arregladitos$ontology)
    colnames(arregladitos) = c("go_id", "name_1006", "namespace_1003")
    
    pruebita = merge(go_terms,arregladitos, by = "go_id")
    pruebita = pruebita[,-c(4,5)]
    pruebita = pruebita[,c(2,3,1,4,5)]
    colnames(pruebita) = colnames(go_terms)
    
    go_terms <- go_terms[-which(go_terms$namespace_1003 == ""), ]
    
    go_terms = rbind(go_terms, pruebita)
    
    # HEMOS TERMINADO DE COGERLOS #
    
    # Seleccionamos los tipos de términos GO
    go_bp = go_terms[go_terms$namespace_1003 == "biological_process",]
    go_bp = go_bp[,c(1:4)]
    gentogo = split(go_bp$go_id, go_bp$hgnc_symbol)
    gotogen = split( go_bp$hgnc_symbol, go_bp$go_id)
    go_cc = go_terms[go_terms$namespace_1003 == "cellular_component",]
    go_cc = go_cc[,c(1:4)]
    gentoset_cc = split(go_cc$go_id, go_cc$hgnc_symbol)
    settogen_cc = split( go_cc$hgnc_symbol, go_cc$go_id)
    go_mf = go_terms[go_terms$namespace_1003 == "molecular_function",]
    go_mf = go_mf[,c(1:4)]
    gentoset_mf = split(go_mf$go_id, go_mf$hgnc_symbol)
    settogen_mf = split( go_mf$hgnc_symbol, go_mf$go_id)
  } else { 
    ## OPCION B: org.Hs.eg.db ##
    
    # ################################ GO  #########################################
    # Extraemos para nuestro universo de genes los términos GO a los que pertenece cada gen
    go_genes <- AnnotationDbi::select(org.Hs.eg.db, keys=names(rindex[[estudio]]),
                                      columns=c("SYMBOL", "GO"), 
                                      keytype="SYMBOL")
    
    # Combinamos estos datos con la base de datos de GO cargada al inicio
    go_term = merge(go_db, go_genes, by.x = "go_id", by.y = "GO")
    go_term_ont = go_term[go_term$ontology == ontologia,]
    
    # Formateamos los datos para poder hacer el enriquecimiento funcional
    gentogo = split(go_term_ont$go_id, go_term_ont$SYMBOL)
    gotogen = split( go_term_ont$SYMBOL, go_term_ont$go_id)
    
    # Seleccionamos los tipos de términos GO
    go_bp = go_term[go_term$ontology == "BP",]
    go_bp = go_bp[,c(1,3,4,6)]
    gentogo = split(go_bp$go_id, go_bp$SYMBOL)
    gotogen = split( go_bp$SYMBOL, go_bp$go_id)
    go_cc = go_term[go_term$ontology == "CC",]
    go_cc = go_cc[,c(1,3,4,6)]
    gentoset_cc = split(go_cc$go_id, go_cc$SYMBOL)
    settogen_cc = split( go_cc$SYMBOL, go_cc$go_id)
    go_mf = go_term[go_term$ontology == "MF",]
    go_mf = go_mf[,c(1,3,4,6)]
    gentoset_mf = split(go_mf$go_id, go_mf$SYMBOL)
    settogen_mf = split( go_mf$SYMBOL, go_mf$go_id)
  }
  
  
  
  # ############################## REACTOME ########################################
  
  genes_entrez <- na.omit(AnnotationDbi::select(org.Hs.eg.db, keys=genes,
                                                columns=c("SYMBOL", "ENTREZID"), 
                                                keytype="SYMBOL"))
  
  reactome_db <- AnnotationDbi::select(reactome.db, keys=genes_entrez$ENTREZID,
                                       columns=c("ENTREZID", "PATHID"),
                                       keytype="ENTREZID")
  
  reactome_db = na.omit(merge(reactome_db, genes_entrez, by.x = "ENTREZID", by.y = "ENTREZID"))
  genetoreactome = split(reactome_db$PATHID, reactome_db$SYMBOL)
  reactometogen = split(reactome_db$SYMBOL, reactome_db$PATHID)
  
  
  # ################################ KEGG ##########################################
  pathways.list <- keggList("pathway", "hsa")
  kegg_db = data.frame(codigo = names(pathways.list),
                       summary = pathways.list)
  
  kegg_genes <- AnnotationDbi::select(org.Hs.eg.db, keys=genes,
                                      columns=c("SYMBOL", "PATH"),
                                      keytype="SYMBOL")
  kegg_genes = na.omit(kegg_genes)
  kegg_genes$PATH = paste0("hsa",kegg_genes$PATH)
  
  kegg = merge(kegg_db, kegg_genes, by.x = "codigo", by.y = "PATH")
  
  gentokegg = split(kegg$codigo, kegg$SYMBOL)
  keggtogen = split(kegg$SYMBOL, kegg$codigo)
  
  rm(kegg_genes)
  rm(kegg_db)
  
  
  lista_universo = list(gotogen, settogen_cc, settogen_mf, reactometogen, keggtogen)
  list_of_datasets = list()
  for (i in 1:length(lista_universo)){
    annot <- annotFilter(lista_universo[[i]], rindex[[estudio]])
    res.uv <- uvGsa(rindex[[estudio]], annot, p.adjust.method = "fdr")
    if (i==1 | i==2 | i==3){
      res.uv$description = getGOnames(res.uv, verbose = TRUE)
    }
    list_of_datasets[[i]] = res.uv[order(res.uv$padj),]
    resultados = rownames(uvSignif(list_of_datasets[[i]]))
    # if (length(resultados) > 0){
    #   dir.create(paste0("/plots","_",lista_universo[[1]]), showWarnings=F)
    #   ruta_4 = paste0(getwd(), "/plots")
    #   for (i in 1:length(resultados)){
    #     plotEnrichment(annot[[resultados[i]]],
    #                    rindex[[contraste]]) + labs(title=resultados[i])
    #     ggsave(paste0(ruta_4,"/", resultados[i],"_rank.png"), width=15, height=10, bg = "white")
    #   }
    # }
  }
  
  # Obtenemos los nombres de los términos GO
  list_of_datasets[[1]]$description = getGOnames(rownames(list_of_datasets[[1]]))
  names(list_of_datasets) = c("BP","CC","MF","REACTOME", "KEGG")
  
  gsea_bp = list_of_datasets[[1]]
  gsea_cc= list_of_datasets[[2]]
  gsea_mf = list_of_datasets[[3]]
  gsea_reactome = list_of_datasets[[4]]
  gsea_kegg = list_of_datasets[[5]]
  
}

# gsea_org_db_Females <- list("gsea_bp" = gsea_bp, "gsea_mf"= gsea_mf, "gsea_cc" = gsea_cc, "gsea_reactome" = gsea_reactome, "gsea_kegg" = gsea_kegg)
# save(file = "functional_results/gsea_org_db_Females.RData", gsea_org_db_Females)
# 
# gsea_org_db_Males <- list("gsea_bp" = gsea_bp, "gsea_mf"= gsea_mf, "gsea_cc" = gsea_cc, "gsea_reactome" = gsea_reactome, "gsea_kegg" = gsea_kegg)
# save(file = "functional_results/gsea_org_db_Males.RData", gsea_org_db_Males)
# 
gsea_org_db_Sex <- list("gsea_bp" = gsea_bp, "gsea_mf"= gsea_mf, "gsea_cc" = gsea_cc, "gsea_reactome" = gsea_reactome, "gsea_kegg" = gsea_kegg)
# save(file = "functional_results/gsea_org_db_Sex.RData", gsea_org_db_Sex)
# 
# gsea_org_db_Case <- list("gsea_bp" = gsea_bp, "gsea_mf"= gsea_mf, "gsea_cc" = gsea_cc, "gsea_reactome" = gsea_reactome, "gsea_kegg" = gsea_kegg)
# save(file = "functional_results/gsea_org_db_Case.RData", gsea_org_db_Case)
# 
# 
# gsea_biomart_Females <- list("gsea_bp" = gsea_bp, "gsea_mf"= gsea_mf, "gsea_cc" = gsea_cc, "gsea_reactome" = gsea_reactome, "gsea_kegg" = gsea_kegg)
# save(file = "functional_results/gsea_biomart_Females.RData", gsea_biomart_Females)
# 
# gsea_biomart_Males <- list("gsea_bp" = gsea_bp, "gsea_mf"= gsea_mf, "gsea_cc" = gsea_cc, "gsea_reactome" = gsea_reactome, "gsea_kegg" = gsea_kegg)
# save(file = "functional_results/gsea_biomart_Males.RData", gsea_biomart_Males)
# 
# gsea_biomart_Sex <- list("gsea_bp" = gsea_bp, "gsea_mf"= gsea_mf, "gsea_cc" = gsea_cc, "gsea_reactome" = gsea_reactome, "gsea_kegg" = gsea_kegg)
# save(file = "functional_results/gsea_biomart_Sex.RData", gsea_biomart_Sex)
# 
# gsea_biomart_Case <- list("gsea_bp" = gsea_bp, "gsea_mf"= gsea_mf, "gsea_cc" = gsea_cc, "gsea_reactome" = gsea_reactome, "gsea_kegg" = gsea_kegg)
# save(file = "functional_results/gsea_biomart_Case.RData", gsea_biomart_Case)

