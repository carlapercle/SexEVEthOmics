# global.R for shiny_app

# Paquetes
library(ggplot2)
library(dplyr)
# library(hipathia)
# library(CellChat)


# De prueba
library(fst)
library(qs)


## Espaciar elementos con lineas en blanco
linebreaks <- function(n) {
  HTML(strrep(br(), n))
}


# Datatable con botones de visivilidad de columna, copiar y descargar

showDatatable <- function(df, hide_columns = NULL, entries = TRUE,
                          length_value = c(10, 25, 50, 100, -1),
                          length_label = c("10", "25", "50", "100", "All")) {
  datatable(df, 
    options = list(
      # autoWidth = TRUE, 
      columnDefs = list(
        # list(width = '200px', targets = c(1)),
        list(targets = hide_columns, visible = FALSE)),
      lengthChange = entries, 
      lengthMenu = list(
        length_value,
        length_label
      ),
      buttons =
        list(
          "colvis",
          list(
            title = NULL,
            extend = list("copy"),
            exportOptions = list(columns = ":visible"),
            text = "Copy"
          ),
          list(
            title = NULL,
            extend = list("csv"),
            exportOptions = list(columns = ":visible"),
            text = "Download"
          )
        ),
      dom = "Blfrtip"
    ), extensions = "Buttons",
    escape = FALSE, rownames = FALSE, filter = "top"
  )
}


showDatatable2 <- function(df, hide_columns = NULL, entries = TRUE, columnnames, 
                          length_value = c(10, 25, 50, 100, -1),
                          length_label = c("10", "25", "50", "100", "All")) {
  datatable(df, colnames = columnnames,
            options = list(
              # autoWidth = TRUE, 
              columnDefs = list(
                # list(width = '200px', targets = c(1)),
                list(targets = hide_columns, visible = FALSE)),
              lengthChange = entries, 
              lengthMenu = list(
                length_value,
                length_label
              ),
              buttons =
                list(
                  "colvis",
                  list(
                    title = NULL,
                    extend = list("copy"),
                    exportOptions = list(columns = ":visible"),
                    text = "Copy"
                  ),
                  list(
                    title = NULL,
                    extend = list("csv"),
                    exportOptions = list(columns = ":visible"),
                    text = "Download"
                  )
                ),
              dom = "Blfrtip"
            ), extensions = "Buttons",
            escape = FALSE, rownames = FALSE, filter = "top"
  )
}

cnames <- c("GO ID", "Term", "MS subtype", "Comparison", "Cell type", "Direction", "Gene ratio", 
            "p.value", "Genes", "Annotated", "Significant", "Expected", "Significance") 

dataList <- c("SPMS", "PPMS", "RRMS")


# allgenes_SPMS <- read.table("allgenes_v3.txt", header = T, sep = "\t")

# allgenes <- read.table("allgenes_alldatasets.txt", header = T, sep = "\t")

# Probar a usar formato fst
# write.fst(allgenes, "allgenes_alldatasets.fst")
# allgenes <- read.fst("allgenes_alldatasets.fst")

# esto no estaba comentado #####################
# allgenes <- read.fst("allgenes_alldatasets_chr.fst")
# allgenes$cell.type <- as.factor(allgenes$cell.type)
# allgenes$dataset <- as.factor(allgenes$dataset)
# allgenes$contrast <- as.factor(allgenes$contrast)
# allgenes$FDR <- signif(allgenes$FDR, digits = 3)
# allgenes$p.value <- signif(allgenes$p.value, digits = 3)
# ---------------

# Seleccionar genes y tipos celulares unicos en cerebro 

# selectGenes_spms <- unique(allgenes$gene[allgenes$dataset == "SPMS"])
# celltypes_spms <- unique(allgenes$cell.type[allgenes$dataset == "SPMS"])
# writeLines(selectGenes_spms, "irene/shiny_app/selectGenes_spms.txt")

# esto no estaba comentado #####################
# selectGenes_spms <- readLines("selectGenes_spms.txt")
# celltypes_spms <- factor(c("Astrocytes", "Microglia", "Neurons", "Oligodendrocytes", "OPCs"),
#                          levels = c("Astrocytes", "Microglia", "Neurons", "Oligodendrocytes", "OPCs"))
# ---------------

# Seleccionar genes y tipos celulares unicos en sangre. Son los mismos en los dos tipos

#selectGenes_blood <- unique(allgenes$gene[allgenes$dataset == "PPMS"]) # ---------------
# celltypes_blood <- unique(allgenes$cell.type[allgenes$dataset == "PPMS"])
# writeLines(selectGenes_blood, "irene/shiny_app/selectGenes_blood.txt")


# ---------------
# selectGenes_blood <- readLines("selectGenes_blood.txt")
# celltypes_blood <- factor(c("B cells", "CD4+ T cells", "CD8+ T cells", "Dendritic cells", "Monocytes", "NK cells"),
#                           levels = c("B cells", "CD4+ T cells", "CD8+ T cells", "Dendritic cells", "Monocytes", "NK cells"))
# 
# # Input inicial
# cellList <- celltypes_spms
# 
# # Input inicial
# selectGenes <- selectGenes_spms # ---------------
 
# Lista de genes significativos en todos los grupos
# genes_signif_spms <- allgenes %>%
#   filter(dataset == "SPMS") %>% 
#   group_by(cell.type, gene) %>%
#   filter(sum(FDR < 0.05) == 3)
# 
# genes_signif_spms <- unique(genes_signif_spms$gene)
# writeLines(genes_signif_spms, "irene/shiny_app/genes_signif_spms.txt")
# genes_signif_spms <- readLines("genes_signif_spms.txt")
# 
# # genes_signif_ppms <- allgenes %>%
# #   filter(dataset == "PPMS") %>% 
# #   group_by(cell.type, gene) %>%
# #   filter(sum(FDR < 0.05) == 3)
# 
# # genes_signif_ppms <- unique(genes_signif_ppms$gene)
# # writeLines(genes_signif_ppms, "irene/shiny_app/genes_signif_ppms.txt")
# genes_signif_ppms <- readLines("genes_signif_ppms.txt")
# 
# # genes_signif_rrms <- allgenes %>%
# #   filter(dataset == "RRMS") %>% 
# #   group_by(cell.type, gene) %>%
# #   filter(sum(FDR < 0.05) == 3)
# 
# # genes_signif_rrms <- unique(genes_signif_rrms$gene)
# # writeLines(genes_signif_rrms, "irene/shiny_app/genes_signif_rrms.txt")
# 
# genes_signif_rrms <- readLines("genes_signif_rrms.txt")
# 
# # Input inicial
# genes_signif <- genes_signif_spms
# 
# 
# #### Load Hipathia results
# pathways <- read.csv("hipathia/SPMS/Astrocytes_paths_IDF.tsv", header = T,
#                        sep = "\t")
# 
# pathdf <- data.frame(id = sapply(strsplit(pathways$pathID, "-"), `[`, 2),
#                      name = sapply(strsplit(pathways$path_name, ":"), `[`, 1))
# 
# pathdf <- pathdf[!duplicated(pathdf), ]
# 
# pathList <- paste0(pathdf$id, " (", pathdf$name, ")")
# 
# 
# # Cargar todos los pathways de la base de datos
# # pathwaysObject <- load_pathways(species = "hsa")
# # 
# # save(pathwaysObject, file = "./irene/shiny_app/pathwaysObject.RData")
# load("pathwaysObject.RData")
# 
# # allpathways <- read.csv("allpathways.txt", header = T, sep = "\t") %>% 
# #   mutate_if(is.numeric, signif, 3)
# 
# allpathways <- read.csv("all_hipathia.tsv", header = T, sep = "\t") %>% 
#   mutate_if(is.numeric, signif, 3)
# 
# 
# # allpathways <- allpathways[, c("pathID", "cell_type", "sex",  "path_name" , "effector_protein", "logFC", "p.value", "p.adjusted")]
# colnames(allpathways) <- c("Path ID", "Dataset", "Cell type", "Comparison", "Path name", "Effector protein", "lambda", "logFC", "p.value", "p.adjusted")
# allpathways$`Cell type` <- as.factor(allpathways$`Cell type`)
# allpathways$Dataset <- as.factor(allpathways$Dataset)
# 
# pathNames <- c("Path ID", "MS subtype", "Cell type", "Comparison", "Path name",
#                "Effector protein", "lambda", "logFC", "p.value", "p.adjusted")
# 
# # Todo en plural
# celltypes2 <- c("Astrocytes", "Microglia", "Neurons", "Oligodendrocytes", "OPC")
# 
# # sexList <- c("Sex differences", "Female", "Male")
# sexList <- c("SDID", "IDF", "IDM")
# 
# 
# #### Load Functional profiling results
# 
# # weight01 <- read.csv("weight01.tsv", header = T, sep = " ") %>% 
# #   mutate_if(is.numeric, signif, 3)
# #  
# # weight01 <- weight01[ ,c(1,2,9:12,6:8,3:5,13)]
# 
# # weight01$Genes <- gsub(x = weight01$Genes, pattern = ",", replacement = ", ")
# 
# # weight01_allres <- read.table("weight01_v2.tsv", header = T, sep = "\t")
# # 
# # write.fst(weight01_allres, "weight01_v2.fst")
# weight01_allres <<- read.fst("weight01_v2.fst")
# 
# 
# weight01_allres <- weight01_allres %>%
#   mutate_at(vars(3:6), factor)
# 
# # Saca los GO significativos para cada contraste y para cada dataset
# # result <- weight01 %>%
# #   filter(Significance == "Significant") %>%
# #   select(Dataset, Comparison, GO.ID)
# # 
# # result$selection <- paste(result$Dataset, 
# #                            result$Comparison, 
# #                            result$GO.ID, sep = "_")
# 
# # Weight01 filtered
# 
# # weight01_allres$selection <- paste(weight01_allres$Dataset, 
# #                                    weight01_allres$Comparison, 
# #                                    weight01_allres$GO.ID, sep = "_")
# # 
# # weight01 <- weight01_allres[weight01_allres$selection %in% result$selection, ]
# # weight01 <- weight01[,1:13]
# # write.table(weight01, "irene/shiny_app/weight01_significants.tsv",
# #             row.names = F, col.names = T, sep = "\t")
# 
# weight01 <- read.table("weight01_significants.tsv", header = T, sep = "\t")
# weight01 <- weight01 %>%
#   mutate_at(vars(3:6), factor)
# weight01$Direction <- factor(weight01$Direction, levels = c("UP", "DOWN"))
# 
# ### ---- Cell-cell communication ----
# 
# names.object = c("Control females", "MS females", "Control males", "MS males")
# 
# # object.list_ppms <- readRDS("CCC/PPMS/ccc_list.rds")
# # object.list_rrms <- readRDS("CCC/RRMS/ccc_list.rds")
# # object.list_spms <- readRDS("CCC/SPMS/ccc_list.rds")
# # qsave(object.list_ppms, "CCC/PPMS/ccc_list.qs")
# # qsave(object.list_rrms, "CCC/RRMS/ccc_list.qs")
# # qsave(object.list_spms, "CCC/SPMS/ccc_list.qs")
# 
# # names(object.list_ppms) <- names.object
# # names(object.list_rrms) <- names.object
# # names(object.list_spms) <- names.object
# 
# # ccc <- read.table("all_ccc.csv", sep = "\t", header = T)
# cccnames <-c("Cell source", "Cell target", "Ligand", "Receptor", "Pathway",
#              "MS subtype", "Group", "Interaction strength", "p.value", "p.adjusted",
#              "Annotation", "Evidence", "Interaction.name")
# ccc <- read.csv("all_ccc.csv", header = T) %>% mutate_if(is.numeric, signif, 3)
# # colnames(ccc) <- cccnames
# # ccc <- ccc[, c(1:12,14)]
# 
# ccc_path_list <- unique(ccc$pathway_name)
# # Colores para SPMS
# 
# as = "#937364" #"#F99D72"
# mi = "#FFF175"
# ne = "#A68FA6"
# ol = "#2D5075"
# op = "#e7adad" #"#279087"
# 
# ccc_spms_colors = c(as, mi, ne, ol, op)
# 
# # Colores para RRMS y PPMS
# 
# b = "#D2A8C1"
# cd4 = "#44959A"
# cd8 = "#3D405B"
# nk = "#81E8B7"
# dc = "#E2D166"
# mono = "#D7962C"
# 
# ccc_blood_colors = c(b, cd4, cd8,   dc, mono, nk)
# 
# 
# 
