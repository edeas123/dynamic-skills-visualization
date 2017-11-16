
# install required packages
if (!require(ndtv)) {
  install.packages(ndtv)  
}

if (!require(dplyr)) {
  install.packages(dplyr)  
}

if (!require(dplyr)) {
  install.packages(tidyr)  
}

# load required packages
library(ndtv)
library(dplyr)
library(tidyr)

# skills
project_file = "projects.jl"

# read the skills from the projects file
# skillID, skillName, projectID, projectDesc, projectYear, skillCategory
resume <- read.csv2(file=file.path("data", project_file), stringsAsFactors = FALSE) %>%
            as_tibble() %>% 
            mutate(skills = strsplit(skills, ",")) %>% 
            unnest(skills) %>% # each skill should be on a row
            mutate(skills = tolower(skills)) %>% # convert skills to lowercase for uniformity
            mutate(skillsID = as.numeric(as.factor(skills))) #%>% # get index for the skills
            # group_by(projectYear) %>%
            # add_count(skillsID) %>% # add a column, n representing the number of unique projects
            # ungroup

# construct vertex data
rawVerts <- resume %>% 
          mutate(vertex_id = as.numeric(skillsID), onset = projectYear, terminus = Inf) %>%
#          group_by(projectYear) %>%
#          add_count(vertex_id) %>% # add a column, n representing the number of unique projects
#          ungroup %>%
          select(-skillsID, -projectID, -projectDesc, -projectYear) %>%
          arrange(onset) %>%
          group_by(vertex_id) %>%
          do(head(.,1)) %>%

# TODO: construct column for skills category
          mutate("category" = "Default")

# construct edge data
rawEdges <- resume %>% 
            group_by(projectYear, projectID) %>% 
            filter(n() > 1) %>% # ignore vertex without edges
            do(as_tibble(t(combn(.$skillsID, 2)))) %>%  # build the edges
            mutate(onset = projectYear, terminus = Inf)


# construct networkDynamic objects
rawVerts <- as.data.frame(rawVerts) # reformat before using to build networkDynamic object (required)
rawEdges <- as.data.frame(rawEdges) # reformat before using to build networkDynamic object (required)

resumenet <- networkDynamic(vertex.spells = rawVerts[,c(3,4,2,1,5)], edge.spells = rawEdges[,c(5,6,3,4,1,2)])

# add vertex attributes
set.vertex.attribute(resumenet,"skill", rawVerts$skills) # add in name of each vertex (i.e. skills)
set.vertex.attribute(resumenet,"category", rawVerts$category) # add in the unchanging vertex attribute data
#set.vertex.attribute(resumenet,"color", rawVerts$Color) # add in the category color
set.vertex.attribute(resumenet, "vertex.names", rawVerts$skills)

# add dynamic network attributes
# add the number of unique project for each skills up to each time instance.
for (i in 1:nrow(rawVerts)) {
  r2 <- resume %>% filter(skillsID == i)  %>% group_by(projectYear) %>% summarise(n = n()) %>% mutate(n = cumsum(n))
  for (j in 1: nrow(r2)) {
    value <- r2$n[j]
    year <- r2$projectYear[j]
    activate.vertex.attribute(resumenet,'weight', value, onset=year,terminus=Inf, v=i)
  }
}



render.d3movie(resumenet, vertex.tooltip = function(slice){paste("<b>",(slice %v% "skill") , 
                                                                 ",", (slice %v% "weight"),"</b>")},
               vertex.cex = function(slice){slice %v% "weight"},
               usearrows = FALSE,
               vertex.lwd = 3,
               displaylabels = TRUE
               
               )

#render.d3movie(net, filename = filename, launchBrowser = FALSE, 
            #   script.type = 'embedded', vertex.col=net%v%"color",
            #   vertex.tooltip = tooltip,
             #  edge.lwd=function(slice){slice%e%'weight'})


