library(xml2)
library(RColorBrewer)

colour_spectrum <- brewer.pal(11, "Spectral")

classes = c("Rogue", "Fighter", "Wizard", "Person")

for (cls in 1:length(classes)) {
  if(classes[cls] == "Person"){
    width = 5
  }else{
    width=10
  }
  for (c in 1:length(colour_spectrum)) {
    x = read_xml(paste0('./', classes[cls], '.svg'))
    
    paths = xml_find_all(x, "(//*[local-name() = 'path']|//*[local-name() = 'circle'])")
    for (i in 1:length(paths)) {
      if (classes[cls] != "Rogue" | i > 1) {
        xml_set_attr(
          paths[i],
          "style",
          paste0(
            "fill: ",
            colour_spectrum[c],
            "; stroke: black; stroke-width: ",width,"px;"
          )
        )
      }
    }
    
    write_xml(x, paste0('./', classes[cls],'_', colour_spectrum[c], '.svg'))
    
  }
}

# 11 players
setwd("C:/Users/George/Dropbox/D&D/Kushona/six-weeks-on-kushona")
x_xform = 100
y_xform = 75
person_files = list()
for (c in 1:length(colour_spectrum)){
  person_files[[c]] = read_xml(paste0('./img/Person_',colour_spectrum[c],'.svg'))
  person_paths = xml_find_all(person_files[[c]], "(//*[local-name() = 'path']|//*[local-name() = 'circle'])")
  for (i in 1:length(person_paths)) {
    xform = paste('translate(',c*x_xform,')')
    if((c%%2)==0){
      xform = paste('translate(',c*x_xform,',',y_xform,')')
    }
    xml_set_attr(
      person_paths[i],
      "transform",xform)
  }
}

# add in two chunks so evens are on top
person_1_paths = xml_find_all(person_files[[1]], "(//*[local-name() = 'path']|//*[local-name() = 'circle'])")
for (c in seq(2,length(colour_spectrum),2)){
  person_c_paths = xml_find_all(person_files[[c]], "(//*[local-name() = 'path']|//*[local-name() = 'circle'])")
  xml_add_sibling(person_1_paths,person_c_paths)
}
for (c in seq(1,length(colour_spectrum),2)){
  person_c_paths = xml_find_all(person_files[[c]], "(//*[local-name() = 'path']|//*[local-name() = 'circle'])")
  xml_add_sibling(person_1_paths,person_c_paths)
}

xml_set_attr(person_files[[1]],'viewBox','0 0 3200 1300')
write_xml(person_files[[1]],paste0('./img/Person_combined_xform.svg'))



# 11 classes
setwd("C:/Users/George/Dropbox/D&D/Kushona/six-weeks-on-kushona")
x_xform = 205
y_xform = 100
person_files = list()
#[1] "Black Bear"      "Bryn"            "Elizabeth Swann" "Lophelios"       "Malotir"         "Mya"             "Orangelad1"     
#[8] "Seka"            "Tarquinn"        "YÃ viel"         "Ygritte"   
classes = c("Fighter","Wizard","Rogue","Fighter","Fighter","Wizard","Wizard","Rogue","Wizard","Rogue","Rogue")
for (c in 1:length(colour_spectrum)){
  cls = classes[[c]]
  person_files[[c]] = read_xml(paste0('./img/',cls,'_',colour_spectrum[c],'.svg'))
  person_paths = xml_find_all(person_files[[c]], "(//*[local-name() = 'path']|//*[local-name() = 'circle'])")
  for (i in 1:length(person_paths)) {
    xform = paste('translate(',c*x_xform,')')
    if((c%%2)==0){
      xform = paste('translate(',c*x_xform,',',y_xform,')')
    }
    xml_set_attr(
      person_paths[i],
      "transform",xform)
  }
}

# add in two chunks so evens are on top
person_1_paths = xml_find_all(person_files[[1]], "(//*[local-name() = 'path']|//*[local-name() = 'circle'])")
for (c in seq(2,length(colour_spectrum),2)){
  person_c_paths = xml_find_all(person_files[[c]], "(//*[local-name() = 'path']|//*[local-name() = 'circle'])")
  xml_add_sibling(person_1_paths,person_c_paths)
}
for (c in seq(1,length(colour_spectrum),2)){
  person_c_paths = xml_find_all(person_files[[c]], "(//*[local-name() = 'path']|//*[local-name() = 'circle'])")
  xml_add_sibling(person_1_paths,person_c_paths)
}

xml_set_attr(person_files[[1]],'viewBox','0 0 3200 1300')
write_xml(person_files[[1]],paste0('./img/Classes_combined_xform.svg'))