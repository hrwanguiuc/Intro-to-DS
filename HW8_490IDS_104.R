# HW 8 - Due Tuesday Nov 8, 2016 in moodle and hardcopy in class. 
# Upload R file to Moodle with filename: HW8_490IDS_104.R
# Do not remove any of the comments. These are marked by #

### This assignment will use Google Earth for data display. 
### The .rda file is uploaded to Moodle.

### Load HW8.rda and attach the XML library


### Part 1.  Create the data frame
### Look at the instructions in HW8.pdf.
### Functions you'll want to use: xmlParse(), xmlRoot(), xpathSApply(), xmlGetAttr().
### It also might make it easier to use: xmlToList(), merge().

### Load the data frame called LatLon from HW8.rda. 

load("/Users/wanghaoran/Downloads/hw8.rda")

### Download the gzipped XML factbook document from
### http://jmatchparser.sourceforge.net/factbook/
### and create an XML "tree" in R 

# use the package XML 
library("XML")
# read XML file
factbook <- xmlTreeParse("/Users/wanghaoran/Downloads/factbook.xml",useInternalNodes = TRUE)
root <- xmlRoot(factbook)
### Use XPath to extract the infant mortality and the CIA country codes from the XML tree
###  

# extract infant mortality value from the xml tree 
cia_im_country_set <- xpathSApply(root, "//field[@name = 'Infant mortality rate']")
cia_im = c()
cia_country = c()
temp_xml = cia_im_country_set[[1]]
for (i in 2:xmlSize(temp_xml)){
  temp_cia_im = xmlGetAttr(temp_xml[[i]],'number')
  cia_im = c(cia_im, as.numeric(temp_cia_im))
  temp_cia_country = xmlGetAttr(temp_xml[[i]],'country')
  temp_cia_country = toupper(temp_cia_country)
  cia_country[i] = temp_cia_country
}
# remove NA value in the vector
cia_country <- cia_country[!is.na(cia_country)]


### Create a data frame called IM using this XML file.
### The data frame should have 2 columns: for Infant Mortality and CIA.Codes.

IM <- data.frame("InfantMortality" = cia_im, "CIA.Codes" = cia_country)

### Extract the country populations from the same XML document
### Create a data frame called Pop using these data.
### This data frame should also have 2 columns, for Population and CIA.Codes.

pop_set <- xpathSApply(root, "//field[@name = 'Population']")
pop_country = c()
pop = c()
for (i in 2:xmlSize(pop_set[[1]])){
  temp_pop = xmlGetAttr(pop_set[[1]][[i]],'number')
  pop = c(pop, as.numeric(temp_pop))
  temp_pop_country = toupper(xmlGetAttr(pop_set[[1]][[i]],'country'))
  pop_country = c(pop_country, temp_pop_country)
}

Pop <- data.frame("Population" = pop, "CIA.Codes" = pop_country)

### Merge the two data frames to create a data frame called IMPop with 3 columns:
### IM, Pop, and CIA.Codes

# merge two data frame
IMPop <- merge(IM,Pop)

### Now merge IMPop with LatLon (from newLatLon.rda) to create a data frame called AllData that has 6 columns
### for Latitude, Longitude, CIA.Codes, Country Name, Population, and Infant Mortality
### (please check lat,long are not reversed in the file)

# create new data frame
AllData <- merge(IMPop,LatLon)

### Part 2.  Create a KML document
### Make the KML document described in HW8.pdf.  It should have the basic
### structure shown in that document.  You can use the addPlacemark function below to make
### the Placemark nodes, you just need to complete the line for the Point node and
### figure out how to use the function.

makeBaseDocument = function(){
### This code creates the template KML document 

  temp_doc = newXMLDoc()
  root = newXMLNode("kml",parent = temp_doc)
  document = newXMLNode("Document",parent = root)
  d_name = newXMLNode("name","Country Facts",parent = document)
  d_description = newXMLNode("description","Infant Mortality",parent = document)
  folder = newXMLNode("Folder",parent=document)
  f_name = newXMLNode("name","CIA Fact Book",parent = folder)
  look_at = newXMLNode("LookAt",parent = document)
  
  
  return (temp_doc)
}

kml_doc = makeBaseDocument()
kml_root = xmlRoot(kml_doc)
# generate the children of document
kml_document_children = xmlChildren(xmlChildren(kml_root)[[1]])
kml_folder = kml_document_children[[3]]
# complete point code 
addPlacemark = function(lat, lon, ctryCode, ctryName, pop, infM, parent, 
                        inf1, pop1, style = FALSE)
{
  pm = newXMLNode("Placemark", 
                  newXMLNode("name", ctryName), attrs = c(id = ctryCode), 
                  parent = parent)
       newXMLNode("description", paste(ctryName, "\n Population: ", pop, 
                                  "\n Infant Mortality: ", infM, sep =""),
             parent = pm)

        point = newXMLNode("Point", parent = pm)
        newXMLNode("coordinates", paste(lon, ",", lat,",", 0, sep = ""),parent = point)
        
             
### You need to fill in the code for making the Point node above, including coordinates.
### The line below won't work until you've run the code for the next section to set up
### the styles.

  if(style) newXMLNode("styleUrl", paste("#YOR", inf1, "-", pop1, sep = ''), parent = pm)
}

## add tages via addplacemark function

init = function(par){
  for (i in 1:(nrow(AllData))){
    addPlacemark(lat = AllData$Latitude[i], lon = AllData$Longitude[i],
                 ctryCode = AllData$CIA.Codes[i],ctryName = AllData$Country.Name[i],
                 pop = AllData$Population[i],infM = AllData$InfantMortality[i],
                 parent = par)
  }
  return (kml_doc)
}
# genetrate final kml doc
kml_final_doc = init(kml_folder)

### Save your KML document here, call it Part2.kml, and open it in Google Earth.
### (You will need to install Google Earth.)  
### It should have pushpins for all the countries.

file_name = "Part2.kml"
saveXML(kml_final_doc,file=file_name)

### Part 3.  Add Style to your KML
### Now you are going to make the visualizatiion a bit fancier.  Pretty much all the code is given to you
### below to create style elements that are to be placed near the top of the document.
### These , you just need to figure out what it all does.

### Start fresh with a new KML document, by calling makeBaseDocument()

doc2 = makeBaseDocument()
kml_root2 = xmlRoot(doc2)
# generate the children of document
kml_document_children2 = xmlChildren(xmlChildren(kml_root2)[[1]])
kml_folder2 = kml_document_children2[[3]]

# complete point code 
### The following code is an example of how to create cut points for 
### different categories of infant mortality and population size.
### Figure out what cut points you want to use and modify the code to create these 
### categories.
infCut = cut(AllData$InfantMortality, breaks = c(0, 10, 25, 50, 75, 200))
infCut = as.numeric(infCut)
popCut = cut(log(AllData$Population), breaks = 5)
popCut = as.numeric(popCut)

### Now figure out how to add styles and placemarks to doc2
### You'll want to use the addPlacemark function with style = TRUE

### Below is code to make style nodes. 
### You should not need to do much to it.

### You do want to figure out what scales to you for the sizes of your circles
scales = c(0.5, 1, 3, 5, 10)

addStyle = function(col1, pop1, parent, urlBase, scales = scales)
{
  color_set =c("blue", "green", "orange", "red", "yellow")
  choosed_color = color_set[col1]
  st = newXMLNode("Style", attrs = c("id" = paste("YOR", col1, "-", pop1, sep="")), parent = parent)
  newXMLNode("IconStyle", 
			 newXMLNode("scale", scales[pop1]),  
		     newXMLNode("Icon", paste(urlBase, "color_label_circle_", choosed_color, ".png", sep ="")), parent = st)
}

for (k in 1:5)
{
  for (j in 1:5)
  {
    addStyle(j, k, kml_folder2, 'http://www.stanford.edu/~vcs/StatData/circles/',scales)
  }
}

## implementation with style
init2 = function(par){
  for (i in 1:(nrow(AllData))){
    addPlacemark(lat = AllData$Latitude[i], lon = AllData$Longitude[i],
                 ctryCode = AllData$CIA.Codes[i],ctryName = AllData$Country.Name[i],
                 pop = AllData$Population[i],infM = AllData$InfantMortality[i],
                 inf1 = infCut[i], pop1 = popCut[i],
                 parent = par, style=TRUE)
  }
  return (doc2)
}
# genetrate final kml doc
doc2_final = init2(kml_folder2)

file_name2 = "Part3.kml"
saveXML(doc2_final,file=file_name2)
### You will need to figure out what order to call addStyle() and addPlacemark()
### so that the tree is built properly. You may need to adjust the code to call the png files

### Finally, save your KML document, call it Part3.kml and open it in Google Earth to 
### verify that it works.  For this assignment, you only need to submit your code, 
### nothing else.  You can assume that the grader has already loaded HW8.rda.

