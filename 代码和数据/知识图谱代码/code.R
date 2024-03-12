setwd("D:\\OneDrive\\桌面\\R_code\\2023-2024（1）R语言结课论文要求\\代码和数据\\知识图谱代码")

install.packages("remotes")
remotes::install_github("neo4j-rstats/neo4r")

library(neo4r)
con <- neo4j_api$new(
  url = "http://localhost:7474",
  user = "neo4j", 
  password = "password"
)

get_relation = function() {
  d = df[, c("name", "relation")]
  # remove duplicated (name, relation)
  d1 = d[!duplicated(d),]
  # people with relation
  d11 = d1[!d1$relation == "",]
  # people without relation
  d12 = d1[d1$relation == "",]
  res = d11
  for (n in d12$name) {
    if (!(n %in% d11$name) && n != "") {
      res = rbind(res, c(n, ""))
    }
  }
  res
}

get_relation2 = function() {
  df = read.csv("Su-shi.csv", stringsAsFactors = FALSE)
  d = df[, c("name", "relation", "career")]
  # remove duplicated (name, relation)
  d1 = d[!duplicated(d[,1:2]),]
  # people with relation
  d11 = d1[!d1$relation == "",]
  # people without relation
  d12 = d1[d1$relation == "",]
  res = d11
  for (n in d12$name) {
    if (!(n %in% d11$name) && n != "") {
      res = rbind(res, c(n, "", ""))
    }
  }
  idx = grep("太守", res$career)
  res[idx, "career"] = "太守"
  idx = grep("县", res$career)
  res[idx, "career"] = "县官"
  res
}

data = get_relation2()
write.csv(data, "D:\\Applications\\neo4j-community-3.5.30\\import\\Sushi_relation.csv", row.names = FALSE)



