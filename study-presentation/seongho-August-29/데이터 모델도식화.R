#데이터 관계도 그리기 (일단 7개 데이터 )
#devtools::install_github("bergant/datamodelr")
#install.packages("DiagrammeR")
library(datamodelr) #데이터모델 도식화
library(DiagrammeR)

colnames(all_things_industry)
colnames(all_things_kibon)
colnames(all_things_sise)
colnames(foreinger_proportion)
colnames(industry_distribution)
colnames(investment_index)
colnames(investor)


trade_model <- dm_from_data_frames(all_things_industry,
                                    all_things_kibon,
                                    all_things_sise,
                                    foreinger_proportion,
                                    industry_distribution,
                                    investment_index,
                                    investor)
trade_graph <- dm_create_graph(trade_model, rankdir = "BT", col_attr = c("column", "type"))
dm_render_graph(trade_graph)

trade_model <- dm_add_references(
  trade_model,
  industry_distribution$업종명== all_things_industry$업종명,
  all_things_kibon$단축코드==all_things_sise$종목코드,
  all_things_kibon$단축코드==all_things_industry$종목코드,
  all_things_kibon$단축코드==foreinger_proportion$종목코드,
  all_things_kibon$단축코드==investment_index$종목코드,
  all_things_kibon$단축코드==investor$종목코드
  
)



trade_graph <- dm_create_graph(trade_model, rankdir = "BT", col_attr = c("column", "type"))
dm_render_graph(trade_graph)
#https://github.com/bergant/datamodelr   데이터 관계 그림 정리된 깃헙헙
