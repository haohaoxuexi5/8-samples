# 加载依赖包
library(tidyverse)
library(ggrepel)
library(grid)

# 自定义函数
mutiVolcano = function(plot_df,         # 差异分析整理好的绘图数据
                       P = 0.05,        # P值阈值
                       FC = 1.5,        # Fold Change 阈值
                       GroupName = c("Up", "Down", "Not Sig"),      # 分组标签
                       pointColor = c("#CC3333", "#3399FF", "Grey"), # Sig vs Not Sig 点颜色
                       barFill = "#efefef",                 # 柱子的填充色
                       pointSize = 0.9,                     # 散点大小
                       labeltype = "1",                     # 标记类型（1 = topN，2 = 指定名称）
                       labelNum = 10,                        # 每组标记 top 基因数
                       labelName = NULL,                    # 如果 labeltype=2，指定标注基因
                       tileLabel =  "Label",                # 比较组标签显示方式
                       tileColor = NULL                     # 每组 tile 的颜色
){
  message("🚀 mutiVolcano function started...")
  
  # 数据预处理
  plot_df <- plot_df %>%
    mutate(log2FC = log2(FC)) %>%
    filter(FC>{{FC}} | FC <(1/{{FC}})) %>%
    filter(log2FC > -6 & log2FC < 6) %>%
    mutate(
      Group = case_when(
        PValue < P & log2FC > 0 ~ GroupName[1],  # Up
        PValue < P & log2FC < 0 ~ GroupName[2],  # Down
        TRUE ~ GroupName[3]                      # Not Sig
      ),
      Group = factor(Group, levels = GroupName),
      Cluster = factor(Cluster, levels = unique(Cluster))
    )
  
  # 柱状范围
  dfBar <- plot_df %>%
    group_by(Cluster) %>%
    summarise(min = min(log2FC, na.rm = TRUE),
              max = max(log2FC, na.rm = TRUE))
  
  # 抖动点
  dfJitter <- plot_df %>%
    mutate(jitter = jitter(as.numeric(Cluster), factor = 2))
  
  # 标注基因选择
  if (labeltype == "1") {
    dfLabel <- dfJitter %>%
      filter(Group == GroupName[1]) %>%
      arrange(desc(abs(log2FC))) %>%
      slice_head(n = labelNum)
  }else if (labeltype == "2") {
    dfLabel <- dfJitter %>%
      filter(Name %in% labelName)
  } else {
    dfLabel <- dfJitter
  }
  
  # 主图绘制
  p <- ggplot() +
    # 柱子上下
    geom_col(data = dfBar, aes(x = Cluster, y = max), fill = alpha(barFill, 0.3)) +
    geom_col(data = dfBar, aes(x = Cluster, y = min), fill = alpha(barFill, 0.3)) +
    
    # 散点
    geom_point(data = dfJitter,
               aes(x = jitter, y = log2FC, color = Group),
               size = pointSize, show.legend = NA) +
    # 给标注的基因加黑边框白心点
    geom_point(data = dfLabel,
               aes(x = jitter, y = log2FC),
               color = "black",
               
               shape = 21,
               size = pointSize + 1,
               stroke = 0.8,
               show.legend = FALSE) +
    # 中间tile
    geom_tile(data = plot_df,
              aes(x = Cluster, y = 0, fill = Cluster),
              color = "black", height = log2(FC) * 1.5, show.legend = NA)
  
  # 标签内容
  if (tileLabel == "Label") {
    p <- p +
      geom_text(data = plot_df, aes(x = Cluster, y = 0, label = Cluster)) +
      scale_fill_manual(values = tileColor, guide = NULL)
  } else if (tileLabel == "Num") {
    p <- p +
      geom_text(data = plot_df, aes(x = Cluster, y = 0, label = as.numeric(Cluster))) +
      scale_fill_manual(values = tileColor,
                        labels = paste0(1:length(unique(plot_df$Cluster)), ": ", unique(plot_df$Cluster)))
  }
  
  # 添加标注
  p <- p + ggrepel::geom_text_repel(
    data = dfLabel,
    aes(x = jitter, y = log2FC, label = Name),
    min.segment.length = 0.1,
    max.overlaps = 10000,
    size = 4,
    box.padding = unit(0.5, 'lines'),
    point.padding = unit(0.1, 'lines'),
    segment.color = 'black',
    show.legend = FALSE
  )
  
  # 美化主题
  p <- p +
    scale_color_manual(values = pointColor) +
    theme_classic() +
    scale_y_continuous(n.breaks = 5) +
    theme(
      legend.position = "right",
      legend.title = element_blank(),
      legend.background = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8)  # 加完整边框
    ) +
    labs(x = "Clusters", y = "log2FC") +
    guides(color = guide_legend(override.aes = list(size = 3)))
  
  message("✅ mutiVolcano finished.")
  return(p)
}

