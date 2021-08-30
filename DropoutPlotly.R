library(plotly)
df <- read.csv(file.choose(),header = TRUE) #choose DeltaDropoutPlotly.csv: plot input file with ID number, ORF 1ab CT, and N CT value

fig <- df %>%
  plot_ly(
    type = 'scatter',
    mode = 'markers',
    x = ~N_CTValue,
    y = ~ORFab_CTValue,
    text = ~ID,
    marker = list(),
    hovertemplate =paste("<b>%{text}</b><br><br>",
                         "%{yaxis.title.text}: %{y:}<br>",
                         "%{xaxis.title.text}: %{x:}<br>",
                         "<extra></extra>")) 

fig
