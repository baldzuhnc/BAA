library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)  # for saving svg

d <- grViz("
digraph {
  compound = true
  graph [ranksep = 0.5]
  
  node [shape = none]
    A [label = 'Macro', shape = none, style = bold]
    D [label = 'Meso \n Inter-industry effect', shape = none]
    G [label = 'Micro \n Intra-industry effect', shape = none]
    
    B [label = 'Production of \n complex goods']
    C [label = 'Spatial agglomeration']
    
    
    E [label = 'Industries which produce \n complex goods']
    F [label = 'Settlement in large \n metropolitan areas']
    
    
    H [label = 'Companies need employees \n from manifold backgrounds']
    I [label = 'Companies settle where \n location factors are beneficial']

    
  edge []
    
    rankdir = 'LR'
    B -> C [minlen = 15, style = dashed, dir = both]
    B -> E [style = dashed]
    
    E -> F [minlen = 10, style = dashed]
    E -> H [style = dashed]
    
    H -> I [minlen = 5, style = dashed]
    I -> F [style = dashed, style = dashed]
    F -> C [style = dashed, style = dashed]
    
{rank = same; A; B; C}
{rank = same; D; E; F}
{rank = same; G; H; I}
           
    rankdir = 'TB'
    
    A -> D -> G [style = invis]
           
                  
                  
                  
    
}
")
d


#Export
export_svg(d) %>%
  charToRaw %>% 
  rsvg_pdf("model.pdf")
