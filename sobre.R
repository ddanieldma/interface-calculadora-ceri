sobre_ui <- function(id){
  tagList(
    h3(class = "text-blue", "Sobre este dashboard"),
    
    p(class = "text-blue", "Este dashboard foi criado com o objetivo de apresentar dados sobre o mercado de gás no Brasil. Ele foi desenvolvido com o objetivo de promover a abertura do mercado de gás no Brasil e conta com uma série de patrocinadores."),
    
    p(class = "text-blue","Informações sobre os autores podem ser encontradas em:"),
    
    tags$ul(
      tags$li(
        p(class = "text-blue", 
          "Joisa Dutra (", 
          tags$a(href = "https://ceri.fgv.br/equipe/joisa-dutra", "FGV CERI", target = "_blank"), ", ", 
          tags$a(href = "https://www.linkedin.com/in/joisa-dutra-saraiva-429b735b/", "LinkedIn", target = "_blank"),");"
        )
      ),
      
      tags$li(
        p(class = "text-blue", 
          "Diogo Romeiro (",
          tags$a(href = "https://ceri.fgv.br/equipe/diogo-lisbona-romeiro", "FGV CERI", target = "_blank"), ", ", 
          tags$a(href = "https://www.linkedin.com/in/diogo-lisbona-romeiro-913819252/", "LinkedIn", target = "_blank"),");"
        )
      ),
      
      tags$li(
        p(class = "text-blue", 
          "Rafael Martins de Souza (",
          tags$a(href = "https://ceri.fgv.br/equipe/rafael-martins-de-souza", "FGV CERI", target = "_blank"), ", ", 
          tags$a(href = "https://www.linkedin.com/in/rafaelmartinsdesouza/", "LinkedIn", target = "_blank"),")."
        )
      )
    ),
    
    p(class = "text-blue", 
      "Ele também contou com a participação dos seguintes bolsistas de pesquisa."
    ),
    
    tags$ul(
      tags$li(
        p(class = "text-blue", 
          "Ícaro Hernandes (",
          tags$a(href = "https://ceri.fgv.br/equipe/icaro-franco-hernandes", "FGV CERI", target = "_blank"), ", ", 
          tags$a(href = "https://www.linkedin.com/in/icaro-franco-hernandes/", "LinkedIn", target = "_blank"),");"
        )
      ),
      
      tags$li(
        p(class = "text-blue", 
          "Daniel Almeida (",
          tags$a(href = "https://ceri.fgv.br/equipe/daniel-de-miranda-almeida", "FGV CERI", target = "_blank"), ", ", 
          tags$a(href = "https://www.linkedin.com/in/daniel-de-miranda-almeida/", "LinkedIn", target = "_blank"),");"
        )
      ),
      
      tags$li(
        p(class = "text-blue", 
          "Rachel Granville (",
          tags$a(href = "https://ceri.fgv.br/equipe/rachel-granville-garcia-leal", "FGV CERI", target = "_blank"), ", ", 
          tags$a(href = "https://www.linkedin.com/in/rachelgranville/", "LinkedIn", target = "_blank"),")."
        )
      )
    ),
    
    h3(class = "text-blue", "Sobre o FGV CERI"),
    
    p(class = "text-blue", "FGV CERI colabora com o desenvolvimento da regulação econômica no Brasil se valendo de sólidos fundamentos econômicos."),
    
    p(class = "text-blue", "O Centro de Estudos em Regulação e Infraestrutura é a unidade da Fundação Getulio Vargas destinada a pensar, de forma estruturada e com sólidos fundamentos econômicos, a regulação dos setores de infraestrutura no Brasil."),
    
    p(class = "text-blue", "O caráter multidisciplinar da regulação coloca essa instituição em condição privilegiada para contribuir para o desenvolvimento e o fortalecimento da regulação no País."),
    
    p(class = "text-blue", "A regulação tem um papel central na atração de investimentos. Além disso, é protagonista na criação de um ambiente propício para que esses investimentos sejam convertidos em serviços de qualidade a preços competitivos, mas também capazes de garantir a sustentabilidade econômico-financeira de seus prestadores e refletir a alocação de riscos na cadeia de fornecimento."),
    
    p(class = "text-blue", "O FGV CERI compreende os desafios e as oportunidades inerentes ao desenvolvimento dos setores de infraestrutura no Brasil. Por meio de seminários, palestras e encontros realizados por todo o país, do contato com especialistas e imprensa, e da publicação de estudos e resultados de pesquisas aplicadas, apresenta alternativas para problemas-chave e fomenta o debate com as diversas vozes da opinião pública, contribuindo, assim, para o desenvolvimento nacional."),
    
    p(class = "text-blue", 
      "Mais informações podem ser encontradas em ", 
      tags$a(href = "https://ceri.fgv.br/sobre", "https://ceri.fgv.br/sobre", target = "_blank")
    )
  ) 
}