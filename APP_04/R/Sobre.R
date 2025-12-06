# Card de apresentação dos membros
card = function(name, description, profile_link){
  ns = NS(name)
  
  tagList(
    div(
      img(src = "profile-photo.jpg", height = "200px"),
      h2(
        name,
        class = 'card-name'
      ),
      p(
        description,
        class = 'card-description'
      ),
      class = 'card-container'
    )
  )
}

Sobre = function(id){
  ns = NS(id)
  
  tagList(
    h1('Sobre', class = 'sobre-title'),
    p('Este aplicativo interativo foi desenvolvido como requisito final da disciplina Elementos da Programação para 
      Estatística e representa a aplicação prática de técnicas de Ciência de Dados utilizando a linguagem R e o framework
      Shiny, sendo fruto do trabalho colaborativo de uma equipe de quatro estudantes que se dedicaram a explorar o universo 
      da série The Simpsons. Utilizando como base o conjunto de dados disponibilizado pelo projeto TidyTuesday, nossa análise 
      foca especificamente no recorte temporal das temporadas 21 a 28, integrando múltiplas fontes de informação que variam
      desde as linhas de diálogo dos roteiros e metadados de localizações até as estatísticas de audiência e as avaliações da
      crítica no IMDB. A metodologia adotada fundamentou-se na filosofia do Tidyverse para garantir a limpeza, manipulação e
      estruturação eficiente de milhares de registros, transformando dados brutos em insights visuais que permitem ao usuário
      navegar pela evolução da qualidade da série, investigar padrões de fala dos personagens e compreender as correlações
      estatísticas entre o cenário e a recepção do público, sendo que todo o desenvolvimento, incluindo o relatório científico
      detalhado e o código-fonte deste dashboard, está documentado e disponível publicamente em nosso repositório no GitHub 
      cumprindo o objetivo acadêmico de comunicar resultados analíticos de forma clara e reprodutível.', class = 'sobre-text'),
    h2('Integrantes', class = 'integrantes-title'),
    div(
      card('Lucas William', 'Meu nome é Lucas William, tenho 18 anos. Gosto de programar, escrever e praticar esportes.', '#'),
      card('Lucas William', 'Meu nome é Lucas William, tenho 18 anos. Gosto de programar, escrever e praticar esportes.', '#'),
      card('Lucas William', 'Meu nome é Lucas William, tenho 18 anos. Gosto de programar, escrever e praticar esportes.', '#'),
      card('Lucas William', 'Meu nome é Lucas William, tenho 18 anos. Gosto de programar, escrever e praticar esportes.', '#'),
      class = 'integrantes-cards'
    ),
    h2('Dados e Fontes', class = 'dadosEfontes-title'),
    p('⮕ Conjunto de dados utilizados no projeto:',
      a("Simpsons Dataset", href="https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-02-04/readme.md"),
      class = 'dataset-link'
    ),
    p('⮕ Relatório final da análise realizada:',
      a("[LINK AQUI]", href="#"),
      class = 'dataset-link'
    )
  )
}

