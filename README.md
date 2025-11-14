
<div align="center">
  <img width="200"  alt="5968214" src="https://github.com/user-attachments/assets/db667a3f-fb11-419d-98db-88d4c118f29e" />
  <h1>Sistema De Inventario em Haskell</h1>
  

  <p>
    Sistema desenvolvido em Haskell para por em pratica conceitos de programação funcional, manipulação de estado e operações de I/O em Haskell
para desenvolver um programa que gerencie um sistema de inventário.
  </p>
</div>



## :globe_with_meridians:	Rodar em ambiente virtual

- Para utilizar o programa, abra o link do projeto hospedado no [OnlineGDB](https://www.onlinegdb.com/#)
- 

- Para rodar ele localmente, instale [SWI-Prolog](https://www.swi-prolog.org/download/stable) e [Visual Studio Code](https://code.visualstudio.com/download). 

### :globe_with_meridians:	VS Code

Caso deseje utilizar o VS Code, durante a instalação do [SWI-Prolog](https://www.swi-prolog.org/download/stable) garanta que estejam selecionados:
- [x]  Add swipl to the system PATH for current users

 ou
 
- [x]  Add swipl to the system PATH for all users 

 Caso contrario você deve adicionar na sua variavel de sistema Path:

    
    'C:\Program Files\swipl\bin' - (Caminho padrão da instalação)

Se tiver alterado o local de instalação, encontre e adicione o caminho da pasta:
    
    '..\swipl\bin'


Por ultimo, instale as extensões [Prolog](https://marketplace.visualstudio.com/items?itemName=rebornix.prolog) e [VSC-Prolog](https://marketplace.visualstudio.com/items?itemName=arthurwang.vsc-prolog) pelo link ou direto no VS Code pelo menu Extensions e reinicie o programa.


## :hammer_and_wrench: Instalação

Baixe o [código do projeto](https://github.com/GMizuguti/Sistema-Especialista-para-Recomendacao-de-Trilha-Academica.git) e descompacte o arquivo dentro de uma pasta. Seu diretorio deve ficar da seguinte forma:

    Documentos
    └── Sistema-Academico
        ├── sistema.pl
        ├── base_conhecimento.pl
        ├── motor_de_inferencia.pl
        ├── perfil_1.pl
        ├── perfil_2.pl
        ├── perfil_3.pl  
        ├── perfil_4.pl
        └── perfil_5.pl

Para abrir o projeto, no VS Code siga os seguintes passos:
      
      File > Open Folder > {DIRETORIO_DO_PROJETO} > Select Folder

O setup foi finalizado.


## :runner: Realizar questionario Interativo


Abra o terminal com o comando integrado do VS Code e execute:


```javascript
swipl sistema.pl
```

Para iniciar o questionario:

```javascript
iniciar.
```

Com o questionario em andamento, o sistema ira avançar para a proxima questão assim que a atual for respondida. Para responder as perguntas, basta digitar "s." ou "n." (sem as aspas) e apertar enter.
```javascript
1. Você é uma pessoa sistemática? (s/n) :
```
```javascript
1. Você é uma pessoa sistemática? (s/n) : n.
```

Ao final das 14 questões, serão mostrados as três trilhas de especialização que mais combinam com o perfil do usuário.

<details>

<summary>:bangbang:	 Resultado do Questionario :bangbang:</summary>


```javascript
-------As 3 Trilhas mais Recomendadas para o seu perfil-------

-------Primeiro lugar-------

1) Desenvolvimento de Jogos! Crie experiencias interativas em 2D e 3D, unindo criatividade, design e programacao! (15 pontos)
Perguntas que te trouxeram a essa trilha:

1. Voce e uma pessoa sistematica? (s/n) -> s

...

14. Programar e estudar as minuncias de assembly soa como entretenimento? (s/n) -> s



-------Segundo lugar-------

2) Gestao de Projetos! Planeje, organize e lidere equipes para alcancar resultados de forma eficiente! (9 pontos)


-------Terceiro lugar-------

 3) Neurotecnologia! Explore a interface entre o cerebro humano e dispositivos tecnologicos para saude e inovacao! (8 pontos)
```
</details>

Para encerrar o processo, digite o seguinte comando e aperte enter.

```javascript
halt.
```

## :construction:	Arquivos de Teste

Estão disponiveis os seguintes arquivos de teste:
  
- perfil_1.pl
- perfil_2.pl
- perfil_3.pl
- perfil_4.pl
- perfil_5.pl

Cada arquivo contém as respostas predefinidas de 5 perfis com afinidades à trilhas distintas.

Para executar esses arquivos rode o seguinte comando
```bash
iniciar_teste_perfil(ARQUIVO).
```
Substitua ARQUIVO pelo nome do arquivo desejado 

O resto da execução permanece a mesma do Questionario Interarivo

> [!WARNING]
> Os arquivos perfil_1.pl ... perfil_5.pl devem estar no [mesmo diretorio](#hammer_and_wrench-instalação) que o arquivo sistema.pl!

## :school: Instituiçao

Pontifícia Universidade Católica do Paraná (PUCPR)

Disciplina: Programação Lógica e Funcional

Curso: Ciência da Computação

Turma: 4º U

Professor: Frank Coelho de Alcantara

## :bust_in_silhouette:	 Integrantes

Gustavo Botan Mizuguti - GitHub: GMizuguti


