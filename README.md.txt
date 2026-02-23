# 🚀 Sistema de Cadastro Híbrido (Client/Server RESTful)

Este projeto é uma prova de conceito de uma arquitetura limpa (Clean Architecture) desenvolvida em **Delphi VCL** e **Horse**, demonstrando comunicação assíncrona, processamento de dados em alta performance e integração com APIs externas.

## 🧠 Decisões de Arquitetura e Padrões Aplicados
Este repositório não é apenas um CRUD. Ele foi desenhado visando resiliência e boas práticas de Engenharia de Software:
* **Clean Code & P.O.O:** Separação estrita de responsabilidades (Controllers, Services, DAOs e Models). Nenhuma regra de negócio ou acesso a banco de dados reside nas telas (Views).
* **Alta Performance em Lote:** O `POST` de 50.000 pessoas não usa loops de inserção simples. Ele utiliza **Prepared Statements e Transações ACID**, garantindo integridade de chave estrangeira (Pessoa -> Endereço) de forma extremamente rápida.
* **Processamento Assíncrono (Multithreading):** A sincronização de endereços com a API do ViaCEP ocorre inteiramente em Background via `TThread`, evitando travamentos na UI ou gargalos no Servidor.
* **Resiliência de Memória:** Tratamento rigoroso de exceções e injeção de dependências controlada (prevenção absoluta de *Memory Leaks* e *Access Violations*).
* **API RESTful Semântica:** Uso correto dos verbos HTTP (GET, POST, PUT, DELETE) e Status Codes (200, 201, 202, 204, 400, 500) utilizando o middleware **Horse.Jhonson** para respostas em JSON nativo.

## 🛠️ Tecnologias Utilizadas
* **Backend:** Delphi (Horse Framework)
* **Frontend:** Delphi VCL
* **Banco de Dados:** PostgreSQL (FireDAC)
* **Integrações:** API ViaCEP via `THTTPClient`