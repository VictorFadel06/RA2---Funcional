# Sistema de Inventário (RA2) - Haskell

**Autores (exemplo):**
- Anna Bosquilia Navarro - https://github.com/AnnaYx
- Nicole Pereira Guarnieri	 - https://github.com/nick11nic
- Victor Valerio Fadel - https://github.com/VictorFadel06

**Disciplina:** Programação Lógica e Funcional 

**Professor:** Frank Coelho de Alcantara

---

## Objetivo
Programa interativo em terminal em Haskell que gerencia um inventário, persiste estado em disco (`Inventario.dat`) e registra todas as operações em log de auditoria (`Auditoria.log`). Lógica de negócio implementada em funções puras (módulo `Logic.hs`). IO isolado no `Main.hs`.

---

## Arquivos no repositório
- `Main.hs` — loop principal e I/O.
- `DataTypes.hs` — tipos de dados (Item, Inventario, LogEntry, AcaoLog, StatusLog).
- `Logic.hs` — funções puras: `addItem`, `removeItem`, `updateQty`.
- `Report.hs` — funções puras para análise de logs: `logsDeErro`, `historicoPorItem`, `itemMaisMovimentado`.
- `Inventario.dat` — exemplo de inventário serializado (gerado automaticamente).
- `Auditoria.log` — exemplo de log.
- `README.md` — este documento.
- `.gitignore`

---

## Como executar (Online GDB / Repl.it)

### Online GDB
1. Abra o link do projeto no Online GDB: https://www.onlinegdb.com/ZlUmsFex2
2. Selecione e execute o arquivo 'main.hs'. O programa irá criar `Inventario.dat` e `Auditoria.log` no diretório se não existirem.

---

## Comandos disponíveis (no terminal)
- `add`    — adiciona item (interativo)
- `remove` — remove quantidade de um item (`remove <id> <quantidade>` também funciona)
- `update` — atualiza quantidade (`update <id> <novaQuantidade>`)
- `list`   — lista inventário atual
- `report` — submenu: `errors`, `historico <id>`, `maismovimentado`
- `help`   — mostra ajuda
- `exit`   — encerra

---

## Formato de persistência
- `Inventario.dat` contém a representação `show` do `Inventario` (usando `Read`/`Show` para desserializar).
- `Auditoria.log` contém linhas com `show` de `LogEntry` — cada linha é um `read` válido.

---

## Cenários de Teste (documentados)

### Cenário 1: Persistência de Estado (Sucesso)
1. Iniciar o programa (sem `Inventario.dat`/`Auditoria.log`).
2. Executar `add` e adicionar 3 itens distintos.
3. Fechar o programa com `exit`.
4. Verificar que `Inventario.dat` e `Auditoria.log` foram criados no diretório.
5. Reiniciar o programa.
6. Executar `list` — deverá mostrar os 3 itens adicionados.

**Resultado esperado:** `Inventario.dat` contém os 3 itens; `Auditoria.log` tem 3 entradas de sucesso.

---

### Cenário 2: Erro de Lógica (Estoque Insuficiente)
1. Adicionar item `teclado` com quantidade 10.
2. Tentar `remove teclado 15`.
3. Programa deve imprimir mensagem de erro clara ("Estoque insuficiente").
4. `Inventario.dat` permanece mostrando 10 unidades.
5. `Auditoria.log` contém uma entrada com `StatusLog` sendo `Falha "Estoque insuficiente"`.

---

### Cenário 3: Geração de Relatório de Erros
1. Após executar o Cenário 2, executar `report` → `errors`.
2. O relatório deve listar a entrada de log referente à tentativa de remoção com estoque insuficiente.

---
