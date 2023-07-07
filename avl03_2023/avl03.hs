import Data.List

data DadosPessoa
  = Pessoa
      { nome :: String,
        idade :: String,
        rua :: String,
        nCasa :: String,
        cidade :: String
      }
  | Vazio
  deriving (Eq, Ord, Show)

printDadosPessoa :: DadosPessoa -> IO ()
printDadosPessoa (Pessoa nome idade rua nCasa cidade) = putStrLn $ "Nome: " ++ nome ++ ", Idade: " ++ idade ++ ", Endereço: " ++ rua ++ ", Número: " ++ nCasa ++ ", Cidade: " ++ cidade

addPessoa :: [DadosPessoa] -> IO [DadosPessoa]
addPessoa dados = do
  putStrLn "\n-------ADICIONAR-------"
  putStrLn "Digite o NOME"
  nome <- getLine
  putStrLn "Digite a IDADE"
  idade <- getLine
  putStrLn "Digite a RUA"
  rua <- getLine
  putStrLn "Digite o NUMERO DA CASA"
  nCasa <- getLine
  putStrLn "Digite a CIDADE"
  cidade <- getLine
  let novaPessoa = Pessoa nome idade rua nCasa cidade
      novoBancoDados = sort (novaPessoa : dados)
  return novoBancoDados

verificaPessoaCadastrada :: String -> DadosPessoa -> Bool
verificaPessoaCadastrada _ Vazio = False
verificaPessoaCadastrada nome (Pessoa n _ _ _ _) = n == nome

pesquisarPessoaNome :: String -> [DadosPessoa] -> IO ()
pesquisarPessoaNome nome pessoas = do
  let pessoasEncontradas = filter (verificaPessoaCadastrada nome) pessoas
  if null pessoasEncontradas
    then putStrLn "PESSOA NAO ENCONTRADA"
    else do
      putStrLn "\n--------PESSOA ENCONTRADA--------"
      mapM_ printDadosPessoa pessoasEncontradas

verificaCidadeCadastrada :: String -> [DadosPessoa] -> Bool
verificaCidadeCadastrada cidade pessoas = any (\(Pessoa _ _ _ _ cidadePessoa) -> cidade == cidadePessoa) pessoas

totalPessoasCidade :: String -> [DadosPessoa] -> IO Int
totalPessoasCidade cidade pessoas
  | verificaCidadeCadastrada cidade pessoas = do
      let pessoasDaCidade = filter (\(Pessoa _ _ _ _ cidade') -> cidade == cidade') pessoas
      return (length pessoasDaCidade)
  | otherwise = do
      putStrLn "CIDADE NÃO ENCONTRADA"
      return 0

obterIdades :: [DadosPessoa] -> [Int]
obterIdades [] = []
obterIdades (Pessoa _ idade _ _ _ : rest) = read idade : obterIdades rest
obterIdades (_ : rest) = obterIdades rest

mediaIdadePopulacao :: [DadosPessoa] -> Int
mediaIdadePopulacao pessoas =
  let idades = [read idade :: Int | Pessoa _ idade _ _ _ <- pessoas]
      somaIdades = sum idades
      qtdPessoas = length idades
      media = fromIntegral somaIdades / fromIntegral qtdPessoas
   in round media

editarPessoa :: String -> [DadosPessoa] -> IO [DadosPessoa]
editarPessoa nomePessoa dados = do
  let pessoasEncontradas = filter (verificaPessoaCadastrada nomePessoa) dados
  if null pessoasEncontradas
    then do
      putStrLn "PESSOA NAO ENCONTRADA"
      return dados
    else do
      putStrLn "\n--------PESSOA ENCONTRADA--------"
      mapM_ printDadosPessoa pessoasEncontradas
      putStrLn "Digite o NOVO NOME"
      novoNome <- getLine
      putStrLn "Digite a NOVA IDADE"
      novaIdade <- getLine
      putStrLn "Digite a NOVA RUA"
      novaRua <- getLine
      putStrLn "Digite o NOVO NÚMERO DA CASA"
      novoNCasa <- getLine
      putStrLn "Digite a NOVA CIDADE"
      novaCidade <- getLine
      let pessoaAtualizada = Pessoa novoNome novaIdade novaRua novoNCasa novaCidade
          novosDados = map (\p -> if verificaPessoaCadastrada nomePessoa p then pessoaAtualizada else p) dados
      putStrLn "Dados atualizados com sucesso!"
      return novosDados

menu :: [DadosPessoa] -> IO ()
menu dados = do
  putStrLn "\n--------MENU--------"
  putStrLn "Digite 1 para INSERIR pessoa"
  putStrLn "Digite 2 para PESQUISAR pessoa"
  putStrLn "Digite 3 para ATUALIZAR pessoa"
  putStrLn "Digite 4 para mostrar RELATORIO cidade"
  putStrLn "Digite 5 para mostrar MEDIA DE IDADE da populacao"
  putStrLn "Digite 0 para sair"
  putStr "Opção: "
  opt <- getChar
  getChar -- descarta o Enter
  case opt of
    '1' -> do
      db <- addPessoa dados
      putStrLn "------------PESSOA CADASTRADA COM SUCESSO------------"
      menu db
    '2' -> do
      putStrLn "NOME da pessoa que deseja pesquisar"
      nome <- getLine
      pesquisarPessoaNome nome dados
      menu dados
    '3' -> do
      putStrLn "NOME da pessoa que deseja editar"
      nome <- getLine
      db <- editarPessoa nome dados
      menu db
    '4' -> do
      putStrLn "Nome da cidade que deseja pesquisar"
      cidade <- getLine
      qtdPessoas <- totalPessoasCidade cidade dados
      putStrLn $ "Quantidade de pessoas na cidade: " ++ show qtdPessoas
      menu dados
    '5' -> do
      let media = mediaIdadePopulacao dados
      putStrLn $ "Média de idade da população: " ++ show media
    '0' -> putStrLn "\n--------FIM--------"
    _ -> do
      putStrLn "\nOpção inválida!"
      menu dados

main :: IO ()
main = do
  menu []
  return ()
