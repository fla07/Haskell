# Haskell

[] -> ZERO ELEMENTOS
_ -> ZERO OU MAIS ELEMENTOS
xs -> ZERO OU MAIS ELEMENTOS
_:[] -> UM
x:[] -> UM
x:xs -> UM OU MAIS
x:_:[] -> DOIS
x:z:[] -> DOIS
x:_:xs-> DOIS OU MAIS
x:y:w:_ -> TRES OU MAIS
x:y:w:ws -> TRES OU MAIS

_:_:_:_:_:_:xs -> SEIS OU MAIS
_:_:d:_:[] -> 4 elementos


sudo apt-get update
sudo apt-get install -y software-properties-common
sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update
sudo apt-get install -y cabal-install-1.22 ghc-7.10.3
cat >> ~/.bashrc <<EOF
export PATH="\$HOME/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.3/bin:\$PATH"
EOF
export PATH=~/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.3/bin:$PATH
ghci


:! clear = limpa
:l nomeArquivo.hs = load 
:r = reload
ctrl + c = para a aplicação
# Haskell
