sbt frontend/fullLinkJS
yarn exec vite -- build
cp dist/index.html dist/200.html
surge ./dist 'vinsan.surge.sh'
