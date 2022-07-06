# URL Shortener

URL Shortener in Haskell

## Quick Start

1. Run `cabal run` inside `url-shortener` directory, **or**
     Run `cabal repl` inside `url-shortener` directory, then execute `main`
    function.
2. Open your browser then go to `http://localhost:3000`. It is gives `param uri
   not found`, then your shortener is running.
3. To shorten a URL, go to `http://localhost:3000/?uri=<target url>` where
   `target url` is the url you want to shorten.
    * Example: `http://localhost:3000/?uri=http://riz.maulana.me`
4. After that, you should receive the shortened version of `target url`
    * Example: `http://localhost:3000/TRBB22` 
    * This shortened result will redirect you to a page that contained
      `http://riz.maulana.me`


