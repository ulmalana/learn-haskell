# Chapter 13 - Building Projects

* Cabal (Common Architecture fo Building Applications and Libraries): Haskell
  package and dependency manager
* Haskell programs are organized in modules that can be exported or imported.
* **Cabal sandbox** is a **deprecated feature**

## Importing modules

* Importing a whole module: `import Data.Bool`
* Selective import: `import Data.Bool (bool, not)`
* Qualified import: `import Data.Bool as B`
