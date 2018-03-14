# sample-project

to run in browser 

`stack exec ghcjs -- --interactive`

:r reload
main to run main function

To pass field constructor as argument, must used lens

example field `_title` will have a function `title` with type 
`(fieldType -> Lens.Identity fieldType) -> record -> Lens.Identity record)`

in update
```
Input func s ->
  noEff model {_todoInput = newTodoInput}
    where
      -- func = _tiTitle
      newTodoInput = Lens.set func (Entering s) (_todoInput model) 
```



# Old 

follow miso tutorial

run python server in the build file

~/projects/haskell/miso/app/.stack-work/dist/x86_64-linux/Cabal-1.24.0.0_ghcjs/build/app/app.jsexe$

python -m SimpleHTTPServer