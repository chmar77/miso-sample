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
