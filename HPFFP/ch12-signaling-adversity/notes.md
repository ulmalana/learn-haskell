# Chapter 12 - Signaling Adversity

This chapter is about how to deal with "adverse" arguments/return values that
is quite difficult to handle in other languages.

## Maybe type

* Dealing with handling the possibility of "Nothingness"
* `data Maybe a = Nothing | Just a`
* **Smart constructor**: construct the type when the values meet the criteria.
    * ```
        type Name = String
        type Age = Int
        data Person = Person Name Age deriving Show

        -- mkPerson is smart constructor
        mkPerson :: Name -> Age -> Maybe Person
        mkPerson name age
            | name /= "" && age >= 0 = Just $ Person name age
            | otherwise = Nothing
      ```
## Either type

* `Maybe` type above only handles the input, but doesnt give the reason when we
  give inappropiate inputs.
* `Either` type can be used to return **either** the correct return value or
  the error from processing inputs.
* `data Either a b = Left a | Right b`
* Example:
    * ```
        type Name = String
        type Age = Integer
        data Person = Person Name Age deriving Show

        data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

        mkPerson :: Name -> Age -> Either PersonInvalid Person 
        mkPerson name age
            | name /= "" && age >= 0 = Right $ Person name age
            | name == "" = Left NameEmpty
            | otherwise = Left AgeTooLow
      ```
* The example above only returns the first error (`NameEmpty`) when both
  arguments are invalid. To fix this, we can check the arguments separately and
combine the results.
    * ```
        type Name = String
        type Age = Integer
        type ValidatePerson a = Either [PersonInvalid] a

        data Person = Person Name Age deriving Show

        data PersonInvalid = EmptyName | AgeTooLow deriving (Eq, Show)

        -- the checking functions
        ageOK :: Age -> Either [PersonInvalid] Age
        ageOK age = case age >= 0 of
            True -> Right age
            False -> Left [AgeTooLow]

        nameOK :: Name -> Either [PersonInvalid] Name
        nameOK name = case name /= "" of
            True -> Right name
            False -> Left [EmptyName]

        -- the constructor
        mkPerson :: Name -> Age -> ValidatePerson Person
        mkPerson name age = mkPerson' (nameOK name) (ageOK age)

        mkPerson' :: Validate Name -> Validate Age -> Validate Person
        mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
        mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
        mkPerson' (Left badName) _ = Left badName
        mkPerson' _ (Left badAge) = Left badAge
      ```

* We can make a shorter version of `mkPerson` above with lifting
    * ```
        mkPerson :: Name -> Age -> ValidatePerson Person
        mkPerson name age = 
            liftA2 Person (nameOK name) (ageOK age)
      ```

## Kinds

* `Maybe` type: **\* -> \*** (takes one type as argument)
* Tuple `(,)` type: **\* -> \* -> \*** (takes two types as argument)
* `Either` type: **\* -> \* -> \*** (takes two types as argument)

### Lifted and unlifted types

* kind **\*** is the kind of all standard lifted types
* kind **\#** is for unlifted.
* standard lifted types (including user-defined) can be inhabited by bottom.
    * represented by a pointer
* unlifted types cant be inhabited by bottom
    * often native machine types and raw pointers
* special case: `newtype`
    * kind: **\***
    * but unlifted since its representation is identical to the type it
      contains
    * not creating any new pointer
    * so it can be inhabited by bottom

## Unfolds

* Folds or catamorphism: **destructuring the data**
* Unfolds or **anamorphism**: **build the data**
* Example: `iterate` and `unfoldr`
    * ```
        > :t iterate
        iterate :: (a -> a) -> a -> [a]
        > take 10 $ iterate (+1) 0
        [0,1,2,3,4,5,6,7,8,9]

        > :t unfoldr
        unfoldr :: (b -> Maybe (a,b)) -> b -> [a]
        > take 10 $ unfoldr (\b -> Just(b, b+1)) 0
        [0,1,2,3,4,5,6,7,8,9]
      ```
