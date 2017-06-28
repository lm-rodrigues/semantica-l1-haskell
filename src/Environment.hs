module Environment where


type Environment a b = [(a,b)]


emptyEnv :: (Eq a) => Environment a b
emptyEnv =
    []


insertEnv :: (Eq a) => a -> b -> Environment a b -> Environment a b
insertEnv key value environment =
    ( key, value ) : environment


envLookup :: (Eq a) => a -> Environment a b -> b
envLookup searchKey environment =
    case environment of
        ( key, value ) : xs ->
            if searchKey == key then
                value
            else
                envLookup searchKey xs

        [] ->
            error "key not defined"
