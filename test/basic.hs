{-# LANGUAGE TypeFamilies #-}
import CES.Data
import CES.Monad

data Comp1 = Comp1 Int deriving (Show)
data Comp2 = Comp2 String deriving (Show)

newtype Comp1Ty = Comp1Ty () deriving (Show)
instance ComponentPhantom Comp1Ty where
    type ComponentType Comp1Ty = Comp1
    componentPhantomInt _ = 10
    componentName _ = "Comp1" 

newtype Comp2Ty = Comp2Ty () deriving (Show)
instance ComponentPhantom Comp2Ty where
    type ComponentType Comp2Ty = Comp2
    componentPhantomInt _ = 20
    componentName _ = "Comp2" 



main = do
    let c1 = Comp1Ty ()
        c2 = Comp2Ty ()
    c <- execCEST (do
        i1 <- newEntity
        i2 <- newEntity
        i3 <- newEntity
        addComponent c1 i1 (Comp1 1)
        addComponent c1 i2 (Comp1 2)
        addComponent c1 i3 (Comp1 3)
        addComponent c2 i1 (Comp2 "Bacon")
        addComponent c2 i3 (Comp2 "Eggs")
        tot <- foldEntities c1 0 $ \o k (Comp1 n) -> do
            lift $ putStrLn (show (k, n))
            return (o+n)
        lift $ putStrLn (show tot)
        onEntities c2 $ \k (Comp2 n) -> do
            lift $ putStrLn (show (k, n))
        i4 <- newEntity
        addComponent c1 i4 (Comp1 999)
        remComponent c1 i4
        ) defCES
    putStrLn (show c)

