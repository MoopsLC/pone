module Pone.Option where 

data Option a = Some a | None

instance Show a => Show (Option a) where
    show None = "None"
    show (Some x) = "Some " ++ (show x)