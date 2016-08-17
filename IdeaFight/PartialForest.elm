-- a partially ordered forest
module IdeaFight.PartialForest exposing (Forest, empty, fromList)

type Forest a = Forest (List a)

fromList : List a -> Forest a
fromList values = Forest values

empty : Forest a
empty = fromList []
