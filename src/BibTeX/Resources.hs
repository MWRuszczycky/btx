module BibTeX.Resources
    ( getRef
    , supported
    ) where

---------------------------------------------------------------------
-- Standard references
---------------------------------------------------------------------

getRef :: String -> Maybe T.Ref
-- ^Return an empty refernce of the specified type with empty fields.
getRef t = lookup t supported

supported :: [ (String, T.Ref) ]
-- ^Associative list of all supported reference types.
supported = [ ( "article" , article )
            , ( "book" , book )
            , ( "incollection" , incollection )
            , ( "inbook" , inbook )
            , ( "phdthesis" , phdthesis )
            , ( "manual" , manual ) ]

article :: T.Ref
article = T.Ref
    { T.refType = "article"
    , T.refComments = []
    , T.refFields = [ ("author","")
                    , ("title","")
                    , ("year","")
                    , ("journal","")
                    , ("volume","")
                    , ("number","")
                    , ("pages","")
                    , ("month","")
                    , ("note","") ] }

book :: T.Ref
book = T.Ref
    { T.refType = "book"
    , T.refComments = []
    , T.refFields = [ ("author","")
                    , ("title","")
                    , ("publisher","")
                    , ("address","")
                    , ("year","")
                    , ("edition","")
                    , ("volume","")
                    , ("series","")
                    , ("pages","")
                    , ("number","")
                    , ("month","")
                    , ("note","") ] }

incollection :: T.Ref
incollection = T.Ref
    { T.refType = "incollection"
    , T.refComments = []
    , T.refFields = [ ("author","")
                    , ("editor","")
                    , ("title","")
                    , ("booktitle","")
                    , ("chapter","")
                    , ("pages","")
                    , ("publisher","")
                    , ("address","")
                    , ("year","")
                    , ("volume","")
                    , ("number","")
                    , ("series","")
                    , ("type","")
                    , ("edition","")
                    , ("month","")
                    , ("note","") ] }

inbook :: T.Ref
inbook = T.Ref
    { T.refType = "inbook"
    , T.refComments = []
    , T.refFields = [ ("author","")
                    , ("editor","")
                    , ("title","")
                    , ("chapter","")
                    , ("pages","")
                    , ("publisher","")
                    , ("address","")
                    , ("year","")
                    , ("volume","")
                    , ("number","")
                    , ("series","")
                    , ("type","")
                    , ("edition","")
                    , ("month","")
                    , ("note","") ] }

phdthesis :: T.Ref
phdthesis = T.Ref
    { T.refType = "phdthesis"
    , T.refComments = []
    , T.refFields = [ ("author","")
                    , ("title","")
                    , ("school","")
                    , ("year","")
                    , ("type","")
                    , ("address","")
                    , ("month","")
                    , ("note","") ] }

manual :: T.Ref
manual = T.Ref
    { T.refType = "manual"
    , T.refComments = []
    , T.refFields = [ ("author","")
                    , ("title","")
                    , ("publisher","")
                    , ("address","")
                    , ("year","") ] }
