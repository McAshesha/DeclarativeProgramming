module Graph where
import Text.ParserCombinators.Parsec
import Data.List (nub)

data Graph = Digraph ID [Node] [Edge] [Attr]
        deriving (Show)
data Node = Node ID [Attr]
        deriving (Show)
data Edge = Edge ID ID [Attr]
        deriving (Show)
data Attr = Attr ID ID
        deriving (Show)
type ID = String

symbol :: String -> Parser String
symbol s = string s <* mySpaces

mySpaces :: Parser ()
mySpaces = skipMany (space <|> tab <|> newline)

identifier :: Parser String
identifier = many1 alphaNum <* mySpaces

type StmtResult = ([Attr], [(ID, [Attr])], [Edge], [ID])

graph :: Parser Graph
graph = do
    symbol "digraph"
    graphName <- identifier
    symbol "{"
    (gAttrs, nodeAttrs, edges, edgeNodeIds) <- stmt_list
    symbol "}"
    let allNodeIds = nub (map fst nodeAttrs ++ edgeNodeIds)
        nodes = [ Node nodeId (maybe [] id (lookup nodeId nodeAttrs)) | nodeId <- allNodeIds ]
    return $ Digraph graphName nodes edges gAttrs

stmt_list :: Parser StmtResult
stmt_list = do
    stmts <- sepEndBy stmt (optional (symbol ";"))
    let gas = concatMap (\(g, _, _, _) -> g) stmts
        nas = concatMap (\(_, n, _, _) -> n) stmts
        eas = concatMap (\(_, _, e, _) -> e) stmts
        enis = concatMap (\(_, _, _, eni) -> eni) stmts
    return (gas, nas, eas, enis)

stmt :: Parser StmtResult
stmt = choice [ try edge_stmt, try node_stmt, try attr_stmt ]

attr_stmt :: Parser StmtResult
attr_stmt = do
    id1 <- identifier
    symbol "="
    id2 <- identifier
    return ([Attr id1 id2], [], [], [])

node_stmt :: Parser StmtResult
node_stmt = do
    nid <- identifier
    attrs <- option [] attr_list
    return ([], [(nid, attrs)], [], [])

edge_stmt :: Parser StmtResult
edge_stmt = do
    from <- identifier
    to_ids <- edgeRHS
    attrs <- option [] attr_list
    let edges = zipWith (\src tgt -> Edge src tgt attrs) (from : to_ids) to_ids
        node_ids = from : to_ids
    return ([], [], edges, node_ids)

edgeRHS :: Parser [ID]
edgeRHS = many1 edgeop_node

edgeop_node :: Parser ID
edgeop_node = do
    symbol "->"
    identifier

attr_list :: Parser [Attr]
attr_list = between (symbol "[") (symbol "]") (sepBy attr_pair (choice [symbol ";", symbol ","]))

attr_pair :: Parser Attr
attr_pair = do
    id1 <- identifier
    symbol "="
    id2 <- identifier
    return $ Attr id1 id2

main :: IO ()
main = do
    text <- readFile "graph.txt"
    let result = parse graph "" text
    case result of
        Left e -> print e
        Right g -> do
            print "Read graph:"
            print g