

----------------------------------------------------------------------------------------------------------------------------------------------------------------

--Following is a grammar.

-- sentence ::= noun verb noun | sentence and sentence         (R1) | R2)
-- noun ::= dogs | teeth                                       (R3) | R4)
-- verb ::= have                                               (R5)

--This grammar can be used to generate many sentences --> Each sentence that confirms to this grammar can be represented as a syntax tree.
--For example. "dogs have teeth"  is an example sentence that can be formed from the grammar above and this sentence can be represented using a syntax tree.
--Note that this syntax tree that you write for this specific sentence can have a different order of derivation that is not represeted (and thus discernible) from the
--syntax tree itself.

--We can use Haskell as a tool to use this grammar to generate different sentences that are possible to generate using this grammar.
--However, to be able to do that, (since this grammar does not exist in Haskell), you have to create data types in Haskell that correspond to each production rule
--in this grammar.

--Algortihm to convert any grammar to Haskell data type:
--1.Define a data type for each non-terminal.
--2.Define a constructor for each rule.

--Lets define data types for the grammar above.

data Sentence = Phrase Noun Verb Noun | And Sentence Sentence -- deriving Show
data Noun = Dogs | Teeth deriving Show                                               --Here "Dogs" represents "dogs" in the grammar above and does not refer itself to real animal.
data Verb = Have  deriving Show                                                      --You could have used "Cats" to represent "dogs" in the grammar and it would still be fine.

--Lets generate some example sentences that confirm to the grammar above but use Haskell type definition we just defined above as a tool for this matter.

s1 :: Sentence
s1 = Phrase Dogs Have Teeth

s2 :: Sentence
s2 = Phrase Dogs Have Dogs

s3 :: Sentence
s3 = And s1 s2


------ASIDE : Pretty Printing a Sentence data type-----------------------

instance Show Sentence where
    show (Phrase n v o) = show n++" "++show v++" "++show o
    show (And s1 s2) = show s1 ++ " and " ++ show s2



----------------------------------------------------------------------------------------------------------------------------------------------------------------

--Pretty Printer --> Allows us to print data types in a pretty format instead of the abstract data type comstrubctor names that we use.

data BExp = T | F | Not BExp | Or BExp BExp --deriving Show -- commenting this Show here since we have custom-defined this show in the pretty printer below.

nnt :: BExp
nnt = Not (Not T)  --This T does not mean fundamental boolean value T but rather T constructor of the data type BExp defined above. Same for Not.

tonnt :: BExp
tonnt = Or T nnt

-------------------------------------------------------------------------------------------------------------------------------------------------------
--We can print the values of dofferent boolean expressions that we create using BExp data type above. However, this would not be pretty to look.
--For example,  if you input a BExp value (e.g., nnt) in ghci (after uncommenting "deriving Show" of BExp data type), it returns  Not (Not T)).
--Note that this Not is not a boolean not but just a name for the data type constructor that we provided.

--Without a pretty printer, we are thus printing sth that does not have its fundamental meaning. Using a pretty printer allows us to change this.
--(Dont forget to comment out "deriving Show" when using your own custom Pretty Printer.)

-- --A Simple Version of Pretty Printer
pretty_printer :: BExp -> String
pretty_printer T = "T"
pretty_printer F = "F"
pretty_printer (Not bool_exp) = "not (" ++ pretty_printer bool_exp ++ ")"
pretty_printer (Or bexp1 bexp2) = "(" ++pretty_printer bexp1 ++ ")" ++ " or "++ "(" ++pretty_printer bexp2 ++ ")"


-- -- --After you write this pretty printer, you need to install this pretty printer as below:
instance Show BExp where
    show = pretty_printer


--Now typing nnt in ghci returns not(not(T))
--A complicated version of pretty printer