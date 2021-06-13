{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List
import Debug.Trace

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}

data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target


{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)
	

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}

data Cell = Hunter | Targets{target :: Target}| Obstacole | Gateway{position1 :: Position, position2 :: Position, hasTarget :: Int, hasHunter :: Int} | Blank deriving(Eq, Ord)


instance Show Cell
    where 
		show Hunter = "!"
		show (Targets _) = "*"
		show Obstacole = "@"
		show (Gateway _ _ 0 0) = "#"
		show (Gateway _ _ 1 0) = "*"
		show (Gateway _ _ _ 1) = "!"
		show Blank = " "
		
		
data Game = Game { numberOfRows :: Int 
                 , numberOfColumns :: Int
				 , targets :: [Target]
				 , movedTargets :: [Target]
				 , hunter :: (Position, Cell)
				 , gameSetup :: [[(Position, Cell)]]
				 } deriving (Eq, Ord)
				

{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}

showCell :: (Position, Cell) -> String 
showCell (pos, cell) = show (snd (pos, cell))

gameAsString :: Game -> String
gameAsString game = intercalate "\n" stringLines
    where stringLines = (map (\x -> concatMap showCell x) gameLines)
	    where gameLines = gameSetup game


instance Show Game where
    show = gameAsString

{-
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}

createCell :: Position -> Int -> Int -> (Position, Cell)
createCell (x,y) rows columns
    | x == 0 || x == rows - 1 || y == 0 || y == columns - 1 = ((x,y), Obstacole)
	| x == 1 && y == 1 = ((x,y), Hunter)
	| otherwise = ((x,y) , Blank)


emptyGame :: Int -> Int -> Game
emptyGame rows columns = Game { numberOfRows = rows
							   , numberOfColumns = columns
							   , targets = []
							   , movedTargets = []
							   , hunter = ((1,1), Hunter)
							   , gameSetup = [[ (createCell (r,c) rows columns) | c <- [0..columns - 1]] | r <- [0..rows - 1]]
							   }


{- 
  Verifică daca o pozitie data este valida 
-}
isValid :: Position -> Int -> Int -> (Position, Cell) -> Bool
isValid pos rows columns ((x,y), cell)
    | pos == (x,y) && x >= 0  && x <= rows - 1 && y >= 0 && y <= columns - 1 && cell == Blank = True
	| pos == (x,y) && x >= 0  && x <= rows - 1 && y >= 0 && y <= columns - 1 && show cell == "#" = True
	| pos == (x,y) && x >= 0  && x <= rows - 1 && y >= 0 && y <= columns - 1 && show cell == "*" = True
	| otherwise = False
	
{-
  Verifică exitsa in lista o vreo pozitie valida
-}
isValidAll :: Game -> Position -> Int -> Int -> Bool
isValidAll game pos rows columns = any (True ==)  (concatMap (\x -> x) (map (\x -> map (isValid pos rows columns) x) (gameSetup game)))

updateCellHunter :: Game -> Position -> Int -> Int -> (Position, Cell) -> (Position, Cell)
updateCellHunter game pos rows columns (p, cell)
    | p == (1,1) && cell == Hunter && (isValidAll game pos rows columns) == True = (p, Blank)
    | (isValid pos rows columns (p, cell)) == True && cell == Blank = (p, Hunter)
	| (isValid pos rows columns (p, cell)) == True && (show cell) == "#" = (p, Gateway{ position1 = position1 cell, position2 = position2 cell, hasTarget = hasTarget cell, hasHunter = 1})
	| otherwise = (p, cell)

{-
    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}	
addHunter :: Position -> Game -> Game
addHunter pos game = game { hunter = (pos, Hunter)
                          , gameSetup = map (\x -> x) (map (\x -> map (updateCellHunter game pos (numberOfRows game) (numberOfColumns game)) x) (gameSetup game))				
						  }

{-
    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}

updateCellTarget :: Game -> Position -> Behavior -> Int -> Int -> (Position, Cell) -> (Position, Cell)
updateCellTarget game pos bhv rows columns (p, cell)
    | (isValid pos rows columns (p, cell)) == True && (show cell) == "#" = (p, Gateway{ position1 = (position1 cell), position2 = (position2 cell), hasTarget = 1 , hasHunter = hasHunter cell})
    | (isValid pos rows columns (p, cell)) == True && (show cell) == " " = (p, Targets{target = Target{ position = p, behavior = bhv}})
	| otherwise = (p, cell)
	
	
addTarget :: Behavior -> Position -> Game -> Game
addTarget behv (x,y) game 
    |x >= 0 && x <= (numberOfRows game) - 1 && y >= 0 && y <= (numberOfColumns game) - 1 && (snd ((gameSetup game) !! x !! y)) == Blank = game { targets = (targets game) ++ [Target{position = (x, y), behavior = behv}]
	                                                                                                                                           ,  gameSetup = map (\x -> x) (map (\t-> map (updateCellTarget game (x,y) behv (numberOfRows game) (numberOfColumns game)) t) (gameSetup game))		
																																			   }
	|x >= 0 && x <= (numberOfRows game) - 1 && y >= 0 && y <= (numberOfColumns game) - 1 && (show (snd ((gameSetup game) !! x !! y))) == "#" = game { targets = (targets game) ++ [Target{position = (position2 (snd ((gameSetup game) !! x !! y))), behavior = behv}]
																								                                                    , gameSetup = map (\x -> x) (map (\t-> map (updateCellTarget game (position2 (snd ((gameSetup game) !! x !! y))) behv (numberOfRows game) (numberOfColumns game)) t) (gameSetup game))				
																								                                                    }  
	| otherwise = game 
{-
    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}

updateCellGateway :: Game -> Position -> Position -> Int -> Int -> (Position, Cell) -> (Position, Cell)
updateCellGateway game pos1 pos2 rows columns (p, cell) 
	| p == pos1 = (p, Gateway{position1 = pos1, position2 = pos2, hasTarget = 0, hasHunter = 0})
	| p == pos2 = (p, Gateway{position1 = pos2, position2 = pos1, hasTarget = 0, hasHunter = 0})
	| otherwise = (p, cell)
	
addGateway :: (Position, Position) -> Game -> Game
addGateway (pos1, pos2) game = game { gameSetup = map (\x -> x) (map (\x -> map (updateCellGateway game pos1 pos2 (numberOfRows game) (numberOfColumns game)) x) (gameSetup game))}


{-
    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}

updateCellObstacle :: Game -> Position -> Int -> Int -> (Position, Cell) -> (Position, Cell)
updateCellObstacle game pos rows columns (p, cell)
    | (isValid pos rows columns (p, cell)) == True = (p, Obstacole)
	| otherwise = (p, cell)
	
addObstacle :: Position -> Game -> Game
addObstacle pos game = game {gameSetup = map (\x -> x) (map (\x -> map (updateCellObstacle game pos (numberOfRows game) (numberOfColumns game)) x) (gameSetup game))}

{-    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
	
attemptMove :: Position -> Game -> Maybe Position
attemptMove pos game 
    | (snd ((gameSetup game) !! (fst pos) !! (snd pos))) == Blank = Just pos
	| (snd ((gameSetup game) !! (fst pos) !! (snd pos))) == (Gateway pos (position2 (snd ((gameSetup game) !! (fst pos) !! (snd pos)))) 0 (hasHunter (snd ((gameSetup game) !! (fst pos) !! (snd pos))))) = Just (position2 (snd ((gameSetup game) !! (fst pos) !! (snd pos))))
	| (snd ((gameSetup game) !! (fst pos) !! (snd pos))) == (Gateway pos (position2 (snd ((gameSetup game) !! (fst pos) !! (snd pos)))) 1 (hasHunter (snd ((gameSetup game) !! (fst pos) !! (snd pos))))) = Just (position2 (snd ((gameSetup game) !! (fst pos) !! (snd pos))))
	| otherwise = Nothing

showJust (Just a) = a
{-
    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}
goEast :: Behavior
goEast (x, y) game 
	| (attemptMove (x, y + 1) game) /= Nothing = Target { position = (x, y + 1)
													    , behavior = goEast
													    }	
    | otherwise = Target { position = (x, y)
						 , behavior = goEast
						 }											  

{-
    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest (x, y) game 
	| (attemptMove (x, y - 1) game) /= Nothing = Target { position = (x, y - 1)
														, behavior = goWest
														}	
    | otherwise = Target { position = (x, y)
						 , behavior = goWest
						 }	

{-
    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth (x, y) game
    | (attemptMove (x - 1, y) game) /= Nothing = Target { position = (x - 1, y)
													    , behavior = goNorth
													    }	
    | otherwise = Target { position = (x, y)
						 , behavior = goNorth
						 }	

{-
    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth (x, y) game
	| (attemptMove (x + 1, y) game) /= Nothing = Target { position = (x + 1, y)
														, behavior = goSouth
														}	
    | otherwise = Target { position = (x, y)
						 , behavior = goSouth
						 }

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
bounceSouth :: Behavior
bounceSouth (x, y) game
   | (attemptMove(x + 1, y) game) == Nothing = Target {position =  (x - 1, y), behavior = (bounce (-1))}
   | otherwise  = Target {position = (x + 1, y), behavior = (bounce 1)}

bounceNorth :: Behavior
bounceNorth (x, y) game
   | (attemptMove(x - 1, y) game) == Nothing = Target {position =  (x + 1, y) , behavior = (bounce 1)}
   | otherwise  = Target {position = (x - 1, y), behavior = (bounce (-1))}   

bounce :: Int -> Behavior
bounce go (x, y) game
	| go == 1 = (bounceSouth (x, y) game)
	| go == -1 = (bounceNorth (x, y) game)

{-
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}  
removeTarget :: (Position, Cell) -> (Position, Cell)
removeTarget (p, cell) 
   | show cell == "*"  && cell == Gateway {position1 = (position1 cell) , position2 = (position2 cell), hasTarget = (hasTarget cell), hasHunter = (hasHunter cell)}  = (p, Gateway {position1 = (position1 cell) , position2 = (position2 cell), hasTarget = 0, hasHunter = (hasHunter cell)})
   | show cell == "*" = (p, Blank)
   | otherwise = (p, cell)
 
removeAllTargets :: Game -> Game
removeAllTargets game = game { gameSetup = map (\x -> x) (map (\x -> map removeTarget x) (gameSetup game))}
							 
							 
addMovedTarget :: Behavior -> Position -> Position -> Game -> Game
addMovedTarget behv currentPosition nextPosition game = game { movedTargets = (movedTargets game) ++ [Target{position = (showJust (attemptMove nextPosition game)) , behavior = (behavior (behv (showJust (attemptMove currentPosition game))  game))}]
															 , gameSetup = map (\t-> map (updateCellTarget game (showJust (attemptMove nextPosition game))  behv (numberOfRows game) (numberOfColumns game)) t) (gameSetup game)
													         }
															 
moveAllTargets :: Game -> Game
moveAllTargets game
    | targets game == [] = game { targets = movedTargets game
								, movedTargets = []
								, gameSetup = gameSetup game
								}
    | otherwise = moveAllTargets game { targets = (tail (targets game))
								      , movedTargets = movedTargets (addMovedTarget (behavior (head (targets game))) (position (head (targets game))) (position ((behavior (head (targets game))) (position (head (targets game))) game)) game)
									  , gameSetup = gameSetup (addMovedTarget (behavior (head (targets game))) (position (head (targets game))) (position ((behavior (head (targets game))) (position (head (targets game))) game)) game)
				                      }
								   
						
moveTargets :: Game -> Game
moveTargets game = moveAllTargets game { gameSetup = gameSetup (removeAllTargets game)}

{-
    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled (x,y) target
    | x == fst(position target) && y == snd(position target) + 1 = True
	| x == fst(position target) && y == snd(position target) - 1 = True
	| x == fst(position target) + 1 && y == snd(position target) = True
	| x == fst(position target) - 1 && y == snd(position target) = True
	| otherwise = False
	
{-
    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}

removeHunterCell :: Position -> (Position, Cell) -> (Position, Cell)
removeHunterCell pos (p, cell)
    | pos == p && cell == Hunter = (p, Blank)
	| pos == p && cell == Gateway { position1 = position1 cell, position2 = position2 cell, hasTarget = hasTarget cell, hasHunter = hasHunter cell} = (p, Gateway { position1 = position1 cell, position2 = position2 cell, hasTarget = hasTarget cell, hasHunter = 0})
	| otherwise = (p, cell)
	
removeHunter :: Game -> Game
removeHunter game = Game { numberOfRows = numberOfRows game
                          , numberOfColumns = numberOfColumns game
						  , targets = targets game
						  , movedTargets = movedTargets game
						  , hunter = ((1,1), Hunter)
                          , gameSetup = map (\x -> x) (map (\x -> map (removeHunterCell (fst (hunter game))) x) (gameSetup game))		
                          }

moveHunter :: Game -> Direction -> Position -> Game
moveHunter game direction (x, y)
    | direction == South && (attemptMove (x + 1, y) (removeHunter game)) /= Nothing = addHunter (showJust (attemptMove (x + 1, y) (removeHunter game))) (removeHunter game)
	| direction == North && (attemptMove (x - 1, y) (removeHunter game)) /= Nothing = addHunter (showJust (attemptMove (x - 1, y) (removeHunter game))) (removeHunter game)
	| direction == East && (attemptMove (x, y + 1) (removeHunter game)) /= Nothing = addHunter (showJust (attemptMove (x, y + 1) (removeHunter game))) (removeHunter game) 
	| direction == West && (attemptMove (x, y - 1) (removeHunter game)) /= Nothing = addHunter (showJust (attemptMove (x, y - 1) (removeHunter game))) (removeHunter game)
	| otherwise = addHunter (x, y) (removeHunter game)
	
killTarget :: Position -> Target -> (Position, Cell) -> (Position, Cell)
killTarget hunterPosition targ (p, cell)
    | cell == (Targets targ) && (isTargetKilled hunterPosition targ) == True = (p, Blank)
	| position targ == p && cell == Gateway { position1 = position1 cell, position2 = position2 cell, hasTarget = 1, hasHunter = hasHunter cell} && (isTargetKilled hunterPosition targ) == True = (p, Gateway { position1 = position1 cell, position2 = position2 cell, hasTarget = 0, hasHunter = hasHunter cell})
	| otherwise = (p, cell)

killTargets :: Game -> Game
killTargets game 
	| targets game == [] = game { targets = movedTargets game
								, movedTargets = []
								}
    | otherwise = killTargets game { targets = tail (targets game) 
								   , movedTargets = if (isTargetKilled (fst (hunter game)) (head (targets game))) == True then (movedTargets game) else ((movedTargets game) ++ [head (targets game)])
				                   , gameSetup =  map (\x -> x) (map (\x -> map (killTarget (fst (hunter game)) (head (targets game))) x) (gameSetup game))	
				                   }
									  

advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState direction move game 
    | move == False = moveHunter game direction (fst (hunter game))
	| otherwise = (killTargets (moveTargets (killTargets (moveHunter game direction (fst (hunter game))))))


{-
    Verifică dacă mai există Target-uri pe table de joc.
-}

isTarget :: (Position, Cell) -> Bool
isTarget (p, cell)
    | show cell == "*" = True
	| otherwise = False
    
areTargetsLeft :: Game -> Bool
areTargetsLeft game = any (True ==) (concatMap (\x -> x) (map (\x -> map isTarget x) (gameSetup game)))

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined


instance ProblemState Game Direction where
    {-
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = directions 
       where 
        southGame = (South, advanceGameState South False game)
        northGame = (North, advanceGameState North False game)
        eastGame = (East, advanceGameState East False game)
        westGame = (West, advanceGameState West False game)
        directions = [southGame, northGame, eastGame, westGame]		

    {-
        Verifică dacă starea curentă este una în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
	
    isGoal game = any (True ==) (map (\x -> (isTargetKilled (fst (hunter game)) x)) (targets game))

    {-
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h game = if filt == [] then 0.0 else hEuclidean (fst (hunter game)) (position (head filt)) 
	    where 
	       filt = (filter (\x -> (isTargetKilled (fst (hunter game)) x) == True)  (targets game))

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined



