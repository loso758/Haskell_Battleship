-- Importiere die benoetigten Pakete

import Data.List

-- Deklaration des Datentyps 'Spalte'
-- da normale Buchstaben bereits zu Char gehoeren, werden sie hier durch X' unterschieden
-- diese Daten haben eine Groeßenordnung (<, >, ==) und koennen gelesen und ausgegeben werden
data Spalte = A' | B' | C' | D' | E'
  | F' | G' | H' | I' | J' deriving (Eq,Ord,Read,Show,Enum)



-- Benennung der im Skript verwendeten Typen

type Zeile = Int

type Koordinate = (Spalte, Zeile)

type Spieler = String

type Feld = [Koordinate]

-- gegnerische Schiffe und versuchte Koordinaten
type Zustand = ([Feld], Feld)



benenne :: IO [Spieler]
-- erfragt Spielernamen und gibt sie in einer Liste zurueck
-- verlangt 2 Inputs der Benutzer
-- gibt Liste vom Typ Spieler zurueck

benenne = do
          putStrLn "Wie heisst Spieler 1?"
          spieler1 <- getLine
          putStrLn "Wie heisst Spieler 2?"
          spieler2 <- getLine
          return [spieler1, spieler2]



sp :: Char -> Spalte
-- erkennt ein Char-Zeichen als Element vom Typ Spalte
-- Input: Char - Zeichen, das als Typ Spalte gelesen werden soll
-- Output: Spalte - Spaltenbezeichnung

sp eingabe = read ([eingabe] ++ "\'") :: Spalte



zl :: [Char] -> Zeile
-- erkennt eine Liste von Char-Zeichen als Element vom Typ Zeile
-- Input: Liste von Char - Zeichenkette, die als Zeile gelesen werden soll
-- Output: Zeile - Zeilenbezeichnung

zl eingabe = read eingabe :: Zeile



erkenneKoordinate :: String -> Koordinate
-- erkennt durch weiteren Funktionenaufruf einen String als Element vom Typ Koordinate
-- Input: String: eine eingegebene Koordinate (noch von Typ String)
-- Output: Koordinate: Koordinate vom richtigen Typ und passendem Format

-- Fallunterscheidung 1: eine Ziffer (1-9)
erkenneKoordinate [x, y] = (sp x, zl [y])

-- Fallunterscheidung 2: zwei Ziffern (10)
erkenneKoordinate [x, y_1, y_2] = (sp x, zl [y_1, y_2])



erkenneKoordinaten :: [String] -> Feld
-- erkennt durch weiteren Funktionenaufruf eine Liste von Strings als Element vom Typ Feld
-- Input: Liste von Strings - Liste der eingegebenen Koordinaten (String)
-- Output: Feld - Liste der erkannten Koordinaten

-- Fallunterscheidung 1: keine leere Liste
erkenneKoordinaten (x:xs) = [erkenneKoordinate x] ++ erkenneKoordinaten xs

-- Fallunterscheidung 2: leere Liste
erkenneKoordinaten [] = []



teile :: String -> [String]
-- erstellt aus einem String mit Stopper "," eine Liste von Strings
-- Input: String - gesamte gelesene Eingabe
-- Output: Liste von Strings - Eingabe durch "," nach Koordinaten getrennt

-- Fallunterscheidung 1: nicht leer
teile (x:xs) = if x == ',' then
                    [] : teile xs
               else
                    (x : head (teile xs)) : tail (teile xs)

-- Fallunterscheidung 2: leere Liste
teile [] = [[]]



setze_schiffe :: [Int] -> [Feld] -> IO [Feld]
-- fordert einen Spieler auf, alle seine Schiffe zu setzen
-- Input: Liste von Int - Laenge der zu setzenden Schiffe
-- Input: Liste von Feldern - Liste schon gesetzter Schiffe
-- Output: Liste von Feldern - aktualisierte Liste von Schiffen

setze_schiffe (x:xs) schiffe = do
    -- fordere ein Schiff der Laenge x an durch weiteren Funktionsaufruf und lese die Rueckgabe
    schiff <- setze_schiff ("Gib Koordinaten fuer Schiff der Laenge " ++ show x ++ " ein") x schiffe
    -- setze die restlichen Schiffe, solange nicht alle gesetzt wurden
    if null xs
        then return schiff
    else
        setze_schiffe xs schiff



setze_schiff :: String -> Int -> [Feld] -> IO [Feld]
-- fordert einen Spieler auf, ein Schiff mit gewisser Laenge zu setzen
-- Input: String - Anweisung fuer die Spieler (zur Ausgabe auf der Konsole)
-- Input: Int - Laenge des zu setzenden Schiffs
-- Input: Liste von Feldern - Liste der gesetzten Schiffe
-- Output: Liste von Feldern - aktualisierte Liste von Schiffen

setze_schiff x n schiffe = do
    -- ergrage ein schiff der Laenge x
    s <- fragen x n
    -- pruefe, ob das Schiff dort gesetzt werden kann
    if keineStapel schiffe s
        then return (schiffe ++ [s])
    -- sonst fordere eine neue Eingabe
    else setze_schiff "Hier liegt schon ein Schiff! Gib neue Koordinaten ein." n schiffe



fragen :: String -> Int -> IO Feld
-- fordert die Eingabe der Koordinaten fuer ein Schiff/ einen Schuss solange, bis die Koordinaten nebeneinander liegen
-- Input: String - Anweisung fuer die Spieler (zur Ausgabe auf der Konsole)
-- Input: Int - Laenge des zu setzenden Schiffs
-- Output: Feld - Liste der Koordinaten fuer ein Schiff

fragen x n = do
    -- hole die Eingabe
    eing <- lese x n
    -- lese die Eingabe als Koordindaten
    let s_1 = erkenneKoordinaten eing
    -- pruefe, dass die Koordinaten nebeneinander in einer Spalte oder Zeile liegen
    if nachbarZeile s_1 || nachbarSpalte s_1
        then return s_1
    -- sonst fordere eine neue Eingabe
    else fragen "Deine Koordinaten muessen in einer Zeile/Spalte nebeneinander liegen! Gib neue Koordinaten ein." n



lese :: String -> Int -> IO [String]
-- fordert die Eingabe der Koordinaten fuer ein Schiff/ einen Schuss solange, bis die Eingabe den Anforderungen an Koordinaten entspricht
-- Input: String - Anweisung fuer die Spieler (zur Ausgabe auf der Konsole)
-- Input: Int - Laenge des zu setzenden Schiffs
-- Output: Liste von Strings - Eingabe in zu erkennende Koordinaten geteilt

lese x n = do
    print x
    -- lese den Input
    input <- getLine
    -- teile den Input
    let liste = teile input
    -- pruefe, ob Eingaben in Liste echte Koordinaten sind
    if gueltig liste n
        then return liste
    -- sonst fordere eine neue Eingabe
    else lese "Die Eingabe war ungueltig, verwende Zahlen zwischen 1-10 und Buchstaben zwischen A-J und gebe die passende Anzahl an Koordinaten ein!" n



gueltig :: [String] -> Int -> Bool
-- ueberprueft, ob genuegend Koordinaten eingegeben wurden und ob die Eingabe den Anforderungen einer Koordinate erfuellt
-- Input: Liste von Strings - Liste der noch zu erkennenden Koordinaten
-- Input: Int - Laenge des zu setzenden Schiffs
-- Output: Bool - Wahrheitswert

gueltig (x:xs) n = and [length (x:xs) == n, pruefe (x:xs)]
    where pruefe [] = True
          pruefe (x:xs) = and [check x, pruefe xs]
             -- vergleiche Eingaben mit moeglichen Chars fuer Spalte/Zeile
             where check [x, y] = x `elem` ['A'..'J'] && y `elem` ['1'..'9']
                   check [x, y_1, y_2] = x `elem` ['A'..'J'] && [y_1,y_2] == ['1','0']
                   check [] = True
                   check a = False



nachbarZeile :: Feld -> Bool
-- stelle sicher, dass Eingaben nebeneinander in einer Zeile liegen
-- Input: Feld - Liste von Koordinaten eines Schiffes
-- Output: Bool - Wahrheitswert

-- Pattern-Matching ueber die ersten Beiden Listenelemente für Zeilen- u. Spaltenangaben
nachbarZeile ((c1, r1):(c2,r2):xs) = (c1, r1 + 1) == (c2, r2) && nachbarZeile ((c2,r2):xs)

-- fuer das letzte Element in der Liste
nachbarZeile [x] = True



nachbarSpalte :: Feld -> Bool
-- stelle sicher, dass Eingaben nebeneinander in einer Spalte liegen
-- Input: Feld - Liste von Koordinaten eines Schiffes
-- Output: Bool - Wahrheitswert

-- Pattern-Matching ueber die ersten Beiden Listenelemente für Zeilen- u. Spaltenangaben
nachbarSpalte (((c1, r1):(c2,r2):xs)) = (succ c1, r1) == (c2,r2) && nachbarSpalte ((c2,r2):xs)

-- fuer das letzte Element in der Liste
nachbarSpalte [x] = True



keineStapel:: [Feld] -> Feld -> Bool
-- ueberprueft, dass sich keine Schiffe ueberlappen
-- Input: Liste von Feldern - biher gesetzte Schiffe
-- Input: Feld - Liste von Koordinaten eines Schiffes
-- Output: Bool - Wahrheitswert

-- -- Fallunterscheidung 1: nicht leer
keineStapel schiffe (x:xs) = x  `notElem` (concat schiffe) && keineStapel schiffe xs

-- Fallunterscheidung 2: leere Liste
keineStapel schiffe [] = True


spielen :: [Spieler] -> [Zustand] -> IO()
-- alle Spielrunden
-- Input: Liste von Spielern
-- Input: Zustand - fuer jeden Spieler die gegnerischen Schiffe und die bereits geschossenen Koordinaten
-- Output: auf der Konsole

spielen [n_1, n_2] [z_1, z_2] = do
    -- Spieler anreden
    print (n_1 ++ " du bist dran!")
    -- Reaktion und neuen Zustand auslesen
    (erg, nz_1) <- zug n_1 z_1
    -- Reaktion ausgeben
    print erg
    -- weiteres Spielverlauf bestimmen
    if erg == "verfehlt!"
        then spielen [n_2, n_1] [z_2, nz_1]
    else if erg == "besiegt"
        then print (n_1 ++ ", du hast gewonnen!")
    else spielen [n_1, n_2] [nz_1, z_2]
    
    
    
zug :: Spieler -> Zustand -> IO (String, Zustand)
-- fordert einen Spieler zum Schiessen auf und reagiert entsprechend
-- Input: Spieler - wer gerade am Zug ist
-- Input: Zustand - gegnerische Schiffe und bereits geschossende Koordianten
-- Output: (String, Zustand) - geeignete Reaktion und aktualisierter Zustand

zug namen (schiffe, versuche) = do
    -- Zielkoordinate erfragen
    koord <- zielen "Wohin moechtest Du schiessen?" versuche
    -- Spielzustand aktualisieren
    let zustand = aktualisiere koord (schiffe, versuche)
    -- Reaktion bestimmen
    if treffen koord zustand
        then if besiegt zustand then return ("besiegt", zustand)
             else if versenkt koord zustand then return ("versenkt", zustand)
             else return ("getroffen", zustand)
    else return ("verfehlt!", zustand)



zielen :: String -> Feld -> IO Koordinate    
-- erfragt eine Zielkoordinate fuer einen Schuss bis die Koordinate noch kein Ziel war
-- Input: String - Anweisung fuer den Spieler (zur Ausgabe auf der Konsole)
-- Input: Feld - Liste der bereits geschossenen Koordinaten

zielen text versuche = do
    -- Koordinate erfragen
    koord <- fragen text 1 
    -- pruefen, ob neue Koordinate
    if (head koord) `notElem` versuche
        then return (head koord)
    -- sonst neue Koordinate anfordern
    else zielen "Hierhin hast Du schon einmal geschossen! Gib ein neues Ziel ein." versuche



treffen :: Koordinate -> Zustand -> Bool
-- bestimmt, ob Schuss ein Schiff des Gegners getroffen hat
-- Input: Koordinate - Ziel des Angriffs
-- Input: Zustand - gegnerische Schiffe und bereits geschossene Koordinaten
-- Output: Bool - Wahrheitswert

treffen pos (gs,_) =  pos `elem` (concat gs)



versenkt :: Koordinate -> Zustand -> Bool
-- bestimmt, ob Schuss ein Schiff des Gegners versenkt hat
-- Input: Koordinate - Ziel des Angriffs
-- Input: Zustand - gegnerische Schiffe und bereits geschossene Koordinaten
-- Output: Bool - Wahrheitswert

versenkt ziel ([], versuche) = False

versenkt ziel (schiffe, versuche) = sinken ziel (head schiffe) versuche || versenkt ziel ((tail schiffe), versuche)
    where sinken pos schiff versuche = pos `elem` schiff && (all (`elem` versuche) schiff)



besiegt:: Zustand -> Bool
-- bestimmt, ob ein Schuss die gesamte Flotte des Gegners versenkt hat
-- Input: Zustand - gegnerische Schiffe und bereits geschossene Koordinaten
-- Output: Bool - Wahrheitswert

besiegt (gs,g) = all (`elem` g) (concat gs)



aktualisiere :: Koordinate -> Zustand -> Zustand 
 -- aktualisiert die Liste der bereits geschossenen Koordinaten
 -- Input: Koordinate - Ziel des Angriffs
-- Input: Zustand - gegnerische Schiffe und bereits geschossene Koordinaten
-- Output: Zustand - gegnerische Schiffe und aktualisierte Liste der geschossenen Koordinaten

aktualisiere p (gs,g) = (gs, g ++ [p])



main :: IO()
-- main: ruft alle weiteren Funktionen auf

main = do
    [s1, s2] <- benenne
    print ("Gebt bitte alle Koordinaten so an: >> A1 <<. Fuer die Platzierung eurer Schiffe trennt ihr die Koordinaten bitte durch ein Komma >> A1,A2 << ohne Leerzeichen! "
            ++ "Beachtet: In dieser Variante duerfen Schiffe auch direkt nebeneinander platziert werden.")
    
    print (s1 ++ ", verteile deine Schiffe!")
    schiffe_s1 <- setze_schiffe [2,3,3,4,5] []
    
    print (s2 ++ ", verteile deine Schiffe!")
    schiffe_s2 <- setze_schiffe [2,3,3,4,5] []
    
    spielzug <- spielen [s1, s2] [(schiffe_s2, []), (schiffe_s1, [])]
    print ("Gutes Spiel!")