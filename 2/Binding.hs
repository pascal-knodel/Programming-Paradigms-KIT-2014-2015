--
-- ÜB 2, Programmierparadigmen KIT 2014/2015
--
-- Pascal Knodel
--
module Binding where




-- Eine Bindungsstelle (oder Definitionsstelle) ist die Stelle an dem ein Name eingeführt (gebunden) wird.
-- In verschiedenen Gültigkeitsbereichen ist die Einführung gleicher Namen möglich. Bindung findet hier bei 
-- Definitionen (=) (Funktionsname, Argument, usw.) und Lambda Abstraktionen (\) statt.
--
-- Für den Gültigkeitsbereich eines Namens sind u. A. diese Regeln wichtig:
--
-- (R1) Abseitsregel: 'Gleiche Einrückung bildet ein (Gültigkeits-)Rechteck.'
--
-- (R2) Die Bindung eines Namens ist im Inneren am stärksten, d.h. gleiche äußere Namen
--      können nicht verwendet werden - der innere Name gilt:
--
--      (R2-1) Namen in "let"-Blöcken binden stärker als "where"-Blöcke, d.h. gleiche Namen
--             aus "where"-Blöcken können im "let"-Block nicht verwendet werden - der Name im
--             "let"-Block gilt.
--
--      (R2-2) Argumente in Lambda Abstraktionen binden stärker, d.h. gleichnamige Argumente
--             aus der äußeren Definition können nicht verwendet werden - das Argument der
--             Lambda Abstraktion gilt.
--
--      (R2-3) Namen in Listen-Kurzschreibweise (list comprehension) binden stärker, d.h.
--             gleichnamige äußere Definitionen können nicht verwendet werden - der im Inneren
--             der Listen-Kurzschreibweise definierte Name gilt.
--
--
-- (!) Verschachtelte Funktionen werden von außen nach innen ausgewertet. Ansonsten von links nach rechts.
--
-- (!) 'Was im Inneren definiert wird, gilt auch dort und nicht außerhalb'.
--     Z.B. bei verschachtelten let-Ausdrücken: let ... in let ... in ... = let ... in (let ... in ...)
--
-- (!) Mit Semikolons können mehrere Definitionen in eine Zeile geschrieben werden.
--
-- (!) Vorsicht bei Verdeckungen/Verschattungen (d.h. der äußere Name gilt innen nicht, da es
--     eine innere Bindung gibt, die stärker ist).
--
--
-- Ausführlichere Informationen stehen in der Haskell Sprachspezifikation:
-- https://www.haskell.org/haskellwiki/Language_and_library_specification



-- GHCi-Beispiele:
--
-- Hinweis: GHCi braucht jeweils noch ein let vor einer Definition.
--          Zwischen ":{" und ":}" sind mehrere Zeilen in GHCi möglich.


-- Beispiel zu (R1):
--------------------
--
{- GHCi>


:{

let t = "global" where
        s = "local"

:}
s

-}
-- <interactive>:109:1: Not in scope: `s'


-- Beispiel zu (R2-1):
----------------------
--
{- GHCi>

let t = let s = "let" in s where s = "where"
t

-}
-- "let"


-- Beispiel zu (R2-2):
----------------------
--
{- GHCi>

let t s = (\s -> s) "lambda abstraction"
t "function"

-}
-- "lambda abstraction"


-- Beispiel zu (R2-3):
----------------------
--
{- GHCi>

let t s = (\(s : []) -> s) [ s | s <- ["list comprehension"] ]
t "function"

-}
-- "list comprehension"


-- 'Mehrere' (zwei) "let":
--------------------------
{- GHCi>

let t = let s = "first" in let s = "second" in s
t

-}
-- "second"


-- 'Mehrere' (zwei) "where":
----------------------------
{- GHCi>

let t = s where s = "first" where s = "second"
t

-}
-- "first"


-- Semikolon:
-------------
{- GHCi>

let s = "global"
let t = let t = s ; s = "local" in s
t

-}
-- "local"




-- Teil 1: Bindung und Gültigkeitsbereiche:
-------------------------------------------


-- (1):
f y =  \z -> x + 7 * z - y

-- (2):
x =  1

-- (3):
g x =  x + ( let   y =  x * 2; x =  5 * 5   in   (let   x =  f x 2   in   x + y) )

-- (4):
h =  let   z =  2   in    g x + (\z -> -z) z    where z = 3



-- Welche Vorkommen von Namen in diesem Programm sind Definitions- oder Bindungsstellen?
--
--
-- Definitionsstellen:
--
--
--    Name   Definitionsstelle         Gültigkeit (wenn kein gleicher Name stärker bindet)
--
--    f      In (1)                      Global
--    y      Erstes in (1)               Lokal (Funktion f)
--    z      Erstes in (1)               Lokal (Lambda Abstraktion)
--
--    x      In (2)                      Global
--
--    g      In (3)                      Global
--    x      Erstes in (3)               Lokal (Funktion g)
--    y      Im ersten "let" in (3)      Lokal (let Ausdruck)
--    x      Im ersten "let" in (3)      Lokal (let Ausdruck)
--    x      Im zweiten "let" in (3)     Lokal (let Ausdruck)
--
--    h      In (4)                      Global
--    z      Im "let" in (4)             Lokal (let Ausdruck)
--    z      Bei \z im "let in" in (4)   Lokal (Lambda Abstraktion)
--    z      Im "where" in (4)           Lokal (Funktion h)
--
-- (*) Gesamtes Modul. Erste Verwendung eines Namens nach = bzw. -> (es gibt noch mehr: <- bei IO, ...).
--
-- (Es gibt 13 Definitionsstellen.)


-- Bindung - Auf welche Bindungsstelle bezieht sich jeweils ein Name (der selbst keine Bindungsstelle ist).
--
--
--    Name                 Bindungsstelle
--
--    x in (1)             x aus (2)
--    Letztes z in (1)     Erstes z aus (1)
--    Letztes y in (1)     Erstes y aus (1)
--
--    Zweites x in (3)     Erstes x aus (3)
--    f in (3)             f aus (1)
--    Drittes x in (3)     x aus dem ersten "let" in (3)
--    Sechstes x in (3)    x aus dem ersten "let" in (3)
--    Siebtes x in (3)     x aus dem zweiten "let" in (3)
--    y in (3)             y aus dem ersten "let" in (3)
--
--    x in (4)             x aus (2)
--    g in (4)             g aus (3)
--    Drittes z in (4)     z bei \z in (4)
--    Viertes z in (4)     z im "let" in (4)
--
-- (Es gibt 13 Bindungen)




-- Hinweis: Aus Fehlern lernt man, es bietet sich an die Bindungsstellen zu nummerieren
--          (z.B. BX für die erste Bindungsstellen und an jeden Namen der BX bindet die Zahl X schreiben).
--
--          Oder: Die Einführung jedes Namens markieren (z.B. einkreisen) und Linien von jeder Verwendung eines
--                Namens zu seiner Bindungsstelle ziehen.




-- Verständnisfragen:
--
-- Was sind 1-4 jeweils? Gib je einen Beispielaufruf an und überlege was passiert.
--
-----------------------------
-- f y =  \z -> x + 7 * z - y
-----------------------------
-- x =  1
---------
--
-- f hat ein Argument y und ist über eine Lambda Abstraktion definiert.
-- \z erwartet selbst ein Argument, welches allerdings nicht definiert wird.
-- D.h. \z ist unterversorgt und ist mit zwei Argumenten aufzurufen.
-- Diese müssen numerisch sein, da mit x und y durch Rechenoperationen ver-
-- knüpft sind.
--
-- Beispiel: f 1 1
--
-- f 1 1 = 1 + 7 * 1 - 1 = 7
--
{- GHCi>

f 1 1

-}
-- 7
--
--
---------
-- x =  1
---------
--
-- x ist konstant 1.
--
--
-------------------------------------------------------------------------------------
-- g x =  x + ( let   y =  x * 2; x =  5 * 5   in   (let   x =  f x 2   in   x + y) )
-------------------------------------------------------------------------------------
--
-- g hat ein Argument x. Mit x wird additiv verknüpft und muss daher numerisch sein.
-- Im äußeren let Ausdruck werden y und ein weiteres x definiert, welchs das Argument x
-- verdeckt. Im inneren let Ausdruck wird x wieder durch eine andere Definition verdeckt.
-- Diese Definition ist endlos rekursiv.
--
{- GHCi>

g 1

-}
-- *** Exception: <<loop>>
--
--
--------------------------------------------------------------
-- h =  let   z =  2   in    g x + (\z -> -z) z    where z = 3
--------------------------------------------------------------
--
-- h hat keine Argumente. Der let Ausdruck definiert z als 2.
-- Im in-Teil wird die Auswertung von g 1 verlangt. D.h. es
-- kommt wieder zu einer Endlosrekursion.
--
--
{- GHCi>

h

-}
-- 
-- Berechnung unterbrochen mit STRG + C (Windows CMD).



-- Fragen:
--
-- Wieso erkennt GHC(i) (The Glorious Glasgow Haskell Compilation System, version 7.8.3)
-- beim Aufruf "g 1" die Schleife, beim Aufruf "h" jedoch nicht?
--
-- Ein Unterstrich bindet nicht. Warum? Wie wird ein Unterstrich übersetzt?




