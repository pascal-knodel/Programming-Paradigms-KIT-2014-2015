:mortar_board: Aufgabe lesen
   - Zusicherungen erkennen (entweder markieren oder noch Mal extra abschreiben)
   - Wie helfen diese bei der Definition der Problemlösung?
   - Welche Fälle müssen wir wegen einer Zusicherung beachten oder nicht beachten?
   
:mortar_board: Rekursive Funktionen
   - Welche Spezialfälle gibt es, in denen z.B. ein "error" Aufruf angebracht ist?
   - Wie viele Abbruchbedingungen gibt es?
   - Bricht die Rekursion ab?
   - Bricht die Rekursion immer ab?
   - Welche/r Klammerung / Kombinator? 
   - Wenn es Mal nicht gleich mit einem Kombinator geht,
     eben erst die primitive Rekursion hinschreiben.
   - Manchmal hilft es die Liste im Argument der Funktion aufzuteilen,
     (e1 : ... : es), um einen Kombinator anwenden zu können.
   - Wenn Endrekursion verlangt wird, ist oft eine Definition mit 'guard(s)'
     einfacher und gilt vielleicht schon als Lösung.
	 
:mortar_board: Streams
   - 'ist ein Fluss da' / könnte die Berechnung ins Stocken kommen?
   - Streams von Folgen lassen sich einfach durch weitergeben
     der Argumente in einem Akkumulator definieren. Das ist besser als
	 nichts, wenn unklar ist, welche Kombinatoren eine kurze Definition
	 ermöglichen.
   - Manchmal ist es hilfreich, eine weitere Liste von unendlichen Listen
     zu definieren und diese im nachhinein wieder rekursiv zu bearbeiten.

:mortar_board: Hilfreiches
   - Partielle Anwendung von zip [startIndex ..] nummeriert Listenelemente als Tupel durch.

:mortar_board: Typische Fehler vermeiden:
   - Falsche Anzahl von Argumenten / Argument vergessen, ... .
   - Nach einer Umbenennung (z.B. eines Arguments) stimmt die Definition nicht mehr.
   - Listen von Listen: Listenumklammerung zu viel oder zu wenig
     (z.B. f :: [[t]] -> [t] ; f [a] = a).
   - 'Den richtigen' Vergleich wählen: < oder <=, > oder >=, ...
   - Auf 'die richtige' Reihenfolge beim 'pattern matsching' achten.
   - Muster die nicht jedes Mal verglichen werden müssen beim 'pattern matching' 
     hinten anstellen.
   - Bei Lambda Abstraktionen 'rule of cancelation' beachten, damit am Ende
     der Typ nach dem Gleichheizzeichen mit einer gewünschten Typsignatur vor
	 dem Funktionsdefinition übereinstimmt.

:mortar_board: Wenn noch Zeit übrig ist: Test Eingabe(n) im Kopf oder schriftlich auswerten
   (kurze Eingaben nehmen, die evtl. einen schwierigeren Fall beschreiben).




