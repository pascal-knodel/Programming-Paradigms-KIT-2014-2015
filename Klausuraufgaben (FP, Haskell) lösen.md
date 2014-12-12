:) Aufgabe lesen

:) Zusicherungen erkennen (entweder markieren oder noch Mal extra abschreiben)
   - Wie helfen diese bei der Definition der Probleml�sung?
   - Welche F�lle m�ssen wir wegen einer Zusicherung beachten oder nicht beachten?
   
:) Bei rekursiven Funktionen
   - Welche Spezialf�lle gibt es, in denen z.B. ein "error" Aufruf angebracht ist?
   - Wie viele Abbruchbedingungen gibt es?
   - Bricht die Rekursion ab?
   - Bricht die Rekursion immer ab?
   - Im Fall von Streams: 'ist ein Fluss da'?
   - Streams von Folgen lassen sich einfach durch weitergeben
     der Argumente in einem Akkumulator definieren. Das ist besser als
	 nichts, wenn unklar ist, welche Kombinatoren eine kurze Definition
	 erm�glichen.
   - Welche/r Klammerung / Kombinator? 
   - Wenn es Mal nicht gleich mit einem Kombinator geht,
     eben erst die primitive Rekursion hinschreiben.
   - Manchmal hilft es die Liste im Argument der Funktion aufzuteilen,
     (e1 : ... : es), um einen Kombinator anwenden zu k�nnen.
   - Wenn Endrekursion verlangt wird, ist oft eine Definition mit 'guard(s)'
     einfacher und gilt vielleicht schon als L�sung.

:) Typische Fehler vermeiden:
   - Falsche Anzahl von Argumenten / Argument vergessen, ... .
   - Nach einer Umbenennung (z.B. eines Arguments) stimmt die Definition nicht mehr.
   - Listen von Listen: Listenumklammerung zu viel oder zu wenig.
   - 'Den richtigen' Vergleich w�hlen: < oder <=, > oder >=, ...
   - Auf 'die richtige' Reihenfolge beim 'pattern matsching' achten.

:) Wenn noch Zeit �brig ist: Test Eingabe(n) im Kopf oder schriftlich auswerten
   (kurze Eingaben nehmen, die evtl. einen schwierigeren Fall beschreiben).