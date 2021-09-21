# Politikanalyse Uster

## Programm
Unter [politikbarometer.shinyapps.io/uster/](https://politikbarometer.shinyapps.io/uster/) steht ein kommunaler Politikbarometer mit einer Politikfeld- sowie einer Netzwerkanalyse zur Verfügung. Das Programm ist in der [Statistiksprache R](https://de.wikipedia.org/wiki/R_(Programmiersprache)) geschrieben und wir auf einem Shiny-Server zur Verfügung gestellt.

**LIZENZ:** Das Programm steht unter [GPLv3](https://github.com/data-socialthink/politik-uster/blob/main/politik-uster/License) zur Verfügung, der Quellcode findest du hier: https://github.com/data-socialthink/politik-uster/tree/main/politik-uster

## Datensätze
### Gemeinderatsgeschäfte
In der Datei [**geschafte.csv**](https://github.com/data-socialthink/politik-uster/blob/main/geschafte.csv) werden sämtliche auf der Webseite der Stadt Uster publizierten Gemeinderatsgeschäfte aufgeführt. Diese werden 1x täglich mit einem Script von der Webseite gelesen (sogenanntes Web-Scraping oder Web-Harvesting). Folgende Daten stehen zur Verfügung:
- Nummer: offizielle Nummer des Geschäfts
- Datum: offizielles Datum des Geschäfts (im Format dd.mm.yyyy)
- Art: Art des Geschäfts
- Geschäft: Titel des Geschäfts
- URL: URL zum Eintrag auf der Webseite
- Date: offizielles Datum des Geschäfts (im Format yyyy-mm-dd, identisch mit Datum)
 
*Beachte, dass Geschäfte bis zum Februar 2015 teilweise mehrfach verzeichnet sind. Beispielsweise wurden Antworten des Stadtrats auf Anfragen oder "Berichte und Anträge" zu Motionen jeweils als neue Geschäfte im (diesem Datensatz zugrundeliegenden) Verzeichnis aufgenommen.*

**LIZENZ:** Diese Daten unterstehen nicht dem Urheberrecht und sind Public Domain (siehe unten)

### Stichwörter
Mit Hilfe des Stichwortverzeichnis ([**stichwortliste.csv**](https://github.com/data-socialthink/politik-uster/blob/main/stichwortliste.csv)) können die einzelnen Geschäfte automatisiert einem Politikfeld zugeordnet werden. Die Stichwortliste sowie die Politikfelder wurde ausgehend von einer Analyse sämtlicher in Titeln von Geschäften enthaltenen Wörter entwickelt. Es ist zu beachten, dass Begriffe teilweise mehrdeutig sind und eine auf dieser Stichwortliste basierende Analyse somit nur eine Annäherung darstellt.
- Stichwort: Aus den Titeln abgeleitete Stichworte (nicht übertragbar auf andere Gemeinden, da teilweise lokaler Bezug)
- Kategorie: Kategorisierung der Stichworte zu übergeordneten Themen
- Politikfeld: Zuordnung der Stichworte zu den wichtigsten Politikfelder.

**LIZENZ:** Diese Stichwortliste steht unter CC BY 4.0 (siehe unten) zur Verfügung.

### Gemeinderät:innen
In der Datei gr_liste.csv werden ehemalige und aktive Gemeinderät:innen aufgeführt. Es sind folgende Daten erfasst:
- Vorname
- Name
- Fraktion (aktuell oder bei Austritt aus dem Gemeinderat)
- Partei (aktuell oder bei Austritt aus dem Gemeinderat)
- von (Eintritt in Gemeinderat, wenn unbekannt wird 1970-01-01 angegeben)
- bis (Austritt aus dem Gemeinderat, tatsächliches Austrittsdatum wenn ausgetreten, ansonsten Ende der Amtszeit)
- aktiv (ob aktiv oder ehemalig)
- twitter (Twitter-Account oder Twitter der Partei)

**LIZENZ:** Diese Stichwortliste steht unter CC BY 4.0 (siehe unten) zur Verfügung.

## Lizenzen

### Public Domain
Gemäss Bundesgesetz über das Urheberrecht und verwandte Schutzrechte sind Gesetze, Verordnungen, Entscheidungen, Protokolle und Berichte von Behörden und öffentlichen Verwaltungen sowie andere amtliche Erlasse nicht urheberrechtlich geschützt (URG Art. 5 Abs. 1). Der Datensatz der Gemeinderatsgeschäfte steht daher zur freien Verwendung zur Verfügung.

### Creative Commons Attribution 4.0 (CC BY 4.0)
Unter [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/deed.de) lizenzierte Daten dürfen geteilt und bearbeitet werden und zwar für beliebige Zwecke, sogar kommerziell. Dabei muss die Quelle angegeben  (Namensnennung) und es müssen angemessene Urheber- und Rechteangaben gemacht werden. Es ist ein Link zur Lizenz beizufügen und anzugeben, ob Änderungen vorgenommen wurden. Die Daten, sofern sie nicht unter Public Domain fallen, stehen unter CC BY 4.0 zur Verfügung.

### GNU GENERAL PUBLIC LICENSE Version 3
Der Programmcode des Politbarometers steht unter GNU GENERAL PUBLIC LICENSE Version 3. Diese Lizenz gewährt das Recht, die Software auszuführen, zu ändern und zu verbreiten. Aus diesem Programmcode abgeleitetete Software muss unter den gleichen Bedingungen zugänglich sein. Details zur Lizenz findest du im [Lizenztext](https://github.com/data-socialthink/politik-uster/blob/main/politik-uster/License).



