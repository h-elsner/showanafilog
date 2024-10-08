resourcestring                                     {deutsch}
  manual='ShowAnafiLog.pdf';
  {$IFDEF LINUX}
    DownURL='/downloads/ShowAnafiLog_de.tar.gz';
  {$ELSE}
    {$IFDEF DARWIN}                                {MAC OS X}
      DownURL='/downloads/ShowAnafiLog_de.dmg.zip';
    {$ELSE}
      DownURL='/downloads/ShowAnafiLog_de.zip';
    {$ENDIF}
  {$ENDIF}

  {Alternative column header data}
    ahdr0 ='Zeit ab Einschalten';
    ahdr1 ='Batterieladezustand';
    ahdr2 ='RC Breitengrad';
    ahdr3 ='RC Längengrad';
    ahdr4 ='Flug Modus';
    ahdr5 ='Alarm Status';
    ahdr6 ='WiFi Signalstärke';
    ahdr7 ='Anafi GPS Lock';
    ahdr8 ='Anafi Längengrad';
    ahdr9 ='Anafi Breitengrad';
    ahdr10='Anafi GPS-Fehler';
    ahdr11='Anzahl Sats';
    ahdr12='Geschw. vorwärts';
    ahdr13='Geschw. seitwärts';
    ahdr14='Geschw. senkrecht';
    ahdr15='Roll Winkel';
    ahdr16='Pitch Winkel';
    ahdr17='Kompassrichtung';
    ahdr18='Realtive Höhe';
    ahdr19='Flip Type';
    ahdr20='Fluggeschwindigkeit';
    ahdr21='Entfernung Anafi zu RC';
    ahdr22='Spannung';                             {Replaces column 1}
    ahdr23='Motorfehler';                          {Replaces column 12}
    ahdr24='Anafi GPS Höhe';

  {Other column header}
    ovDate='Datum';
    ovFrom='von';
    ovTo='bis';
    ovDuration='Dauer';
    ovAltMax='Max Höhe';
    ovTasMax='Max Geschw.';
    ovDistMax='Entfernung';
    ovRoute='Strecke';
    ovBattMax='Max Batterie';
    ovBattMin='Min Batterie';

    rsDateTime='Datum/Zeit';
    csvDist='Entfernung zum RC';

    staHd1='Min/max Werte';
    staHd2='Erstes Auftreten';

    dtlJCol0='Details';
    dtlJCol1='Wert';
    dtlFCol0='Parameter';
    dtlFCol1='Wert';

    altCount='Anzahl Werte';
    altDescription='Beschreibung';

  {Tab page header}
    thdOverview='Übersicht';
    thdData='Logdaten';
    thdDia='Höhendiagramm';
    thdAtti='Fluglage';
    thdDetails='Metadaten';
    thdSettings='Einstellungen';

  {Captions and hints}
    hntLogDir='Wähle ein Verzeichnis mit Anafi JSON log Dateien aus';
    capLogDir='Wähle Verzeichnis';
    hntChart1='Zoomen mit Mausrad, Verschieben mit gehaltener linken Maustaste';
    dtSpeed='XYZ Geschw.';
    dtAngle='XYZ Winkel';
    clnLeft='0-Linie links';
    clnRight='0-Linie rechts';
    hntChartListBox='An-/abwählen der Diagramme für XYZ Geschwindigkeit oder Winkel';
    hntChart2='Fluglage des Kopters: Diagramme für XYZ Geschwindigkeit und Winkel';
    hntFDRdir='Wähle Verzeichnis mit FDR log zur Suche nach UUID aus';
    hntedUUID='UUID, nach der gesucht werden soll';
    capbtSearch='Suche UUID';
    hntbtSearch='Suche nach UUID in den FDR Metadaten starten';
    capFDRresult='Dateiliste mit passenden UUID';

  {Buttons}
    capClose='&Beenden';
    hntClose='Beenden. Es wird nichts weiter gespeichert.';
    capScrShot='&Screenshot';
    hntScrShot='Screenshot in eine Datei speichern';
    capBtnConv='Konvertieren';
    hntBtnConf='Konvertiere eine JSON Logdatei entsprechend der Einstellungen. Die Datei wird im selben Verzeichnis wie die Logdaten gespeichert.';
    capColor='KML Flugpfad';
    hntColor='Wähle eine Farbe für den KML Flugpfad aus. Der Pfad vom RC wird immer schwarz dargestellt.';

  {Other strings}
    rsFiles='Dateien';
    rsScreen='Screen';
    rsUnKnown='Unbekannt';
    rsSaved='erfolgreich angelegt und gespeichert';
    rsMetaDataJ='Metadaten JSON';
    rsMetaDataF='Metadaten FDR';
    rsMetaDateB='Metadaten Blackbox';
    rsStatistics='Statistiken';
    errWrongData='Kein Anafi JSON log oder falsche Daten in der Datei';
    errMissingFiles='Keine JSON Datei im Verzeichnis';
    errLessData='Nicht genug Datensätze in der Datei ';
    errWrongValue='Fehlender oder ungültiger Wert';
    errRename='Kann Datei nicht umbenennen';
    rsChart='Diagramm für ';
    rsList='Werte und Zähler für ';
    rsStatCopy='Kopie der Statistik';
    rsGPSavailable='Anafi GPS verfügbar';
    rsGPSnotAvailable='Anafi GPS nicht verfügbar';
    rsContact='Kontakt';
    rsManufacture='Herstellungsdatum';
    rsLogBook='Pilot Logbuch';
    rsYes='Ja';
    rsNo='Nein';
    rsLocation='Standort';
    rsGPSfix='GPS fix beim Start';
    rsFrom=' von ';
    rsFilesRenamed='Dateien umbenannt';
    rsUUIDfound='UUID gefunden';
    rsNothingFound='Nichts gefunden';
    rsExtFilter='FDR log Dateien|*.bin|Alle Dateien|*.*';
    rsLatestVersion='Du hast bereits die neueste Version.';
    rsDownloading='Download ZIP Datei...';
    errDownloading='Repository nicht vorhanden';

  {Menu items}
    mniFile='&Datei';
    mniOpen='Wähle &JSON log Verzeichnis';
    mniJump='Datei &Manager mit log Verzeichnis aufrufen';
    mniCSVex='Export als CS&V Datei';
    mniKMLex='Export als &KML Datei';
    mniGPXex='Export als &GPX Datei';

    mniTools='&Tools';
    mniSettings='&Einstellungen';
    mniTas='Air speed berechnen';
    mniRename='JSON Dateien mit Datum/Zeit umbenennen';
    mniFDRlog='Metadaten aus FDR log anzeigen';

    mniHelp='&Hilfe';
    mniHomepage='Homepage';
    mniInfo='&Über';

    mniCopy='In Zwischenablage';
    mniFileSave='Speichern als...';
    mniShowGM='Zeige in Google Maps';
    mniShowOSM='Zeige in Open Street Map';

  {Settings}
    capUnit='Maßeinheit';
    hntUnit='Wähle Maßeinheiten entsprechend der Einstellungen auf deinem Steuergerät';
    capConv='Konvert';
    hntBoxConv='Einstellungen für Konvertierungen';
    hntConv='Wähle Ausgabeformat für Export konvertierter Dateien';
    capConvSel='Ausgabeformat';
    capExtrude='Extrude';
    hntExtrude='Extrude zeigt vertikale Linien vom Flugpfad zum Boden in GoogleEarth';
    capRCtrack='RC track';
    hntRCtrack='Zeige den Standort/Weg der Fernsteuerung';
    capHeader='Alternative Spaltenbezeichnungen';
    hntHeader='Alternative Spaltenbezeichnungen statt der originalen Bezeichnungen zur besseren Verständlichkeit';
    capDia='Zweites Diagramm';
    hntDia='Wähle Spalte für die zweite Linie im Diagramm (die rote Linie)';
    capCSVsep='Semicolon als Trenner bei CSV';
    hntCSVsep='Semicolon statt Komma als Trenner bei CSV-Dateien';
    capLogBook='Ausgabe Format';
    hntLogBook='Ausgabe Format für das Pilot Logbuch';
    capCrLogBook='&Logbuch';
    hntCrLogBook='Legt ein Pilot Logbuch von allen JSON Logdateien im ausgewählten Verzeichn is an';
    capTable='Tabelleneinstellungen';
    hntTable='Alternatives Verhalten der Datentabelle';
    capDegree='Winkel in Grad';
    hntDegree='Überschreibe die originalen Werte in Radiant mit Werten in Grad';
    rsLatest='Neueste SW Version';
    rsManual='Benutzerhandbuch';
    capGitHub='GitHub repository';

