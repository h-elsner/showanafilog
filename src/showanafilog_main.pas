{********************************************************}
{                                                        }
{     Auswertung FlightLog Daten der Parrot ANAFI        }
{                                                        }
{       Copyright (c) 2019         Helmut Elsner         }
{                                                        }
{       Compiler: FPC 3.0.4   /    Lazarus 1.8.4         }
{                                                        }
{ Pascal programmers tend to plan ahead, they think      }
{ before they type. We type a lot because of Pascal      }
{ verboseness, but usually our code is right from the    }
{ start. We end up typing less because we fix less bugs. }
{           [Jorge Aldo G. de F. Junior]                 }
{********************************************************}

(*
This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your option)
any later version.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

A copy of the GNU General Public License is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1335, USA.

================================================================================

Auswertung der FlightLogDaten des Parrot Anafi Quadkopters
----------------------------------------------------------

Files are *.JSON with a fix data structure.
Example:
{
"details_headers": [
  "time",
  "battery_level",
  "controller_gps_latitude",
  "controller_gps_longitude",
  "flying_state",
  "alert_state",
  "wifi_signal",
  "product_gps_available",
  "product_gps_longitude",
  "product_gps_latitude",
  "product_gps_position_error",
  "product_gps_sv_number",
  "speed_vx",
  "speed_vy",
  "speed_vz",
  "angle_phi",
  "angle_theta",
  "angle_psi",
  "altitude",
  "flip_type",
  "speed"
],
"details_data": [
  [
Values 1..21
],
...
"version": "1.2",
"date": "2018-11-10T094343-0500",
"product_id": 2324,
"product_name": "Anafi",
"boot_id": "F67685A6413B0171BDACC08497F3BB1B",
"serial_number": "PI040416AA8E004784",
"hardware_version": "HW_03",
"software_version": "1.2.1",
"uuid": "61E649E50F8A81BE89292E8ADFA12979",
"controller_model": "RC,Skycontroller 3",
"controller_application": "PI040443AA8E002764,1.2.0",
"run_origin": 0,
"crash": 0,
"total_run_time": 369806,
"run_time": 362286,
"gps_available": true,
"gps_latitude": 48.41409350000001,
"gps_longitude": -89.62652150000038
}

Form1.Tag:         Indicates if first run done (1=done}
dtlGrid.Tag:       Type of file: 0..JSON, 1..FDR, 2..Blackbox
csvGrid.Tag:       Current used file index (1...n)
ovGrid.Tag:        Selected file index
PageControl1.Tag:  Last used tab-page
cbDegree.Tag:      Something changed in Settings that needs reload

Form.Activate --> BuildList --> ReadOneFile, the first
                  Start thread to fill overview.

History:
0.1  2019-01-19 Proof of concept, no functionality
0.2  2019-01-23 Load Data working, Charts working
     2019-01-24 Faster start-up, bugfixes, KML export
0.3  2019-01-25 Additional chart or counter table in extra window
     2019-01-26 GPX export is working, Main menu and context menus added.
                Recompute air speed when missing.
0.4  2019-01-27 Drag & Drop of files or directories.
                Cursor in charts in second window is moving
                with the selection in the data table.
0.5  2019-01-28 Unit conversion added solved.
                Assumption: JSON Input always Metric
                F5 to refresh/reload all files
                Anafi splash screen by Augustine.
0.6  2019-01-29 Sort overview table by header click,
                double-click inverts the sorting.
1.0  2019-01-31 Final Release. Missing Context menu added.
                User Manual updated.
1.1  2019-02-02 Fill overview table by 2nd thread.
     2019-02-03 Use advantage of multi-thread. Start-up faster.
1.2  2019-02-04 Zooming, panning and horizontal cursor (Ctrl-key + mouse)
                added to Charts.
     2019-02-08 Progress bar added. Date of manufacture on Details page.
1.3  2019-02-12 Pilot log book.
     2019-02-24 Thread handling improved.
                Bug (missing JSON Parser) in LINUX version fixed.
     2019-03-05 Keep default behaviour in selected cells of a table.
1.4  2019-03-12 Attitude chart added
     2019-03-23 Alternative degrees for angle in Details table.
     2019-03-27 Time label at cursor in additional chart.
     2019-03-28 Refresh tabs if changed. Dotted lines as zero axis line.
                Toggle Attitude chart: All -> Speed -> Angle -> Nick/Roll -> All.
     2019-03-31 Cell hints updated for all tables. Skip wrong data in meta data.
1.5  2019-06-14 Rename JSON files with date/time stamp.
     2019-07-12 Updated "flying_state".
1.6  2019-11-06 Resolve Product_ID
     2019-11-16 Read meta data from FDR log files
     2019-11-18 More Smart battery info added
     2019-11-19 Bugfix: Skip empty parameter in FDR meta data
     2019-11-21 Search for UUID as reference added
     2019-11-22 DPI alignment removed, additional checks if JSON nodes exists
     2020-03-26 Load last file instead of first, column width in LogData updated
     2020-11-10 Alert state 6 added (Almost empty battery alert)
     2021-01-16 Query for latest version added, GitHub link
2.0  2021-08-01 Blackbox files, first try: Header

Icon and splash screen by Augustine (Canada):
https://parrotpilots.com/threads/json-files-and-airdata-com.1156/page-5#post-10388

Tester: Agustine, Dietmar K., liger 1956, DIRK_ANAFI, Landbo

Regarding false virus alerts see also:
http://blog.nirsoft.net/2009/05/17/antivirus-companies-cause-a-big-headache-to-small-developers/

-----------------------------
https://www.drohnen-forum.de/index.php/Thread/41391-Flugdatenspeicher/?postID=277218

Man kann im Internet - Parrot.de, nach der Anmeldung, unter "my Parrot" die
Daten anfordern. Unter Einstellungen des eigenen Konto, besteht die Möglichkeit.
Man bekommt dann eine Mail mit ZIP-Dateien, welche die JSON-Dateien enthält.
Der Download link ist aber nur 10 Tage gültig.

--------------------------------
https://www.drohnen-forum.de/index.php/Thread/39045-log-bin-Datei-auf-SD-Card/?postID=254229
Habe noch eine zweite Option gefunden, um an die json Files für den
FlightData Manager zu kommen, z.B. wenn du deine Flüge nicht in der
Parrot Cloud speichern willst:
- In der FF6 App auf My.Parrot gehen.
- Dort sind dann ja unter "Meine Flüge" deine Flüge aufgelistet.
- Hinter einem der Flüge auf das "Upload-Symbol" (Pfeil nach oben) tippen.
- Dann z.B. deinen E-Mail Client auswählen und die Daten an deine E-Mail Adresse senden.

Die sind dann als json File in FlightData Manager importierbar.


*)

unit showanafilog_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TATransformations, TAIntervalSources,
  TASeries, TATools, TAChartListbox, Ipfilebroker, Forms, Controls, Graphics,
  fpjson, jsonparser, Dialogs, StdCtrls, Grids, ComCtrls, XMLPropStorage,
  EditBtn, math, Buttons, strutils, dateutils, LCLIntf, LCLType, ExtCtrls,
  Menus, anzwerte, Iphttpbroker, IpHtml;

{.$I anafi_en.inc}                                  {Include a language file}
{$I anafi_dt.inc}

type
  TDatArr = array[0..20] of string;

type

  { TForm1 }

  TForm1 = class(TForm)
    btSearchFDR: TBitBtn;
    btLogBook: TBitBtn;
    btScrShot: TBitBtn;
    btClose: TBitBtn;
    btConv: TBitBtn;
    cbHeader: TCheckBox;
    Chart1: TChart;
    Chart1AreaSeries1: TAreaSeries;
    Chart1LineSeries1: TLineSeries;
    Chart2: TChart;
    Chart2ConstantLine1: TConstantLine;
    Chart2ConstantLine2: TConstantLine;
    Chart2LineSeries1: TLineSeries;
    Chart2LineSeries2: TLineSeries;
    Chart2LineSeries3: TLineSeries;
    Chart2LineSeries4: TLineSeries;
    Chart2LineSeries5: TLineSeries;
    Chart2LineSeries6: TLineSeries;
    ChartAxisTransformations1: TChartAxisTransformations;
    cbExtrude: TCheckBox;
    ChartAxisTransformations1AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    ChartAxisTransformations2: TChartAxisTransformations;
    ChartAxisTransformations2AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    btColor: TColorButton;
    cbCSVsep: TCheckBox;
    ChartAxisTransformations3: TChartAxisTransformations;
    ChartAxisTransformations3AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    ChartAxisTransformations4: TChartAxisTransformations;
    ChartAxisTransformations4AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    ipHTMLin: TIpHttpDataProvider;
    lblGitHub: TLabel;
    lbLegende: TChartListbox;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointCrosshairTool1: TDataPointCrosshairTool;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomMouseWheelTool1: TZoomMouseWheelTool;
    ChartToolset2: TChartToolset;
    ChartToolset2PanDragTool1: TPanDragTool;
    ChartToolset2ZoomMouseWheelTool1: TZoomMouseWheelTool;
    cbDegree: TCheckBox;
    cmnClipbrd2: TMenuItem;
    cmnSaveAs2: TMenuItem;
    csvGrid: TStringGrid;
    boxConv: TGroupBox;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    boxLogBook: TGroupBox;
    DateTimeIntervalChartSource2: TDateTimeIntervalChartSource;
    boxTable: TGroupBox;
    FDRdir: TDirectoryEdit;
    edUUID: TEdit;
    grpConv: TRadioGroup;
    lblFDRresult: TLabel;
    lblProduct: TLabel;
    lblDetails: TLabel;
    lblStat: TLabel;
    grpDia: TRadioGroup;
    lblManual: TLabel;
    lblDownload: TLabel;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
    cmnSaveAs: TMenuItem;
    cmnClipbrd: TMenuItem;
    MenuItem1: TMenuItem;
    cmnShowGM: TMenuItem;
    cmnShowOSM: TMenuItem;
    mmnDownload: TMenuItem;
    mmnFDRlog: TMenuItem;
    N1: TMenuItem;
    mmnRename: TMenuItem;
    mmnLogBook: TMenuItem;
    mmnManual: TMenuItem;
    mmnHomepage: TMenuItem;
    MenuItem7: TMenuItem;
    mmnTas: TMenuItem;
    mmnJump: TMenuItem;
    mmnInfo: TMenuItem;
    mmnSettings: TMenuItem;
    MenuItem2: TMenuItem;
    mmnScrShot: TMenuItem;
    mmnHelp: TMenuItem;
    mmnTools: TMenuItem;
    mmnClose: TMenuItem;
    mmnGPXex: TMenuItem;
    mmnFile: TMenuItem;
    mmnOpen: TMenuItem;
    MenuItem3: TMenuItem;
    mmnCSVex: TMenuItem;
    MenuItem5: TMenuItem;
    mmnKMLex: TMenuItem;
    FDRDialog: TOpenDialog;
    PopupMenu1: TPopupMenu;                        {for Overview and Charts}
    PopupMenu2: TPopupMenu;                        {For Details table}
    LogDirDialog: TSelectDirectoryDialog;
    ProgressFile: TProgressBar;
    grpLogBook: TRadioGroup;
    Splitter1: TSplitter;
    staGrid: TStringGrid;
    TabImages: TImageList;
    lblSelDir: TLabel;
    LogDir: TDirectoryEdit;
    PageControl1: TPageControl;
    grpUnit: TRadioGroup;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    ovGrid: TStringGrid;
    dtlGrid: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    XMLPropStorage1: TXMLPropStorage;
    procedure btConvClick(Sender: TObject);
    procedure btLogBookClick(Sender: TObject);
    procedure btScrShotClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btSearchFDRClick(Sender: TObject);
    procedure cbDegreeChange(Sender: TObject);
    procedure cbHeaderChange(Sender: TObject);
    procedure Chart1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Chart2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbLegendeDblClick(Sender: TObject);
    procedure cmnClipbrd2Click(Sender: TObject);
    procedure cmnClipbrdClick(Sender: TObject);
    procedure cmnSaveAs2Click(Sender: TObject);
    procedure cmnSaveAsClick(Sender: TObject);
    procedure cmnShowGMClick(Sender: TObject);
    procedure cmnShowOSMClick(Sender: TObject);
    procedure csvGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure csvGridHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure csvGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure csvGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure csvGridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure dtlGridDblClick(Sender: TObject);
    procedure dtlGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure dtlGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FDRdirDblClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure grpConvClick(Sender: TObject);
    procedure grpUnitClick(Sender: TObject);
    procedure lblDownloadClick(Sender: TObject);
    procedure lblDownloadMouseEnter(Sender: TObject);
    procedure lblDownloadMouseLeave(Sender: TObject);
    procedure lblGitHubClick(Sender: TObject);
    procedure lblGitHubMouseEnter(Sender: TObject);
    procedure lblGitHubMouseLeave(Sender: TObject);
    procedure lblManualClick(Sender: TObject);
    procedure lblManualMouseEnter(Sender: TObject);
    procedure lblManualMouseLeave(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure LogDirChange(Sender: TObject);
    procedure LogDirDblClick(Sender: TObject);
    procedure mmnDownloadClick(Sender: TObject);
    procedure mmnFDRlogClick(Sender: TObject);
    procedure mmnHomepageClick(Sender: TObject);
    procedure mmnCloseClick(Sender: TObject);
    procedure mmnCSVexClick(Sender: TObject);
    procedure mmnGPXexClick(Sender: TObject);
    procedure mmnInfoClick(Sender: TObject);
    procedure mmnJumpClick(Sender: TObject);
    procedure mmnKMLexClick(Sender: TObject);
    procedure mmnLogBookClick(Sender: TObject);
    procedure mmnManualClick(Sender: TObject);
    procedure mmnOpenClick(Sender: TObject);
    procedure mmnRenameClick(Sender: TObject);
    procedure mmnScrShotClick(Sender: TObject);
    procedure mmnSettingsClick(Sender: TObject);
    procedure mmnTasClick(Sender: TObject);
    procedure ovGridClick(Sender: TObject);
    procedure ovGridCompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure ovGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure ovGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ovGridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure PageControl1Change(Sender: TObject);
    procedure staGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure staGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure staGridResize(Sender: TObject);
    procedure CheckVersion;                        {Call version file and check}

  private
    procedure BuildList;                           {Search and count JSON files}
    procedure LoadOneFile(idx: integer);           {Start working one file}
    procedure dtlGridReSize;                       {Rezeize Details table}
    procedure ScreenToBild(fn: string);            {Screenshot}
    procedure MakeKML;                             {Create KML from StringGrid}
    procedure MakeGPX;                             {Create GPX from StringGrid}
    procedure MakeHDia;                            {Fill H-diagrams}
    procedure MakeAtti;                            {Fill attitude chart}
    procedure ChangeHeader;                        {Change column header}
    procedure BlackboxHeader;                      {Create column header for Blackbox files}
    procedure KMLheader(klist: TStringList);       {KML header and meta data}
    procedure DoForm2Show(p: integer);             {Show additional chart}
    procedure DoScreenShot;                        {Screenshot}
    procedure MakeCSV;                             {Create CSV file from csvGrid}
    procedure RestoreTAS;                          {Compute tas from vx, vy, vz}
    procedure CreateLogBook;                       {Create Pilot log book}
    procedure MetaGridInit(mode: integer);         {Set Details labels}
    procedure SearchFDRfiles(dir, mask, vstr: string);  {Find UUID}
    procedure InsertDataset(da: TDatarr; source: byte; var rpos: integer);

    function ShowFDRlog(fn: string; mode: integer=0): string; {Show meta data from FDR}
    function GetCellInfo(aCol, aRow: longint): string; {Cell info in data table}
    function ColorToKMLColor(const AColor: TColor; {Google Farbcodierung}
                        sat: integer=255): string; {Saturation=255}
    function GetCSVsep: char;                      {Define CSV data seperator}

  public
    function UnitToStr(idx: integer;               {Measurement unit string}
                      pure: boolean;               {Brackets no/yes}
                converted: boolean=false): string; {alternative unit like km/h}

    function ConvUnit(idx: integer;                {Index column}
                      value: double;               {Input from JSON}
                      converted: boolean=false): double;
            {Possible conversion: rad to °, m/s to km/h, ft to mi, ft/s to mph}
    var booklist: TStringList;
  end;

  type                                             {Thread for Overview table}
    TMyThread = class(TThread)
  private
    procedure WriteResults;
    procedure EnableSort;
    procedure GetSettings;
    procedure GetFileName;
  protected
    procedure Execute; override;
  public
    Constructor Create(CreateSuspended: boolean);
    var res: array [0..12] of string;
        idx, metr, numfiles: integer;
        dirname, filename: string;
  end;


const
  appName='ShowAnafiLog';
  appVersion='V2.0 08/2021';                       {Major version}
  appBuildno='2021-08-08';                         {Build per day}
  versfile='/v';

  hpmydat='/pdf/';
  email='helmut.elsner@live.com';

{Define if input in JSON data is always metric else
 depends on control device settings (variable)}
  metric=true;                                     {assumption, always metric}
  useau=false;                                     {use alternative units like km}
  mtoft=3.2808399;                                 {meter to feet}

{Links}
  githublink='https://github.com/h-elsner/showanafilog';
  homepage='http://h-elsner.mooo.com';             {my Homepage}
  gmapURL='https://maps.google.com/maps';
  osmURL='https://www.openstreetmap.org/';         {OSM link}
  starticon='http://maps.google.com/mapfiles/dir_0.png';       {blue}
  stopicon='http://maps.google.com/mapfiles/dir_walk_60.png';  {grey}
  aircrafticon='http://earth.google.com/images/kml-icons/track-directional/track-0.png';

{JSON keywords}
  jsonHeaderJ='details_headers';
  jsonData='details_data';

  json1Hz='datas_1Hz';
  json5Hz='datas_5Hz';
  jsonDatas='datas';
  jsonHeaderB='header';

  jsonType='type';
  jsonTimest='timestamp';
  jsonalt='altitude';
  jsonlat='latitude';
  jsonlon='longitude';


{Meta data keywords}
  datVersion='version';
  datUTC='date';
  datProdID='product_id';
  datProdName='product_name';
  datBootID='boot_id';
  datSerialNo='serial_number';
  datHWvers='hardware_version';
  datSWvers='software_version';
  datUUID='uuid';
  datRCmodel='controller_model';
  datRCapp='controller_application';
  datRun='run_origin';
  datCrash='crash';
  datRuntimeTotal='total_run_time';
  datRunTime='run_time';
  datGPSavail='gps_available';
  datGPSlat='gps_latitude';
  datGPSlon='gps_longitude';

{Keywords for BlackBox JSON files}

  datProdSerial='product_serial';
  datProdFWhard='product_fw_hard';
  datProdMotor='product_motor_version';
  datProdFWsoft='product_fw_soft';
  datProdGPS='product_gps_version';
  datProdBlackbox='blackbox_version';
  datTimeBase='timestamp_base';
  datDeviceOS='device_os';
  datRC='remote_controller';
  datModel='Model';
  datDevModel='device_model';
  datPI='PI';
  rsRC='RC ';

{Datas types}
  jtypWIFIband='wifi_band';                        {to Meta data}
  jtypWIFIctry='wifi_country';

  jtypWIFIchan='wifi_channel';                     {WiFi channel may change}
  jtypProdBatt='product_battery';                  {Batt in %}
  jtypProdFState='product_flying_state';
  jtypProdAlert='product_alert';
  jtypGPSfix='product_gps_fix';
  jtypMotErr='product_motor_error';
  jtypRTH='product_rth_state';
  jtypHome='product_home';
  jtypFLand='product_forced_landing';
  jtypWind='product_wind';
  jtypVibr='product_vibration_level';
  jtypFPstate='product_fp_state';
  jtypFMstate='product_followme_state';
  jtypRunID='product_run_id';
  jtypGPSto='product_gps_takingoff';
  jtypMPPbtn='mpp_button';

{1Hz types}
  jtypVolt='product_battery_voltage';
  jtypRSSI='wifi_rssi';
  jtypRCgps='device_gps';
  jtypProdGPS='product_gps';
  jtypMPPcmd='mpp_pcmd';
  jtypSource='source';
  jtypVert='gaz';


{5Hz types}

  jtypProdAlt='product_alt';
  jtypProdAngles='product_angles';
  jtypPitch='pitch';
  jtypRoll='roll';
  jtypYaw='yaw';
  jtypRCcmd='device_pcmd';
  jtypFlag='flag';
  jtypHeight='product_height_above_ground';
  jtypProdSpeed='product_speed';
  jtypVX='vx';
  jtypVY='vy';
  jtypVZ='vz';

  MPP='MPP pcmd ';
  pcmd='RC pcmd ';
  lcbb=48;                                         {Index of last column BlackBox}
  ncjs=23;                                         {Number columns legacy JSON}


  ovheader: array[1..12] of string=(ovDate, ovFrom, ovTo, ovDuration,
                                    ovAltMax, ovDistMax, ovRoute, ovTasMax,
                                    ovBattMax, ovBattMin, rsLocation, rsGPSfix);
  boxheader: array[0..25] of string=('RC GPS alt', 'Height above ground',
                                     MPP+'source', MPP+'vert', MPP+'pitch',
                                     MPP+'roll', MPP+'yaw',
                                     pcmd+'flag', pcmd+'vert', pcmd+'pitch',
                                     pcmd+'roll', pcmd+'yaw', 'RTH state',
                                     'Forced landing', 'Follow-me state',
                                     'Wind', 'Vibration level', 'WiFi channel',
                                     'MPP button', 'Run ID',
                                     'Home alt', 'Home lat', 'Home lon',
                                     'Take-off alt', 'Take-off lat', 'Take-off lon');


{Output data formats}
  frmCoord='0.000000000';
  frmFloat='0.000000';
  frmOut1='0.0';
  frmOut2='0.00';
  frmOut3='0.000';
  hnsz='hh:nn:ss.zzz';
  hns='hh:nn:ss';
  ymd='yyyy-mm-dd';

  rfm=[1..5];                                      {Real in air flight modes}

  tab1=' ';
  tab2='  ';
  tab4='    ';
  tab6='      ';
  dpkt=': ';
  uscr='_';
  secpd=86400;                                     {seconds per day}
  strc='-';
  gzoom='16';                                      {Zoom value for maps}
  jext='.json';
  cext='.csv';
  pext='.png';
  fext='.bin';
  wldcd='*';
  jsonMinLines=20;
  isep=';';
  hc=11;                                           {Hidden column}
  ziff=['0'..'9'];                                 {valid digits}
  def0='0';                                        {Null string}
  home='Home';

{Strings fpr KML or GPX}
  xmlvers='<?xml version="1.0" encoding="UTF-8"?>'; {ID XML/GPX header}
  kmlvers='<kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2">';
  gpxvers='<gpx xmlns="http://www.topografix.com/GPX/1/1" version="1.1"';
  GPXlat='lat="';
  GPXlon='" lon="';
  GPXet1='</wpt>';
  amtag='altitudeMode>';
  pmtag='Placemark>';
  doctag='Document>';
  cotag='coordinates>';
  extru='<extrude>1</extrude>';

var
  Form1: TForm1;
  hdrList: array[0..25] of string;
  ovThread: TMyThread;

implementation

{$R *.lfm}

{ TForm1 }

///////////// Some standard routines to handle data structures ////////////////

function SekToDT(numsek: string; mode: integer): TDateTime; inline;
                                {Timestring to DT}
                                {mode 0: Number seconds to TDatetime
                                 mode 1: Number milliseconds to TDatetime
                                 mode 2: Number microseconds to TDatetime
         returns 0 if numsek is not an integer or mode is unknown}
var tst: uint64;
begin
  result:=0;
  tst:=StrToIntDef(trim(numsek), 0);
  if tst>0 then begin
    case mode of
      0: result:=tst/secpd;
      1: result:=tst/(secpd*1000);                 {ms}
      2: result:=tst/(secpd*1000000);              {µs}
    end;
  end;
end;

{Conversion ISO time stamp to internal time format.
        "date": "2018-11-10T094343-0500",
 FDR time stamp: 20181206T204438+0100}

function datISOtoDT(tst: string; form: string=ymd): TDateTime;
var dt, zt, zone: string;
    direction: char;                               {Time zone + or -}
begin
  dt:=ExtractWord(1, tst, ['T']);
  zt:=ExtractWord(2, tst, ['T']);
  if length(zt)>10 then begin
    direction:=zt[7];                              {+ or -}
    zone:=copy(zt, 8, 4);
  end else zone:='0000';;
  zt:=copy(zt, 1, 6);
  try
    result:=ScanDateTime(form, dt)+
            ScanDateTime('hhnnss', zt);
    if direction='-' then
      result:=result-ScanDateTime('hhnn', zone)
    else
      if direction='+' then result:=result+ScanDateTime('hhnn', zone);
  except
    result:=0;
  end;
end;

{Conversion ISO time stamp to internal time format.
        "date": "2020-05-09 16:14:23 +0200",
 FDR time stamp: 20181206T204438+0100}

function datBBtoDT(tst: string): TDateTime;
var dt, zt, zone: string;
    direction: char;                               {Time zone + or -}
begin
  dt:=tst.Split([' '])[0];
  zt:=tst.Split([' '])[1];
  if length(tst)>ncjs then begin
    zone:=tst.Split([' '])[2];
    if length(zone)=5 then begin
      direction:=zone[1];
      zone:=copy(zone, 2, 4)
    end else begin
      direction:='+';
      zone:='0000';
    end;
  end;
  try
    result:=ScanDateTime(ymd, dt)+
            ScanDateTime(hns, zt);
    if direction='-' then
      result:=result-ScanDateTime('hhnn', zone)
    else
      if direction='+' then
        result:=result+ScanDateTime('hhnn', zone);
  except
    result:=0;
  end;
end;

{FDR time stamp: 20181206T204438+0100}
function FDRtimeToStr(s: string): string; inline;  {FDR date conversion to string}
var tme: TDateTime;
begin
  result:='';
  tme:=datISOtoDT(s, 'yyyymmdd');
  if tme>0 then
    result:=FormatDateTime(ymd+tab1+hns, tme);
end;

{ 2018-11-17 12:12:56.979  into
  2018-11-17T12:12:56.979Z for KML or
  2018-11-17T12:12:56Z for GPX}
function GoogleTime(tp: string; gx: boolean=false): string; inline;
begin                                              {Default: KML}
  result:='';
  if length(tp)=ncjs then begin
    if gx then result:=copy(tp, 1, 19)             {suppress tenth of sec}
          else result:=tp;
    result:=result+'Z';
    result[11]:='T';
  end;
end;

function prepKeyw(k: string): string; inline;      {Column header from keyword}
begin
  result:='';
  if k<>'' then
    result:=trim(k);
  result:=StringReplace(result, uscr,' ',[rfReplaceAll]);
  result:=StringReplace(result, 'gps','GPS',[rfReplaceAll]);
  result[1]:=UpCase(result[1]);
end;

function FormatUUID(uuid: string): string;         {Beautify output UUID}
begin
  result:=uuid;                                    {default}
  if length(uuid)=32 then begin
    insert(strc, result, 21);
    insert(strc, result, 17);
    insert(strc, result, 13);
    insert(strc, result, 9);
  end;
end;

{https://parrotpilots.com/threads/serial-no.1342/
 Locate 18 digit serial number ( numbers / letters)
 count last 6 digits from the end, 7th digit will be a letter,
 this denotes "month" in my case it was 'H' which denotes August
 (8th letter of alphabet / 8th month of year)
 Next digit is the year, in my case '8' equates to 2018.}

function SerialToManufacture(const sn: string): string;
var snd: TDateTime;                                {Find date of manufacture}
    y: integer;
begin
  try
    y:=StrToInt(sn[11]);
    if y<7 then                                    {Valid from 2017 to 2026}
      y:=2020+y
    else
      y:=2010+y;
    snd:=EncodeDateTime(y, ord(upcase(sn[12]))-64, 1, 0, 0, 0, 0);
    result:='No '+copy(sn, 13, 6)+rsFrom+FormatDateTime('mmmm yyyy', snd);
  except
    result:=rsUnknown;
  end;
end;

{https://developer.parrot.com/docs/olympe/arsdkng_ardrone3_piloting.html}
function F_StateToStr(s: string): string;          {JSON flying_state (5)}
var snr: integer;
begin
  result:=rsUnknown+tab1+s;
  snr:=StrToIntDef(trim(s), 99);
  case snr of
    0: result:='Landed';
    1: result:='Taking Off';
    2: result:='Hovering';
    3: result:='Flying';
    4: result:='Landing';
    5: result:='Emergency';
    6: result:='Waiting for user take off';        {Waiting for user action to take off}
    7: result:='Motor ramping';
    8: result:='Emergency landing';                {Emergency landing state.
                               Drone autopilot has detected defective sensor(s).
                               Only Yaw argument in PCMD is taken into account.
                               All others flying commands are ignored.}
  end;
end;

{https://developer.parrot.com/docs/olympe/arsdkng_ardrone3_piloting.html}
function AlertStateToStr(s: string): string;       {JSON alert_state (6)}
var snr: integer;
begin
  result:=rsUnknown+tab1+s;
  snr:=StrToIntDef(trim(s), 99);
  case snr of
    0: result:='No alert';                         {Normal flight}
    1: result:='User emergency alert';
    2: result:='Cut out alert';                    {Something hit/blocked propeller}
    3: result:='Battery level critical';
    4: result:='Battery level low; RTH in < 3min';
    5: result:='Flight angle too high';
    6: result:='Almost empty battery alert';
    7: result:='Magnetometer is disturbed by a magnetic element';
    8: result:='Local terrestrial magnetic field is too weak';
  end;
end;

{https://parrotpilots.com/threads/anafi-thermal-data-overlay.3074/

https://forum.developer.parrot.com/t/skycontroller-vs-skycontroller2/4490/7

static const uint16_t ARDISCOVERY_Discovery_ProductTable[ARDISCOVERY_PRODUCT_MAX] =
    // BLE Service
    [ARDISCOVERY_PRODUCT_MINIDRONE]     = 0x0900,
    [ARDISCOVERY_PRODUCT_MINIDRONE_EVO_LIGHT] = 0x0907,
    [ARDISCOVERY_PRODUCT_MINIDRONE_EVO_BRICK] = 0x0909,
    [ARDISCOVERY_PRODUCT_MINIDRONE_EVO_HYDROFOIL] = 0x090a,
    [ARDISCOVERY_PRODUCT_MINIDRONE_DELOS3] = 0x090b,
    [ARDISCOVERY_PRODUCT_MINIDRONE_WINGX] = 0x0910,

    // NSNet Service
    [ARDISCOVERY_PRODUCT_ARDRONE]       = 0x0901,
    [ARDISCOVERY_PRODUCT_JS]            = 0x0902,
    [ARDISCOVERY_PRODUCT_SKYCONTROLLER] = 0x0903,
    [ARDISCOVERY_PRODUCT_JS_EVO_LIGHT]  = 0x0905,
    [ARDISCOVERY_PRODUCT_JS_EVO_RACE]   = 0x0906,
    [ARDISCOVERY_PRODUCT_BEBOP_2]       = 0x090c,
    [ARDISCOVERY_PRODUCT_POWER_UP]      = 0x090d,
    [ARDISCOVERY_PRODUCT_EVINRUDE]      = 0x090e,
    [ARDISCOVERY_PRODUCT_UNKNOWNPRODUCT_4] = 0x0911,

    // USB Service
    [ARDISCOVERY_PRODUCT_SKYCONTROLLER_2] = 0x090f,

    // Unsupported Service
    [ARDISCOVERY_PRODUCT_TINOS] = 0x0912,
}

function ProdIDtoLabel(s: string): string;         {Parrot Product IDs}
var prID: integer;
begin
  result:=s;                                       {Default}
  try
    prID:=StrToInt(s);
  except
    prID:=0;
  end;
  case prID of
    2305: result:='Bebop 1';                       {901'h}
    2307: result:='Skycontroller';                 {903'h}
    2316: result:='Bebop 2';                       {90c'h}
    2318: result:='Disco';                         {90e'h}
    2319: result:='Skycontroller 2';               {90f'h}
    2324: result:='Anafi 4K';                      {914'h}
    2328: result:='Remote controller';             {not known to what is is related}
    2329: result:='Anafi Thermal';                 {919'h}
  end;
end;

{possibly this: olympe.enums.animation.type}
function FlipTypeToStr(s: string): string;         {JSON flip_type (20)}
var snr: integer;
begin
  result:=rsUnknown+tab1+s;
  snr:=StrToIntDef(trim(s), 99);
  case snr of
    0: result:='None';                             {Normal flight ?}
    1: result:='Front';
    2: result:='Back';
    3: result:='Right';                            {its right side goes up, is it left?}
    4: result:='Left';                             {its left side goes up, is it right?}
  end;
end;

function GPSerrToStr(s: string): string;           {JSON GPS_pos_err (11)}
var snr: integer;
begin
  result:=rsUnknown+tab1+s;
  snr:=StrToIntDef(trim(s), 99);
  case snr of
    0: result:='No error';                         {Normal flight ?}
    1: result:='Not in outdoor mode';
    2: result:='GPS not fixed';
    3: result:='Compass not calibrated';
  end;
end;

function GPSavailToStr(s: string): string;         {JSON GPS_available (0)}
begin
  result:=rsGPSnotAvailable;
  if LowerCase(s)='true' then
    result:=rsGPSavailable;
end;

function AltHeaderToStr(idx: integer): string;
begin
  result:=rsUnKnown;                               {If idx is not in range}
  case idx of
     0: result:=rsDateTime;
     1: result:=ahdr0;                             {Time since boot [ms]'}
     2: result:=ahdr1;                             {Battery Charge Level [%]}
     3: result:=ahdr2;                             {Controller Latitude}
     4: result:=ahdr3;                             {Controller Longitude}
     5: result:=ahdr4;                             {Flying Mode}
     6: result:=ahdr5;                             {Alert_state}
     7: result:=ahdr6;                             {WiFi Signal Strength [dBm]}
     8: result:=ahdr7;                             {Anafi GPS Lock (Yes/No)}
     9: result:=ahdr8;                             {Anafi Longitude}
    10: result:=ahdr9;                             {Anafi Latitude}
    11: result:=ahdr10;                            {Anafi GPS Error}
    12: result:=ahdr11;                            {Number of Sats}
    13: result:=ahdr12;                            {Forward Speed}
    14: result:=ahdr13;                            {Sideways Speed}
    15: result:=ahdr14;                            {Vertical Speed}
    16: result:=ahdr15;                            {Roll Angle}
    17: result:=ahdr16;                            {Pitch Angle}
    18: result:=ahdr17;                            {Compass Heading}
    19: result:=ahdr18;                            {Altitude above take off}
    20: result:=ahdr19;                            {Flip type}
    21: result:=ahdr20;                            {Ground Speed}
    22: result:=ahdr21;                            {Distance Anafi to RC}
  end;
end;

{http://www.joerg-buchwitz.de/temp/googlemapssyntax.htm
 https://www.google.de/maps?q=48.235367,10.0944453&z=13&om=0 }

function URLGMap(lati, long: string): string; {URL for coordinates in Google Maps}
begin
  result:=gmapURL+'?q='+lati+','+long+'&z='+
                        gzoom+'&t=h&om=0';         {&t=k: Sat, &t=h: hybrid}
end;

{ http://wiki.openstreetmap.org/wiki/Browsing
 http://www.openstreetmap.org/?mlat=49.9676&mlon=9.9673#map=10/49.9676/9.9673&layers=Q}

function URLosm(lati, long: string): string; {URL for coordinates in OpenStreetMap}
begin
  result:=osmURL+'?mlat='+lati+'&mlon='+long+'#map='+
          gzoom+'/'+lati+'/'+long+'&layers=S';
end;

function OpenManual: boolean;                      {Open manual local or from WWW}
var s: string;
begin
  s:=Application.Location+manual;
  if FileExists(s) then
    result:=OpenDocument(s)
  else
    result:=OpenURL(homepage+hpmydat+manual);
end;

{Distance between coordinaten in m according
 Haversine formula, Radius: 6,371km depending on latitude
 https://rechneronline.de/erdradius/
 6365.692 optimized for 50° latitude and 60m altitude}
function DeltaKoord(lat1, lon1, lat2, lon2: double): double; inline;
begin
  result:=0;
  try
    result:=6365692*arccos(sin(lat1*pi/180)*sin(lat2*pi/180)+
            cos(lat1*pi/180)*cos(lat2*pi/180)*cos((lon1-lon2)*pi/180));

    if IsNan(result) or
       (result>30000) or                           {> 30km = implausible values}
       (result<0.005) then                         {reduce errors due to GPS uncertainty}
      result:=0;
  except
    result:=0;
  end;
end;

procedure ShowAbout;                               {AboutBox}
begin
  if MessageDlg(appName+sLineBreak+appVersion+sLineBreak+sLineBreak+
             'Build: '+appBuildno+
              sLineBreak+sLineBreak+rsContact+': '+email,
              mtInformation,[mbHelp, mbOK], 0)=mrNone then
  OpenManual;
end;

function CleanNum(const s: string): string; inline; {Filter digits from String}
var i: integer;
begin
  result:='';
  for i:=1 to length(s) do
    if s[i] in ziff then
      result:=result+s[i];
end;

function RadToGrad180(const r: double): double;        {Radiant to +/-180°}
begin
  result:=r*180/pi;
end;

function RadToGrad360(const r: double): double;        {Radiant to 0..360°}
begin
  result:=(RadToGrad180(r)+360) mod 360;
end;


//////////////////////////// Sub-thread overview //////////////////////////////

constructor TMyThread.Create(CreateSuspended : boolean);
begin
  FreeOnTerminate:=true;                           {Clean up after finished}
  inherited Create(CreateSuspended);
end;

procedure TMyThread.EnableSort;                    {Enables sort after finished}
begin
  Form1.ovGrid.ColumnClickSorts:=true;
  Form1.btLogBook.Enabled:=true;
  Form1.mmnLogBook.Enabled:=true;
end;

procedure TMyThread.GetSettings;                   {Metric / Imperial / Settings}
begin
  metr:=Form1.grpUnit.ItemIndex;
  numfiles:=Form1.ovGrid.RowCount-1;
  dirname:=Form1.LogDir.Directory;
end;

procedure TMyThread.GetFileName;                   {Get current file number}
begin
  filename:=Form1.ovGrid.Cells[0, idx];
end;


procedure TMyThread.Execute;                       {Scan files}
var i, k, batt, battmin, fmode: integer;
    fn: string;
    inf: TFileStream;
    j0, j1, j2, j3: TJsonData;                     {4 level}
    w, tasmax, altmax, distmax, route: double;
    latp, lonp, latc, lonc, lat1, lon1: double;
    tme: TDateTime;
    gpsfix: boolean;
    timebase, timest, vx, vy, vz: double;

//    typelist: TStringList;
//    typestr: string;

{Copies of the two functions to make it Thread-save}
  function UnitToStr(idx: integer;                 {Measurement unit string}
                     pure: boolean;                {Brackets no/yes}
                     converted: boolean=false): string; {alternative unit like km/h}
  begin
    result:='';                                    {No measurement unit}
    case idx of                                    {Set metric}
      1: result:='ms';
      2: result:='%';
      7: result:='dBm';
      13..15: result:='m/s';
      16..18: if converted then result:='°' else result:='rad';
      19: result:='m';
      21: if converted then result:='km/h' else result:='m/s';
      22: if converted then result:='km' else result:='m';
      23: result:='V';
    end;
    if metr=1 then                                 {Overwrite with imperial}
      case idx of
        13..15: result:='ft/s';
        19: result:='ft';
        21: if converted then result:='mph' else result:='ft/s';
        22: if converted then result:='mi' else result:='ft';
      end;
    if (not pure) and                              {With brackets}
       (result<>'') then
      result:='['+result+']';
  end;

  function ConvUnit(idx: integer;                  {Index column}
                         value: double;            {input}
                         converted: boolean=false): double;
            {Possible conversion: rad to °, m/s to km/h, ft to mi, ft/s to mph}
  begin
    result:=value;                                 {metric to metric, ft to ft}
    case idx of
      13..15, 19: if metric and
                     (metr=1) then                 {Imperial set}
                       result:=value*mtoft;        {m/s --> ft/s}
      16, 17: if converted then
                result:=RadToGrad180(value);       {rad to ° +/-180}
      18:     if converted then
                result:=RadToGrad360(value);       {rad to ° 0..360, 0 is north}

  {possibly converted values m --> km}
      21: begin
            if metric and                          {Speed: m/s --> ft/s}
               (metr=1) then                       {Imperial set}
                 result:=value*mtoft;              {ft/s}
            if converted then begin                {overwrite}
              if metr=1 then
                result:=result*0.68181818          {mph}
              else
                result:=result*3.6;                {km/h}
            end;
          end;
      22: begin
            if metric and                          {Distance: m --> ft}
               (metr=1) then                       {Imperial set}
              result:=value*mtoft;                 {ft}
            if converted then begin                {overwrite}
              if metr=1 then
                result:=result*0.00018939          {mi (miles)}
              else
                result:=result/1000;               {km}
            end;
          end;
    end;
  end;

begin
  metr:=0;                                         {default Metric}
  Synchronize(@GetSettings);                       {Read settings}
  for i:=1 to numfiles do begin                    {for each file}
    for k:=0 to High(res) do                       {delete result array}
      res[k]:='';
    tasmax:=0;
    altmax:=-9999;
    distmax:=0;
    battmin:=99999;
    tme:=0;
    lat1:=0;
    lon1:=0;
    route:=0;
    gpsfix:=false;
    idx:=i;                                        {Index of the file in use}
    Synchronize(@GetFileName);
    fn:=IncludeTrailingPathDelimiter(dirname)+filename+jext;
    if FileExists(fn) then begin
      if terminated then
        break;
      inf:=TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
      if inf.Size>512 then begin                   {0.5kB minimal}
        j0:=GetJson(inf);                          {load whole JSON file, level 0}
        try

 ///////////// Blackbox JSON format ////////////////
          j1:=j0.FindPath(json1hz);                {Test blackbox file}
          if j1<>nil then begin                    {1Hz data}
            j2:=j1.Items[j1.Count-1];              {1Hz Last dataset}
            if j2<>nil then begin
              timest:=j2.FindPath(jsonTimest).AsFloat;
              battmin:=j2.FindPath(jtypVolt).AsInteger;  {Voltage in mV}
            end;
            j2:=j1.Items[0];                       {1Hz first dataset}
            if j2<> nil then begin
              w:=j2.FindPath(jsonTimest).AsFloat;  {temporäry value}
              res[9]:=FormatFloat(frmOut2,(j2.FindPath(jtypVolt).AsInteger)/1000)+
                      UnitToStr(23, true);         {Voltage max}
            end;
            for k:=1 to j1.Count-1 do begin
              j2:=j1.Items[k];                     {Read voltage}
              if j2<>nil then begin
                batt:=j2.FindPath(jtypVolt).AsInteger;
                if batt<battmin then
                  battmin:=batt;
              end;
            end;
            res[10]:=FormatFloat(frmOut2,battmin/1000)+
                     UnitToStr(23, true);          {Voltage min}

            j1:=j0.FindPath(jsonHeaderB);          {Header for time base}
            if j1<>nil then begin
              tme:=datBBtoDT(j1.FindPath(datUTC).AsString);
              res[0]:=j1.FindPath(datProdSerial).AsString;
              res[2]:=FormatDateTime(hns, tme);
              timebase:=j1.FindPath(datTimeBase).AsFloat;
              if timebase>w then                   {Compared to 1st dataset 1Hz}
                timebase:=timebase/1000;           {Correction for Android}
            end;

            j1:=j0.FindPath(json5hz);              {5Hz data}
            if j1<>nil then begin
              j2:=j1.Items[j1.Count-1];            {5Hz Last dataset}
              if j2<>nil then begin
                w:=j2.FindPath(jsonTimest).AsFloat;
                if w>timest then                   {find latest time stamp}
                  timest:=w;
              end;

              for k:=0 to j1.Count-1 do begin      {check all datasets}
                j2:=j1.Items[k];
                if j2<>nil then begin              {find 5Hz data}
                  w:=j2.FindPath(jtypProdAlt).AsFloat;
                  if w>altmax then                 {Product Altitude}
                    altmax:=w;
                  j3:=j2.FindPath(jtypProdSpeed);
                  if j3<>nil then begin            {Speed values}
                    vx:=j3.FindPath(jtypVX).AsFloat;
                    vy:=j3.FindPath(jtypVY).AsFloat;
                    vz:=j3.FindPath(jtypVZ).AsFloat;
                  end;
                  w:=sqrt((vx*vx)+(vy*vy)+(vz*vz));
                  if w>tasmax then                 {w is TAS}
                    tasmax:=w;
                end;
              end;
            end;

            j1:=j0.FindPath(jsonDatas);            {Datas}
            if j1<>nil then begin
              for k:=0 to j1.Count-1 do begin      {check all datasets}
                j2:=j1.Items[k];
                if j2<>nil then begin              {Find GPS fix}
                  if j2.FindPath(jsonType).AsString=jtypGPSfix then
                    if j2.FindPath(jsonDatas).AsInteger>0 then begin
                      gpsfix:=true;
                      break;
                    end;
                end;
              end;

(*
              typelist:=TStringList.Create;
              for k:=0 to j1.Count-1 do begin
                j2:=j1.Items[k];                   {Test only: List of types}
                if j2<>nil then begin
//                  if j2.FindPath(jsonType).AsString=jtypProdFState then begin
//                  typestr:=j2.FindPath(jsonDatas).AsString;
                  typestr:=j2.FindPath(jsonType).AsString;
                    if typelist.IndexOf(typestr)<0 then
                      typelist.Add(typestr);

//                  end;
                end;
              end;
              typelist.SaveToFile('D:\temp\types.txt');
              typelist.Free;            *)

            end;
            timest:=(timest-timebase)/secpd;       {Duration as TDateTime}
            res[3]:=FormatDateTime(hns, tme+timest);
            res[4]:=FormatDateTime(hns, timest);
          end;                                     {End Blackbox file}

///////////// legacy/easy JSON format ////////////////
          j1:=j0.FindPath(jsonData);               {Datasets, Level 1, JSON file}
          if j1<>nil then begin

            tme:=datISOtoDT(j0.FindPath(datUTC).AsString, ymd);
            res[4]:=FormatDateTime(hns,            {Duration}
                                   SekToDT(j0.FindPath(datRunTime).AsString, 1));
            res[0]:=j0.FindPath(datSerialNo).AsString;
            res[11]:='"'+URLGMap(FormatFloat(frmCoord,
                                 j0.FindPath(datGPSlat).AsFloat),   {Homepoint}
                                 FormatFloat(frmCoord,
                                 j0.FindPath(datGPSlon).AsFloat))+'"';

            j2:=j1.Items[0];                       {First dataset: Begin time}
            if j2<>nil then begin
              res[2]:=FormatDateTime(hns, SekToDT(j2.Items[0].AsString, 1)+tme);
              res[9]:=IntToStr(j2.Items[1].AsInteger)+ {Battery level max}
                      UnitToStr(2, true);
            end;
            j2:=j1.Items[j1.Count-1];              {Last dataset: End time}
            if j2<>nil then
              res[3]:=FormatDateTime(hns, SekToDT(j2.Items[0].AsString, 1)+tme);
            for k:=0 to j1.Count-1 do begin        {Read all datasets}
              j2:=j1.Items[k];                     {read data, level 2}
              if j2<>nil then begin
                fmode:=j2.Items[4].AsInteger;      {Flying state}
                if (fmode=1) and
                    j2.Items[7].AsBoolean then
                  gpsfix:=true;
                batt:=j2.Items[1].AsInteger;       {Battery charge level}
                if batt<battmin then
                  battmin:=batt;
                latc:=j2.Items[2].AsFloat;         {Read coordinates}
                lonc:=j2.Items[3].AsFloat;
                lonp:=j2.Items[8].AsFloat;
                latp:=j2.Items[9].AsFloat;
                if ((lat1<>0) or (lon1<>0)) and
                   ((latp<>0) or (lonp<>0)) then begin {Length route}
                  w:=DeltaKoord(latp, lonp, lat1, lon1);
                  if not IsNan(w) then route:=route+w;
                end;
                lat1:=latp;
                lon1:=lonp;
                w:=j2.Items[18].AsFloat;           {Altitude}
                if w>altmax then
                  altmax:=w;
                w:=j2.Items[20].AsFloat;           {TAS}
                if w>tasmax then
                  tasmax:=w;
                w:=DeltaKoord(latc, lonc, latp, lonp); {Distance to RC}
                if (not IsNan(w)) and (w>distmax) then
                  distmax:=w;
              end;
              res[6]:=FormatFloat(frmOut1, ConvUnit(22, distmax, useau))+
                                           UnitToStr(22, true, useau);
              res[7]:=FormatFloat(frmOut1, ConvUnit(22, route, useau))+
                                           UnitToStr(22, true, useau);
              res[10]:=IntToStr(battmin)+          {min Battery level}
                                UnitToStr(2, true);
            end;
          end;                                     {End JSON file}
          res[1]:=FormatDateTime(ymd, tme);        {Date/Time from meta data}
          res[5]:=FormatFloat(frmOut1, ConvUnit(19, altmax))+
                                       UnitToStr(19, true);
          res[8]:=FormatFloat(frmOut2, ConvUnit(21, tasmax, true))+
                                       UnitToStr(21, true, true);
          if gpsfix then
            res[12]:=rsYes
          else
            res[12]:=rsNo;

          if terminated then
            break;                                 {Jump out before write}
          Synchronize(@WriteResults);
        finally
          inf.Free;
          j0.Free;
        end;
      end;
    end;                                           {End if File exists}
  end;                                             {End for each file}
  Synchronize(@EnableSort);                        {Enables sort again}
end;

procedure TMyThread.WriteResults;
var i: integer;                     {Write results per file into overview table}
    s: string;
begin
  for i:=1 to 10 do begin
    Form1.ovGrid.Cells[i, idx]:=res[i];
    Form1.ovGrid.AutoSizeColumn(i);
  end;

  s:='';
  for i:=0 to high(res) do
    s:=s+res[i]+isep;
  Form1.ovGrid.Cells[hc, idx]:=s;                  {Write into hidden column}
end;

//////////////////////////////// Main thread ///////////////////////////////////

procedure TForm1.FormCreate(Sender: TObject);      {Initialize the application}
var i: integer;
begin
  Form1.Tag:=1;                                    {First start}
  DefaultFormatSettings.DecimalSeparator:='.';
  for i:=0 to High(hdrList) do
    hdrList[i]:='';
  FDRDialog.Filter:=rsExtFilter;

  Caption:=appName+tab1+appVersion;
  Hint:=Caption;
  LogDir.TextHint:=hntLogDir;
  LogDir.Hint:=hntLogDir;
  LogDir.DialogTitle:=hntLogDir;
  LogDirDialog.Title:=hntLogDir;
  lblSelDir.Caption:=capLogDir;
  lblSelDir.Hint:=hntLogDir;
  lblStat.Caption:=rsStatistics;
  lblFDRresult.Caption:=capFDRresult;
  Chart1.Hint:=hntChart1;
  Chart2.Hint:=hntChart2;
  FDRdir.Hint:=hntFDRdir;
  edUUID.Hint:=hntedUUID;
  edUUID.TextHint:=hntedUUID;
  btSearchFDR.Caption:=capbtSearch;
  btSearchFDR.Hint:=hntbtSearch;

  lblManual.Caption:=rsManual;
  lblManual.Hint:=Application.Location+manual;     {default}
  if not FileExists(lblManual.Hint) then begin
    lblManual.Hint:=homepage+hpmydat+manual;       {Overwrite with inet link}
  end else begin
  {$IFDEF DARWIN}
    lblManual.Hint:=manual;                        {overwrite for MAC OS X}
  {$ENDIF}
  end;
  lblDownload.Caption:=rsLatest;
  lblDownload.Hint:=homepage+downURL;
  lblGitHub.Hint:=githublink;
  lblGitHub.Caption:=capGitHub;

  cbExtrude.Caption:=capExtrude;
  cbExtrude.Hint:=hntExtrude;
  cbHeader.Caption:=capHeader;
  cbHeader.Hint:=hntHeader;
  cbCSVsep.Caption:=capCSVsep;
  cbCSVsep.Hint:=hntCSVsep;
  cbDegree.Caption:=capDegree;
  cbDegree.Hint:=hntDegree;
  cbDegree.Tag:=0;

{Menus}
  mmnFile.Caption:=mniFile;
  mmnOpen.Caption:=mniOpen;
  mmnJump.Caption:=mniJump;
  mmnCSVex.Caption:=mniCSVex;
  mmnKMLex.Caption:=mniKMLex;
  mmnGPXex.Caption:=mniGPXex;
  mmnClose.Caption:=mniClose;

  mmnTools.Caption:=mniTools;
  mmnSettings.Caption:=mniSettings;
  mmnScrShot.Caption:=mniScrShot;
  mmnTas.Caption:=mniTas;
  mmnLogBook.Caption:=mniLogBook;
  mmnRename.Caption:=mniRename;
  mmnFDRlog.Caption:=mniFDRlog;

  mmnHelp.Caption:=mniHelp;
  mmnManual.Caption:=rsManual;
  mmnHomepage.Caption:=mniHomepage;
  mmnDownload.Caption:=rsLatest;
  mmnInfo.Caption:=mniInfo;

  cmnClipbrd.Caption:=mniCopy;
  cmnSaveAs.Caption:=mniFileSave;

  cmnClipbrd2.Caption:=mniCopy;
  cmnSaveAs2.Caption:=mniFileSave;
  cmnShowGM.Caption:=mniShowGM;
  cmnShowOSM.Caption:=mniShowOSM;

 {Buttons}
  btClose.Caption:=capClose;
  btClose.Hint:=hntClose;
  btScrShot.Caption:=capScrShot;
  btScrShot.Hint:=hntScrShot;
  btConv.Caption:=capBtnConv;
  btConv.Hint:=hntBtnConf;
  btConv.Enabled:=false;
  btColor.Caption:=capColor;
  btColor.Hint:=hntColor;
  btColor.ButtonColor:=clDarkOrange;
  btLogBook.Caption:=capCrLogBook;

  grpUnit.Caption:=capUnit;
  grpUnit.Hint:=hntUnit;
  grpConv.Caption:=capConvSel;
  grpConv.Hint:=hntConv;
  grpDia.Caption:=capDia;
  grpDia.Hint:=hntDia;
  grpLogBook.Caption:=capLogBook;
  grpLogBook.Hint:=hntLogBook;

  boxConv.Caption:=capConv;
  boxConv.Hint:=hntBoxConv;
  boxLogBook.Caption:=rsLogBook;
  boxLogBook.Hint:=hntCrLogBook;
  boxTable.Caption:=capTable;
  boxTable.Hint:=hntTable;

  TabSheet1.Caption:=thdOverview;
  TabSheet2.Caption:=thdData;
  TabSheet3.Caption:=thdDia;
  TabSheet4.Caption:=thdDetails;
  TabSheet5.Caption:=thdSettings;
  TabSheet6.Caption:=thdAtti;

  Chart2ConstantLine1.Title:=clnLeft;              {Setup attitude charts}
  Chart2ConstantLine2.Title:=clnRight;
  Chart2ConstantLine1.SeriesColor:=Chart2LineSeries1.SeriesColor;
  Chart2ConstantLine2.SeriesColor:=Chart2LineSeries4.SeriesColor;
  Chart2.AxisList[0].Title.LabelFont.Color:=Chart2ConstantLine1.SeriesColor;
  Chart2.AxisList[2].Title.LabelFont.Color:=Chart2ConstantLine2.SeriesColor;
  lbLegende.Hint:=hntChartListBox;
  for i:=0 to lbLegende.SeriesCount-1 do           {Set all as default}
    lbLegende.Checked[i]:=true;

  csvGrid.ColCount:=ncjs;                          {Data columns, fix number of cols (23)}
  csvGrid.RowCount:=6;
  csvGrid.Cells[0, 0]:=rsDateTime;
  ovGrid.Tag:=1;                                   {Start with first file}
  csvGrid.Tag:=1;

  ovGrid.ColCount:=hc+1;                           {Last column hidden}
  ovGrid.RowCount:=1;
  for i:=1 to 10 do                                {Write overview header}
    ovGrid.Cells[i, 0]:=ovheader[i];
  ovGrid.ColWidths[hc]:=0;                         {Hidden column for log book}

  MetaGridInit(0);                                 {Standard JSON labels}

  staGrid.RowCount:=6;
  staGrid.Cells[0, 0]:=rsStatistics;
  staGrid.Cells[1, 0]:=staHd1;
  staGrid.Cells[2, 0]:=staHd2;
  staGrid.Cells[0, 1]:=ovAltMax;
  staGrid.Cells[0, 2]:=ovDistMax;
  staGrid.Cells[0, 3]:=ovTasMax;
  staGrid.Cells[0, 4]:=ovBattMax;
  staGrid.Cells[0, 5]:=ovBattMin;
  staGrid.Width:=staGrid.ColWidths[0]+staGrid.ColWidths[1]+staGrid.ColWidths[2]+4;
end;

procedure TForm1.MetaGridInit(mode: integer);      {Set Details labels}
begin
  dtlGrid.Tag:=mode;
  case mode of
    0: begin
         lblDetails.Caption:=rsMetaDataJ;          {Meta data JSON}
         dtlGrid.Cells[0, 0]:=dtlJCol0;
         dtlGrid.Cells[1, 0]:=dtlJCol1;
         dtlGrid.RowCount:=16;
       end;
    1: begin
         lblDetails.Caption:=rsMetaDataF;          {Meta data FDR}
         dtlGrid.Cells[0, 0]:=dtlFCol0;
         dtlGrid.Cells[1, 0]:=dtlFCol1;
         dtlGrid.RowCount:=1;
       end;
    2: begin
         lblDetails.Caption:=rsMetaDataB;          {Meta data Blackbox}
         dtlGrid.Cells[0, 0]:=dtlFCol0;
         dtlGrid.Cells[1, 0]:=dtlFCol1;
         dtlGrid.RowCount:=1;
       end;
  end;
end;

procedure TForm1.FormDblClick(Sender: TObject);    {About box on DblClick}
begin
  ShowAbout;
end;

function FindDirName(d: string): string;           {Check if JSON file dir}
begin
  result:=d;
  if pos(jext, d)>0 then begin                     {File dropped}
    result:=ExtractFileDir(d);
  end else begin                                   {Directory dropped}
    if not DirectoryExists(d) then                 {or other file name}
      result:=ExtractFileDir(d);
  end;
end;

procedure TForm1.FormDropFiles(Sender: TObject;    {Drag& Drop to app window}
                               const FileNames: array of String);
var dir: string;
begin
  dir:=FindDirName(FileNames[0]);
  if DirectoryExists(dir) then begin
    LogDir.Directory:=dir;
    BuildList;
  end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; {Reload by F5}
                           Shift: TShiftState);
begin
  if (key=vk_F5) and (Form1.Tag=1) then
    BuildList;
  if key=VK_ESCAPE then
    Close;
end;

function TForm1.GetCSVsep: char;                   {Define CSV data seperator}
begin
  result:=',';                                     {Default}
  if cbCSVsep.Checked then
    result:=isep;                                  {Alternative data seperator}
end;

procedure TForm1.DoForm2Show(p: integer);          {Show additional chart, width=p}
begin                                              {p=0 --> Breite Form1}
  Form2.Show;                                      {show above the main window}
  if p=0 then
    Form2.Width:=PageControl1.Width
  else
    Form2.Width:=p;
  Form2.csvsep:=GetCSVsep;
  Form2.Left:=Form1.Left+PageControl1.Left;        {Left aligned to the tabs}
  Form2.Top:=Form1.Top-Form2.Height-20;            {20 pixel correction}
  if Form2.Top<Screen.DesktopTop then
    Form2.Top:=Screen.DesktopTop;
end;

function TForm1.ColorToKMLColor(const AColor: TColor;   {Google Farbcodierung}
                                sat: integer=255): string;    {Saturation=255}
begin
  Result:=IntToHex(sat, 2)+IntToHex(ColorToRgb(AColor), 6);
end;

procedure TForm1.BlackboxHeader;                   {Create column header for Blackbox files}
var
  i: integer;

begin
  for i:=2 to ncjs-1 do                            {Legacy JSON header}
    csvGrid.Cells[i, 0]:=AltHeaderToStr(i);
  csvGrid.Cells[1, 0]:=ahdr22;                     {Changed columns compared to legacy JSON}
  csvGrid.Cells[11, 0]:=ahdr24;
  csvGrid.Cells[12, 0]:=ahdr23;                    {Motor error}
  csvGrid.Cells[20, 0]:='FP state';                {Is this flip state?}

  for i:=0 to high(Boxheader) do                   {New columns}
    csvGrid.Cells[i+ncjs, 0]:=Boxheader[i];

end;

procedure TForm1.ChangeHeader;                     {Change column header}
var i: integer;
begin
  if cbHeader.Checked then begin
    for i:=1 to ncjs-1 do
      csvGrid.Cells[i, 0]:=AltHeaderToStr(i);
  end else begin
    for i:=0 to 20 do
      csvGrid.Cells[i+1, 0]:=hdrList[i];
    csvGrid.Cells[22, 0]:=csvDist;                 {Dist to RC}
  end;
end;

procedure TForm1.grpConvClick(Sender: TObject);    {output format changed}
begin
  StatusBar1.Panels[3].Text:=grpConv.Items[grpConv.ItemIndex];
end;

procedure TForm1.grpUnitClick(Sender: TObject);    {Unit setting changed}
begin
  cbDegree.Tag:=1;
end;

procedure TForm1.lblDownloadClick(Sender: TObject);        {Click link homepage}
begin
  CheckVersion;
end;

function DoDownload: string;                      {Download new version}
begin
  if OpenURL(homepage+DownURL) then
    result:=rsDownloading
  else
    result:=errDownloading;
end;

procedure TForm1.CheckVersion;                     {Call version file and check}
var strm: TStream;
    inlist: TStringList;
    i: integer;
    ct: string;
begin
  inlist:=TStringList.Create;
  Screen.Cursor:=crHourGlass;
  ct:='';
  strm:=nil;
  try
    try
      ipHTMLin.Reference(AppName);
      if ipHTMLin.CheckURL(homepage+versfile, ct) then
        strm:=ipHTMLin.DoGetStream(homepage+versfile);
    except
      on e: Exception do begin
        StatusBar1.Panels[4].Text:=e.Message;
        exit;
      end;
    end;
    if (strm<>nil) and (strm.Size>0) then
      inlist.LoadFromStream(strm);
    if inlist.count>0 then begin
      lblDownload.Font.Color:=clPurple;
      for i:=0 to inlist.count-1 do begin
        if pos(AppName, inlist[i])>0 then begin
          ct:=inlist[i].Split([','])[1];
          if ct>=appBuildno then begin
            MessageDlg(rsLatestVersion+sLineBreak+sLineBreak+
                       Form1.Caption+sLineBreak+'Build: '+appBuildno,
                       mtInformation,[mbOK],0);
          end else
            StatusBar1.Panels[4].Text:=DoDownload;  {Download new version}
          break;
        end;
      end;
    end else
      StatusBar1.Panels[4].Text:=DoDownload;        {Download new version}
  finally
    inlist.Free;
    Screen.Cursor:=crDefault;
  end;
end;


procedure TForm1.lblDownloadMouseEnter(Sender: TObject);   {Animate link}
begin
  lblDownload.Font.Style:=lblDownload.Font.Style+[fsBold];
end;

procedure TForm1.lblDownloadMouseLeave(Sender: TObject);   {Animate link}
begin
  lblDownload.Font.Style:=lblDownload.Font.Style-[fsBold];
end;

procedure TForm1.lblGitHubClick(Sender: TObject);          {Click link Git repo}
begin
  if OpenURL(lblGitHub.Hint) then
    lblGitHub.Font.Color:=clPurple;
end;

procedure TForm1.lblGitHubMouseEnter(Sender: TObject);     {GitHub link animated}
begin
  lblGitHub.Font.Style:=lblGitHub.Font.Style+[fsBold];
end;

procedure TForm1.lblGitHubMouseLeave(Sender: TObject);
begin
  lblGitHub.Font.Style:=lblGitHub.Font.Style-[fsBold];
end;

procedure TForm1.lblManualClick(Sender: TObject);          {Click link manual}
begin
  if OpenManual then
    lblManual.Font.Color:=clPurple;
end;

procedure TForm1.lblManualMouseEnter(Sender: TObject);     {Animate link}
begin
  lblManual.Font.Style:=lblDownload.Font.Style+[fsBold];
end;

procedure TForm1.lblManualMouseLeave(Sender: TObject);     {Animate link}
begin
  lblManual.Font.Style:=lblDownload.Font.Style-[fsBold];
end;

procedure TForm1.ListBox1Click(Sender: TObject);   {Show FDR meta data from file}
begin
  if ListBox1.ItemIndex>=0 then begin
    ShowFDRlog(ListBox1.Items[ListBox1.ItemIndex], 0);
  end;
end;

procedure TForm1.LogDirChange(Sender: TObject);    {Select dir by dialog}
begin
  BuildList;                                       {search JSON files}
end;

procedure TForm1.LogDirDblClick(Sender: TObject);  {Call file explorer}
begin
  OpenDocument(LogDir.Directory);
end;

procedure TForm1.mmnDownloadClick(Sender: TObject); {Main menu help/downlod}
begin
  CheckVersion;
end;

procedure TForm1.mmnFDRlogClick(Sender: TObject);  {Menu Show meta data from FDR}
begin
  if FDRdialog.Execute then begin
    ShowFDRlog(FDRdialog.FileName);
    if FDRdir.Text='' then
      FDRdir.Directory:=FDRdialog.InitialDir;
  end;
end;

{
hardware			anafi4k
product.model.id		0914
product.board_id		0
product.usb.pid			0x6001
build.date			Thu Oct  3 16:38:39 UTC 2019
parrot.build.group		drones
parrot.build.product		anafi
parrot.build.project		anafi
parrot.build.region		20
parrot.build.uid		anafi-4k-1.6.1
parrot.build.variant		4k
parrot.build.version		1.6.1
revision			3
serialno			23
factory.hcam_serial		PI020739AA8F021162
factory.serial			PI040416AA8G033000
factory.product.pro		13
boot.uuid			384CBC80CA7FBC9AA791FEA6D55ED016
smartbattery.gfw_version	26100020001900038502
smartbattery.usb_version	0.10
smartbattery.version		1.0.9.0
smartbattery.serial		B8I226166
smartbattery.hw_version		4
smartbattery.design_cap		2700
smartbattery.device_info	Naxos
esc.fw_version			1.19.R.4
esc.hw_version			2
ddr_info.sync			7:9:7:7
smartbattery.cycle_count	28
smartbattery.soh		100
index				1
date				20191115T141035+0000
}
function TForm1.ShowFDRlog(fn: string; mode: integer=0): string; {Show meta data from FDR}
var buf: array [0..2047] of byte;
    FDRfile: file;
    i: integer;
    str, str1, str2: string;

const pID='ro.';

  procedure TestResult;
  begin
    if str<>'' then begin
      if (pos(pID, str)=1) or                      {Is it 'ro.'... or 'date'?}
         (pos('_info', str)>0) or
         (str='index') or
         (str=datUTC) then begin
        str1:=StringReplace(str, pID, '', []);     {Parameter ID}
        str2:='';                                  {Delete old value}
        buf[i+1]:=0;                               {delete next lenght indicator}
      end else begin
        if str2='' then
          str2:=str;                               {String value}
      end;
    end;
    if (mode=1) and (str1='boot.uuid') and (str2<>'') then begin
      result:=str2;
      exit;                                        {go out when UUID found}
    end;
    if (mode=0) and
       (str1<>'') and (str2<>'') then begin        {Parameter ID and Value available}
      if str1=datUTC then                          {Set date/time in header}
        lblDetails.Caption:=rsMetaDataF+tab1+ovFrom+tab1+FDRtimeToStr(str2);
      if str1='boot.uuid' then begin               {Save UUID}
        edUUID.Text:=str2;
        str2:=FormatUUID(str2);
      end;
      dtlGrid.RowCount:=dtlGrid.RowCount+1;        {New dataset - new line in table}
      dtlGrid.Cells[0, dtlGrid.RowCount-1]:=str1;  {Fill table Parameter ID}
      dtlGrid.Cells[1, dtlGrid.RowCount-1]:=str2;  {and value}
      str1:='';                                    {Empty all data}
      str2:='';
    end;
    str:='';
  end;

begin
  result:='';
  if FileSize(fn)>SizeOf(buf) then begin
    if mode=0 then begin
      MetaGridInit(1);
      lblDetails.Caption:=rsMetaDataF+tab1+ExtractFileName(fn);
      StatusBar1.Panels[4].Text:=fn;
    end;
    AssignFile(FDRfile, fn);
    Reset(FDRfile, 1);
    try
      BlockRead(FDRfile, buf, SizeOf(buf));        {Read first part of the file}
      str1:='';
      str2:='';
      str:='';
      for i:=33 to SizeOf(buf)-1 do begin          {Find meta data}
        case buf[i] of
          0: TestResult;
          32, 34..127: str:=str+Chr(buf[i]);
        end;
        if result<>'' then
          exit;                                    {UUID found for mode 1}
      end;
    finally
      CloseFile(FDRfile);
    end;
    dtlGridResize;
    PageControl1.ActivePageIndex:=4;               {Go to details page}
  end else
    StatusBar1.Panels[4].Text:=errLessData;
end;

procedure TForm1.mmnHomepageClick(Sender: TObject); {Menu Homepage}
begin
  OpenURL(homepage);
end;

procedure TForm1.mmnCloseClick(Sender: TObject);   {Menu Close application}
begin
  Close;
end;

procedure TForm1.mmnCSVexClick(Sender: TObject);   {Menu export CSV}
begin
  if ovGrid.RowCount>0 then
    MakeCSV;
end;

procedure TForm1.mmnGPXexClick(Sender: TObject);   {Menu export GPX}
begin
  if ovGrid.RowCount>0 then
    MakeGPX;
end;

procedure TForm1.mmnInfoClick(Sender: TObject);    {About Box}
begin
  ShowAbout;
end;

procedure TForm1.mmnJumpClick(Sender: TObject);    {Menu Open file manager}
begin
  OpenDocument(IncludeTrailingPathDelimiter(LogDir.Directory));
end;

procedure TForm1.mmnKMLexClick(Sender: TObject);   {Menu export KML}
begin
  if ovGrid.RowCount>0 then
    MakeKML;
end;

procedure TForm1.mmnLogBookClick(Sender: TObject); {Menu Pilot log book}
begin
  CreateLogBook;
end;

procedure TForm1.mmnManualClick(Sender: TObject);  {Menü Info/Manual}
begin
  OpenManual;
end;

procedure TForm1.mmnOpenClick(Sender: TObject);    {Menu open JSON dir}
begin
  if LogDir.Directory<>'' then
    LogDirDialog.InitialDir:=LogDir.Directory;
  if LogDirDialog.Execute then
    LogDir.Directory:=LogDirDialog.FileName;
end;

procedure TForm1.mmnRenameClick(Sender: TObject);  {Menu Rename files with date/time}
var filelist: TStringList;
    i, zhl: integer;
    fn, newfn, ts, uuid: string;
    inf: TFileStream;
    j0: TJsonData;                                 {1. level}

begin                                              {New feature in V1.5}
  ProgressFile.Position:=0;
  zhl:=0;                                          {File counter}
  if (LogDir.Directory<>'') and
      DirectoryExists(LogDir.Directory) then begin  {Only when Directory valid}
    filelist:=TStringList.Create;
    try
      FindAllFiles(filelist, LogDir.Directory, wldcd+jext, false);
      if filelist.Count>0 then begin
        StatusBar1.Panels[0].Text:=rsFiles+dpkt+IntToStr(filelist.Count);
        StatusBar1.Panels[4].Text:='';             {Empty file name field}
        ProgressFile.Max:=filelist.Count;
        try
          for i:=0 to filelist.Count-1 do begin
            uuid:='';
            ts:='';                                {Timestamp}
            fn:=ExtractFileName(filelist[i]);
            try
              inf:=TFileStream.Create(filelist[i], fmOpenRead or fmShareDenyWrite);
              j0:=GetJson(inf);                    {load whole JSON file, level 0}
              ts:=j0.FindPath(datUTC).AsString;    {Date/time from Metadata}
              uuid:=j0.FindPath(datUUID).AsString; {UUID from Metadata}
            finally
              inf.Free;
            end;
            if (ts<>'') and (length(uuid)>6) then begin
              newfn:=StringReplace(filelist[i], fn,
                     ts+uscr+copy(uuid, 1, 6)+jext, [rfIgnoreCase]);
              if RenameFile(filelist[i], newfn)    {Rename file}
                then inc(zhl);                     {Count successful renamed files}
              ProgressFile.Position:=i;
              StatusBar1.Panels[1].Text:=IntToStr(zhl);
            end;
          end;
        except
          StatusBar1.Panels[4].Text:=errRename+tab2+fn;
        end;
        StatusBar1.Panels[4].Text:=IntToStr(zhl)+tab1+rsFilesRenamed;
        if (zhl>0) and (Form1.Tag=1) then
          BuildList;                               {Reload file list}
      end;                                         {No JSON files available}
    finally
      FileList.Free;
    end;
  end;                                             {No valid directory}
end;

procedure TForm1.mmnScrShotClick(Sender: TObject); {Menu Screenshot}
begin
  DoScreenShot;
end;

procedure TForm1.mmnSettingsClick(Sender: TObject); {Go to Settings page}
begin
  PageControl1.ActivePageIndex:=5;
end;

procedure TForm1.mmnTasClick(Sender: TObject);     {Recompute TAS from xyz-Speed}
begin
  RestoreTAS;
end;

procedure TForm1.ovGridClick(Sender: TObject);     {Process a file}
begin
  if (Form1.Tag=1) and
     (ovGrid.Tag<>csvgrid.Tag) then begin          {A new file selected}
    LoadOneFile(ovGrid.Tag);
    MakeHDia;
    MakeAtti;
  end;
  if (PageControl1.Tag>0) then                     {Switch to previous Tab}
    PageControl1.ActivePageIndex:=PageControl1.Tag;
end;

procedure TForm1.ovGridCompareCells(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);             {Alternative sorting routines}
begin
  case ACol of
    1..3: result:=CompareText(ovGrid.Cells[1, ARow]+             {Sort date time}
                              ovGrid.Cells[2, ARow]+
                              ovGrid.Cells[3, ARow],
                              ovGrid.Cells[1, BRow]+
                              ovGrid.Cells[2, BRow]+
                              ovGrid.Cells[3, BRow]);
    5..10: result:=StrToInt(CleanNum(ovGrid.Cells[ACol, Arow]))-
                   StrToInt(CleanNum(ovGrid.Cells[BCol, Brow])); {Sort as Integer}
  else
    result:=CompareText(ovGrid.Cells[ACol, ARow],
            ovGrid.Cells[BCol, BRow]);             {Default single cell as text}
  end;
  if ovGrid.SortOrder=soDescending then
    Result:=-Result;                               {Sort direction}
end;

procedure TForm1.ovGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
  var HintText: String);
begin
  HintText:=ovGrid.Cells[aCol, aRow];
end;

procedure TForm1.ovGridKeyUp(Sender: TObject; var Key: Word;
                             Shift: TShiftState);  {Overview short keys}
begin
  if (key=vk_c) and
     (ssCtrl in Shift) then
    ovGrid.CopyToClipboard(false);                 {Ctrl+C copy to clipboard}
end;

procedure TForm1.ovGridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  ovGrid.Tag:=aRow;                                {store the Selection for onclick}
end;

procedure TForm1.PageControl1Change(Sender: TObject); {Save previous Tab}
begin
  case PageControl1.ActivePageIndex of
    1: if (cbDegree.Tag=1) and
          (Form1.Tag=1)
       then LoadOneFile(csvGrid.Tag);

    2: MakeHDia;
    3: MakeAtti;
    4: dtlGridReSize;
  end;
  if (PageControl1.ActivePageIndex>0) and          {not Overview}
     (PageControl1.ActivePageIndex<5) then         {not Settings}
    PageControl1.Tag:=PageControl1.ActivePageIndex;
  PageControl1.ActivePage.Refresh;                 {Redraw the page}
end;

procedure TForm1.staGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
  var HintText: String);
begin
  HintText:=staGrid.Cells[aCol, aRow];
end;

procedure TForm1.staGridKeyUp(Sender: TObject; var Key:  {Statistics short keys}
                              Word; Shift: TShiftState);
begin
  if (key=vk_c) and
     (ssCtrl in Shift) then
    staGrid.CopyToClipboard(false);                {Ctrl+C copy to Clipboard}
end;

procedure TForm1.staGridResize(Sender: TObject);   {Beautify}
begin
  edUUID.Width:=staGrid.Width;
  FDRdir.Width:=staGrid.Width;
end;

procedure TForm1.FormActivate(Sender: TObject);    {Start running with settings from XML}
var dir: string;
begin
  try
    StatusBar1.Panels[3].Text:=grpConv.Items[grpConv.ItemIndex];
    Chart2.AxisList[0].Title.Caption:=dtSpeed+tab1+UnitToStr(15, false);
    Chart2.AxisList[2].Title.Caption:=dtAngle+tab1+UnitToStr(16, false, cbDegree.Checked);
    if Form1.Tag=0 then begin                      {first start}
      if (ParamCount>0) and                        {FileName as 1st parameter}
         (Length(ParamStr(1))>3) then begin        {Something like a file name?}
        dir:=FindDirName(ParamStr(1));
        if DirectoryExists(dir) then
          LogDir.Directory:=dir;                   {Use this one}
      end;
      FDRDialog.InitialDir:=LogDir.Directory;      {Default wie JSON}
      if LogDir.Directory='' then
        PageControl1.ActivePageIndex:=5            {Go to settings page}
      else
        BuildList;                                 {search JSON files}
    end;
  except
    StatusBar1.Panels[4].Text:='Error during start procedure';
  end;
end;

procedure TForm1.btCloseClick(Sender: TObject);    {Button Close}
begin
  Close;
end;

procedure TForm1.SearchFDRfiles(dir, mask, vstr: string);  {Find UUID}
var flist: TStringList;
    i: integer;
begin
  if DirectoryExists(dir) and (mask<>'') then begin
    ListBox1.Items.Clear;
    ProgressFile.Position:=0;
    fList:=FindAllFiles(IncludeTrailingPathDelimiter(dir), mask, true);
    StatusBar1.Panels[0].Text:=IntToStr(flist.Count);
    try
      if flist.Count>0 then begin
        ProgressFile.Max:=flist.Count;
        for i:=0 to flist.Count-1 do begin         {List all files matching mask}
          if vstr='' then begin
            ListBox1.Items.Add(flist[i]);
          end else begin                           {List files with UUID}
            if ShowFDRlog(flist[i], 1)=edUUID.Text then
              ListBox1.Items.Add(flist[i]);
          end;
          ProgressFile.Position:=i+1;
        end;
      end;
      StatusBar1.Panels[1].Text:=IntToStr(ListBox1.Items.Count);
      if ListBox1.Items.Count=0 then
        StatusBar1.Panels[4].Text:=rsNothingFound
      else
        StatusBar1.Panels[4].Text:=rsUUIDfound;
    finally
      flist.Free;
    end;
  end;
end;

procedure TForm1.btSearchFDRClick(Sender: TObject); {Search for UUID in FDR dir}
begin
  SearchFDRfiles(FDRdir.Directory, wldcd+fext, edUUID.Text);
end;

procedure TForm1.cbDegreeChange(Sender: TObject);  {Setting changed}
begin
  cbDegree.Tag:=1;                                 {Indicates that setting changed}
end;

procedure TForm1.cbHeaderChange(Sender: TObject);  {Use alternative Header}
begin
  if dtlGrid.Tag=0 then
    ChangeHeader;
end;

procedure TForm1.Chart1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssCtrl in Shift) or                          {Klicken mit gedrückter Ctrl}
     (ssMiddle in Shift) then                      {Klicken mit mittlerer Taste}
    Chart1.ZoomFull;
end;

procedure TForm1.Chart2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssCtrl in Shift) or                          {Klicken mit gedrückter Ctrl}
     (ssMiddle in Shift) then                      {Klicken mit mittlerer Taste}
    Chart2.ZoomFull;
end;

procedure TForm1.lbLegendeDblClick(Sender: TObject); {Toggle Attitude Charts}
var i: integer;

  procedure SetSpeed;
  begin
    lbLegende.Checked[3]:=false;               {Angles}
    lbLegende.Checked[4]:=false;
    lbLegende.Checked[5]:=false;               {Heading}
    lbLegende.Checked[7]:=false;
    lbLegende.Tag:=1;
  end;

  procedure SetAngle;
  begin
    lbLegende.Checked[0]:=false;               {Speed}
    lbLegende.Checked[1]:=false;
    lbLegende.Checked[2]:=false;
    lbLegende.Checked[6]:=false;               {Zero axis lines}
    lbLegende.Tag:=2;
  end;

  procedure SetNickRoll;
  begin
    lbLegende.Checked[0]:=false;               {Speed}
    lbLegende.Checked[1]:=false;
    lbLegende.Checked[2]:=false;
    lbLegende.Checked[5]:=false;               {Heading}
    lbLegende.Checked[6]:=false;               {Zero axis lines}
    lbLegende.Tag:=3;
  end;

begin
  for i:=0 to lbLegende.SeriesCount-1 do       {Set all as default}
    lbLegende.Checked[i]:=true;

  case lbLegende.Tag of
    0: SetSpeed;
    1: SetAngle;
    2: SetNickRoll;
  else
    lbLegende.Tag:=0;
  end;
end;

procedure TForm1.cmnClipbrd2Click(Sender: TObject); {Menu for data table}
begin
  csvGrid.CopyToClipboard(false);                  {Data table}
end;

procedure TForm1.cmnClipbrdClick(Sender: TObject); {Context menu Clipboard}
var templist: TStringList;
begin
  case PageControl1.ActivePageIndex of
    0: begin
         templist:=TStringList.Create;
         templist.Assign(ovGrid.Cols[hc]);         {Save hidden column}
         ovGrid.ColCount:=hc;                      {Avoid copy log book}
         ovGrid.CopyToClipboard(false);            {Overview}
         ovGrid.ColCount:=hc+1;                    {Create hidden column again}
         ovGrid.ColWidths[hc]:=0;                  {Hide}
         ovGrid.Cols[hc].Assign(templist);         {Restore hidden column}
         templist.Free;
       end;
    2: Chart1.CopyToClipboardBitmap;               {Chart altitude}
    3: Chart2.CopyToClipboardBitmap;               {Charts attitude}
  end;
end;

procedure TForm1.cmnSaveAs2Click(Sender: TObject); {Data table save as}
begin
  SaveDialog1.Title:=mniFileSave;
  SaveDialog1.FileName:=thdData+uscr+'1'+cext;
  if SaveDialog1.Execute then begin
    csvGrid.SaveToCSVFile(SaveDialog1.FileName, getCSVsep, true);
    StatusBar1.Panels[4].Text:=SaveDialog1.FileName+tab1+rsSaved;
  end;
end;

procedure TForm1.cmnSaveAsClick(Sender: TObject);  {Context menu Save as}
var templist: TStringList;
begin
  SaveDialog1.Title:=mniFileSave;
  SaveDialog1.FileName:=PageControl1.ActivePage.Caption+uscr+'1';
  case PageControl1.ActivePageIndex of
    0: begin                                       {Overview}
         SaveDialog1.FileName:=SaveDialog1.FileName+cext;
         if SaveDialog1.Execute then begin
           templist:=TStringList.Create;
           try
             templist.Assign(ovGrid.Cols[hc]);
             ovGrid.ColCount:=hc;                  {Avoid save log book}
             ovGrid.SaveToCSVFile(SaveDialog1.FileName, getCSVsep, true);
             StatusBar1.Panels[4].Text:=SaveDialog1.FileName+tab1+rsSaved;
           finally
             ovGrid.ColCount:=hc+1;
             ovGrid.ColWidths[hc]:=0;
             ovGrid.Cols[hc].Assign(templist);
             templist.Free;
           end;
         end;
       end;
    2: begin                                       {Chart altitude}
         SaveDialog1.FileName:=SaveDialog1.FileName+pext;
         if SaveDialog1.Execute then begin
           Chart1.SaveToFile(TPortableNetworkGraphic, SaveDialog1.FileName);
           StatusBar1.Panels[4].Text:=SaveDialog1.FileName+tab1+rsSaved;
         end;
       end;
    3: begin                                       {Attitude}
         SaveDialog1.FileName:=SaveDialog1.FileName+pext;
         if SaveDialog1.Execute then begin
           Chart2.SaveToFile(TPortableNetworkGraphic, SaveDialog1.FileName);
           StatusBar1.Panels[4].Text:=SaveDialog1.FileName+tab1+rsSaved;
         end;
       end;
  end;
end;

procedure TForm1.cmnShowGMClick(Sender: TObject);  {Menu Show in Google Maps}
begin
  if csvGrid.Selection.Left<7 then                 {Columns left of wifi signal}
    OpenURL(URLGMap(csvGrid.Cells[3, csvGrid.Selection.Top],
                    csvGrid.Cells[4, csvGrid.Selection.Top]))
  else                                             {Anafi GPS coords instead RC}
    OpenURL(URLGMap(csvGrid.Cells[10, csvGrid.Selection.Top],
                    csvGrid.Cells[9, csvGrid.Selection.Top]))
end;

procedure TForm1.cmnShowOSMClick(Sender: TObject); {Menu Show in OSM}
begin
  if csvGrid.Selection.Left<7 then                 {Columns left of wifi signal}
    OpenURL(URLosm(csvGrid.Cells[3, csvGrid.Selection.Top],
                    csvGrid.Cells[4, csvGrid.Selection.Top]))
  else                                             {Anafi GPS coords instead RC}
    OpenURL(URLosm(csvGrid.Cells[10, csvGrid.Selection.Top],
                   csvGrid.Cells[9, csvGrid.Selection.Top]))
end;

procedure TForm1.csvGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
  var HintText: String);
begin
  if csvGrid.RowCount>1 then
    HintText:=GetCellInfo(aCol, aRow);
end;

procedure TForm1.RestoreTAS;                       {Compute tas from vx, vy, vz}
var i: integer;
    tas, vx, vy, vz, tasmax: double;
    ttasmax: TDateTime;
begin
  tasmax:=0;
  if csvGrid.RowCount>jsonMinLines then begin
    for i:=1 to csvGrid.RowCount-1 do begin
      try
        vx:=StrToFloat(csvGrid.Cells[13, i]);
        vy:=StrToFloat(csvGrid.Cells[14, i]);
        vz:=StrToFloat(csvGrid.Cells[15, i]);
        tas:=sqrt((vx*vx)+(vy*vy)+(vz*vz));        {Recompute TAS}
      except
        tas:=0;
      end;
      if tas>tasmax then begin
        tasmax:=tas;                               {Top speed}
        ttasmax:=ScanDateTime(ymd+tab1+hnsz, csvGrid.Cells[0, i]);
      end;
      csvGrid.Cells[21, i]:=FormatFloat(frmFloat, tas);
    end;
    staGrid.Cells[1, 3]:=FormatFloat(frmFloat, ConvUnit(21, tasmax, useau))+
                         UnitToStr(21, true, useau);
    staGrid.Cells[2, 3]:=FormatDateTime(hnsz, ttasmax);
    ovGrid.Cells[7, csvGrid.Tag]:=FormatFloat(frmOut2, ConvUnit(21, tasmax, true))+
                                  UnitToStr(21, true, true);
    if PageControl1.ActivePageIndex=2 then         {Chart}
      MakeHDia;                                    {Recreate with tas}
  end;
end;

function TForm1.GetCellInfo(aCol, aRow: longint): string;
                                                   {Cell info in data table}
  function DefaultHnt: string; inline;             {Default hint: Header=Value}
  begin
    if dtlGrid.Tag=0 then
      result:=AltHeaderToStr(aCol)+'='+csvGrid.Cells[aCol, aRow]
    else
      result:=csvGrid.Cells[aCol, 0]+'='+csvGrid.Cells[aCol, aRow];
  end;

  function FrmValue: string; inline;               {Format float for better reading}
  begin
    try
      result:=AltHeaderToStr(aCol)+'='+
              FormatFloat(frmFloat,
                          ConvUnit(aCol, StrToFloat(csvGrid.Cells[aCol, aRow])))+
              UnitToStr(aCol, true);
    except
      result:=DefaultHnt;
    end;
  end;

begin                                              {Get additional info per cell}
  result:=csvGrid.Cells[aCol, aRow];
  if aRow=0 then begin
    if dtlGrid.Tag=0 then
      result:=AltHeaderToStr(aCol);                {Better explained header}
  end else begin
    if csvGrid.Cells[aCol, aRow]<>'' then begin
      result:=DefaultHnt;
      case aCol of
        1, 2, 7: result:=DefaultHnt+UnitToStr(aCol, true);
        5: result:=AltHeaderToStr(aCol)+': '+
                   F_StateToStr(csvGrid.Cells[aCol, aRow]);
        6: result:=AltHeaderToStr(aCol)+': '+
                   AlertStateToStr(csvGrid.Cells[aCol, aRow]);
        8: result:=AltHeaderToStr(aCol)+': '+
                   GPSavailToStr(csvGrid.Cells[aCol, aRow]);
        13..15, 19: result:=FrmValue;
        16..18: result:=AltHeaderToStr(aCol)+'='+
                        FormatFloat(frmFloat,
                        ConvUnit(aCol, StrToFloat(csvGrid.Cells[aCol, aRow]),
                                 not cbDegree.Checked))+
                        UnitToStr(aCol, true, true); {with conversion to °}
        21, 22: result:=AltHeaderToStr(aCol)+'='+
                        FormatFloat(frmFloat,
                        ConvUnit(aCol, StrToFloat(csvGrid.Cells[aCol, aRow]), useau))+
                        UnitToStr(aCol, true, useau); {with alternative units}
      end;
    end;
  end;
end;

procedure TForm1.csvGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);                         {Cells get colors}
var w: integer;
    ts: TTextStyle;

begin
  if (aState=[]) and                               {Only if cell is not selected}
     (aRow>0) then begin                           {not for header}
    if csvGrid.Cells[aCol, aRow]<>'' then begin
      ts:=csvGrid.Canvas.TextStyle;
      ts.Alignment:=taCenter;
      case aCol of
        2: begin                                   {Battery level}
             w:=StrToIntDef(csvGrid.Cells[aCol, aRow], 0);
               if w>0 then begin
                 if w>50 then begin                {100%..51% green}
                   csvGrid.Canvas.Brush.Color:=clGreen;
                   exit;
                 end;
                 if w<25 then begin                {0...24 red}
                   csvGrid.Canvas.Brush.Color:=clRed;
                   exit;
                 end else begin                    {25...50 Sats orange}
                   csvGrid.Canvas.Brush.Color:=clDarkOrange;
                   exit;
                 end;
               end else exit;
           end;
        5: begin                                   {flight state}
             w:=StrToIntDef(csvGrid.Cells[aCol, aRow], 99);
             csvGrid.Canvas.Brush.Color:=Form2.ColorSourceFM(w);
             csvGrid.Canvas.TextStyle:=ts;
           end;
        6: begin                                   {alert state}
             w:=StrToIntDef(csvGrid.Cells[aCol, aRow], 99);
             csvGrid.Canvas.Brush.Color:=Form2.ColorSourceAS(w);
             csvGrid.Canvas.TextStyle:=ts;
           end;
        8: if LowerCase(csvGrid.Cells[aCol, aRow])='true' then
             csvGrid.Canvas.Brush.Color:=clMoneyGreen;
        11: if (dtlGrid.Tag=0) and                 {GPS error}
               (csvGrid.Cells[aCol, aRow]<>def0) then
              csvGrid.Canvas.Brush.Color:=clRed;
        12: begin                                  {Num sats}
               w:=StrToIntDef(csvGrid.Cells[aCol, aRow], 0);
                 if w>0 then begin
                   if w>10 then begin              {11...x Sats green}
                     csvGrid.Canvas.Brush.Color:=clGreen;
                     exit;
                   end;
                   if w<5 then begin               {1...4 Sats red}
                     csvGrid.Canvas.Brush.Color:=clRed;
                     exit;
                  end else begin                   {5...10 Sats rose}
                    csvGrid.Canvas.Brush.Color:=clAttention;
                    exit;
                  end;
                end else exit;
             end;
         20: if csvGrid.Cells[aCol, aRow]<>def0 then
               csvGrid.Canvas.Brush.Color:=clOrange;  {flip type}
      end;
    end;
  end;
end;

function TForm1.UnitToStr(idx: integer;            {Measurement unit string}
                  pure: boolean;                   {Brackets no/yes}
                  converted: boolean=false): string; {alternative unit like km/h}
begin
  result:='';                                      {No measurement unit}
  case idx of                                      {Set metric}
    1: if dtlGrid.Tag=0 then result:='ms' else result:='V';
    2: result:='%';
    7: result:='dBm';
    13..15: result:='m/s';
    16..18: if converted then result:='°' else result:='rad';
    19: result:='m';
    21: if converted then result:='km/h' else result:='m/s';
    22: if converted then result:='km' else result:='m';
  end;
  if Form1.grpUnit.ItemIndex=1 then                {Overwrite with imperial}
    case idx of
      13..15: result:='ft/s';
      19: result:='ft';
      21: if converted then result:='mph' else result:='ft/s';
      22: if converted then result:='mi' else result:='ft';
    end;
  if (not pure) and                                {With brackets}
     (result<>'') then
    result:='['+result+']';
end;

function TForm1.ConvUnit(idx: integer;             {Index column}
                         value: double;            {input}
                         converted: boolean=false): double;
            {Possible conversion: rad to °, m/s to km/h, ft to mi, ft/s to mph}
begin
  result:=value;                                   {metric to metric, ft to ft}
  case idx of
    13..15, 19: if metric and
                   (grpUnit.ItemIndex=1) then      {Imperial set}
                     result:=value*mtoft;          {m/s --> ft/s}
    16, 17: if converted then
              result:=value*180/pi;                {rad to ° +/-180}
    18:     if converted then begin
              result:=value*180/pi;                {rad to ° 0..360, 0 is north}
              if result<0 then result:=result+360;
            end;

{possibly converted values m --> km}
    21: begin
          if metric and                            {Speed: m/s --> ft/s}
             (grpUnit.ItemIndex=1) then            {Imperial set}
               result:=value*mtoft;                {ft/s}
          if converted then begin                  {overwrite}
            if Form1.grpUnit.ItemIndex=1 then
              result:=result*0.68181818            {mph}
            else
              result:=result*3.6;                  {km/h}
          end;
        end;
    22: begin
          if metric and                            {Distance: m --> ft}
             (grpUnit.ItemIndex=1) then            {Imperial set}
            result:=value*mtoft;                   {ft}
          if converted then begin                  {overwrite}
            if Form1.grpUnit.ItemIndex=1 then
              result:=result*0.00018939            {mi (miles)}
            else
              result:=result/1000;                 {km}
          end;
        end;
  end;
end;

procedure TForm1.csvGridHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);                                 {Show additional window}

  procedure ShowStatistics;                        {Copy of Statistics table}
  begin
    DoForm2Show(400);
    Form2.Caption:=rsStatCopy;
    Form2.addGrid.Visible:=true;
    Form2.edTime.Visible:=false;
    Form2.addGrid.Assign(staGrid);
    Form2.addGrid.AutoSizeColumns;
    Form2.FormatGrid;
  end;

   procedure ShowFloat(idx: integer);              {Chart with floating numbers}
   var i: integer;
       ts: TDateTime;
       w: double;
       convert: boolean=false;                     {Default: no conversion}
   begin
     DoForm2Show(0);
     Form2.Caption:=rsChart+AltHeaderToStr(idx);
     case idx of
       16..18: convert:=true;                      {Unit string will be converted rad to °}
     end;
     Form2.Chart1.AxisList[0].Title.Caption:=AltHeaderToStr(idx)+   {y-axis}
                                             tab1+UnitToStr(idx, false, convert);
     Form2.Chart1.Visible:=true;
     Form2.edTime.Visible:=true;
     Form2.Chart1LineSeries1.BeginUpdate;
       Form2.MoveVCursor(ScanDateTime(ymd+tab1+hnsz, csvGrid.Cells[0, 1]), 0);
       for i:=1 to csvGrid.RowCount-1 do begin     {Create chart}
         w:=StrToFloatDef(csvGrid.Cells[idx, i], 0);
         case idx of
           13..15: w:=-w;                          {speed_v xyz reverse in chart}
           16..18: w:=ConvUnit(idx, w, not cbDegree.Checked);  {rad to °}
         end;
         w:=ConvUnit(idx, w);                      {metric or, imperial}
         ts:=ScanDateTime(ymd+tab1+hnsz, csvGrid.Cells[0, i]);
         Form2.Chart1LineSeries1.AddXY(ts, w);
       end;
     Form2.Chart1LineSeries1.EndUpdate;
   end;

   procedure ShowInteger(idx: integer);            {Chart from integer}
   var i: integer;
       ts: TDateTime;
       w: double;
   begin
     DoForm2Show(0);
     Form2.Caption:=rsChart+AltHeaderToStr(idx);
     Form2.Chart1.AxisList[0].Title.Caption:=AltHeaderToStr(idx)+   {y-axis}
                                             tab1+UnitToStr(idx, false);
     Form2.Chart1.Visible:=true;
     Form2.edTime.Visible:=true;
     Form2.Chart1LineSeries1.BeginUpdate;
       Form2.MoveVCursor(ScanDateTime(ymd+tab1+hnsz, csvGrid.Cells[0, 1]), 0);
       for i:=1 to csvGrid.RowCount-1 do begin
         w:=ConvUnit(idx, StrToIntDef(csvGrid.Cells[idx, i], 0));
         ts:=ScanDateTime(ymd+tab1+hnsz, csvGrid.Cells[0, i]);
         Form2.Chart1LineSeries1.AddXY(ts, w);
       end;
     Form2.Chart1LineSeries1.EndUpdate;
   end;

   procedure ShowList(idx: integer);               {Table with counter}
   type
     sdat=Record
       Value: string;
       Count: integer;
   end;

   var
     a: Array of sdat;                             {Fast sorting with dyn. Array}
     i, j: integer;
     s: string;
     b: boolean;

   begin
     a:=nil;                                       {Clean array}
     DoForm2Show(600);
     Form2.addGrid.ColCount:=3;
     Form2.addGrid.Visible:=true;
     Form2.edTime.Visible:=false;
     Form2.Caption:=rsList+AltHeaderToStr(idx);

     for i:=1 to csvGrid.RowCount-1 do begin       {Read data}
       s:=trim(csvGrid.Cells[idx, i]);
       if assigned(a) then begin                   {Array already initialized ?}
         b:=false;                                 {Check on}
         for j:=0 to high(a) do begin              {Check if value already exists}
           if a[j].Value=s then begin
             a[j].Count:=a[j].Count+1;             {Found one - count it}
             b:=true;
             break;
           end;
         end;
         if not b then begin                       {new value to array}
           setlength(a, high(a)+2);
           a[high(a)].Value:=s;
           a[high(a)].Count:=1;
         end;
       end else begin                              {Array not assigned yet}
         setlength(a, 1);                          {First value in array}
         a[0].Value:=s;
         a[0].Count:=1;
       end;
     end;

     Form2.idx:=idx;
     Form2.addGrid.BeginUpdate;                    {Output to table}
     Form2.addGrid.Cells[0, 0]:=AltHeaderToStr(idx);
     Form2.addGrid.Cells[1, 0]:=altCount;
     Form2.addGrid.Cells[2, 0]:=altDescription;
     if assigned(a) then begin                     {Array is filled}
       Form2.addGrid.RowCount:=high(a)+2;          {Set number of rows}
       for i:=0 to high(a) do begin
         Form2.addGrid.Cells[0, i+1]:=a[i].Value;
         Form2.addGrid.Cells[1, i+1]:=
           IntToStr(a[i].Count);
         case idx of
            5: Form2.addGrid.Cells[2, i+1]:=
                 F_StateToStr(a[i].Value);
            6: Form2.addGrid.Cells[2, i+1]:=
                 AlertStateToStr(a[i].Value);
            8: Form2.addGrid.Cells[2, i+1]:=
                 GPSAvailToStr(a[i].Value);
           11: Form2.addGrid.Cells[2, i+1]:=
                 GPSerrToStr(a[i].Value);
           20: Form2.addGrid.Cells[2, i+1]:=
                 FlipTypeToStr(a[i].Value);
         end;
       end;
     end;
     Form2.addGrid.AutoSizeColumns;
     Form2.FormatGrid;
     Form2.addGrid.EndUpdate;
   end;

begin
  if IsColumn and (csvGrid.RowCount>2) then begin
    Form2.addGrid.RowCount:=1;                     {Kill all}
    Form2.addGrid.Visible:=false;
    Form2.edTime.Visible:=false;
    Form2.Chart1LineSeries1.Clear;
    Form2.Chart1.Visible:=false;
    case index of
      0: ShowStatistics;                           {Copy of staGrid}
      2, 7, 12: ShowInteger(index);                {Integer values; Chart}
      3, 4, 9, 10, 22: ShowFloat(22);              {Distance; Chart}
      5, 6: ShowList(index);                       {Data Table with time *
                                                    not yet implemented}
      8, 11, 20: ShowList(index);                  {Data Table with counter}
      13..19, 21: ShowFloat(index);                {Float values; Chart}
    end;
    if dtlGrid.Tag=0 then begin                    {Legacy JSON}
      case index of
        1: ShowStatistics;
        11: ShowList(index);
        12: ShowInteger(index);
      end;
    end;
    if dtlGrid.Tag=2 then begin                    {Blackbox JSON}
      case index of
        1, 11: ShowFloat(index);
        12: ShowList(index);
        23, 24, 26..29, 31..34, 38, 39: ShowFloat(index);
        25, 30, 35..37, 40..42: ShowList(index);
      end;
    end;
  end;
end;

procedure TForm1.csvGridKeyUp(Sender: TObject; var Key: Word;
                              Shift: TShiftState);
begin
  if (key=vk_c) and
     (ssCtrl in Shift) then
    csvGrid.CopyToClipboard(true);
end;

procedure TForm1.csvGridSelection(Sender: TObject; aCol, aRow: Integer);
var ts, tb, te: TDateTime;
begin                                              {Cell selected}
  if aRow>0 then begin
    StatusBar1.Panels[2].Text:=IntToStr(aRow);     {Show line number}
    if Form2.Chart1ConstantLine1.Active then begin {Show Cursor}
      ts:=ScanDateTime(ymd+tab1+hnsz, csvGrid.Cells[0, aRow]);
      tb:=ScanDateTime(ymd+tab1+hnsz, csvGrid.Cells[0, 1]);
      te:=ScanDateTime(ymd+tab1+hnsz, csvGrid.Cells[0, csvGrid.RowCount-1]);
      Form2.MoveVCursor(ts, round((ts-tb)/(te-tb)*10000));
    end;                                           {Where we are in time line}
  end;
end;

procedure TForm1.dtlGridDblClick(Sender: TObject); {Show in GoogleMaps}
begin
  if dtlGrid.Tag=0 then
    OpenURL(URLGMap(dtlGrid.Cells[1, 14], dtlGrid.Cells[1, 15]));
end;

procedure TForm1.dtlGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
  var HintText: String);
begin
  HintText:=dtlGrid.Cells[0, aRow]+'='+dtlGrid.Cells[1, aRow];
  if dtlGrid.Cells[0, aRow]='product.model.id' then
    HintText:=dtlGrid.Cells[0, aRow]+'='+
              ProdIDtoLabel('$'+dtlGrid.Cells[1, aRow]);
  if dtlGrid.Cells[0, aRow]=datUTC then
    HintText:=rsDateTime+'='+
              FDRtimeToStr(dtlGrid.Cells[1, aRow]);
  if dtlGrid.Cells[0, aRow]='smartbattery.soh' then
    HintText:='Smart battery state of health='+dtlGrid.Cells[1, aRow]+'%';
end;

procedure TForm1.dtlGridKeyUp(Sender: TObject;     {Ctrl+c copy}
                              var Key: Word;
                              Shift: TShiftState);
begin
  if (key=vk_c) and
     (ssCtrl in Shift) then
    dtlGrid.CopyToClipboard(false);
end;

procedure TForm1.FDRdirDblClick(Sender: TObject);
begin
  OpenDocument(FDRdir.Directory);
end;

procedure TForm1.ScreenToBild(fn: string);         {Screenshot}
var bld: TPortableNetworkGraphic;
    ScreenDC: HDC;
begin
  bld:=TPortableNetworkGraphic.Create;             {create PNG-picture}
  try
    bld.Canvas.Clear; {sicherstellen, dass bld bereits vollständig erzeugt wurde}
    ScreenDC:=Canvas.Handle;                       {Whole program window}
    bld.LoadFromDevice(ScreenDC);
    bld.SaveToFile(fn);
  finally
    FreeAndNil(bld);
  end;
end;

procedure TForm1.DoScreenShot;                     {Screenshot}
begin
  SaveDialog1.Title:=hntScrShot;
  SaveDialog1.FileName:=rsScreen+uscr+
                        PageControl1.ActivePage.Caption+uscr+'1'+pext;
  if SaveDialog1.Execute then begin
    ScreenToBild(SaveDialog1.FileName);
    StatusBar1.Panels[4].Text:=SaveDialog1.FileName+tab1+rsSaved;
  end;
end;

procedure TForm1.btScrShotClick(Sender: TObject);  {Screenshot}
begin
  DoScreenshot;
end;

procedure TForm1.btConvClick(Sender: TObject);     {Convert files}
begin
  if ovGrid.RowCount>0 then begin                  {if JSON files are available}
    case grpConv.ItemIndex of
      0: MakeKML;
      1: MakeGPX;
      2: MakeCSV;
    end;
  end;
end;

procedure TForm1.btLogBookClick(Sender: TObject);  {Button LogBook}
begin
  CreateLogBook;
end;

procedure TForm1.dtlGridReSize;                    {Rezeize Details table}
var i: integer;
begin
  dtlGrid.AutoSizeColumns;
  dtlGrid.ColWidths[0]:=dtlGrid.ColWidths[0]+nicecols;
  dtlGrid.ColWidths[1]:=dtlGrid.ColWidths[1]+
                        dtlGrid.Width-
                        dtlGrid.ColWidths[0];
  staGrid.AutoSizeColumns;
  for i:=0 to 2 do
  staGrid.ColWidths[i]:=staGrid.ColWidths[i]+nicecols;
  staGrid.Width:=staGrid.ColWidths[0]+
                 staGrid.ColWidths[1]+
                 staGrid.ColWidths[2]+nicecols;
end;

procedure TForm1.BuildList;                        {search and count JSON files}
var filelist: TStringList;
    i: integer;
    fn: string;
begin
  if (LogDir.Directory<>'') and
     DirectoryExists(LogDir.Directory) then begin  {only when Directory valid}
    if Form2<>nil
      then Form2.Close;                            {Close 2nd window}
    btConv.Enabled:=false;
    filelist:=TStringList.Create;
    ovGrid.RowCount:=1;                            {Delete file list}
    PageControl1.Tag:=0;
    try
      FindAllFiles(filelist, LogDir.Directory, wldcd+jext, false);
      if filelist.Count>0 then begin               {only if >=1 JSON file was found}
        StatusBar1.Panels[0].Text:=rsFiles+dpkt+IntToStr(filelist.Count);
        StatusBar1.Panels[4].Text:='';             {Empty file name field}

        ovGrid.BeginUpdate;                        {File names to overview}
          ovGrid.Cells[0, 0]:=StatusBar1.Panels[0].Text;
          ovGrid.RowCount:=filelist.Count+1;
          for i:=0 to filelist.Count-1 do begin
            fn:=ExtractFileName(filelist[i]);
            ovGrid.Cells[0, i+1]:=ChangeFileExt(fn, '');
          end;
          ovGrid.Row:=ovGrid.RowCount-1;           {Last item selected}
        ovGrid.EndUpdate;

        PageControl1.ActivePageIndex:=0;           {Go to overwiev first}
        PageControl1.Tag:=1;

        if ovThread<>nil then                      {Still living}
          ovThread.Terminate;                      {Kill for new list}
        ovGrid.ColumnClickSorts:=false;            {Disable sort when thread runs}
        btLogBook.Enabled:=false;                  {Disable Pilot log book}
        mmnLogBook.Enabled:=false;
        ovThread:=TMyThread.Create(false);         {Start overview immediatley}

        ovGrid.Tag:=1;                             {First file in table}
        csvGrid.Tag:=ovGrid.RowCount-1;            {Set to last item}
        LoadOneFile(csvGrid.Tag);                  {Load last item}
        btConv.Enabled:=true;                      {Allow converting}
        if filelist.Count=1 then
          PageControl1.ActivePageIndex:=1;         {Only one -> go to data}
        ovGrid.AutoSizeColumn(0);
      end else
        StatusBar1.Panels[4].Text:=errMissingFiles+tab1+LogDir.Directory;
    finally
      Application.ProcessMessages;
      FileList.Free;
    end;
  end;
  Form1.Tag:=1;                                    {First run done}
end;

procedure TForm1.InsertDataset(da: TDatarr; source: byte; var rpos: integer);
var
  dts: array[0..lcbb] of string;

  procedure AddDatas;                              {Add new row, rpos not touched}
  var
    i: integer;

  begin
    if csvGrid.RowCount=1 then begin               {Initialize first dataset to write}
      for i:=0 to High(dts) do
        dts[i]:='';
    end else begin
      for i:=1 to High(dts) do                     {Keep previous data}
        dts[i]:=csvGrid.Cells[i, rpos];            {rpos points to last added dataset}
    end;
    dts[0]:=da[0];   {Time}
    dts[2]:=da[1];   {Batt %}
    dts[5]:=da[2];   {Flying state}
    dts[6]:=da[3];   {Alert state}
    dts[8]:=da[4];   {GPS available}
    dts[12]:=da[5];  {Motor state}
    dts[35]:=da[6];
    dts[36]:=da[7];
    dts[38]:=da[8];
    dts[39]:=da[9];
    dts[20]:=da[10];
    dts[37]:=da[11];
    dts[42]:=da[12];
    dts[41]:=da[13];
    dts[43]:=da[14]; {Home alt/pos}
    dts[44]:=da[15];
    dts[45]:=da[16];
    dts[46]:=da[17]; {Take-off alt/pos}
    dts[47]:=da[18];
    dts[48]:=da[19];
    dts[40]:=da[20];
    csvGrid.InsertRowWithValues(csvGrid.RowCount, dts); {Add dataset at the end}
    rpos:=csvGrid.RowCount-1;
  end;

  function FindTime(tme: string): integer;
  var
    i: integer;

  begin
    result:=3;                                     {AddMode: Append}
    for i:=rpos to csvGrid.RowCount-1 do begin
      if tme=csvGrid.Cells[0, i] then begin
        result:=1;                                 {Overwrite}
        rpos:=i;
        break;
      end;
      if csvGrid.Cells[0, i]>tme then begin
        result:=2;                                 {Insert}
        rpos:=i;
        break;
      end;
    end;
  end;

  procedure Insert1Hz;
  begin
    dts[0]:=da[0];   {Time}
    dts[1]:=da[1];   {Volt}
    dts[23]:=da[2];  {RC alt}
    dts[3]:=da[3];   {RC lat}
    dts[4]:=da[4];   {RC lon}
    dts[11]:=da[5];  {Drone alt}
    dts[10]:=da[6];  {Drone lat}
    dts[9]:=da[7];   {Drone lon}

    dts[25]:=da[8];  {MPP_pcmd}
    dts[26]:=da[9];
    dts[27]:=da[10];
    dts[28]:=da[11];
    dts[29]:=da[12];

    dts[7]:=da[13];  {RSSI}
    dts[22]:=da[14]; {dist}
  end;

  procedure Insert5Hz;
  begin
    dts[0]:=da[0];   {Time}
    dts[19]:=da[1];  {Product alt}
    dts[17]:=da[2];  {pitch}
    dts[16]:=da[3];  {roll}
    dts[18]:=da[4];  {yaw}
    dts[13]:=da[5];  {vx}
    dts[14]:=da[6];  {vy}
    dts[15]:=da[7];  {vz}

    dts[30]:=da[8];  {device pcmd}
    dts[31]:=da[9];
    dts[32]:=da[10];
    dts[33]:=da[11];
    dts[34]:=da[12];

    dts[24]:=da[13]; {height}
    dts[21]:=da[14]; {tas}
  end;

  procedure InsertData(z: integer);
  var
    mode: integer;

  begin
    mode:=FindTime(da[0]);
    case z of
      1: Insert1Hz;
      2: Insert5Hz
    end;
    case mode of
      1: csvGrid.Rows[rpos].AddStrings(dts);         {overwrite}
      2: csvGrid.InsertRowWithValues(rpos, dts);     {insert}
      3: csvGrid.InsertRowWithValues(csvGrid.RowCount, dts); {Add dataset at the end}
    end;
  end;

begin
  if da[0]>def0 then begin
    if source=0 then
      AddDatas                                     {Fill table first}
    else
      InsertData(source);                          {Insert 1Hz/5Hz datasets}
  end;
end;

procedure TForm1.LoadOneFile(idx: integer);        {Start working one file}
var inf: TFileStream;
    j0, j1, j2, j3: TJsonData;                     {4 level}
    fn, keyw: string;
    w, tas, tasmax, alt, altmax, distmax, timebase, timest: double;
    latp, lonp, latc, lonc: double;
    i, k, ttasmax, taltmax, tp, batt, battmin, tbattmin, tdistmax, spos: integer;
    tme, trt: TDateTime;
    datarray: TDatArr;

  function GetMString(const kw: string): string; inline;
                                                   {Skip destroyed data in file}
  begin
    try
      result:=j0.FindPath(kw).AsString;
    except
      result:=errWrongValue;
    end;
  end;

  function GetMFloat(const kw: string): double; inline;
                                                   {Skip destroyed data in file}
  begin
    try
      result:=j0.FindPath(kw).AsFloat;
    except
      result:=0;
    end;
  end;

  procedure WriteDtlGrid;                          {Fill Details table}
  var sn: string;
  begin
    dtlGrid.BeginUpdate;                           {Fill Details from level 0}
      keyw:=datProdName;
      dtlGrid.Cells[0, 1]:=prepKeyw(keyw);
      dtlGrid.Cells[1, 1]:=GetMString(keyw);
      keyw:=datProdID;
      dtlGrid.Cells[0, 2]:=prepKeyw(keyw);
      sn:=GetMString(keyw);
      lblProduct.Caption:=ProdIDtoLabel(sn);
      dtlGrid.Cells[1, 2]:=sn+': '+lblProduct.Caption;
      keyw:=datVersion;
      dtlGrid.Cells[0, 3]:=prepKeyw(keyw);
      dtlGrid.Cells[1, 3]:=j0.FindPath(keyw).AsString;

      keyw:=datSerialNo;
      sn:= GetMString(keyw);
      dtlGrid.Cells[0, 4]:=prepKeyw(keyw);
      dtlGrid.Cells[1, 4]:=sn;
      dtlGrid.Cells[0, 5]:=rsManufacture;
      dtlGrid.Cells[1, 5]:=SerialToManufacture(sn);

      keyw:=datHWvers;
      dtlGrid.Cells[0, 6]:=prepKeyw(keyw);
      dtlGrid.Cells[1, 6]:=GetMString(keyw);
      keyw:=datSWvers;
      dtlGrid.Cells[0, 7]:=prepKeyw(keyw);
      dtlGrid.Cells[1, 7]:=GetMString(keyw);

      keyw:=datRuntimeTotal;                       {Runtime total possibly with days}
      dtlGrid.Cells[0, 8]:=prepKeyw(keyw);
      trt:=SekToDT(GetMString(keyw), 1);
      dtlGrid.Cells[1, 8]:=FormatDateTime(hns, trt);
      if trt>=1 then
      dtlGrid.Cells[1, 8]:=IntTostr(trunc(trt))+'d '+dtlGrid.Cells[1, 7];

      keyw:=datCrash;
      dtlGrid.Cells[0, 9]:=prepKeyw(keyw);
      dtlGrid.Cells[1, 9]:=GetMString(keyw);;
      keyw:=datRCmodel;
      dtlGrid.Cells[0, 10]:=prepKeyw(keyw);
      dtlGrid.Cells[1, 10]:=GetMString(keyw);
      keyw:=datRCapp;
      dtlGrid.Cells[0, 11]:=prepKeyw(keyw);
      dtlGrid.Cells[1, 11]:=GetMString(keyw);
      keyw:=datUUID;
      dtlGrid.Cells[0, 12]:=UpCase(keyw);
      edUUID.Text:=GetMString(keyw);
      dtlGrid.Cells[1, 12]:=FormatUUID(edUUID.Text);
      keyw:=datGPSavail;
      dtlGrid.Cells[0, 13]:=prepKeyw(keyw);
      dtlGrid.Cells[1, 13]:=GetMString(keyw);
      keyw:=datGPSlat;
      dtlGrid.Cells[0, 14]:=prepKeyw(keyw);
      dtlGrid.Cells[1, 14]:=FormatFloat(frmCoord, GetMFloat(keyw));
      keyw:=datGPSlon;
      dtlGrid.Cells[0, 15]:=prepKeyw(keyw);
      dtlGrid.Cells[1, 15]:=FormatFloat(frmCoord, GetMFloat(keyw));
    dtlGrid.EndUpdate;
  end;

  procedure WriteStaGrid;                          {Fill statistics table}
  begin
    staGrid.BeginUpdate;
      staGrid.Cells[1, 1]:=FormatFloat(frmFloat, ConvUnit(19, altmax))+
                           UnitToStr(19, true);
      staGrid.Cells[2, 1]:=FormatDateTime(hnsz, taltmax/(secpd*1000)+tme);
      staGrid.Cells[1, 2]:=FormatFloat(frmFloat, ConvUnit(22, distmax, useau))+
                           UnitToStr(22, true, useau);
      staGrid.Cells[2, 2]:=FormatDateTime(hnsz, tdistmax/(secpd*1000)+tme);
      staGrid.Cells[1, 3]:=FormatFloat(frmFloat, ConvUnit(21, tasmax, useau))+
                           UnitToStr(21, true, useau);
      staGrid.Cells[2, 3]:=FormatDateTime(hnsz, ttasmax/(secpd*1000)+tme);
      if dtlGrid.Tag=0 then
        staGrid.Cells[1, 5]:=IntToStr(battmin)+UnitToStr(2, true)      {in %}
      else
        staGrid.Cells[1, 5]:=FormatFloat(frmOut3, battmin/1000)+UnitToStr(23, true);  {in V}
      staGrid.Cells[2, 5]:=FormatDateTime(hnsz, tbattmin/(secpd*1000)+tme);
    staGrid.EndUpdate;
  end;

  procedure ReadMetaHeader;                        {BlackBox: Read meta data from Header node}
  var
    s: string;

  begin
    j1:=j0.FindPath(jsonHeaderB);                  {Meta data}
    if j1<>nil then begin
      tme:=datBBtoDT(j1.FindPath(datUTC).AsString);
      timebase:=j1.FindPath(datTimeBase).AsFloat;
      if timebase>w then                           {Compared to 1st dataset 1Hz}
        timebase:=timebase/1000;                   {Correction for Android}

      dtlGrid.BeginUpdate;                         {Fill Meta data from header level 1/2}
        keyw:=datProdSerial;
        dtlGrid.RowCount:=dtlGrid.RowCount+1;
        dtlGrid.Cells[0, dtlGrid.RowCount-1]:=prepKeyw(keyw);
        s:=j1.FindPath(keyw).AsString;
        dtlGrid.Cells[1, dtlGrid.RowCount-1]:=s;
        dtlGrid.RowCount:=dtlGrid.RowCount+1;
        dtlGrid.Cells[0, dtlGrid.RowCount-1]:=rsManufacture;
        dtlGrid.Cells[1, dtlGrid.RowCount-1]:=SerialToManufacture(s);
        keyw:=datProdID;
        dtlGrid.RowCount:=dtlGrid.RowCount+1;
        dtlGrid.Cells[0, dtlGrid.RowCount-1]:=prepKeyw(keyw);
        s:=j1.FindPath(keyw).AsString;
        lblProduct.Caption:=ProdIDtoLabel(s);
        dtlGrid.Cells[1, dtlGrid.RowCount-1]:=s+': '+lblProduct.Caption;;

        keyw:=datProdFWhard;
        dtlGrid.RowCount:=dtlGrid.RowCount+1;
        dtlGrid.Cells[0, dtlGrid.RowCount-1]:=prepKeyw(keyw);
        dtlGrid.Cells[1, dtlGrid.RowCount-1]:=j1.FindPath(keyw).AsString;
        keyw:=datProdMotor;
        dtlGrid.RowCount:=dtlGrid.RowCount+1;
        dtlGrid.Cells[0, dtlGrid.RowCount-1]:=prepKeyw(keyw);
        dtlGrid.Cells[1, dtlGrid.RowCount-1]:=j1.FindPath(keyw).AsString;
        keyw:=datBootID;
        s:=j1.FindPath(keyw).AsString;
        edUUID.Text:=s;
        dtlGrid.RowCount:=dtlGrid.RowCount+1;
        dtlGrid.Cells[0, dtlGrid.RowCount-1]:=prepKeyw(keyw);
        dtlGrid.Cells[1, dtlGrid.RowCount-1]:=FormatUUID(s);
        keyw:=datProdFWsoft;
        dtlGrid.RowCount:=dtlGrid.RowCount+1;
        dtlGrid.Cells[0, dtlGrid.RowCount-1]:=prepKeyw(keyw);
        dtlGrid.Cells[1, dtlGrid.RowCount-1]:=j1.FindPath(keyw).AsString;
        keyw:=datDevModel;
        dtlGrid.RowCount:=dtlGrid.RowCount+1;
        dtlGrid.Cells[0, dtlGrid.RowCount-1]:=prepKeyw(keyw);
        dtlGrid.Cells[1, dtlGrid.RowCount-1]:=j1.FindPath(keyw).AsString;
        keyw:=datDeviceOS;
        dtlGrid.RowCount:=dtlGrid.RowCount+1;
        dtlGrid.Cells[0, dtlGrid.RowCount-1]:=prepKeyw(keyw);
        dtlGrid.Cells[1, dtlGrid.RowCount-1]:=j1.FindPath(keyw).AsString;
        keyw:=datProdBlackbox;
        dtlGrid.RowCount:=dtlGrid.RowCount+1;
        dtlGrid.Cells[0, dtlGrid.RowCount-1]:=prepKeyw(keyw);
        dtlGrid.Cells[1, dtlGrid.RowCount-1]:=j1.FindPath(keyw).AsString;

        j2:=j1.FindPath(datRC);                    {Remote controller}
        if j2<>nil then begin
          if j2<>nil then begin
            keyw:=datHWvers;
            dtlGrid.RowCount:=dtlGrid.RowCount+1;
            dtlGrid.Cells[0, dtlGrid.RowCount-1]:=rsRC+prepKeyw(keyw);
            dtlGrid.Cells[1, dtlGrid.RowCount-1]:=j2.FindPath(keyw).AsString;
            keyw:=datSWvers;
            dtlGrid.RowCount:=dtlGrid.RowCount+1;
            dtlGrid.Cells[0, dtlGrid.RowCount-1]:=rsRC+prepKeyw(keyw);
            dtlGrid.Cells[1, dtlGrid.RowCount-1]:=j2.FindPath(keyw).AsString;
            keyw:=datModel;
            dtlGrid.RowCount:=dtlGrid.RowCount+1;
            dtlGrid.Cells[0, dtlGrid.RowCount-1]:=rsRC+prepKeyw(keyw);
            dtlGrid.Cells[1, dtlGrid.RowCount-1]:=j2.FindPath(keyw).AsString;
            keyw:=datPI;
            dtlGrid.RowCount:=dtlGrid.RowCount+1;
            dtlGrid.Cells[0, dtlGrid.RowCount-1]:=rsRC+prepKeyw(datSerialNo);
            dtlGrid.Cells[1, dtlGrid.RowCount-1]:=j2.FindPath(keyw).AsString;
          end;
        end;
      dtlGrid.EndUpdate;
    end;
  end;

  procedure ReadDatasNode;                         {BlackBox: Read WiFi Metadata + datasets from Datas}
  var
    i, k: integer;
    s: string;

  begin
    j1:=j0.FindPath(jsonDatas);                    {Datasets datas}
    if j1<>nil then begin                          {Some metadata from Datas}
      for i:=0 to high(datarray) do
        datarray[i]:=def0;

      for k:=0 to j1.Count-1 do begin
        datarray[12]:='';                          {Columns that shall be empty except valid sets i.e. runID}
        for i:=14 to 19 do                         {Empty home & Take-off positions (alt/lat/lon)}
          datarray[i]:='';

        j2:=j1.Items[k];
        if j2<>nil then begin
          s:=j2.FindPath(jsonType).AsString;

          if s=jtypWIFIband then begin
            dtlGrid.RowCount:=dtlGrid.RowCount+1;
            dtlGrid.Cells[0, dtlGrid.RowCount-1]:=prepKeyw(s);
            dtlGrid.Cells[1, dtlGrid.RowCount-1]:=IntToStr(j2.FindPath(jsonDatas).AsInteger);
            Continue;
          end;
          if s=jtypWIFIctry then begin
            dtlGrid.RowCount:=dtlGrid.RowCount+1;
            dtlGrid.Cells[0, dtlGrid.RowCount-1]:=prepKeyw(s);
            dtlGrid.Cells[1, dtlGrid.RowCount-1]:=j2.FindPath(jsonDatas).AsString;
            Continue;
          end;

          if s=jtypWIFIchan then begin
            datarray[20]:=j2.FindPath(jsonDatas).AsString;   {Wifi channel}
            timest:=j2.FindPath(jsonTimest).AsFloat;
            datarray[0]:=FormatDateTime(ymd+' '+hnsz, (timest-timebase)/secpd+tme);
          end;
          if s=jtypProdBatt then begin
            datarray[1]:=j2.FindPath(jsonDatas).AsString;    {Battery in %}
            timest:=j2.FindPath(jsonTimest).AsFloat;
            datarray[0]:=FormatDateTime(ymd+' '+hnsz, (timest-timebase)/secpd+tme);
          end;
          if s=jtypProdFState then begin
            datarray[2]:=j2.FindPath(jsonDatas).AsString;    {Flying_state}
            timest:=j2.FindPath(jsonTimest).AsFloat;
            datarray[0]:=FormatDateTime(ymd+' '+hnsz, (timest-timebase)/secpd+tme);
          end;
          if s=jtypProdAlert then begin
            datarray[3]:=j2.FindPath(jsonDatas).AsString;    {Alert_state}
            timest:=j2.FindPath(jsonTimest).AsFloat;
            datarray[0]:=FormatDateTime(ymd+' '+hnsz, (timest-timebase)/secpd+tme);
          end;
          if s=jtypGPSfix then begin
            datarray[4]:=j2.FindPath(jsonDatas).AsString;    {GPS fix}
            timest:=j2.FindPath(jsonTimest).AsFloat;
            datarray[0]:=FormatDateTime(ymd+' '+hnsz, (timest-timebase)/secpd+tme);
          end;
          if s=jtypMotErr then begin
            datarray[5]:=j2.FindPath(jsonDatas).AsString;    {Motor Error}
            timest:=j2.FindPath(jsonTimest).AsFloat;
            datarray[0]:=FormatDateTime(ymd+' '+hnsz, (timest-timebase)/secpd+tme);
          end;
          if s=jtypRTH then begin
            datarray[6]:=j2.FindPath(jsonDatas).AsString;    {RTH state}
            timest:=j2.FindPath(jsonTimest).AsFloat;
            datarray[0]:=FormatDateTime(ymd+' '+hnsz, (timest-timebase)/secpd+tme);
          end;
          if s=jtypFLand then begin
            datarray[7]:=j2.FindPath(jsonDatas).AsString;    {Forced landing}
            timest:=j2.FindPath(jsonTimest).AsFloat;
            datarray[0]:=FormatDateTime(ymd+' '+hnsz, (timest-timebase)/secpd+tme);
          end;
          if s=jtypWind then begin
            datarray[8]:=j2.FindPath(jsonDatas).AsString;    {Wind}
            timest:=j2.FindPath(jsonTimest).AsFloat;
            datarray[0]:=FormatDateTime(ymd+' '+hnsz, (timest-timebase)/secpd+tme);
          end;
          if s=jtypVibr then begin
            datarray[9]:=j2.FindPath(jsonDatas).AsString;    {Vibration}
            timest:=j2.FindPath(jsonTimest).AsFloat;
            datarray[0]:=FormatDateTime(ymd+' '+hnsz, (timest-timebase)/secpd+tme);
          end;
          if s=jtypFPstate then begin
            datarray[10]:=j2.FindPath(jsonDatas).AsString;   {FP state}
            timest:=j2.FindPath(jsonTimest).AsFloat;
            datarray[0]:=FormatDateTime(ymd+' '+hnsz, (timest-timebase)/secpd+tme);
          end;
          if s=jtypFMstate then begin
            datarray[11]:=j2.FindPath(jsonDatas).AsString;   {FollowMe}
            timest:=j2.FindPath(jsonTimest).AsFloat;
            datarray[0]:=FormatDateTime(ymd+' '+hnsz, (timest-timebase)/secpd+tme);
          end;
          if s=jtypRunID then begin
            datarray[12]:=j2.FindPath(jsonDatas).AsString;   {RunID}
            timest:=j2.FindPath(jsonTimest).AsFloat;
            datarray[0]:=FormatDateTime(ymd+' '+hnsz, (timest-timebase)/secpd+tme);
          end;
          if s=jtypMPPbtn then begin
            datarray[13]:=j2.FindPath(jsonDatas).AsString;   {MPP button}
            timest:=j2.FindPath(jsonTimest).AsFloat;
            datarray[0]:=FormatDateTime(ymd+' '+hnsz, (timest-timebase)/secpd+tme);
          end;

          if s=jtypHome then begin                 {Home point}
            timest:=j2.FindPath(jsonTimest).AsFloat;
            datarray[0]:=FormatDateTime(ymd+' '+hnsz, (timest-timebase)/secpd+tme);
            j3:=j2.FindPath(jsonDatas);
            if j3<>nil then begin
              w:=j3.FindPath(jsonlat).AsFloat;
              if w<200 then begin
                datarray[14]:=FormatFloat(frmOut2, j3.FindPath(jsonalt).AsFloat);
                datarray[15]:=FormatFloat(frmCoord, w);
                datarray[16]:=FormatFloat(frmCoord, j3.FindPath(jsonlon).AsFloat);
              end else
                datarray[0]:=def0;                 {suppress output if invalid values}
            end;
          end;
          if s=jtypGPSto then begin                {GPS take-off point}
            timest:=j2.FindPath(jsonTimest).AsFloat;
            datarray[0]:=FormatDateTime(ymd+' '+hnsz, (timest-timebase)/secpd+tme);
            j3:=j2.FindPath(jsonDatas);
            if j3<>nil then begin
              w:=j3.FindPath(jsonlat).AsFloat;
              if w<200 then begin
                datarray[17]:=FormatFloat(frmOut2, j3.FindPath(jsonalt).AsFloat);
                datarray[18]:=FormatFloat(frmCoord, w);
                datarray[19]:=FormatFloat(frmCoord,j3.FindPath(jsonlon).AsFloat);
              end else
                datarray[0]:=def0;                 {suppress output if invalid values}
            end;
          end;
        end;
        InsertDataset(datarray, 0, spos);          {Add row to csvGrid}
      end;
    end;
  end;

  procedure Read1HzNode;                           {BlackBox: Reas data from 1Hz node}
  var
    i, k, volt: integer;

  begin
    j1:=j0.FindPath(json1Hz);                      {Datasets 1Hz datas}
    if j1<>nil then begin                          {Some metadata from Datas}
      for i:=0 to high(datarray) do                {Clear result array}
        datarray[i]:=def0;
      for k:=0 to j1.Count-1 do begin              {Read all datasets}

        j2:=j1.Items[k];
        if j2<>nil then begin
          timest:=j2.FindPath(jsonTimest).AsFloat;
          datarray[0]:=FormatDateTime(ymd+' '+hnsz, (timest-timebase)/secpd+tme);

          volt:=j2.FindPath(jtypVolt).AsInteger;
          datarray[1]:=FormatFloat(frmOut3, volt/1000);                         {Battery in V}
          if volt<battmin then begin
            battmin:=volt;                         {Minimal voltage}
            tbattmin:=round((timest-timebase)*1000); {time of min V}
          end;
          if staGrid.Cells[1, 4]='' then begin     {Max voltage set}
            staGrid.Cells[1, 4]:=datarray[1];
            staGrid.Cells[2, 4]:=FormatDateTime(hnsz, (timest-timebase)/secpd+tme);
          end;
          datarray[13]:=j2.FindPath(jtypRSSI).AsString;                         {WiFi RSSI}

          j3:=j2.FindPath(jtypRCgps);              {Controller position}
          if j3<>nil then begin
            datarray[2]:=FormatFloat(frmOut2, j3.FindPath(jsonalt).AsFloat);    {Alt}
            latc:=j3.FindPath(jsonlat).AsFloat;
            datarray[3]:=FormatFloat(frmCoord, latc);                           {Lat}
            lonc:=j3.FindPath(jsonlon).AsFloat;
            datarray[4]:=FormatFloat(frmCoord, lonc);                           {Lon}
          end;
          j3:=j2.FindPath(jtypProdGPS);            {Drone position}
          if j3<>nil then begin
            datarray[5]:=FormatFloat(frmOut2, j3.FindPath(jsonalt).AsFloat);    {Alt}
            latp:=j3.FindPath(jsonlat).AsFloat;
            datarray[6]:=FormatFloat(frmCoord, latp);                           {Lat}
            lonp:=j3.FindPath(jsonlon).AsFloat;
            datarray[7]:=FormatFloat(frmCoord, lonp);                           {Lon}
          end;
          j3:=j2.FindPath(jtypMPPcmd);             {MPP pcmd}
          if j3<>nil then begin
            datarray[8]:=j3.FindPath(jtypSource).AsString;
            datarray[9]:=j3.FindPath(jtypVert).AsString;
            datarray[10]:=j3.FindPath(jtypPitch).AsString;
            datarray[11]:=j3.FindPath(jtypRoll).AsString;
            datarray[12]:=j3.FindPath(jtypYaw).AsString;
          end;
          if (latp<200) and (latc<200) then begin      {Compute distance}
            w:=DeltaKoord(latc, lonc, latp, lonp);     {Distance RC - A/C}
            datarray[14]:=FormatFloat(frmOut1, w);
            if w>distmax then begin
              distmax:=w;                              {highest distance}
              tdistmax:=round((timest-timebase)*1000); {time of max distance}
            end;
          end;
        end;
        InsertDataset(datarray, 1, spos);          {Insert row to csvGrid}
      end;
    end;
  end;

  procedure Read5HzNode;                           {BlackBox: Reas data from 5Hz node}
  var
    i, k: integer;
    vx, vy, vz: double;

  begin
    j1:=j0.FindPath(json5Hz);                      {Datasets 1Hz datas}
    if j1<>nil then begin                          {Some metadata from Datas}
      for i:=0 to high(datarray) do                {Clear result array}
        datarray[i]:=def0;

      for k:=0 to j1.Count-1 do begin
        j2:=j1.Items[k];
        if j2<>nil then begin
          timest:=j2.FindPath(jsonTimest).AsFloat;
          datarray[0]:=FormatDateTime(ymd+' '+hnsz, (timest-timebase)/secpd+tme);
          alt:=j2.FindPath(jtypProdAlt).AsFloat;
          datarray[1]:=FormatFloat(frmOut2, alt);  {Altitude}
          if alt>altmax then begin
            altmax:=alt;
            taltmax:=round((timest-timebase)*1000);
          end;
          datarray[13]:=FormatFloat(frmOut2, j2.FindPath(jtypHeight).AsFloat);  {Height}

          j3:=j2.FindPath(jtypProdAngles);         {Angles}
          if j3<>nil then begin
            datarray[2]:=j3.FindPath(jtypPitch).AsString;
            datarray[3]:=j3.FindPath(jtypRoll).AsString;
            datarray[4]:=j3.FindPath(jtypYaw).AsString;
          end;
          j3:=j2.FindPath(jtypProdSpeed);          {Speed}
          if j3<>nil then begin
            vx:=j3.FindPath(jtypVX).AsFloat;
            vy:=j3.FindPath(jtypVY).AsFloat;
            vz:=j3.FindPath(jtypVZ).AsFloat;
            datarray[5]:=FormatFloat(frmFloat, vx);
            datarray[6]:=FormatFloat(frmFloat, vy);
            datarray[7]:=FormatFloat(frmFloat, vz);
            tas:=sqrt((vx*vx)+(vy*vy)+(vz*vz));    {tas}
            if tas>tasmax then begin
              tasmax:=tas;
              ttasmax:=round((timest-timebase)*1000);
            end;
            datarray[14]:=FormatFloat(frmFloat, tas);
          end;
          j3:=j2.FindPath(jtypRCcmd);              {pcmd}
          if j3<>nil then begin
            datarray[8]:=j3.FindPath(jtypFlag).AsString;
            datarray[9]:=j3.FindPath(jtypVert).AsString;
            datarray[10]:=j3.FindPath(jtypPitch).AsString;
            datarray[11]:=j3.FindPath(jtypRoll).AsString;
            datarray[12]:=j3.FindPath(jtypYaw).AsString;
          end;
        end;
        InsertDataset(datarray, 2, spos);          {Insert row to csvGrid}
      end;
    end;
  end;

begin
  tme:=0;
  spos:=1;                                         {Insert dataset from this position}
  battmin:=99999;
  tasmax:=0;
  altmax:=-9999;
  distmax:=0;
  dtlGrid.Tag:=3;                                  {undef}
  ProgressFile.Position:=0;
  if ovGrid.Cells[0, idx]<>'' then begin
    fn:=IncludeTrailingPathDelimiter(LogDir.Directory)+ovGrid.Cells[0, idx]+jext;
    if FileExists(fn) then begin
      inf:=TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
      Screen.Cursor:=crHourGlass;
      Application.Processmessages;
      try
        if inf.Size>512 then begin                 {0.5kB minimal}
          csvGrid.RowCount:=1;
          if Form2<>nil then
            Form2.Close;                           {Close add chart when new file}
          try
            csvGrid.Tag:=idx;                      {identify index of current file}
            j0:=GetJson(inf);               {load whole JSON file, level 0, Metadata}
            try
{Read and write Header, (re)create columns in data table, check file type}
              j1:=j0.FindPath(jsonHeaderJ);          {load header}
              if j1<>nil then begin
                dtlGrid.Tag:=0;                      {legacy JSON file}
                MetagridInit(dtlGrid.Tag);           {Set standard labels for JSON}
                csvGrid.ColCount:=ncjs;
                for i:=0 to j1.Count-1 do begin
                  hdrList[i]:=j1.Items[i].AsString;  {fill original header array}
                end;
                ChangeHeader;                        {use alternative headers or not}
              end;

              j1:=j0.FindPath(json1Hz);              {Test if Blackbox file}
              if j1<>nil then begin
                dtlGrid.Tag:=2;                      {legacy JSON file}
                MetagridInit(dtlGrid.Tag);           {Set standard labels for JSON}
                csvGrid.ColCount:=lcbb+1;
                BlackboxHeader;                      {Blackbox CSV header}
                j2:=j1.Items[0];                     {1Hz first dataset}
                if j2<> nil then                     {w: temporäry value for Android correction}
                  w:=j2.FindPath(jsonTimest).AsFloat;
              end;
              csvGrid.AutoSizeColumns;               {Align to headers}
              StatusBar1.Panels[4].Text:=fn;         {Show file name}

///////////// legacy/easy JSON format ////////////////
              if dtlGrid.Tag=0 then begin
                tme:=datISOtoDT(j0.FindPath(datUTC).AsString, ymd); {Date/Time from meta data}
                j1:=j0.FindPath(jsonData);             {Datasets, Level 1}
                if j1<>nil then begin
                  StatusBar1.Panels[1].Text:=IntToStr(j1.Count);
                  j2:=j1.Items[0];                     {First dataset: Begin time}
                                                       {Battery level max + time}
                  if j2<>nil then begin
                    staGrid.Cells[1, 4]:=IntToStr(j2.Items[1].AsInteger)+
                                         Form1.UnitToStr(2, true);
                    staGrid.Cells[2, 4]:=FormatDateTime(hnsz,
                                         SekToDT(j2.Items[0].AsString, 1)+tme);
                  end;
                  ProgressFile.Max:=j1.Count-1;

                  csvGrid.BeginUpdate;
                    csvGrid.RowCount:=j1.Count+1;
                    for i:=0 to j1.Count-1 do begin    {Read datasets}
                      j2:=j1.Items[i];                 {read data, level 2}
                      if j2<>nil then begin
                        for k:=0 to J2.Count-1 do begin     {Fill one Row}
                          case k of                    {distribute values}
                            0: tp:=j2.Items[k].AsInteger;
                            1: batt:=j2.Items[k].AsInteger;
                            2: latc:=j2.Items[k].AsFloat;
                            3: lonc:=j2.Items[k].AsFloat;
                            8: lonp:=j2.Items[k].AsFloat;
                            9: latp:=j2.Items[k].AsFloat;
                            12..14: csvGrid.Cells[k+1, i+1]:=
                                      FormatFloat(frmFloat, j2.Items[k].AsFloat);
                            15..17: csvGrid.Cells[k+1, i+1]:=
                                      FormatFloat(frmFloat,
                                      ConvUnit(k+1, j2.Items[k].AsFloat, cbDegree.Checked));
                            18: alt:=j2.Items[k].AsFloat;
                            20: tas:=j2.Items[k].AsFloat;
                          else
                            csvGrid.Cells[k+1, i+1]:=j2.Items[k].AsString;
                          end;
                        end;
                                                       {fill fix columns}
                        csvGrid.Cells[0, i+1]:=
                          FormatDateTime(ymd+tab1+hnsz, tp/(secpd*1000)+tme);
                        csvGrid.Cells[1, i+1]:=IntToStr(tp);
                        csvGrid.Cells[2, i+1]:=IntToStr(batt);
                        if batt<battmin then begin
                          battmin:=batt;               {lowest batt level}
                          tbattmin:=tp;                {time of lowest batt level}
                        end;
                        csvGrid.Cells[3, i+1]:=FormatFloat(frmCoord, latc);
                        csvGrid.Cells[4, i+1]:=FormatFloat(frmCoord, lonc);
                        csvGrid.Cells[9, i+1]:=FormatFloat(frmCoord, lonp);
                        csvGrid.Cells[10, i+1]:=FormatFloat(frmCoord, latp);
                        csvGrid.Cells[19, i+1]:=FormatFloat(frmFloat, alt);
                        if alt>altmax then begin
                          altmax:=alt;                 {highest altitude}
                          taltmax:=tp;                 {time of highest altitude}
                        end;
                        csvGrid.Cells[21, i+1]:=FormatFloat(frmFloat, tas);
                        if tas>tasmax then begin
                          tasmax:=tas;                 {highest speed}
                          ttasmax:=tp;                 {time of max speed}
                        end;
                        w:=DeltaKoord(latc, lonc, latp, lonp);   {Distance RC - A/C}
                        csvGrid.Cells[22, i+1]:=FormatFloat(frmOut1, w);
                        if w>distmax then begin
                          distmax:=w;                  {highest distance}
                          tdistmax:=tp;                {time of max distance}
                        end;
                        ProgressFile.Position:=i;
                        WriteDtlGrid;                  {Write statistics}
                        WriteStaGrid;                  {Fill statistics table}
                      end;
                    end;
                    csvGrid.AutoSizeColumn(0);
                  csvGrid.EndUpdate(false);

                end;
              end;

///////////// Blackbox JSON format ////////////////
              if dtlGrid.Tag=2 then begin
                ReadMetaHeader;       {Read Meta data from node Header, set tme and timebase}
                staGrid.Cells[1, 4]:='';
                staGrid.Cells[2, 4]:='';
                csvGrid.BeginUpdate;
                try
                  ReadDatasNode;     {Set up the table with time line, must be the first action}

                  spos:=1;           {Start to search at the beginning}
                  Read5HzNode;       {Insert datasets at right position in the timeline}
                  spos:=1;
                  Read1HzNode;

                  if csvGrid.RowCount>3 then begin
                     for i:=1 to csvGrid.ColCount-1 do begin   {Fill empty fiels in first row}
                       if csvGrid.Cells[i, 1]='' then begin
                         case i of
                           1, 2, 5..8, 11..41: csvGrid.Cells[i, 1]:=def0;
                           3, 4, 9..10: csvGrid.Cells[i, 1]:='500';
                         end;
                       end;
                     end;
                     for k:=2 to csvGrid.RowCount-1 do begin
                       for i:=1 to 41 do begin
                         if csvGrid.Cells[i, k]='' then
                           csvGrid.Cells[i, k]:=csvGrid.Cells[i, k-1];
                       end;
                     end;
                  end;

                  WriteStaGrid;
                  csvGrid.AutoSizeColumn(0);
                finally
                  csvGrid.EndUpdate;
                end;
                StatusBar1.Panels[1].Text:=IntToStr(csvGrid.RowCount-1);
              end;

            finally
              j0.Free;
            end;
          except
            StatusBar1.Panels[4].Text:=errWrongData+ExtractFileName(fn);
          end;
        end else                                   {Not enough data}
          StatusBar1.Panels[4].Text:=errLessData+ExtractFileName(fn);
      finally
        inf.Free;
        Screen.Cursor:=crDefault;
        cbDegree.Tag:=0;                           {Reload done}
      end;                                         {End load and read file}
    end;                                           {End if file exists}
  end;                                             {End if file name available}
end;

procedure TForm1.MakeHDia;                         {Fill H-diagrams}
var i: integer;
    w1, w2: double;
    bg: TDateTime;
begin
  if csvGrid.RowCount>jsonMinLines then begin      {minimum 20 lines}
    Chart1AreaSeries1.Clear;
    Chart1LineSeries1.Clear;
    Chart1.AxisList[0].Title.Caption:=AltHeaderToStr(19)+    {y left altitude}
                                      tab1+UnitToStr(19, false);
    Chart1.ZoomFull;
    case grpDia.ItemIndex of                       {y-axis altitude right}
      0: Chart1.AxisList[2].Title.Caption:=ahdr1+tab1+UnitToStr(2, false);
      1: Chart1.AxisList[2].Title.Caption:=ahdr6+tab1+UnitToStr(7, false);
      2: Chart1.AxisList[2].Title.Caption:=ahdr11+tab1+UnitToStr(12, false);
      3: Chart1.AxisList[2].Title.Caption:=ahdr14+tab1+UnitToStr(15, false);
      4: Chart1.AxisList[2].Title.Caption:=ahdr20+tab1+UnitToStr(21, false, useau);
      5: Chart1.AxisList[2].Title.Caption:=ahdr21+tab1+UnitToStr(22, false, useau);
      6: Chart1.AxisList[2].Title.Caption:=ahdr4;  {Flight mode w/o unit}
    end;
    for i:=1 to csvGrid.RowCount-1 do begin        {Create Diagrams from csvGrid}
      try
        w1:=0;                                     {Default, also in case of errors}
        case grpDia.ItemIndex of
          0: w1:=StrToInt(csvGrid.Cells[2, i]);    {Battery}
          1: w1:=StrToFloat(csvGrid.Cells[7, i]);  {WiFi}
          2: w1:=StrToInt(csvGrid.Cells[12, i]);   {Num Sats}
          3: w1:=-ConvUnit(15, StrToFloat(csvGrid.Cells[15, i])); {Vertical speed}
          4: w1:=ConvUnit(21, StrToFloat(csvGrid.Cells[21, i]), useau);      {tas}
          5: w1:=ConvUnit(22, StrToFloat(csvGrid.Cells[22, i]), useau); {Distance}
          6: w1:=StrToInt(csvGrid.Cells[5, i]);    {Flight mode}
        end;
        bg:=ScanDateTime(ymd+tab1+hnsz, csvGrid.Cells[0, i]);
        Chart1LineSeries1.AddXY(bg, w1);           {Selected column}
        w2:=ConvUnit(19, StrToFloat(csvGrid.Cells[19, i]));
        Chart1AreaSeries1.AddXY(bg, w2);           {Altitude}
      except                                       {Error data conversion}
        w2:=0;                                     {Altitude = 0}
      end;
    end;
  end;
end;

procedure TForm1.MakeAtti;                         {Fill attitude chart}
var i: integer;
    w: double;
    bg: TDateTime;
begin
  if csvGrid.RowCount>jsonMinLines then begin      {minimum 20 lines}
    Chart2.AxisList[0].Title.Caption:=dtSpeed+tab1+UnitToStr(15, false);
    Chart2.AxisList[2].Title.Caption:=dtSpeed+tab1+UnitToStr(16, false, cbDegree.Checked);
    Chart2LineSeries1.Title:=csvGrid.Cells[13, 0]; {Write titles from headers}
    Chart2LineSeries2.Title:=csvGrid.Cells[14, 0];
    Chart2LineSeries3.Title:=csvGrid.Cells[15, 0];
    Chart2LineSeries4.Title:=csvGrid.Cells[16, 0];
    Chart2LineSeries5.Title:=csvGrid.Cells[17, 0];
    Chart2LineSeries6.Title:=csvGrid.Cells[18, 0];
    Chart2LineSeries1.Clear;                       {xyz speed}
    Chart2LineSeries2.Clear;
    Chart2LineSeries3.Clear;
    Chart2LineSeries4.Clear;                       {xyz angle}
    Chart2LineSeries5.Clear;
    Chart2LineSeries6.Clear;
    Chart2.ZoomFull;                               {Reset zoom if any}
    for i:=1 to csvGrid.RowCount-1 do begin        {Create Diagrams from csvGrid}
      try
        bg:=ScanDateTime(ymd+tab1+hnsz, csvGrid.Cells[0, i]); {Time base}
        w:=ConvUnit(13, StrToFloat(csvGrid.Cells[13, i]));    {speed vx}
        Chart2LineSeries1.AddXY(bg, w);
        w:=ConvUnit(14, StrToFloat(csvGrid.Cells[14, i]));    {speed vy}
        Chart2LineSeries2.AddXY(bg, w);
        w:=-ConvUnit(15, StrToFloat(csvGrid.Cells[15, i]));   {Vertical speed}
        Chart2LineSeries3.AddXY(bg, w);
        w:=StrToFloat(csvGrid.Cells[16, i]);       {Angle phi}
        Chart2LineSeries4.AddXY(bg, w);
        w:=StrToFloat(csvGrid.Cells[17, i]);       {Angle theta}
        Chart2LineSeries5.AddXY(bg, w);
        w:=StrToFloat(csvGrid.Cells[18, i]);       {Angle psi - heading}
        Chart2LineSeries6.AddXY(bg, w);
      except                                       {Error data conversion}
        w:=0;                                      {value = 0}
      end;
    end;
  end;
end;

procedure TForm1.MakeCSV;                          {Create CSV file from csvGrid}
var fn: string;
begin
  fn:=IncludeTrailingPathDelimiter(LogDir.Directory)+
      ovGrid.Cells[0, csvGrid.Tag]+grpConv.Items[2];
  csvGrid.SaveToCSVFile(fn, GetCSVsep, true);
  StatusBar1.Panels[4].Text:=ExtractFileName(fn)+tab1+rsSaved;
end;

function ValidModeCoord(fm, lat, lon: string): boolean;        {Validity check}
begin
  result:=StrToIntDef(fm, 0) in rfm;               {state in real flight modes}
  try
    result:=result and
            (StrToFloat(lat)<100) and              {GPS data valid}
            (StrToFloat(lon)<200);
  except
  end;
end;

procedure TForm1.KMLheader(klist: TStringList);    {KML header and styles}
begin
  klist.Add(xmlvers);
  klist.Add(kmlvers);
  klist.Add('<'+doctag);
  klist.Add('<name>'+ovGrid.Cells[0, csvGrid.Tag]+'</name>');
  klist.Add('<description>'+csvGrid.Cells[0, 1]+'</description>');

  klist.Add('<Style id="Flightpath">');            {Style flight track}
  klist.Add(tab2+'<LineStyle>');
  klist.Add(tab4+'<color>'+ColorToKMLColor(btColor.ButtonColor, 255)+
                 '</color>');                      {KML track color}
  klist.Add(tab4+'<width>2</width>');
  klist.Add(tab2+'</LineStyle>');
  klist.Add(tab2+'<PolyStyle><color>7f00ff00</color></PolyStyle>'); {for Waypoints}
  klist.Add(tab2+'<IconStyle><Icon><href>'+aircrafticon+'</href></Icon></IconStyle>');
  klist.Add('</Style>');

  klist.Add('<Style id="GrndStn">');               {Style track RC}
  klist.Add(tab2+'<LineStyle>');
  klist.Add(tab4+'<color>FF000000</color>');       {Color black for RC}
  klist.Add(tab4+'<width>2</width>');
  klist.Add(tab2+'</LineStyle>');
  klist.Add('</Style>');

  klist.Add('<Style id="starting">');
  klist.Add(tab2+'<IconStyle><Icon><href>'+starticon+'</href></Icon></IconStyle>');
  klist.Add('</Style>');

  klist.Add('<Style id="landing">');
  klist.Add(tab2+'<IconStyle><Icon><href>'+stopicon+'</href></Icon></IconStyle>');
  klist.Add('</Style>');
end;

{https://developers.google.com/kml/documentation/kml_tut
 http://www.zonums.com/gmaps/kml_color/
 http://kml4earth.appspot.com/icons.html     (Icons)
 http://googlegeodevelopers.blogspot.de/2010/07/making-tracks-new-kml-extensions-in.html
 http://gps.hillclimb.de/?page_id=504

 <extrude>1</extrude>
 Extrude draws vertical lines from flight track to ground}

procedure TForm1.MakeKML;                          {Create KML from StringGrid}
var i, p, l, state: integer;
    kmllist, colist, rclist: TSTringList;
    fn: string;
begin
  if csvGrid.RowCount>jsonMinLines then begin      {minimum 20 lines}
    kmllist:=TStringList.Create;
    colist:=TStringList.Create;
    rclist:=TStringList.Create;
    Screen.Cursor:=crHourGlass;
    try
      KMLHeader(kmllist);                          {write headers}
      p:=0;
      repeat                                       {looking for first start coord}
        inc(p);
      until ValidModeCoord(csvGrid.Cells[5, p],    {state in real flight modes}
                           csvGrid.Cells[10, p],   {coordinates lat, lon}
                           csvGrid.Cells[9, p]) or
            (p>csvGrid.RowCount-3);                {table at the end}
      if csvGrid.RowCount>(p+5) then begin         {some lines should be in}
        kmllist.Add('<'+pmtag);                    {Placemark Start}
        kmllist.Add('<TimeStamp><when>'+
                    GoogleTime(csvGrid.Cells[0, p])+
                    '</when></TimeStamp>');
        kmllist.Add('<styleUrl>#starting</styleUrl>');
        kmllist.Add('<Point><'+cotag+
                    csvGrid.Cells[9, p]+','+       {lon}
                    csvGrid.Cells[10, p]+',0.0</'+cotag+'</Point>');
        kmllist.Add('</'+pmtag);
        kmllist.Add('<'+pmtag);
        kmllist.Add(tab2+'<name>'+dtlGrid.Cells[1, 1]+'</name>');
        kmllist.Add(tab2+'<description>'+dtlGrid.Cells[1, 1]+'</description>');
        kmllist.Add(tab2+'<styleUrl>#Flightpath</styleUrl>');
        kmllist.Add(tab2+'<gx:Track>');
        kmllist.Add(tab4+'<'+amtag+'relativeToGround</'+amtag);
        if cbExtrude.Checked then
          kmllist.Add(tab4+extru);
        for i:=p to csvGrid.RowCount-1 do begin    {next line till the end}
          state:=StrToIntDef(csvGrid.Cells[5, i], 0);
          if (state in rfm) then begin             {Only flight modes}
            colist.Add(tab6+'<gx:coord>'+
                       csvGrid.Cells[9, i]+tab1+   {lon}
                       csvGrid.Cells[10, i]+tab1+  {lat}
                       csvGrid.Cells[19, i]+       {alt}
                       '</gx:coord>');
            rclist.Add(tab6+csvGrid.Cells[4, i]+','+
                            csvGrid.Cells[3, i]);  {lat RC track}

            l:=i;                                  {index last coordinate}
            kmllist.Add(tab6+'<when>'+GoogleTime(csvGrid.Cells[0, i])+'</when>');
          end;
        end;
        for i:=0 to colist.Count-1 do
          kmllist.Add(colist[i]);
        kmllist.Add(tab2+'</gx:Track>');
        kmllist.Add('</'+pmtag);

        kmllist.Add('<'+pmtag);                    {Placemark Landing}
        kmllist.Add('<TimeStamp><when>'+
                    GoogleTime(csvGrid.Cells[0, l])+
                    '</when></TimeStamp>');
        kmllist.Add('<styleUrl>#landing</styleUrl>');
        kmllist.Add('<Point><'+cotag+
                    csvGrid.Cells[9, l]+','+
                    csvGrid.Cells[10, l]+','+
                    csvGrid.Cells[19, l]+          {alt}
                    '</'+cotag+'</Point>');
        kmllist.Add('</'+pmtag);

        kmllist.Add('<'+pmtag);                    {RC track}
        kmllist.Add('<name>RC</name>');
        kmllist.Add('<description>RC</description>');
        kmllist.Add('<styleUrl>#GrndStn</styleUrl>');
        kmllist.Add('<LineString>');
        kmllist.Add(tab2+'<'+amtag+'clampToGround</'+amtag);
        kmllist.Add(tab4+'<'+cotag);
        for i:=1 to rclist.Count-1 do
          kmllist.Add(rclist[i]);                  {RC track coordinates}
        kmllist.Add(tab4+'</'+cotag);
        kmllist.Add('</LineString>');
        kmllist.Add('</'+pmtag);
        kmllist.Add('</'+doctag);
        kmllist.Add('</kml>');
        fn:=IncludeTrailingPathDelimiter(LogDir.Directory)+
                           ovGrid.Cells[0, csvGrid.Tag]+
                           grpConv.Items[0];
        kmllist.SaveToFile(fn);                    {Save KML file}
        StatusBar1.Panels[4].Text:=ExtractFileName(fn)+tab1+rsSaved;
      end;
    finally
      kmllist.Free;
      colist.Free;
      rclist.Free;
      Screen.Cursor:=crDefault;
    end;
  end;
end;

{GPX format:
 http://www.topografix.com/gpx.asp
 http://www.topografix.com/gpx/1/1/
 https://en.wikipedia.org/wiki/GPS_Exchange_Format
 http://www.doarama.com/api/0.2/docs
 http://publicgpx.blog-me.de/2010/11/13/gpx-dateiformat-1/

Show GPX or KML files: http://www.atlsoft.de/gpx/
                       https://ayvri.com/
}

procedure TForm1.MakeGPX;                          {Create GPX from StringGrid}
var i, p, state: integer;
    outlist: TSTringList;
    fn: string;
begin
  if csvGrid.RowCount>jsonMinLines then begin      {minimum 20 lines}
    outlist:=TStringList.Create;
    Screen.Cursor:=crHourGlass;
    try
      p:=0;
      repeat                                       {looking for first valid start coord}
        inc(p);
      until ValidModeCoord(csvGrid.Cells[5, p],    {state in real flight modes}
                           csvGrid.Cells[10, p],   {coordinates lat, lon valid?}
                           csvGrid.Cells[9, p]) or
            (p>csvGrid.RowCount-3);                {table at the end}
      if csvGrid.RowCount>(p+5) then begin         {some lines should be in}
        outlist.Add(xmlvers);                      {Write header}
        outlist.Add(gpxvers+' creator="'+Caption+'">');
        outlist.Add('<metadata>');
        outlist.Add(tab2+'<name>'+ovGrid.Cells[0, csvGrid.Tag]+'</name>');
        outlist.Add(tab2+'<desc>'+ovGrid.Cells[0, csvGrid.Tag]+'</desc>');
        outlist.Add('</metadata>');
                                                   {Waypoint Begin}
        outlist.Add('<wpt '+GPXlat+
                    csvGrid.Cells[10, p]+GPXlon+
                    csvGrid.Cells[9, p]+'">');
        outlist.Add(tab2+'<ele>0.0</ele>');
        outlist.Add(tab2+'<time>'+GoogleTime(csvGrid.Cells[0, p], true)+
                    '</time>');
        outlist.Add(tab2+'<name>Begin</name>');
        outlist.Add(GPXet1);

        outlist.Add('<trk>');                      {Track segment}
        outlist.Add(tab2+'<trkseg>');
        outlist.Add(tab2+'<name>'+dtlGrid.Cells[1, 1]+'</name>');
        for i:=p to csvGrid.RowCount-1 do begin    {Rest of the data list}
          state:=StrToIntDef(csvGrid.Cells[5, i], 0);
          if (state in rfm) then begin             {Only in real flight modes}
             outlist.Add(tab4+'<trkpt '+GPXlat+    {GPX Trackpoints}
                         csvGrid.Cells[10, i]+GPXlon+
                         csvGrid.Cells[9, i]+'"> <ele>'+
                         csvGrid.Cells[19, i]+'</ele> <time>'+
                         GoogleTime(csvGrid.Cells[0, i], true)+
                         '</time></trkpt>');
          end;
        end;
        outlist.Add(tab2+'</trkseg>');
        outlist.Add('</trk>');
                                                   {Waypoint End}
        outlist.Add('<wpt '+GPXlat+
                    csvGrid.Cells[10, i]+GPXlon+
                    csvGrid.Cells[9, i]+'">');
        outlist.Add(tab2+'<ele>'+csvGrid.Cells[19, i]+'</ele>');
        outlist.Add(tab2+'<time>'+GoogleTime(csvGrid.Cells[0, i], true)+
                    '</time>');
        outlist.Add(tab2+'<name>End</name>');
        outlist.Add(GPXet1);
        outlist.Add('</gpx>');
      end;
      fn:=IncludeTrailingPathDelimiter(LogDir.Directory)+
                               ovGrid.Cells[0, csvGrid.Tag]+
                               grpConv.Items[1];
      outlist.SaveToFile(fn);                      {Save GPX file}
      StatusBar1.Panels[4].Text:=ExtractFileName(fn)+tab1+rsSaved;
    finally
      outlist.Free;
      Screen.Cursor:=crDefault;
    end;
  end;
end;

{https://parrotpilots.com/threads/best-pilot-log-book-format.1320/}

procedure TForm1.CreateLogBook;                    {Create Pilot log book}
var outlist: TStringList;
    sep: Char;
    ext: string;
const
  fotab='%-20s';

  procedure CSVAusgabe;                            {Output in CSV format}
  var i: integer;
      s: string;
  begin
    sep:=GetCSVsep;
    ext:=cext;                                     {CSV file}
    for i:=1 to ovGrid.RowCount-1 do begin         {Fill data}
      s:=ovGrid.Cells[hc, i];
      outlist.Add(StringReplace(s, isep, sep, [rfReplaceAll]));
    end;
    outlist.Sort;
    s:=dtlGrid.Cells[0, 4];
    for i:=1 to 12 do                              {Create header}
      s:=s+sep+ovheader[i];
    outlist.Insert(0, s);
  end;

  procedure TextAusgabe;                           {Output in Textformat}
  var i, k: integer;
      sn: string;
      splitlist, inlist: TStringList;
  begin
    sn:='';
    inlist:=TStringList.Create;
    splitlist:=TStringList.Create;
    splitlist.Delimiter:=isep;
    splitlist.StrictDelimiter:=true;
    try
      for i:=1 to ovGrid.RowCount-1 do
        inlist.Add(ovGrid.Cells[hc, i]);
      inlist.Sort;
      for i:=0 to inlist.Count-1 do begin
        splitlist.DelimitedText:=StringReplace(inlist[i], '"', '', [rfReplaceAll]);
        if splitlist.Count>11 then begin
          if splitlist[0]<>sn then begin
            sn:=splitlist[0];
            outlist.Add('');
            outlist.Add(Format(fotab, [dtlGrid.Cells[0, 4]+dpkt])+sn);
            outlist.Add('');
          end;
          for k:=1 to 12 do
            outlist.Add(Format(fotab, [ovheader[k]+dpkt])+splitlist[k]);
          outlist.Add('');
        end;
      end;
    finally
      splitlist.Free;
      inlist.Free;
    end;
  end;

begin
  ext:='.txt';                                     {Default: text file}
  if ovGrid.RowCount>1 then begin
    Screen.Cursor:=crHourGlass;
    outlist:=TStringList.Create;
    try
      ProgressFile.Max:=ovGrid.RowCount-1;
      ProgressFile.Position:=0;
      case grpLogBook.ItemIndex of
        0: TextAusgabe;
        1: CSVAusgabe;
      end;
      outlist.Insert(0, '');
      outlist.Insert(0, rsLogbook+tab2+'['+FormatDateTime(ymd, now)+']');
      SaveDialog1.Title:=rsLogBook+tab1+mniFileSave;
      SaveDialog1.FileName:=rsLogBook+FormatDateTime(ymd, now)+ext;
      if SaveDialog1.Execute then begin
        outlist.SaveToFile(SaveDialog1.FileName);
        StatusBar1.Panels[4].Text:=SaveDialog1.FileName+tab1+rsSaved;
      end;
    finally
      outlist.Free;
      Screen.Cursor:=crDefault;
    end;
  end;
end;

end.

