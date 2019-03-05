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
csvGrid.Tag:       Current used file index (1...n)
ovGrid.Tag:        Selected file index
PageControl1.Tag:  Last used tab-page

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

Icon and splash screen by Augustine (Canada):
https://parrotpilots.com/threads/json-files-and-airdata-com.1156/page-5#post-10388

See also:
http://blog.nirsoft.net/2009/05/17/antivirus-companies-cause-a-big-headache-to-small-developers/

*)

unit showanafilog_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TATransformations, TAIntervalSources,
  TASeries, TATools, Forms, Controls, Graphics, fpjson, jsonparser,
  Dialogs, StdCtrls, Grids, ComCtrls, XMLPropStorage, EditBtn, math, Buttons,
  strutils, dateutils, LCLIntf, LCLType, ExtCtrls, Menus, Types, anzwerte;

{$I anafi_en.inc}                                  {Include a language file}
{.$I anafi_dt.inc}

type

  { TForm1 }

  TForm1 = class(TForm)
    btLogBook: TBitBtn;
    btScrShot: TBitBtn;
    btClose: TBitBtn;
    btConv: TBitBtn;
    Chart1: TChart;
    Chart1AreaSeries1: TAreaSeries;
    Chart1LineSeries1: TLineSeries;
    ChartAxisTransformations1: TChartAxisTransformations;
    cbExtrude: TCheckBox;
    cbHeader: TCheckBox;
    ChartAxisTransformations1AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    ChartAxisTransformations2: TChartAxisTransformations;
    ChartAxisTransformations2AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    btColor: TColorButton;
    cbCSVsep: TCheckBox;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointCrosshairTool1: TDataPointCrosshairTool;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomMouseWheelTool1: TZoomMouseWheelTool;
    cmnClipbrd2: TMenuItem;
    cmnSaveAs2: TMenuItem;
    csvGrid: TStringGrid;
    boxConv: TGroupBox;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    boxLogBook: TGroupBox;
    grpConv: TRadioGroup;
    lblDetails: TLabel;
    lblStat: TLabel;
    grpDia: TRadioGroup;
    lblManual: TLabel;
    lblDownload: TLabel;
    MainMenu1: TMainMenu;
    cmnSaveAs: TMenuItem;
    cmnClipbrd: TMenuItem;
    MenuItem1: TMenuItem;
    cmnShowGM: TMenuItem;
    cmnShowOSM: TMenuItem;
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
    PopupMenu1: TPopupMenu;                        {for Overview and Charts}
    PopupMenu2: TPopupMenu;                        {For Details table}
    LogDirDialog: TSelectDirectoryDialog;
    ProgressFile: TProgressBar;
    grpLogBook: TRadioGroup;
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
    XMLPropStorage1: TXMLPropStorage;
    procedure btConvClick(Sender: TObject);
    procedure btLogBookClick(Sender: TObject);
    procedure btScrShotClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure cbHeaderChange(Sender: TObject);
    procedure Chart1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cmnClipbrd2Click(Sender: TObject);
    procedure cmnClipbrdClick(Sender: TObject);
    procedure cmnSaveAs2Click(Sender: TObject);
    procedure cmnSaveAsClick(Sender: TObject);
    procedure cmnShowGMClick(Sender: TObject);
    procedure cmnShowOSMClick(Sender: TObject);
    procedure csvGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure csvGridHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure csvGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure csvGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure csvGridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure dtlGridDblClick(Sender: TObject);
    procedure dtlGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure grpConvClick(Sender: TObject);
    procedure lblDownloadClick(Sender: TObject);
    procedure lblDownloadMouseEnter(Sender: TObject);
    procedure lblDownloadMouseLeave(Sender: TObject);
    procedure lblManualClick(Sender: TObject);
    procedure lblManualMouseEnter(Sender: TObject);
    procedure lblManualMouseLeave(Sender: TObject);
    procedure LogDirChange(Sender: TObject);
    procedure LogDirDblClick(Sender: TObject);
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
    procedure mmnScrShotClick(Sender: TObject);
    procedure mmnSettingsClick(Sender: TObject);
    procedure mmnTasClick(Sender: TObject);
    procedure ovGridClick(Sender: TObject);
    procedure ovGridCompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure ovGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ovGridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure PageControl1Change(Sender: TObject);
    procedure staGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

  private
    procedure BuildList;                           {Search and count JSON files}
    procedure LoadOneFile(idx: integer);           {Start working one file}
    procedure dtlGridReSize;                       {Rezeize Details table}
    procedure ScreenToBild(fn: string);            {Screenshot}
    procedure MakeKML;                             {Create KML from StringGrid}
    procedure MakeGPX;                             {Create GPX from StringGrid}
    procedure MakeHDia;                            {Fill H-diagrams}
    procedure ChangeHeader;                        {Change column header}
    procedure KMLheader(klist: TStringList);       {KML header and meta data}
    procedure DoForm2Show(p: integer);             {Show additional chart}
    procedure DoScreenShot;                        {Screenshot}
    procedure MakeCSV;                             {Create CSV file from csvGrid}
    procedure RestoreTAS;                          {Compute tas from vx, vy, vz}
    procedure CreateLogBook;                       {Create Pilot log book}

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
  appName='ShowAnafiLogs';
  appVersion='V1.3 03/2019';                       {Major version}
  appBuildno='2019-03-05';                         {Build per day}

  homepage='http://h-elsner.mooo.com';             {my Homepage}
  hpmydat='/mydat/';
  email='helmut.elsner@live.com';

{Define if input in JSON data is always metric else
 depends on control device settings (variable)}
  metric=true;                                     {assumption, always metric}
  useau=false;                                     {use alternative units like km}
  mtoft=3.2808399;

{Links}
  gmapURL='https://maps.google.com/maps';
  osmURL='https://www.openstreetmap.org/';
  starticon='http://maps.google.com/mapfiles/dir_0.png';       {blue}
  stopicon='http://maps.google.com/mapfiles/dir_walk_60.png';  {grey}
  aircrafticon='http://earth.google.com/images/kml-icons/track-directional/track-0.png';

{JSON keywords}
  jsonHeader='details_headers';
  jsonData='details_data';

{meta data keywords}
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

  header: array[1..12] of string=(ovDate, ovFrom, ovTo, ovDuration,
                                  ovAltMax, ovDistMax, ovRoute, ovTasMax,
                                  ovBattMax, ovBattMin, rsLocation, rsGPSfix);

{Output data formats}
  frmCoord= '0.000000000';
  frmFloat= '0.000000';
  frmOut1='0.0';
  frmOut2='0.00';
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
  wldcd='*';
  jsonMinLines=20;
  isep=';';
  hc=11;                                           {Hidden column}
  ziff=['0'..'9'];                                 {valid digits}

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

{Some standard routines to handle data structures}

function SekToDT(numsek: string; mode: integer): TDateTime;  {Timestring to DT}
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

{Conversion to internal time format.
"date": "2018-11-10T094343-0500",     }
function datUTCtoDT(tst: string): TDateTime;  {Convert JSON time stamp to DT
                                    returns 0 if the string could not parsed}
var dt, zt, ms: string;
begin
  dt:=ExtractWord(1, tst, ['T']);
  zt:=ExtractWord(2, tst, ['T']);
  ms:=ExtractWord(2, zt, ['-']);
  zt:=ExtractWord(1, zt, ['-']);
  try
    result:=ScanDateTime(ymd, dt)+
            ScanDateTime('hhnnss', zt)+
            SekToDT(ms, 1);
  except
    result:=0;
  end;
end;

{ 2018-11-17 12:12:56.979  into
  2018-11-17T12:12:56.979Z or
  2018-11-17T12:12:56Z for GPX}
function GoogleTime(tp: string; gx: boolean=false): string;
begin                                              {Default: KML}
  result:='';
  if length(tp)=23 then begin
    if gx then result:=copy(tp, 1, 19)             {suppress tenth of sec}
          else result:=tp;
    result:=result+'Z';
    result[11]:='T';
  end;
end;

function prepKeyw(k: string): string;              {Column header from keyword}
begin
  result:='';
  if k<>'' then result:=trim(k);
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
    result:='No '+copy(sn, 13, 6)+' from '+FormatDateTime('mmmm yyyy', snd);
  except
    result:=rsUnknown;
  end;
end;

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
    6: result:='Armed';                            {different kind of arming ?}
    7: result:='Armed';                            {Starting?}
  end;
end;

function AlertStateToStr(s: string): string;       {JSON alert_state (6)}
var snr: integer;
begin
  result:=rsUnknown+tab1+s;
  snr:=StrToIntDef(trim(s), 99);
  case snr of
    0: result:='None';                             {Normal flight ?}
    1: result:='Normal flight';                    {Manual flight ?}
    2: result:='Anafi shut down';
    3: result:='Battery level crucial';
    4: result:='Battery level low';
    5: result:='Flight angle exceeded';
  end;
end;

function FlipTypeToStr(s: string): string;         {JSON flip_type (20)}
var snr: integer;
begin
  result:=rsUnknown+tab1+s;
  snr:=StrToIntDef(trim(s), 99);
  case snr of
    0: result:='None';                             {Normal flight ?}
  end;
end;

function GPSerrToStr(s: string): string;           {JSON GPS_pos_err (11)}
var snr: integer;
begin
  result:=rsUnknown+tab1+s;
  snr:=StrToIntDef(trim(s), 99);
  case snr of
    0: result:='No error';                         {Normal flight ?}
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

function GetExePath: string;   {Path to exe-file with path delimiter at the end}
begin
  result:=IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
end;

function OpenManual: boolean;                      {Open manual local or from WWW}
begin
  if not FileExists(GetExePath+manual) then
    result:=OpenURL(homepage+hpmydat+manual)
  else
    result:=OpenDocument(GetExePath+manual);
end;

function SuchFile(path: string;                    {Create a list of files in the path}
                  Mask: string;
                  list: TStringList): integer;
var sr: TSearchRec;                                {Scan directory for valid files}
    pn: string;
    f: integer;
begin
  result:=0;
  list.Clear;
  pn:=IncludeTrailingPathDelimiter(path);
  f:=FindFirst(pn+Mask, faAnyFile, sr);
  try
    while f=0 do begin                             {as long as files available}
      list.Add(pn+sr.Name);
      f:=FindNext(sr);
    end;
  finally
    result:=list.Count;                            {Result: Number of matching files}
    FindClose(sr);
  end;
end;

{Distance between coordinaten in m according
 Haversine formula, Radius: 6,371km depending on latitude
 https://rechneronline.de/erdradius/
 6365.692 optimized for 50° latitude and 60m altitude}
function DeltaKoord(lat1, lon1, lat2, lon2: double): double;
begin
  result:=0;
  try
    result:=6365692*arccos(sin(lat1*pi/180)*sin(lat2*pi/180)+
            cos(lat1*pi/180)*cos(lat2*pi/180)*cos((lon1-lon2)*pi/180));

    if (result>50000) or                           {> 50km = implausible values}
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

function CleanNum(const s: string): string;        {Filter digits from String}
var i: integer;
begin
  result:='';
  for i:=1 to length(s) do
    if s[i] in ziff then
      result:=result+s[i];
end;


///////////////////////////////////////////////////////////////////////////////

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
  dirname:=Form1.LogDir.Text;
end;

procedure TMyThread.GetFileName;                   {Get current file number}
begin
  filename:=Form1.ovGrid.Cells[0, idx];
end;

procedure TMyThread.Execute;                       {Scan files}
var i, k, batt, battmin, fmode: integer;
    fn: string;
    inf: TFileStream;
    j0, j1, j2: TJsonData;                         {3 level}
    w, tasmax, altmax, distmax, route: double;
    latp, lonp, latc, lonc, lat1, lon1: double;
    tme: TDateTime;
    gpsfix: boolean;

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
                result:=value*180/pi;              {rad to ° +/-180}
      18:     if converted then begin
                result:=value*180/pi;              {rad to ° 0..360, 0 is north}
                if result<0 then result:=result+360;
              end;

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
    battmin:=999;
    tme:=0;
    lat1:=0;
    lon1:=0;
    route:=0;
    gpsfix:=false;
    idx:=i;                                        {Index of the file in use}
    Synchronize(@GetFileName);
    fn:=IncludeTrailingPathDelimiter(dirname)+filename+jext;
    if FileExists(fn) then begin
      if terminated then break;
      inf:=TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
      try
        if inf.Size>512 then begin                 {0.5kB minimal}
          j0:=GetJson(inf);                        {load whole JSON file, level 0}
          tme:=datUTCtoDT(j0.FindPath(datUTC).AsString);
          res[1]:=FormatDateTime(ymd, tme);        {Date/Time from meta data}
          res[4]:=FormatDateTime(hns,              {Duration}
                                SekToDT(j0.FindPath(datRunTime).AsString, 1));
          res[0]:=j0.FindPath(datSerialNo).AsString;
          res[11]:='"'+URLGMap(FormatFloat(frmCoord,
                               j0.FindPath(datGPSlat).AsFloat),      {Homepoint}
                               FormatFloat(frmCoord,
                               j0.FindPath(datGPSlon).AsFloat))+'"';
          j1:=j0.FindPath(jsonData);               {Datasets, Level 1}

          j2:=j1.Items[0];                         {First dataset: Begin time}
          res[2]:=FormatDateTime(hns, SekToDT(j2.Items[0].AsString, 1)+tme);
          res[9]:=IntToStr(j2.Items[1].AsInteger)+ {Battery level max}
                  UnitToStr(2, true);

          j2:=j1.Items[j1.Count-1];                {Last dataset: End time}
          res[3]:=FormatDateTime(hns, SekToDT(j2.Items[0].AsString, 1)+tme);

          for k:=0 to j1.Count-1 do begin          {Read all datasets}
            j2:=j1.Items[k];                       {read data, level 2}
            fmode:=j2.Items[4].AsInteger;          {Flying state}
            if (fmode=1) and
                j2.Items[7].AsBoolean then
              gpsfix:=true;
            batt:=j2.Items[1].AsInteger;           {Battery charge level}
            if batt<battmin then
              battmin:=batt;
            latc:=j2.Items[2].AsFloat;             {Read coordinates}
            lonc:=j2.Items[3].AsFloat;
            lonp:=j2.Items[8].AsFloat;
            latp:=j2.Items[9].AsFloat;
            if ((lat1<>0) or (lon1<>0)) and
               ((latp<>0) or (lonp<>0)) then begin {Lenght route}
              w:=DeltaKoord(latp, lonp, lat1, lon1);
              if not IsNan(w) then route:=route+w;
            end;
            lat1:=latp;
            lon1:=lonp;
            w:=j2.Items[18].AsFloat;               {Altitude}
            if w>altmax then
              altmax:=w;
            w:=j2.Items[20].AsFloat;               {TAS}
            if w>tasmax then
              tasmax:=w;
            w:=DeltaKoord(latc, lonc, latp, lonp); {Distance to RC}
            if (not IsNan(w)) and (w>distmax) then
              distmax:=w;
          end;
        end;
        res[5]:=FormatFloat(frmOut1, ConvUnit(19, altmax))+
                                     UnitToStr(19, true);
        res[6]:=FormatFloat(frmOut1, ConvUnit(22, distmax, useau))+
                                     UnitToStr(22, true, useau);
        res[7]:=FormatFloat(frmOut1, ConvUnit(22, route, useau))+
                                     UnitToStr(22, true, useau);
        res[8]:=FormatFloat(frmOut2, ConvUnit(21, tasmax, true))+
                                     UnitToStr(21, true, true);
        res[10]:=IntToStr(battmin)+                 {min Battery level}
                          UnitToStr(2, true);
        if gpsfix then
          res[12]:=rsYes
        else
          res[12]:=rsNo;
        if terminated then break;                  {Jump out before write}
        Synchronize(@WriteResults);
      finally
        inf.Free;
      end;
    end;                                           {End if File exists}
  end;                                             {End for each file}
  Synchronize(@EnableSort);                        {Enables sort again}
end;

procedure TMyThread.WriteResults;
var i: integer;                     {Write results per file into overview table}
    s: string;
begin
  for i:=1 to 10 do
    Form1.ovGrid.Cells[i, idx]:=res[i];
  s:='';
  for i:=0 to high(res) do
    s:=s+res[i]+isep;
  Form1.ovGrid.Cells[hc, idx]:=s;                  {Write into hidden column}
end;

///////////////////////////////////////////////////////////////////////////////

procedure TForm1.FormCreate(Sender: TObject);      {Initialize the application}
var i: integer;
begin
  Tag:=0;                                          {Application before first start}
  DefaultFormatSettings.DecimalSeparator:='.';
  for i:=0 to High(hdrList) do
    hdrList[i]:='';

  Caption:=appName+tab1+appVersion;
  Hint:=Caption;
  LogDir.TextHint:=hntLogDir;
  LogDir.Hint:=hntLogDir;
  LogDir.DialogTitle:=hntLogDir;
  LogDirDialog.Title:=hntLogDir;
  lblSelDir.Caption:=capLogDir;
  lblSelDir.Hint:=hntLogDir;
  lblDetails.Caption:=rsMetaData;
  lblStat.Caption:=rsStatistics;
  Chart1.Hint:=hntChart1;

  lblManual.Caption:=rsManual;
  lblManual.Hint:=GetExePath+manual;               {default}
  if not FileExists(lblManual.Hint) then begin
    lblManual.Hint:=homepage+hpmydat+manual;       {Overwrite with inet link}
  end else begin
  {$IFDEF DARWIN}
    lblManual.Hint:=manual;                        {overwrite for MAC OS X}
  {$ENDIF}
  end;
  lblDownload.Caption:=rsLatest;
  lblDownload.Hint:=homepage+downURL;

  cbExtrude.Caption:=capExtrude;
  cbExtrude.Hint:=hntExtrude;
  cbHeader.Caption:=capHeader;
  cbHeader.Hint:=hntHeader;
  cbCSVsep.Caption:=capCSVsep;
  cbCSVsep.Hint:=hntCSVsep;

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

  mmnHelp.Caption:=mniHelp;
  mmnManual.Caption:=rsManual;
  mmnHomepage.Caption:=mniHomepage;
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

  TabSheet1.Caption:=thdOverview;
  TabSheet2.Caption:=thdData;
  TabSheet3.Caption:=thdDia;
  TabSheet4.Caption:=thdDetails;
  TabSheet5.Caption:=thdSettings;

  csvGrid.ColCount:=23;                            {Data columns, fix number of cols}
  csvGrid.RowCount:=6;
  csvGrid.Cells[0, 0]:=rsDateTime;
  ovGrid.Tag:=1;                                   {Start with first file}
  csvGrid.Tag:=1;

  ovGrid.ColCount:=hc+1;                           {Last column hidden}
  ovGrid.RowCount:=1;
  for i:=1 to 10 do                                {Write overview header}
    ovGrid.Cells[i, 0]:=header[i];
  ovGrid.ColWidths[hc]:=0;                         {Hidden column for log book}

  dtlGrid.Cells[0, 0]:=dtlCol0;
  dtlGrid.Cells[1, 0]:=dtlCol1;
  dtlGrid.RowCount:=16;

  staGrid.RowCount:=6;
  staGrid.Cells[0, 0]:=rsStatistics;
  staGrid.Cells[1, 0]:=staHd1;
  staGrid.Cells[2, 0]:=staHd2;
  staGrid.Cells[0, 1]:=ovAltMax;
  staGrid.Cells[0, 2]:=ovDistMax;
  staGrid.Cells[0, 3]:=ovTasMax;
  staGrid.Cells[0, 4]:=ovBattMax;
  staGrid.Cells[0, 5]:=ovBattMin;
  staGrid.Width:=staGrid.ColWidths[0]+staGrid.ColWidths[1]+staGrid.ColWidths[2];

{$IFDEF LINUX}
  dtlGrid.Height:=392;
  staGrid.Height:=152;
{$ENDIF}

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
    LogDir.Text:=dir;
    BuildList;
  end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; {Reload by F5}
                           Shift: TShiftState);
begin
  if (key=vk_F5) then
    BuildList;
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

procedure TForm1.ChangeHeader;                     {Change column header}
var i: integer;
begin
  if cbHeader.Checked then begin
    for i:=1 to 22 do
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

procedure TForm1.lblDownloadClick(Sender: TObject);        {Click link homepage}
begin
  if OpenURL(homepage+DownURL) then
    lblDownload.Font.Color:=clPurple;
end;

procedure TForm1.lblDownloadMouseEnter(Sender: TObject);   {Animate link}
begin
  lblDownload.Font.Style:=lblDownload.Font.Style+[fsBold];
end;

procedure TForm1.lblDownloadMouseLeave(Sender: TObject);   {Animate link}
begin
  lblDownload.Font.Style:=lblDownload.Font.Style-[fsBold];
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

procedure TForm1.LogDirChange(Sender: TObject);    {Select dir by dialog}
begin
  if Tag=1 then                                    {FileList already buildt}
    BuildList;                                     {search JSON files}
end;

procedure TForm1.LogDirDblClick(Sender: TObject);  {Call file explorer}
begin
  OpenDocument(IncludeTrailingPathDelimiter(LogDir.Text));
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
  OpenDocument(IncludeTrailingPathDelimiter(LogDir.Text));
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
  if LogDir.Text<>'' then
    LogDirDialog.InitialDir:=LogDir.Text;
  if LogDirDialog.Execute then
    LogDir.Text:=LogDirDialog.FileName;
end;

procedure TForm1.mmnScrShotClick(Sender: TObject); {Menu Screenshot}
begin
  DoScreenShot;
end;

procedure TForm1.mmnSettingsClick(Sender: TObject); {Go to Settings page}
begin
  PageControl1.ActivePageIndex:=4;
end;

procedure TForm1.mmnTasClick(Sender: TObject);     {Recompute TAS from xyz-Speed}
begin
  RestoreTAS;
end;

procedure TForm1.ovGridClick(Sender: TObject);     {Process a file}
begin
  if ovGrid.Tag<>csvgrid.Tag then begin            {A new file selected}
    LoadOneFile(ovGrid.Tag);
    MakeHDia;
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
    1: PageControl1.Tag:=PageControl1.ActivePageIndex;
    2: begin
         PageControl1.Tag:=PageControl1.ActivePageIndex;
         MakeHDia;
       end;
    3: begin
         PageControl1.Tag:=PageControl1.ActivePageIndex;
         dtlGridReSize;
       end;
  end;
end;

procedure TForm1.staGridKeyUp(Sender: TObject; var Key:  {Statistics short keys}
                              Word; Shift: TShiftState);
begin
  if (key=vk_c) and
     (ssCtrl in Shift) then
    staGrid.CopyToClipboard(false);                {Ctrl+C copy to Clipboard}
end;

procedure TForm1.FormActivate(Sender: TObject);    {Start running with settings from XML}
var dir: string;
begin
  StatusBar1.Panels[3].Text:=grpConv.Items[grpConv.ItemIndex];
  if Tag=0 then begin                              {first start}
    if (ParamCount>0) and                          {FileName as 1st parameter}
       (Length(ParamStr(1))>3) then begin          {Something like a file name?}
      dir:=FindDirName(ParamStr(1));
      if DirectoryExists(dir) then
        LogDir.Text:=dir;                          {Use this one}
    end;
    BuildList;                                     {search JSON files}
  end;
end;

procedure TForm1.btCloseClick(Sender: TObject);    {Button Close}
begin
  Close;
end;

procedure TForm1.cbHeaderChange(Sender: TObject);  {Use alternative Header}
begin
  ChangeHeader;
end;

procedure TForm1.Chart1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssCtrl in Shift) or                          {Klicken mit gedrückter Ctrl}
     (ssMiddle in Shift) then                      {Klicken mit mittlerer Taste}
    Chart1.ZoomFull;
end;

procedure TForm1.cmnClipbrd2Click(Sender: TObject); {Menu for data table}
begin
  csvGrid.CopyToClipboard(false);                  {Data table}
end;

procedure TForm1.cmnClipbrdClick(Sender: TObject); {Context meny Clipboard}
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
    2: Chart1.CopyToClipboardBitmap;               {Charts}
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
    2: begin                                       {Charts}
         SaveDialog1.FileName:=SaveDialog1.FileName+pext;
         if SaveDialog1.Execute then begin
           Chart1.SaveToFile(TPortableNetworkGraphic, SaveDialog1.FileName);
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
  function DefaultHnt: string;                     {Default hint: Header=Value}
  begin
    result:=AltHeaderToStr(aCol)+'='+csvGrid.Cells[aCol, aRow];
  end;

  function FrmValue: string;                       {Format float for better reading}
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

begin                                             {Get additional info per cell}
  if aRow=0 then begin
    result:=AltHeaderToStr(aCol);                 {More explained header}
  end else begin
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
                      ConvUnit(aCol, StrToFloat(csvGrid.Cells[aCol, aRow]), true))+
                      UnitToStr(aCol, true, true); {with conversion to °}
      20: result:=AltHeaderToStr(aCol)+': '+
                  FlipTypeToStr(csvGrid.Cells[aCol, aRow]);
      21, 22: result:=AltHeaderToStr(aCol)+'='+
                      FormatFloat(frmFloat,
                      ConvUnit(aCol, StrToFloat(csvGrid.Cells[aCol, aRow]), useau))+
                      UnitToStr(aCol, true, useau); {with alternative units}
    end;
  end;
end;

procedure TForm1.csvGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);           {Cells get colors}
var w: integer;

  procedure ColorForCell(fb: TColor);              {Color for one cell}
  begin
    if not (gdSelected in aState) then begin       {Keep default behaviour for selected cell}
      csvGrid.Canvas.Brush.Color:=fb;
      csvGrid.Canvas.FillRect(aRect);
      csvGrid.Canvas.TextOut(aRect.Left+2, aRect.Top+2,  {Restore text}
                             csvgrid.Cells[aCol, aRow]);
    end;
  end;

begin
  if aRow>0 then begin                             {not for header}
    if csvGrid.Cells[aCol, aRow]<>'' then begin
      case aCol of
        2: begin                                   {Battery level}
             w:=StrToIntDef(csvGrid.Cells[aCol, aRow], 0);
               if w>0 then begin
                 if w>50 then begin                {100%..51% green}
                   ColorForCell(clGreen);
                   exit;
                 end;
                 if w<25 then begin                {0...24 red}
                   ColorForCell(clRed);
                   exit;
                 end else begin                    {25...50 Sats orange}
                   ColorForCell(clDarkOrange);
                   exit;
                 end;
               end else exit;
           end;
        5: begin                                   {flight state}
             w:=StrToIntDef(csvGrid.Cells[aCol, aRow], 99);
             ColorForCell(Form2.ColorSourceFM(w));
           end;
        6: begin                                   {alert state}
             w:=StrToIntDef(csvGrid.Cells[aCol, aRow], 99);
             ColorForCell(Form2.ColorSourceAS(w));
           end;
        8: if LowerCase(csvGrid.Cells[aCol, aRow])='true' then
             ColorForCell(clMoneygreen);
        11: if csvGrid.Cells[aCol, aRow]<>'0' then {GPS error}
              ColorForCell(clRed);
        12: begin                                  {Num sats}
               w:=StrToIntDef(csvGrid.Cells[aCol, aRow], 0);
                 if w>0 then begin
                   if w>10 then begin              {11...x Sats green}
                     ColorForCell(clGreen);
                     exit;
                   end;
                   if w<5 then begin               {1...4 Sats red}
                     ColorForCell(clRed);
                     exit;
                  end else begin                   {5...10 Sats rose}
                    ColorForCell(clAttention);
                    exit;
                  end;
                end else exit;
             end;
         20: if csvGrid.Cells[aCol, aRow]<>'0' then
               ColorForCell(clOrange);             {flip type}
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
    1: result:='ms';
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
       16..18: convert:=true;       {Unit string will be converted rad to °}
     end;
     Form2.Chart1.AxisList[0].Title.Caption:=AltHeaderToStr(idx)+   {y-axis}
                                             tab1+UnitToStr(idx, false, convert);
     Form2.Chart1.Visible:=true;
     Form2.Chart1LineSeries1.BeginUpdate;
       Form2.Chart1ConstantLine1.Position:=        {Show Cursor}
         ScanDateTime(ymd+tab1+hnsz, csvGrid.Cells[0,
                      StrToIntDef(StatusBar1.Panels[2].Text, 1)]);

       for i:=1 to csvGrid.RowCount-1 do begin     {Create chart}
         w:=StrToFloat(csvGrid.Cells[idx, i]);
         case idx of
           13..15: w:=-w;                          {speed_v xyz reverse in chart}
           16..18: w:=ConvUnit(idx, w, true);      {rad to °}
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
     Form2.Chart1LineSeries1.BeginUpdate;
     Form2.Chart1ConstantLine1.Position:=          {Show Cursor}
       ScanDateTime(ymd+tab1+hnsz, csvGrid.Cells[0,
                    StrToIntDef(StatusBar1.Panels[2].Text, 1)]);

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
  if IsColumn then begin
    Form2.addGrid.RowCount:=1;                     {Kill all}
    Form2.addGrid.Visible:=false;
    Form2.Chart1LineSeries1.Clear;
    Form2.Chart1.Visible:=false;
    case index of
      0, 1: ShowStatistics;                        {Copy of staGrid}
      2, 7, 12: ShowInteger(index);                {Integer values; Chart}
      3, 4, 9, 10, 22: ShowFloat(22);              {Distance; Chart}
      5, 6: ShowList(index);                       {Data Table with time *
                                                    not yet implemented}
      8, 11, 20: ShowList(index);                  {Data Table with counter}
      13..19, 21: ShowFloat(index);                {Float values; Chart}
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

procedure TForm1.csvGridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);                                     {Cell hint}
var aCol, aRow: longint;
begin
  aCol:=0;
  aRow:=0;
  csvGrid.MouseToCell(x, y, aCol, aRow);
  csvGrid.Hint:=GetCellInfo(aCol, aRow);
end;

procedure TForm1.csvGridSelection(Sender: TObject; aCol, aRow: Integer);
begin                                              {Show line number}
  if aRow>0 then begin
    StatusBar1.Panels[2].Text:=IntToStr(aRow);
    if Form2.Chart1ConstantLine1.Active then       {Show Cursor}
      Form2.Chart1ConstantLine1.Position:=
        ScanDateTime(ymd+tab1+hnsz, csvGrid.Cells[0, aRow]);
  end;
end;

procedure TForm1.dtlGridDblClick(Sender: TObject); {Show in GoogleMaps}
begin
  OpenURL(URLGMap(dtlGrid.Cells[1, 14], dtlGrid.Cells[1, 15]));
end;

procedure TForm1.dtlGridKeyUp(Sender: TObject;     {Ctrl+c copy}
                              var Key: Word;
                              Shift: TShiftState);
begin
  if (key=vk_c) and
     (ssCtrl in Shift) then
    dtlGrid.CopyToClipboard(false);
end;

procedure TForm1.ScreenToBild(fn: string);         {Screenshot}
var bld: TPortableNetworkGraphic;
    ScreenDC: HDC;
begin
  bld:=TPortableNetworkGraphic.Create;             {create PNG-picture}
  try
    bld.Canvas.Clear; {sicherstellen, dass bld bereits vollständig erzeugt wurde}
    ScreenDC:=Form1.Canvas.Handle;                 {whole application}
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
  if (LogDir.Text<>'') and
     DirectoryExists(LogDir.Text) then begin       {only when Directory valid}
    if Form2<>nil
      then Form2.Close;                            {Close 2nd window}
    btConv.Enabled:=false;
    filelist:=TStringList.Create;
    ovGrid.RowCount:=1;                            {Delete file list}
    PageControl1.Tag:=0;
    try
                                                   {only if >=1 JSON file was found}
      if SuchFile(LogDir.Text, wldcd+jext, filelist)>0 then begin
        StatusBar1.Panels[0].Text:=rsFiles+dpkt+IntToStr(filelist.Count);
        StatusBar1.Panels[4].Text:='';             {Empty file name field}

        ovGrid.BeginUpdate;                        {File names to overview}
          ovGrid.Cells[0, 0]:=StatusBar1.Panels[0].Text;
          ovGrid.RowCount:=filelist.Count+1;
          for i:=0 to filelist.Count-1 do begin
            fn:=ExtractFileName(filelist[i]);
            ovGrid.Cells[0, i+1]:=ChangeFileExt(fn, '');
          end;
          ovGrid.AutoSizeColumn(0);
        ovGrid.EndUpdate;

        PageControl1.ActivePageIndex:=0;           {Go to overwiev first}
        PageControl1.Tag:=1;

        if ovThread<>nil then                      {Still living}
          ovThread.Terminate;                      {Kill for new list}
        ovGrid.ColumnClickSorts:=false;            {Disable sort when thread runs}
        btLogBook.Enabled:=false;                  {Disable Pilot log book}
        mmnLogBook.Enabled:=false;
        ovThread:=TMyThread.Create(false);         {Start overview immediatley}

        LoadOneFile(1);                            {Load first file}
        ovGrid.Tag:=1;                             {First file in table}
        csvGrid.Tag:=1;
        btConv.Enabled:=true;                      {Allow converting}
        if filelist.Count=1 then
          PageControl1.ActivePageIndex:=1;         {Only one -> go to data}
        for i:=1 to 10 do
          ovGrid.AutoSizeColumn(i);
      end else
        StatusBar1.Panels[4].Text:=errMissingFiles+tab1+LogDir.Text;
    finally
      Application.ProcessMessages;
      FileList.Free;
    end;
  end;
  Tag:=1;                                          {First run done}
end;

procedure TForm1.LoadOneFile(idx: integer);        {Start working one file}
var inf: TFileStream;
    j0, j1, j2: TJsonData;                         {3 level}
    fn, keyw: string;
    w, tas, tasmax, alt, altmax, distmax: double;
    latp, lonp, latc, lonc: double;
    i, k, ttasmax, taltmax, tp, batt, battmin, tbattmin, tdistmax: integer;
    tme, trt: TDateTime;

  procedure WriteDtlGrid;                          {Fill Details table}
  var sn: string;
  begin
   dtlGrid.BeginUpdate;                            {Fill Details from level 0}
     keyw:=datProdName;
     dtlGrid.Cells[0, 1]:=prepKeyw(keyw);
     dtlGrid.Cells[1, 1]:=j0.FindPath(keyw).AsString;
     keyw:=datProdID;
     dtlGrid.Cells[0, 2]:=prepKeyw(keyw);
     dtlGrid.Cells[1, 2]:=j0.FindPath(keyw).AsString;
     keyw:=datVersion;
     dtlGrid.Cells[0, 3]:=prepKeyw(keyw);
     dtlGrid.Cells[1, 3]:=j0.FindPath(keyw).AsString;

     keyw:=datSerialNo;
     sn:= j0.FindPath(keyw).AsString;
     dtlGrid.Cells[0, 4]:=prepKeyw(keyw);
     dtlGrid.Cells[1, 4]:=sn;
     dtlGrid.Cells[0, 5]:=rsManufacture;
     dtlGrid.Cells[1, 5]:=SerialToManufacture(sn);

     keyw:=datHWvers;
     dtlGrid.Cells[0, 6]:=prepKeyw(keyw);
     dtlGrid.Cells[1, 6]:=j0.FindPath(keyw).AsString;
     keyw:=datSWvers;
     dtlGrid.Cells[0, 7]:=prepKeyw(keyw);
     dtlGrid.Cells[1, 7]:=j0.FindPath(keyw).AsString;

     keyw:=datRuntimeTotal;                        {Runtime total possibly with days}
     dtlGrid.Cells[0, 8]:=prepKeyw(keyw);
     trt:=SekToDT(j0.FindPath(keyw).AsString, 1);
     dtlGrid.Cells[1, 8]:=FormatDateTime(hns, trt);
     if trt>=1 then
       dtlGrid.Cells[1, 8]:=IntTostr(trunc(trt))+'d '+dtlGrid.Cells[1, 7];

     keyw:=datCrash;
     dtlGrid.Cells[0, 9]:=prepKeyw(keyw);
     dtlGrid.Cells[1, 9]:=j0.FindPath(keyw).AsString;
     keyw:=datRCmodel;
     dtlGrid.Cells[0, 10]:=prepKeyw(keyw);
     dtlGrid.Cells[1, 10]:=j0.FindPath(keyw).AsString;
     keyw:=datRCapp;
     dtlGrid.Cells[0, 11]:=prepKeyw(keyw);
     dtlGrid.Cells[1, 11]:=j0.FindPath(keyw).AsString;
     keyw:=datUUID;
     dtlGrid.Cells[0, 12]:=UpCase(keyw);
     dtlGrid.Cells[1, 12]:=FormatUUID(j0.FindPath(keyw).AsString);
     keyw:=datGPSavail;
     dtlGrid.Cells[0, 13]:=prepKeyw(keyw);
     dtlGrid.Cells[1, 13]:=j0.FindPath(keyw).AsString;
     keyw:=datGPSlat;
     dtlGrid.Cells[0, 14]:=prepKeyw(keyw);
     dtlGrid.Cells[1, 14]:=FormatFloat(frmCoord, j0.FindPath(keyw).AsFloat);
     keyw:=datGPSlon;
     dtlGrid.Cells[0, 15]:=prepKeyw(keyw);
     dtlGrid.Cells[1, 15]:=FormatFloat(frmCoord, j0.FindPath(keyw).AsFloat);
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
     staGrid.Cells[1, 5]:=IntToStr(battmin)+UnitToStr(2, true);
     staGrid.Cells[2, 5]:=FormatDateTime(hnsz, tbattmin/(secpd*1000)+tme);
   staGrid.EndUpdate;
  end;

begin
  tme:=0;
  battmin:=999;
  tasmax:=0;
  altmax:=-9999;
  distmax:=0;
  ProgressFile.Position:=0;
  if ovGrid.Cells[0, idx]<>'' then begin
    fn:=IncludeTrailingPathDelimiter(LogDir.Text)+ovGrid.Cells[0, idx]+jext;
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
            tme:=datUTCtoDT(j0.FindPath(datUTC).AsString); {Date/Time from meta data}

{Read and write Header, (re)create columns in data table}
            j1:=j0.FindPath(jsonHeader);           {load header}
            csvGrid.BeginUpdate;
            for i:=0 to j1.Count-1 do begin
              hdrList[i]:=j1.Items[i].AsString;    {fill original header array}
            end;
            ChangeHeader;                          {use alternative headers or not}
            csvGrid.EndUpdate;

{Read and write data, (re)create rows in data table}
            j1:=j0.FindPath(jsonData);             {Datasets, Level 1}
            StatusBar1.Panels[1].Text:=IntToStr(j1.Count);
            j2:=j1.Items[0];                       {First dataset: Begin time}
                                                   {Battery level max + time}
            staGrid.Cells[1, 4]:=IntToStr(j2.Items[1].AsInteger)+
                                 Form1.UnitToStr(2, true);
            staGrid.Cells[2, 4]:=FormatDateTime(hnsz,
                                 SekToDT(j2.Items[0].AsString, 1)+tme);
            ProgressFile.Max:=j1.Count-1;

            csvGrid.BeginUpdate;
              csvGrid.RowCount:=j1.Count+1;
              for i:=0 to j1.Count-1 do begin      {Read datasets}
                j2:=j1.Items[i];                   {read data, level 2}
                if (i=10) and
                   (Tag=0) then
                  csvGrid.AutoSizeColumns;         {Only once in line 10}
                for k:=0 to J2.Count-1 do begin    {Fill one Row}
                  case k of                        {distribute values}
                    0: tp:=j2.Items[k].AsInteger;
                    1: batt:=j2.Items[k].AsInteger;
                    2: latc:=j2.Items[k].AsFloat;
                    3: lonc:=j2.Items[k].AsFloat;
                    8: lonp:=j2.Items[k].AsFloat;
                    9: latp:=j2.Items[k].AsFloat;
                    12..17: csvGrid.Cells[k+1, i+1]:=
                              FormatFloat(frmFloat, j2.Items[k].AsFloat);
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
                  battmin:=batt;                   {lowest batt level}
                  tbattmin:=tp;                    {time of lowest batt level}
                end;
                csvGrid.Cells[3, i+1]:=FormatFloat(frmCoord, latc);
                csvGrid.Cells[4, i+1]:=FormatFloat(frmCoord, lonc);
                csvGrid.Cells[9, i+1]:=FormatFloat(frmCoord, lonp);
                csvGrid.Cells[10, i+1]:=FormatFloat(frmCoord, latp);
                csvGrid.Cells[19, i+1]:=FormatFloat(frmFloat, alt);
                if alt>altmax then begin
                  altmax:=alt;                     {highest altitude}
                  taltmax:=tp;                     {time of highest altitude}
                end;
                csvGrid.Cells[21, i+1]:=FormatFloat(frmFloat, tas);
                if tas>tasmax then begin
                  tasmax:=tas;                     {highest speed}
                  ttasmax:=tp;                     {time of max speed}
                end;
                w:=DeltaKoord(latc, lonc, latp, lonp);   {Distance RC - A/C}
                csvGrid.Cells[22, i+1]:=FormatFloat(frmOut1, w);
                if w>distmax then begin
                  distmax:=w;                      {highest distance}
                  tdistmax:=tp;                    {time of max distance}
                end;
                ProgressFile.Position:=i;
                Application.ProcessMessages;
              end;
            csvGrid.EndUpdate(false);

            StatusBar1.Panels[4].Text:=fn;         {Show file name}
            WriteDtlGrid;                          {Write statistics}
            WriteStaGrid;                          {Fill statistics table}
          except
            StatusBar1.Panels[4].Text:=errWrongData+ExtractFileName(fn);
          end;
      end else                                     {Not enough data}
        StatusBar1.Panels[4].Text:=errLessData+ExtractFileName(fn);
      finally
        inf.Free;
        Screen.Cursor:=crDefault;
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

procedure TForm1.MakeCSV;                          {Create CSV file from csvGrid}
var fn: string;
begin
  fn:=IncludeTrailingPathDelimiter(LogDir.Text)+
      ovGrid.Cells[0, csvGrid.Tag]+grpConv.Items[2];
  csvGrid.SaveToCSVFile(fn, GetCSVsep, true);
  StatusBar1.Panels[4].Text:=ExtractFileName(fn)+tab1+rsSaved;
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
        state:=StrToIntDef(csvGrid.Cells[5, p], 0);
      until (state in rfm) or                      {state in real flight modes}
            (p>csvGrid.RowCount-3);
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
        fn:=IncludeTrailingPathDelimiter(LogDir.Text)+
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

{Visualization: http://www.doarama.com/info
So on that note how about working on a converter to change the CSV files to
the proper GPX or IGC files to work in http://www.doarama.com/.

GPX format:
http://www.topografix.com/gpx.asp
http://www.topografix.com/gpx/1/1/
https://en.wikipedia.org/wiki/GPS_Exchange_Format
http://www.doarama.com/api/0.2/docs
http://publicgpx.blog-me.de/2010/11/13/gpx-dateiformat-1/

Show GPX or KML files: http://www.atlsoft.de/gpx/   }

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
      repeat                                       {looking for first start coord}
        inc(p);
        state:=StrToIntDef(csvGrid.Cells[5, p], 0);
      until (state in rfm) or                      {State in real flight modes}
            (p>csvGrid.RowCount-3);
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
      fn:=IncludeTrailingPathDelimiter(LogDir.Text)+
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
      s:=s+sep+header[i];
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
            outlist.Add(Format(fotab, [header[k]+dpkt])+splitlist[k]);
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

