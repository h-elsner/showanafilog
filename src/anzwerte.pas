unit anzwerte;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAIntervalSources, TASeries, TATools,
  Forms, Controls, Graphics, Dialogs, Grids, Buttons, Menus, LCLIntf, LCLType,
  StdCtrls, ExtCtrls, Types;

type

  {TForm2: Detailanzeige kontextabhängig Diagramme oder Statistiken}

  TForm2 = class(TForm)
    BitBtn1: TBitBtn;
    Chart1: TChart;
    Chart1ConstantLine1: TConstantLine;
    Chart1LineSeries1: TLineSeries;
    ChartToolset1: TChartToolset;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomMouseWheelTool1: TZoomMouseWheelTool;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    cmnClipbrd: TMenuItem;
    cmnSaveAs: TMenuItem;
    edTime: TEdit;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    addGrid: TStringGrid;
    procedure addGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure BitBtn1Click(Sender: TObject);
    procedure Chart1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure cmnClipbrdClick(Sender: TObject);
    procedure cmnSaveAsClick(Sender: TObject);
    procedure addGridCompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure StringGrid1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    {private declarations}
  public
    idx: integer;                                  {Index of column}
    csvsep: char;
    {public declarations}
    procedure FormatGrid;                          {StringGrid format columns}

    function ColorSourceFM(w: integer): TColor;
    function ColorSourceAS(w: integer): TColor;
    procedure MoveVCursor(x: double; p: integer);  {Time and label position}
  end;

var
  Form2: TForm2;

const
{Color definitions}
  clAttention =$008080F0;
  clDarkOrange=$00008CFF;
  clOrange=$000080FF;

  clLanded=clCream;
  clArmed6=clSilver;
  clEmerg=clMaroon;
  clArmed7=clSilver;
  clTakeOff=clMoneyGreen;
  clHover=clSkyBlue;
  clFlying=clAqua;
  clLanding=clGreen;

  clNone=clCream;
  clUser=clFuchsia;
  clCutOut=clMaroon;
  clBattCritical=clRed;
  clBattLow=clDarkOrange;
  clFAngle=clRed;

  niceCols=4;

{$I anafi_en.inc}

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.MoveVCursor(x: double; p: integer); {Move vertical cursor}
var lft: integer;
begin
  Chart1ConstantLine1.Position:=x;                 {Red cursor}
  edTime.Text:=FormatDateTime('hh:nn:ss', x);      {Time label at cursor}
  lft:=p*(Chart1.Width-edTime.Width) div 10000+(edTime.Width div 2);
  if lft<Chart1.Left then                          {Left/right borders for label}
    lft:=Chart1.Left;
  if lft+edTime.Width+BitBtn1.Width>Chart1.Width then
    lft:=Chart1.Width-edTime.Width-BitBtn1.Width;
  edTime.Left:=lft;                                {Position time label}
end;

procedure TForm2.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm2.Chart1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssCtrl in Shift) or                          {Klicken mit gedrückter Ctrl}
     (ssMiddle in Shift) then                      {Klicken mit mittlerer Taste}
    Chart1.ZoomFull;
end;

procedure TForm2.addGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);                         {Grid cells with colors}
var w: integer;

begin
  if (aState=[]) and
     (aRow>0) and                                  {No header and}
     (aCol=0) then begin                           {only 1st column}
    case idx of
      5: begin                                     {Flight mode}
           w:=StrToIntDef(addGrid.Cells[0, aRow], 99);
           addGrid.Canvas.Brush.Color:=ColorSourceFM(w);
         end;
      6: begin                                     {Alert state}
           w:=StrToIntDef(addGrid.Cells[0, aRow], 99);
           addGrid.Canvas.Brush.Color:=ColorSourceAS(w);
         end;
      8: if LowerCase(addGrid.Cells[0, aRow])='true' then
           addGrid.Canvas.Brush.Color:=clMoneyGreen; {GPS available}
     11: if addGrid.Cells[0, aRow]<>'0' then
           addGrid.Canvas.Brush.Color:=clRed;      {GPS error}
     20: if addGrid.Cells[0, aRow]<>'0' then
           addGrid.Canvas.Brush.Color:=clOrange;   {Flip type}
    end;
  end;
end;

procedure TForm2.FormActivate(Sender: TObject);
begin
  Chart1ConstantLine1.Active:=true;
end;

procedure TForm2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Chart1ConstantLine1.Active:=false;
end;

function TForm2.ColorSourceFM(w: integer): TColor; {Flight mode colors}
begin
  result:=clDefault;
  case w of
    0: result:=clLanded;                           {On the ground}
    1: result:=clTakeOff;                          {Taking off}
    2: result:=clHover;                            {Hover}
    3: result:=clFlying;                           {Flying}
    4: result:=clLanding;                          {Landing}
    5: result:=clEmerg;                            {Emergency}
    6: result:=clArmed6;                           {Armed?}
    7: result:=clArmed7;                           {Armed?}
  end;
end;

function TForm2.ColorSourceAS(w: integer): TColor; {Alarm state colors}
begin
  result:=clDefault;
  case w of
    0: result:=clNone;                             {Initializing?}
    1: result:=clUser;                             {User flight mode ?}
    2: result:=clCutOut;                           {Cut out}
    3: result:=clBattCritical;                     {Batt level critical}
    4: result:=clBattLow;                          {Batt level low}
    5: result:=clFAngle;                           {Flight angle exeeded}
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);      {Initialize new window}
begin
  cmnClipbrd.Caption:=mniCopy;                     {Menu items}
  cmnSaveAs.Caption:=mniFileSave;
  addGrid.ColCount:=0;                         {Delete table}
  addGrid.ColCount:=3;
  addGrid.Cells[1, 0]:=altCount;               {Headers}
  addGrid.Cells[2, 0]:=altDescription;
end;

procedure TForm2.FormatGrid;                       {StringGrid format columns}
begin
  if addGrid.Visible then
    addGrid.ColWidths[2]:=addGrid.Width-
                          addGrid.ColWidths[0]-
                          addGrid.ColWidths[1]-nicecols;
end;

procedure TForm2.FormResize(Sender: TObject);      {StringGrid format columns}
begin
  FormatGrid;
end;

procedure TForm2.cmnClipbrdClick(Sender: TObject); {Copy to clipboard}
begin
  if addGrid.Visible then
    addGrid.CopyToClipboard(false)
  else
    Chart1.CopyToClipboardBitmap;
end;

procedure TForm2.cmnSaveAsClick(Sender: TObject);
begin
  SaveDialog1.Title:=mniFileSave;
  if addGrid.Visible then begin                {Table as CSV}
    SaveDialog1.FileName:=caption+'_01.csv';
    if SaveDialog1.Execute then
      addGrid.SaveToCSVFile(SaveDialog1.FileName, csvsep, true);
  end else begin                                   {Chart as PNG picture}
    SaveDialog1.FileName:=caption+'_01.png';
    if SaveDialog1.Execute then
      Chart1.SaveToFile(TPortableNetworkGraphic, SaveDialog1.FileName);
  end;
end;

procedure TForm2.addGridCompareCells(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);             {Sorting routines per column}
begin
  try
    if (ACol=0) or (ACol=1) then begin             {Sort only for numbers}
      Result:=StrToInt(addGrid.Cells[ACol, ARow])-
              StrToInt(addGrid.Cells[BCol, BRow]);
    end else
      result:=CompareText(addGrid.Cells[ACol,ARow],
              addGrid.Cells[BCol,BRow]);       {Sort as string}
  except
    result:=CompareText(addGrid.Cells[ACol,ARow],
            addGrid.Cells[BCol,BRow]);         {Sort as string}
  end;
  if addGrid.SortOrder=soDescending then
    Result:=-Result;                               {Sort direction}
end;

procedure TForm2.StringGrid1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);                             {Copy values}
begin
  if (key=vk_c) and
     (ssCtrl in Shift) then
    addGrid.CopyToClipboard(false);                {Ctrl+C}
end;

end.

