program ShowAnafiLog;

{$mode objfpc}{$H+}

uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  Interfaces,                                      {This includes the LCL widgetset}
  Forms, tachartlazaruspkg, showanafilog_main,
                                                   {You can add units after this}
  splash_sc, anzwerte;

{$R *.res}

begin
  Application.Scaled:=True;
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Form4:=TForm4.Create(nil);                       {Show splash screen before main program}
  Form4.Show;
  Form4.Refresh;
  Application.ProcessMessages;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

