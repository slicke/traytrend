(*
 This file is part of TrayTrend.

 TrayTrend is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 TrayTrend is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with TrayTrend.  If not, see <http://www.gnu.org/licenses/>.

 (c) 2018 Björn Lindh - https://github.com/slicke/traytrend
*)
unit usys;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ExtCtrls, ComCtrls, jsonConf;

type

  { TfSysSettings }

  TfSysSettings = class(TForm)
    btnOK: TButton;
    cbrun: TCheckBox;
    cbNotice: TCheckBox;
    cbAlert: TCheckBox;
    cbOnTop: TCheckBox;
    cbValue: TCheckBox;
    cbTrend: TCheckBox;
    fnRun: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblSnooze: TLabel;
    pnHigh: TPanel;
    pnSoonHigh: TPanel;
    pnOK: TPanel;
    pnLow: TPanel;
    tbSnooze: TTrackBar;
    procedure btnOKClick(Sender: TObject);
    procedure cbAlertChange(Sender: TObject);
    procedure cbrunChange(Sender: TObject);
    procedure pnHighClick(Sender: TObject);
    procedure tbSnoozeChange(Sender: TObject);
  private

  public

  end;

var
  fSysSettings: TfSysSettings;

implementation

{$R *.lfm}

{ TfSysSettings }

procedure TfSysSettings.btnOKClick(Sender: TObject);
var
  c: TJSONConfig;
  cfgname: string;
begin
  cfgname := GetAppConfigFile(false);
  ForceDirectories(ExtractFileDir(cfgname));
  c := TJSONConfig.Create(nil);

  try
     c.Filename := cfgname;
     c.SetValue('/gui/popup', cbNotice.Checked);
     if not cbrun.Checked then
          c.SetValue('/system/app', '')
     else
          c.SetValue('/system/app', AnsiToUtf8(fnRun.FileName));

     c.SetValue('/gui/snooze', tbSnooze.Position);

     c.SetValue('/glucose/chigh', pnHigh.Color);
     c.SetValue('/glucose/csoonhigh', pnSoonHigh.Color);
     c.SetValue('/glucose/clow', pnLow.Color);
     c.SetValue('/glucose/cok', pnOK.Color);

     c.SetValue('/glucose/value', cbValue.Checked);
     c.SetValue('/glucose/trend', cbTrend.Checked);

     c.SetValue('/dose/alert', cbAlert.Checked);
     if cbOnTop.Checked then
          c.SetValue('/gui/window', ord(fsSystemStayOnTop))
     else
          c.SetValue('/gui/window', ord(fsNormal));
  except
   MessageDlg('Error', 'Could not load, or create, the configuration file. Please make sure your AppData folder is writeable.', mtError,
    [mbOK],0);
   Application.Terminate;
   Abort;
  end;
  c.free;
  Close;

end;

procedure TfSysSettings.cbAlertChange(Sender: TObject);
begin

end;

procedure TfSysSettings.cbrunChange(Sender: TObject);
begin
  fnRun.Enabled := cbrun.Checked;
end;

procedure TfSysSettings.pnHighClick(Sender: TObject);
begin
  with TColorDialog.Create(self) do begin
    Title := 'Select a color';
    Execute;
    (sender as Tpanel).Color:=color;
  end;
end;

procedure TfSysSettings.tbSnoozeChange(Sender: TObject);
begin
  lblSnooze.Caption := 'Snooze time: ' + IntToStr(tbSnooze.Position) + ' minutes';
end;

end.

