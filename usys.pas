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
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics, Dialogs,
  EditBtn, StdCtrls, ExtCtrls, ComCtrls, Spin, jsonConf;

type

  { TfSysSettings }

  TfSysSettings = class(TForm)
    btnOK: TButton;
    cbArrowRight: TRadioButton;
    cbArrowMix: TRadioButton;
    cbrun: TCheckBox;
    cbNotice: TCheckBox;
    cbOnTop: TCheckBox;
    cbValue: TCheckBox;
    cbTrend: TCheckBox;
    cbHover: TCheckBox;
    Label10: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    seHigh: TFloatSpinEditEx;
    fnRun: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lblSnooze: TLabel;
    pnHigh: TPanel;
    pnSoonHigh: TPanel;
    pnOK: TPanel;
    pnLow: TPanel;
    cbArrowLeft: TRadioButton;
    seOK: TFloatSpinEditEx;
    seLow: TFloatSpinEditEx;
    seHover: TSpinEdit;
    tbSnooze: TTrackBar;
    procedure btnOKClick(Sender: TObject);
    procedure cbAlertChange(Sender: TObject);
    procedure cbArrowLeftChange(Sender: TObject);
    procedure cbArrowLeftClick(Sender: TObject);
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
  t: integer;
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

//     t := c.GetValue('/gui/arrows', 0);
     if cbArrowRight.Checked then
       c.setValue('/gui/arrows', 1)
     else if cbArrowLeft.Checked then
       c.setValue('/gui/arrows', 2)
     else
       c.setValue('/gui/arrows', 0);

     if t <> c.GetValue('/gui/arrows', 0) then
       ShowMessage('Arrows changes will take effect after the next restart.');

     c.SetValue('/gui/hover', cbHover.Checked);
     c.SetValue('/gui/hovertrans', seHover.Value);

//     c.SetValue('/dose/alert', cbAlert.Checked);
     if cbOnTop.Checked then
          c.SetValue('/gui/window', ord(fsSystemStayOnTop))
     else
          c.SetValue('/gui/window', ord(fsNormal));

  if seHigh.DecimalPlaces > 0 then begin
    c.SetValue('/glucose/high', round(seHigh.Value * 18));
    c.SetValue('/glucose/low', round(seLow.Value * 18));
    c.SetValue('/glucose/ok', round(seOK.Value * 18));
  end else begin
     c.SetValue('/glucose/high', round(seHigh.Value));
     c.SetValue('/glucose/low', round(seLow.Value));
     c.SetValue('/glucose/ok', round(seOk.Value));
  end;
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

procedure TfSysSettings.cbArrowLeftChange(Sender: TObject);
begin

end;

procedure TfSysSettings.cbArrowLeftClick(Sender: TObject);
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

