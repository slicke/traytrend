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
unit uconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, jsonConf;

type

  { TfSettings }

  TfSettings = class(TForm)
    btnOK: TButton;
    edURL: TEdit;
    edSecret: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    rbMmol: TRadioButton;
    rbMgdl: TRadioButton;
    procedure btnOKClick(Sender: TObject);
    procedure cbrunChange(Sender: TObject);
  private

  public

  end;

var
  fSettings: TfSettings;

implementation

{$R *.lfm}

{ TfSettings }

procedure TfSettings.btnOKClick(Sender: TObject);
var
  c: TJSONConfig;
  cfgname: string;
begin
  cfgname := GetAppConfigFile(false);
  ForceDirectories(ExtractFileDir(cfgname));
  c := TJSONConfig.Create(nil);

  try
     c.Filename := cfgname;
     c.SetValue('/glucose/mmol', rbMmol.Checked);
     c.SetValue('/remote/url', edURL.Text);
     c.SetValue('/remote/key', edSecret.Text);
  except
   MessageDlg('Error', 'Could not load, or create, the configuration file. Please make sure your AppData folder is writeable.', mtError,
    [mbOK],0);
   Application.Terminate;
   Abort;
  end;
  c.free;
  Close;
end;

procedure TfSettings.cbrunChange(Sender: TObject);
begin

end;

end.

